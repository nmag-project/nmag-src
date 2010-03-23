"""
This module contains functions to generate unidimensional meshes.
"""

import sys
from lib1 import mesh_from_points_and_simplices

nmesh_version_str = "# PYFEM mesh file version 1.0"

def write_mesh(mesh, out=None, check=True, float_fmt=" %f"):
  """
  This function takes a mesh in the form of:
    mesh =  (list_points, list_simplices, list_surfaces)
  and writes to file the mesh in nmesh-format. 
  """
  if type(out) == str:
    f = open(out, "w")
    def write(string): f.write(string)
    def close(): f.close()
  elif type(out) == file:
    def write(string): out.write(string)
    def close(): out.close()
  elif out == None:
    def write(string): print string,
    def close(): return

  points, simplices, surfaces = mesh
  write(nmesh_version_str)
  ndim  = len(points[0])
  npts  = len(points)
  nsim  = len(simplices)
  nsurf = len(surfaces)
  write( "\n# dim = %d \t nodes = %d \t simplices = %d \t surfaces = %d " \
         "\t periodic = 0\n" % (ndim, npts, nsim, nsurf) )

  write("%d\n" % npts)
  for p in points:
    if check:
      if len(p) != ndim:
        raise "Found point with wrong dimension inside the given list."
    for x in p: write( float_fmt % x )
    write("\n")

  write("%d\n" % nsim)
  d =  ndim+1
  for (body, s) in simplices:
    if check:
      if len(s) != d:
        raise "Found simplex with wrong dimension inside the given list."
    write(" %d" % body)
    for n in s: write( " %d" % n )
    write("\n")

  write("%d\n" % nsurf)
  d =  ndim
  for (body, s) in surfaces:
    if check:
      if len(s) != d:
        raise "Found surface simplex with wrong dimension."
    write(" %d" % body)
    for n in s: write( " %d" % n )
    write("\n")
  write("0\n")
  close()

def generate_1d_mesh_components(regions, discretization,
		                tolerance=lambda x: x):
  """
  This function generates a unidimensional mesh starting
  from the list of regions and the given discretization.
  """
  nbody = 0
  points = []
  simplices = []
  surfaces = []
  pnt_hash = {}
  srf_hash = {}

  def add_point(y):
    i = len(points)
    x = tolerance(y)
    try:
      return pnt_hash[x]
    except KeyError:
      pnt_hash[x] = i
      points.append( [x] )
      return i

  def add_surface(y, idx, body):
    try:
      i, _, _ = srf_hash[y]
      srf_hash[y] = (i+1, body, idx)
    except KeyError:
      srf_hash[y] = (0, body, idx)

  for (left_x, right_x) in regions:
    nbody += 1
    if left_x > right_x: left_x, right_x = right_x, left_x
    width = right_x - left_x
    num_pts_per_reg = abs(int(width/discretization))
    step = width/num_pts_per_reg

    last_idx = add_point(left_x)
    add_surface(left_x, last_idx, nbody)
    for i in range(1, num_pts_per_reg+1):
      idx = add_point(left_x + i*step)
      simplices.append( (nbody, [last_idx, idx]) )
      last_idx = idx

    add_surface(right_x, idx, nbody)

  for s in srf_hash:
    count, body, idx = srf_hash[s]
    if count == 0:
      surfaces.append( (body, [idx]) )

  return (points, simplices, surfaces)

def generate_1d_mesh(regions, discretisation):
  """Return a new mesh from the specifications of the regions and the
  discretisation length. Example:
	  
    m = generate_1d_mesh([(0.0, 5.0), (5.0, 6.0)], 0.5)
    
  Generates a 1D mesh composed by two regions. The left one goes
  from x=0.0 to x=5.0, the right one occupies x=5.0 to x=6.0.
  The regions are subdivided in simplices which have all equal
  length delta_x=0.5."""
  points, simplices, surfaces = \
    generate_1d_mesh_components(regions, discretisation)
  simplices_indices = [indices for _, indices in simplices]
  simplices_regions = [region for region, _ in simplices]
  m = mesh_from_points_and_simplices(points=points,
                                     simplices_indices=simplices_indices,
                                     simplices_regions=simplices_regions,
                                     periodic_point_indices=[],
                                     initial=0, 
                                     do_reorder=False,
                                     do_distribute=False)
  return m

# Just for compatibility
mesh_1d = generate_1d_mesh_components

