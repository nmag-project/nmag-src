"""Simple visualisation of a 3D mesh.

   Author: James Kenny   Last modified: $Date$
"""
import nmesh
import nmesh.visual as viz


# define a simple mesh and submit request to mesher
box=nmesh.box( [3.0,0.0,0.0], [4.0,1.0,1.0] )
cone = nmesh.conic([0.0,0.0,0.0],0.0,[2.0,0.0,0.0],2.0)
bbox = [[-4.,-4.,-4.0],[4.,4.,4.]]
mesh = nmesh.mesh(objects = [box,cone], bounding_box=bbox)
mesh_info=mesh.tolists()

# visualise in MayaVi, using the ratio of inradius:circumradius
# as cell_data for a colour scale
v = viz.solid_in2circ(mesh_info, order=1)
