# Ocaml will provide:
#
# * A function ocaml.get_mesh() which will return
#
#   a pair (coordinates, edges)
#   where coordinates is a list of 3d point coordinates, i.e.
#  [[0.0,1.0,2.0],[1.5,2.5,3.5],...]
# and edges is a list of point index pairs, i.e.
#  [(0,5),(3,4),(8,27),...]

import visual
import numarray as ny
import random 
import os

XXX-this-is-broken-right-now-interfaces-changed

length_scale=0.5
radius=1.0

mesh=ocaml.simple_lattice_mesh(2,8)

plotinfo=ocaml.mesh_plotinfo(mesh)

point_coords=plotinfo[0]
point_links=plotinfo[1]

nr_points=len(point_coords)

print "NR Points: ",nr_points

# generate a set of pairs (i, j)
# point_i & point_j will be connected
# in the mesh visualization

mat = 100* ny.array([[point_coords[i][0], point_coords[i][1],  0] for i in ny.arange(0,nr_points)])

# cancel all the objects in the last visualization
objects = visual.scene.objects
for obj in objects:
    obj.visible = 0
    
    
    
# visualize the present mesh
for point_indices in point_links:
    visual.cylinder(pos = mat[point_indices[0]],
                    axis = mat[point_indices[1]] - mat[point_indices[0]])
    
# wait for a key to be pressed; press 'q' to quit
#if raw_input("press 'q' to quit ") == 'q':
#    os._exit(0)
    
