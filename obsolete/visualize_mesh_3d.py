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

# generate a set of pairs (i, j)
# point_i & point_j will be connected
# in the mesh visualization

ix = ny.array([[i,i+1] for i in range(0,99)])
s = 0

while True:

    s+=1
    # generate a set of random points (x,y,z) 
    random.seed(s)
    mat = 100* ny.array([[random.random(), random.random(),  random.random()] for i in ny.arange(0,100)])

    # cancel all the objects in the last visualization
    objects = visual.scene.objects
    for obj in objects:
        obj.visible = 0

    # visualize the present mesh
    for index in ix:
        visual.sphere(pos = mat[index[0]], radius = 10)
        visual.cylinder(pos = mat[index[0]], axis = mat[index[0]] - mat[index[1]])

    # wait for a key to be pressed; press 'q' to quit
    if raw_input("press 'q' to quit ") == 'q':
        os._exit(0)

