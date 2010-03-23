import nmesh, math, sys

rod = 10.
edge = 100.
thickness = 8.

import math

readfile = open("run_test-demag-2d/test-demag-2d.nmesh", "r")

contentfile =  readfile.readlines()

readfile.close()

coord_nodes = []
nr_nodes = int(contentfile[2])
for i in range(nr_nodes):

    offset = 3 # starting line of nodes coordinates
    x, y = contentfile[offset+i].split()

    fx, fy = float(x), float(y)
    coord_nodes.append([fx, fy])

mobile_pts = []


nr_layers = 1
layer_thick = thickness/float(nr_layers)

for node in coord_nodes:
    x, y = node
    
    for i in range(nr_layers+1):
        layer_heigth = -thickness/2. + layer_thick*i

        mobile_pts.append([x, y, layer_heigth])
        

bbox = [[-edge,-edge,-thickness/2.],[edge,edge,thickness/2.]]


# box
box = nmesh.box([-edge,-edge,-thickness/2.],[edge,edge,thickness/2.])

density = """density = 1.;"""
# create mesh of three objects and bounding box
mesh_ex = nmesh.mesh(objects = [box],
                     cache_name = "",
                     a0=rod,
                     mobile_points = mobile_pts,
                     bounding_box=bbox,
                     density = density,
                     neigh_force_scale = 0.0,
                     shape_force_scale = 0.0,
                     max_steps = 1
                     )

mesh_ex.save("box-2d-to-3d.nmesh")


