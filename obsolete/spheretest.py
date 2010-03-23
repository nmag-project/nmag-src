# (C) 2005 Dr. Thomas Fischbacher

import math

v_magnet = [1.0,0.0]
dim=len(v_magnet)
mesh_order = 1
r_sphere = 1.0
r_sphere_outer = 1.2
r_box = 3.0
rod_length = 0.7

mdefaults=ocaml.mesher_defaults
driver=ocaml.mesher_default_driver

the_mesh = ocaml.mesh_sphere_in_box(driver,mdefaults, dim, r_sphere, r_box, rod_length)

meshed_laplace = ocaml.mesh_laplace(the_mesh, mesh_order)

def magnetization(pos):
    r = math.sqrt(reduce((lambda sf,x: sf+x*x),pos,0))
    r0 = (r_sphere_outer-r)/(r_sphere_outer-r_sphere)
    r0 = min(r0,1.0)
    r0 = max(r0,0.0)
    return (map((lambda x:x*r0),v_magnet))

mumag_solution = ocaml.solve_mumag(meshed_laplace,magnetization)

# Note 1: this is a function!
# Note 2: evaluation of this function at present is excessively costly
# This can be changed, however! (Right now, it's proof-of-concept only.)

for x0 in range(-32,36,4):
    x=x0/10.0
    for y0 in range(-32,36,4):
        y=y0/10.0
        v = mumag_solution([x,y])
        print x,y,v[0],v[1]

