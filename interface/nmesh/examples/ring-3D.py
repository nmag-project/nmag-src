import nmesh, pylab, Numeric

rod = 1.2

outer_radius = 10.0
inner_radius = 6.0 
thickness = 2.0

import math

bbox = [[-10,-10,-thickness],[10,10,thickness]]


# external ring
R = nmesh.conic(
    [0.0,0.0,-thickness], outer_radius, [0.0,0.0,thickness], outer_radius,
    )
#internal ring
r = nmesh.conic(
    [0.0,0.0,-thickness], inner_radius, [0.0,0.0,thickness], inner_radius,
    )
# take the difference
ring = nmesh.difference(R,[r])

density = """density = 1.;"""

N = 10

# create mesh of three objects and bounding box
mesh_ex = nmesh.mesh(objects = [ring],
                     a0=rod,
                     bounding_box=bbox,
                    
                     density = density,

                     # with these values there is
                     # a bit more pressure than with
                     # the default values (as suggested
                     # by Thomas)
                     thresh_del = 1.9,
                     thresh_add = 0.4,

                     # it is important that the parameter
                     # "initial_settling_steps" is a
                     # large number so that the neighbour
                     # forces (default scale = 1.0)
                     # act almost alone at first
                     # and the nodes fill all the space,
                     # then towards the end the shape
                     # forces (default scalee = 0.1)
                     # do the the rest
                     initial_settling_steps = 1000,

                     max_relaxation = 4,
                     
                     max_steps=1000
                     )

mesh_ex.save("ring-3D.nmesh")

