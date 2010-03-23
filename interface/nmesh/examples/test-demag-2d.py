import nmesh, Numeric

rod = 10.
edge = 100

import math

bbox = [[-edge,-edge],[edge,edge]]


# box
box = nmesh.box([-edge,-edge],[edge,edge])

density = """density = 1.;"""

N = 10

# create mesh of three objects and bounding box
mesh_ex = nmesh.mesh(objects = [box],
                     a0=rod,
                     bounding_box=bbox,
                    
                     density = density,
                     )

mesh_ex.save("test-demag-2d.nmesh")
