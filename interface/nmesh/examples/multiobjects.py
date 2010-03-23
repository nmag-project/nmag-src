import nmesh

# create a number of objects
one = nmesh.ellipsoid([3.0,3.0])
two = nmesh.ellipsoid([3.0,3.0],transform=[("shift",[7,0])])
three=nmesh.box( [-4.0,-6], [10,-4] )

bbox = [[-5.,-8.],[11.,5.]]

# create mesh of three objects and bounding box
mesh_ex = nmesh.mesh(objects = [one,two,three], bounding_box=bbox,
                     mesh_bounding_box=True)

# plot mesh
nmesh.visual.plot2d_ps(mesh_ex,"multiobjects.ps")


