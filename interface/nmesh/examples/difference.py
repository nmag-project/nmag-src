import nmesh

big = nmesh.ellipsoid([4.0,3.0])    # create a big ellipsoid
small = nmesh.ellipsoid([3.0,2.0])  # small ellipsoid

diff = nmesh.difference(big,[small])# create difference of ellipsoids 

bbox = [[-5.,-4.],[5.,4.]]
mesh_ex = nmesh.mesh(objects = [diff], a0=0.4, bounding_box=bbox)

# plot mesh
nmesh.visual.plot2d_ps(mesh_ex,"difference.ps")


