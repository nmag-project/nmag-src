import nmesh


# create square
box = nmesh.box([0.0,0.0],[1.0,1.0])

# create cone
cone = nmesh.conic([3.0,0.0],1.0,[3.0,4.0],0.0)

rod= 0.4
bbox = [[-1.,-1.],[7.,6.]]

# create mesh and save it in a file every 50 steps
# during its construction; with the show flag it
# is possible also to visualise it. 
mesh_ex = nmesh.mesh(objects = [box,cone], a0=rod, bounding_box=bbox)

#visualisation missing


# plot mesh
nmesh.visual.plot2d_ps(mesh_ex,"visual_timeseries2d_mesh.ps")
