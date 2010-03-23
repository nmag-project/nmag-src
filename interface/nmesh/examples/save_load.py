import nmesh


# create square
box = nmesh.box( [0.0,0.0], [1.0,1.0] )

# crete cone
cone = nmesh.conic([3.0,0.0],1.0,[3.0,4.0],0.0)

bbox = [[0,0],[4,4]]

# create mesh 
mesh = nmesh.mesh(bounding_box=bbox, objects = [box,cone], a0=0.4)

# save mesh in current directory
mesh.save("save_load.nmesh",directory=".")

# load mesh from file
mesh_loaded = nmesh.load("save_load.nmesh")

# plot loaded mesh
nmesh.visual.plot2d_ps(mesh_loaded,"save_load.ps")


