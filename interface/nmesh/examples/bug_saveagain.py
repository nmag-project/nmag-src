import nmesh


# Need to run save_load.py first to create the data for this example

#load mesh from file
mesh_loaded = nmesh.load("save_load.nmesh")

#and save mesh again 
nmesh.save(mesh_loaded,"test.nmesh")



