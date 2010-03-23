import nmesh

ellipsoid = nmesh.ellipsoid([0.75,1.25,1])

# create mesh
bbox = [[-0.75,-1.25,-1],[0.75,1.25,1]]

mesh = nmesh.mesh(objects = [ellipsoid], bounding_box=bbox,a0=0.5)

# extract the mesh data in a list of lists
mesh_info = mesh.tolists()



    




