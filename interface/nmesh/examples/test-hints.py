import nmesh

ellipsoid = nmesh.ellipsoid([1.25,0.5])

bbox = [[-1.25,-0.75],[10.25,0.75]]

#create a mesh to be saved and then reloaded with another name
mesh2save = nmesh.mesh(objects = [ellipsoid] ,bounding_box=bbox,a0=0.2)

# save the mesh 
mesh2save.save("saved4hint.nmesh")

# reload the mesh with another name
loaded_mesh=nmesh.load("saved4hint.nmesh")

# define a new object
square = nmesh.box([-1.25,-0.75],[1.25,0.75], transform=[("shift",[5.0,0.0])]) 

# first we create a mesh out of the simplices of the loaded mesh which satisfy the
# boundary conditions related to the ellipsoid object. If we
# define some other object, the mesher will triangulate only the
# simplices of the loaded mesh which are within the new boundary conditions.
# the square is meshed after all the loaded_objects are meshed
mesh2 = nmesh.mesh(objects = [square], hints=[(loaded_mesh,ellipsoid)] ,bounding_box=bbox,a0=0.2)

mesh2.save("hint.nmesh")
