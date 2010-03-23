import nmesh

radius = 4
sphere = nmesh.ellipsoid([radius,radius])

bbox = [[-5.,-5.],[5.,5.]]

# periodicity on x-axis
mesh_ex = nmesh.mesh(objects = [sphere], bounding_box=bbox, 
		     mesh_bounding_box=True,periodic=[True,False])

nmesh.visual.plot2d_ps(mesh_ex,"periodic.ps")


mesh_ex.save('periodic.nmesh')
