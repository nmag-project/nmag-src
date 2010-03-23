import nmesh

C_bottom = [0,0,0.0]  #center of spiral
R_spiral = 4        #radius of spiral
C_top = [0,0,10]    #top of spiral
R_circle = 2        #radius of max circle along the spiral

helix = nmesh.helix( C_bottom, R_spiral, C_top, R_circle )

bbox = [[-8,-8,-1],[8,8,11]]

rod = 0.5

mesh = nmesh.mesh(objects = [helix], a0=rod, bounding_box=bbox,
                  max_steps=1500
		  )

#save to file
mesh.save("helix.nmesh")

#create 3d-plot of surfaces and export eps
vis = nmesh.visual.show_bodies_mayavi(mesh)
nmesh.visual.rotate(vis, roll=0, pitch=10, yaw=170)
nmesh.visual.export_visualisation(vis,"helix.eps")


