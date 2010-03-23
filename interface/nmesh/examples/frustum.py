import nmesh

P2 = [0,4]  #center of 'circle' 2
R2 = 3      #radius of 'circle' 2
P1 = [0,0]  #center of 'circle' 1
R1 = 5      #radius of 'circle' 1


frustum = nmesh.conic( P1, R1, P2, R2 )

bbox = [[-5,0],[5,4]]

mesh = nmesh.mesh(objects=[frustum], bounding_box=bbox)

nmesh.visual.plot2d_ps( mesh, "frustum.ps")
