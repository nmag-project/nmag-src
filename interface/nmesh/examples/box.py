import nmesh

P1 = [0,0] #one corner of box
P2 = [5,10]#other corner
box = nmesh.box( P1,P2 )

bbox = [[0,0],[5,10]]

mesh = nmesh.mesh(bounding_box=bbox, objects=[box])

nmesh.visual.plot2d_ps( mesh, "box.ps")
