import nmesh

a=2.3

P1 = [0,0,0] #one corner of box
P2 = [a,a,a]#other corner
box = nmesh.box( P1,P2, use_fixed_corner_points=True)

bbox = [[0,0,0],[a,a,a]]

mesh = nmesh.mesh(bounding_box=bbox, objects=[box])

mv=nmesh.visual.show_bodies_mayavi( mesh )

nmesh.visual.export_visualisation(mv,'box_with_fixed_corner_points.eps')
