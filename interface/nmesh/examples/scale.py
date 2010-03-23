import nmesh

x = nmesh.box( [0,0],[1,1], transform=[("scale",[10,2])])

bbox = [[-2,-2],[12,4]]
mesh = nmesh.mesh(objects=[x],bounding_box=bbox)

nmesh.visual.plot2d_ps(mesh,"scale.ps")
