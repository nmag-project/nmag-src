import nmesh

x = nmesh.box( [0,0],[5,5], transform=[("rotate2d",30)])

bbox = [[-10,-10],[10,10]]
mesh = nmesh.mesh(objects=[x],bounding_box=bbox,mesh_bounding_box=True)

nmesh.visual.plot2d_ps(mesh,"rotate2d.ps")
