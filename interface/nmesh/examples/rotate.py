import nmesh

# rotation of the object is performed
# around the [0,0,1] axis (z-axis). 
box = nmesh.box( [0,0,0],[2,1,1], transform=[("rotate",[0,1],0)])

bbox = [[-1,-1,-1],[3,3,3]]
mesh = nmesh.mesh(objects=[box],bounding_box=bbox,mesh_bounding_box=False,a0=0.5)

mesh.save('rotate.nmesh')

#create 3d-plot of surfaces and export eps
vis=nmesh.visual.show_bodies_mayavi(mesh)
nmesh.visual.export_visualisation(vis,"rotate.eps")

