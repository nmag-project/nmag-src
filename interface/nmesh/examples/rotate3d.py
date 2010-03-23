import nmesh

box1 = nmesh.box( [-1,-1,2],[1,1,4], \
                  transform=[("rotate3d",[0,0,1],45)])

ground = nmesh.box( [-1,-1,-0.3],[1,1,1] )

bbox = [[-2,-2,-0.3],[2,2,4]]

mesh = nmesh.mesh(objects=[ground,box1],bounding_box=bbox,a0=0.5)

mesh.save("rotate3d.nmesh")

# visualise in MayaVi
vis =nmesh.visual.show_bodies_mayavi(mesh)

nmesh.visual.export_visualisation(vis,"rotate3d.eps")
