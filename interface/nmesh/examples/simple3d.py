import nmesh

ellipsoid = nmesh.ellipsoid([0.75,1.25,1])

# create mesh
bbox = [[-0.75,-1.25,-1],[0.75,1.25,1]]

mesh = nmesh.mesh(objects = [ellipsoid], bounding_box=bbox,a0=0.5)

#create 3d-plot of surfaces and export eps
vis = nmesh.visual.show_bodies_mayavi(mesh)
nmesh.visual.export_visualisation(vis,"simple3d.eps")

#save mesh also as nmesh file
mesh.save('simple3d.nmesh')

