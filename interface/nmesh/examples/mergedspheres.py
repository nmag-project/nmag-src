import nmesh

nx = 2 #how many ellipsoids in x direction
x0 = 20 #spacing of ellipsoids in x
ny = 2 #how many ellipsoids in y direction
y0 = 20 #spacing of ellipsoids in x
rx,ry = 10.5,10.5 #radii of ellipsoids
angle = 0    #orientation of ellipsoids

#create list 'objects' with ellipsoids
objects = []
for i in range(nx):
    for j in range(ny):
        objects.append( nmesh.ellipsoid([rx,ry], [("rotate2d",angle),
                                                ("shift",[i*x0,j*y0])]))

union = nmesh.union(objects)

#bounding box
bbox = [[-15,-15],[(nx-1)*x0+15,(ny-1)*y0+15]]

#create the mesh
mesh = nmesh.mesh(objects=[union],a0=2,bounding_box=bbox)

#save plot to file
nmesh.visual.plot2d_ps(mesh,"mergedspheres.ps")

