import nmesh


nx = 2  #how many ellipsoids in x direction
x0 = 3. #spacing of ellipsoids in x
ny = 3  #how many ellipsoids in y direction
y0 = 3. #spacing of ellipsoids in x
rx,ry = 1,1.5 #radii of ellipsoids
angle = 45    #orientation of ellipsoids

#create list 'objects' with ellipsoids
objects = []
for i in range(nx):
    for j in range(ny):
        objects.append( nmesh.ellipsoid([rx,ry], \
                                        [("rotate2d",angle),\
                                         ("shift",[i*x0,j*y0])]))

#bounding box
bbox = [[-2,-2],[(nx-1)*x0+2,(ny-1)*y0+2]]

#create the mesh
mesh = nmesh.mesh(objects=objects,a0=0.5,bounding_box=bbox)

#Create post script plot of mesh
nmesh.visual.plot2d_ps(mesh,"ellipsoid_array.ps")

#save plot to file
mesh.save('ellipsoid_array.nmesh')
