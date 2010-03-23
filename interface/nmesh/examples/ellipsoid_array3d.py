import nmesh

nx = 3 #how many ellipsoids in x direction
x0 = 3. #spacing of ellipsoids in x
ny = 3 #how many ellipsoids in y direction
y0 = 3. #spacing of ellipsoids in x
nz = 3 #how many ellipsoids in y direction
z0 = 3. #spacing of ellipsoids in x
rx,ry,rz = 0.8,1.2,1#radii of ellipsoids

#create list 'objects' with ellipsoids
objects = []
for i in range(nx):
    for j in range(ny):
        for k in range(nz):
            objects.append( nmesh.ellipsoid([rx,ry,rz], [("shift",[i*x0,j*y0,k*z0])]))

#bounding box
bbox = [[-2,-2,-2],[(nx-1)*x0+2,(ny-1)*y0+2,(nz-1)*z0+2]]

#create the mesh
mesh = nmesh.mesh(objects=objects,a0=0.75,bounding_box=bbox)
#save plot to file
mesh.save("ellipsoid_array3d.nmesh")

#create 3d-plot of surfaces and export eps
vis = nmesh.visual.show_bodies_mayavi(mesh)
nmesh.visual.export_visualisation(vis,"ellipsoid_array3d.eps")
