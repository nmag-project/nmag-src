
import os
import nmesh

def create_mesh():

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
    mesh = nmesh.mesh(objects=objects,a0=0.75,bounding_box=bbox,mesh_bounding_box=True)
    #save plot to file
    mesh.save("test.nmesh",directory='.')

    return mesh


import time

starttime = None

def tic():
    global starttime
    starttime = time.time()

def toc():
    runtime = time.time()-starttime
    print "--> %5.2f seconds" % runtime,
    return runtime




if os.path.exists("test.nmesh"):
    print "%40s" % "loading mesh",; tic();
    mesh = nmesh.load("test.nmesh")
    toc()
else:
    mesh = create_mesh()







import ocaml

#total_time:
tt = 0.

print "%40s" % "grow_bookkeeping_data",; tic(); ocaml.mesh_grow_bookkeeping_data_all; tt += toc(); print "(cum: %5.2f)"%tt
print "%40s" % "getting points",; tic(); mesh.points; tt += toc(); print "(cum: %5.2f)"%tt
print "%40s" % "getting pointsregions",; tic(); mesh.pointsregions; tt += toc(); print "(cum: %5.2f)"%tt
print "%40s" % "getting simplices",; tic(); mesh.simplices; tt += toc(); print "(cum: %5.2f)"%tt
print "%40s" % "getting simplicesregions",; tic(); mesh.simplicesregions; tt += toc(); print "(cum: %5.2f)"%tt
print "%40s" % "getting links",; tic(); mesh.links; tt += toc(); print "(cum: %5.2f)"%tt
print "%40s" % "getting surfaces",; tic(); mesh.surfaces; tt += toc(); print "(cum: %5.2f)"%tt
print "%40s" % "getting surfacesregions",; tic(); mesh.surfacesregions; tt += toc(); print "(cum: %5.2f)"%tt
print "%40s" % "getting regionvolumes",; tic(); mesh.regionvolumes; tt += toc(); print "(cum: %5.2f)"%tt

print "%40s" % "getting meshinfo (includes calculation of qualities)",;tic();mesh.tolists();tt += toc(); print "(cum: %5.2f)"%tt




