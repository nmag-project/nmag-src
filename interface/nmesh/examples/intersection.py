import nmesh

ell = nmesh.ellipsoid([3,2])            # create ellipsoid
cone = nmesh.conic([-3,0],2,[3,0],0)    # create cone

inters = nmesh.intersect([ell, cone])   # create intersection of objects 

bbox = [[-5.,-4.],[5.,4.]]
mesh_ex = nmesh.mesh(objects = [inters], a0=0.4, bounding_box=bbox)

nmesh.visual.plot2d_ps(mesh_ex,"intersection.ps")


