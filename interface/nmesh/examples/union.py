import nmesh

box = nmesh.box( [-4,-2],[4,2] )
ellipsoid = nmesh.ellipsoid([3,3])

# create union
union = nmesh.union([box,ellipsoid])

bbox = [[-5,-4],[5,4]]
mesh = nmesh.mesh(objects = [union],bounding_box=bbox)
mesh.save("union.nmesh")
nmesh.visual.plot2d_ps(mesh,"union.ps")


