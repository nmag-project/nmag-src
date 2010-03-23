import nmesh

box = nmesh.box([0.0,0.0], [1.0,1.0],                  \
                transform=[("rotate2d",45),            \
                           ("shift",[-1.0,-2.0]),      \
                           ("scale",[1.5,1.5])])       
# create ellipsoid
ell = nmesh.ellipsoid([1.0,2.0],                       \
                      transform=[("rotate2d",45),      \
                                 ("shift",[1.0,1.0]),])

rod= 0.5
bbox = [[-3.,-3.],[4.,4.]]

# create mesh
mesh = nmesh.mesh(objects = [box,ell], a0=rod, bounding_box=bbox)

# plot mesh
nmesh.visual.plot2d_ps(mesh,"transformations.ps")
