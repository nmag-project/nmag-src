import nmesh

# create ellipsoid
ell = nmesh.ellipsoid([2.0,3.0],transform=[("shift",[0,4])])

bbox = [[-3.,0],[3.,8.]]

# define density string 
density = "density=1.0+5*x[1]*x[1]*0.02;"

# create mesh
mesh_ex = nmesh.mesh(objects = [ell], bounding_box=bbox,
                     mesh_bounding_box=True, density=density)

# plot mesh
nmesh.visual.plot2d_ps(mesh_ex,"density1.ps")

