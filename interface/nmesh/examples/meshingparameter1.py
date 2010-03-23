import nmesh

ellipsoid = nmesh.ellipsoid([1.25,0.75])

bbox = [[-1.25,-0.75],[1.25,0.75]]
#call the mesher with modified parameter object
mesh = nmesh.mesh(objects = [ellipsoid], bounding_box=bbox,a0=0.5,\
                  max_steps=20,                   #modify meshing parameter
                  sliver_correction=1.1           #modify meshing parameter
                  )


