import nmesh

ellipsoid = nmesh.ellipsoid([1.25,0.75])

bbox = [[-1.25,-0.75],[1.25,0.75]]

#get default parameters:
meshing_parameters=nmesh.get_default_meshing_parameters()

#modify one or more
meshing_parameters.set_max_steps(20)
meshing_parameters.set_sliver_correction(1.1)

#Note that meshing_parameter objcets provide a number of 'set_'
#methods to set meshing parameters. These provide documentation and
#may be usefully explored in ipython to learn about the meaning of
#each parameter.

#call the mesher with modified parameter object
mesh = nmesh.mesh(objects = [ellipsoid], bounding_box=bbox,a0=0.5,\
                  meshing_parameters=meshing_parameters)


