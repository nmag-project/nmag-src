import nmesh

ellipsoid = nmesh.ellipsoid([1.25,0.75])

bbox = [[-1.25,-0.75],[1.25,0.75]]

mesh = nmesh.mesh(objects = [ellipsoid], bounding_box=bbox,a0=0.2)

nmesh.visual.plot2d_ps(mesh,"simple2d.ps")
mesh.save("simple2d.nmesh")
