import nmesh

ellipsoid = nmesh.ellipsoid([1.25,0.75])

bbox = [[-1.25,-0.75],[1.25,0.75]]

mesh = nmesh.mesh(objects = [ellipsoid], bounding_box=bbox,a0=0.5)

mesh.save("simplemesh.nmesh")

meshinfo = mesh.tolists()

