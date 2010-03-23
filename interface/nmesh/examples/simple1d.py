import nmesh

ellipse = nmesh.ellipsoid([1])

bbox = [[-2],[2]]

mesh = nmesh.mesh(objects = [ellipse], bounding_box=bbox, a0 = 0.5,
                  mesh_bounding_box=True)

mesh.save("simple1d.nmesh")
