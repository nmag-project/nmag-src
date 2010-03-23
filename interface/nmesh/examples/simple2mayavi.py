import nmesh
ellipsoid = nmesh.ellipsoid([1.25,0.75,1])
bbox = [[-1.25,-0.75,-1],[1.25,0.75,1]]
mesh = nmesh.mesh(objects = [ellipsoid], bounding_box=bbox,a0=0.5)
nmesh.visual.show_bodies_mayavi(mesh)
raw_input("press key to close MayaVi")

