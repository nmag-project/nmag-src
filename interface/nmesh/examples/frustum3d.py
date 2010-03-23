import nmesh
frustum = nmesh.conic([0.0,0.0,0.0],1.0,[2.0,0.0,0.0],2.0)

bbox = [[-2.,-2.,-2.0],[2.,2.,2.]]

mesh = nmesh.mesh(objects = [frustum], bounding_box=bbox)

#create 3d-plot of surfaces
vis = nmesh.visual.show_bodies_mayavi(mesh)

import time
time.sleep(5)
#and save to eps file
nmesh.visual.export_visualisation(vis,"frustum3d.eps")

