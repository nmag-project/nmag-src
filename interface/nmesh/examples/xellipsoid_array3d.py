import nmesh

import os.path,sys

filename="ellipsoid_array3d.nmesh"
if not os.path.exists(filename):
    print "You need to run ellipsoid_array3d.py first to compute %s" % filename
    sys.exit(1)

mesh = nmesh.load(filename)

mymayavi = nmesh.visual.show_surfaces_mayavi(mesh)

nmesh.visual.export_visualisation(mymayavi,'ellipsoid_array3d.eps')

import time
print "Will sleep 3 seconds"
time.sleep(3)
