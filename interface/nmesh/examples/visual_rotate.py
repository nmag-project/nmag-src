"""This is a minimal demonstration of the nmesh.visual module.
   Shows off the rotate() function

   Author: James Kenny   Last modified: $Date$
"""
import nmesh
import nmesh.visual as viz


# define a simple mesh and submit request to mesher
box=nmesh.box( [0.0,0.0], [1.0,1.0] )
cone=nmesh.conic([3.0,0.0],1.0,[3.0,4.0],0.0)
bbox=[[-1.,-1.],[7.,6.]]
mesh=nmesh.mesh(objects=[box,cone], a0=0.4, bounding_box=bbox)
mesh_info=mesh.tolists()

# draw the mesh in simplest form
myv = viz.basic_solid(mesh_info)


# show off the rotate() function
for i in range(180):
    viz.rotate(myv,roll=2)

for i in range(80):
    viz.rotate(myv,pitch=-1)
for i in range(160):
    viz.rotate(myv,pitch=1)
for i in range(80):
    viz.rotate(myv,pitch=-1)


for i in range(180):
    viz.rotate(myv,yaw=2)

for i in range(80):
    viz.rotate(myv,roll=2, pitch=1)
myv.renwin.z_plus_view()

for i in range(360):
    viz.rotate(myv,roll=2, yaw=2)
myv.renwin.z_plus_view()    

for i in range(80):
    viz.rotate(myv, 1, 1, 1)
myv.renwin.z_plus_view()
    


