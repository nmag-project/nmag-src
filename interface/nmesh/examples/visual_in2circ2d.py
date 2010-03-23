"""This is a minimal demonstration of the nmesh.visual module.
   Creates a simple 2D mesh and visualises it as a solid with
   elements coloured by the metric:
       2*inradius/circumradius
   which =1 for an equilateral triangle.

   Author: James Kenny   Last modified: $Date$
"""
import nmesh
import nmesh.visual as viz

# define a simple mesh and submit request to mesher
box=nmesh.box( [0.0,0.0], [1.0,1.0] )
cone=nmesh.conic([3.0,0.0],1.0,[3.0,4.0],0.0)
bbox=[[-1.,-1.],[7.,6.]]
mesh=nmesh.mesh(objects=[box,cone],a0=0.4,bounding_box=bbox)
mesh_info=mesh.tolists()

# visualise in MayaVi, using the ratio of inradius:circumradius
# as cell_data for a colour scale
myv = viz.solid_in2circ(mesh_info)

#viz.export_visualisation(myv, "exampleA.png")  # save a PNG
#viz.export_visualisation(myv, "exampleA.eps")  # save a .eps.gz

