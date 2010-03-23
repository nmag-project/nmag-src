"""Demonstrates how meshing the outer box can have effects on meshing
   of principal parts.

   The user should see that when the outer-box is meshed the triangle
   part comprises an additional 'surface element' which should not
   actually be there.

   Author: James Kenny    Last modified: $Date$
"""
import nmesh
import nmesh.visual as viz


# define a simple mesh and submit request to mesher (include outer box)
box=nmesh.box( [0.0,0.0], [1.0,1.0] )
cone=nmesh.conic([3.0,0.0],1.0,[3.0,4.0],0.0)
bbox=[[-2.,-2.],[7.,6.]]
rod=0.4
mesh=nmesh.mesh(objects=[box,cone],a0=rod,bounding_box=bbox,
                mesh_bounding_box=True)
mesh_info=mesh.tolists()


# visualise in MayaVi, using the ratio of 2*inradius:circumradius
# as cell_data for a colour scale
mesh_info = viz.surface_only(mesh_info)
vtkData = viz.mesh2vtk(mesh_info)
viz.save_vtk(vtkData, "outerbox_bug_1.vtk")


########### NOW DO THE SAME, BUT WITHOUT THE OUTER BOX ################


# define a simple mesh and submit request to mesher 
box=nmesh.box( [0.0,0.0], [1.0,1.0] )
cone=nmesh.conic([3.0,0.0],1.0,[3.0,4.0],0.0)
bbox=[[-2.,-2.],[7.,6.]]
rod=0.4
mesh2=nmesh.mesh(objects=[box,cone],a0=rod,bounding_box=bbox)
mesh_info2=mesh2.tolists()


# visualise in MayaVi, using the ratio of 2*inradius:circumradius
# as cell_data for a colour scale
mesh_info2 = viz.surface_only(mesh_info2)
vtkData2 = viz.mesh2vtk(mesh_info2)
viz.save_vtk(vtkData2, "outerbox_bug_2.vtk")


# display the meshes such that they can be both viewed by moving a slider bar
myv = viz.mesh2mayavi("outerbox_bug_1.vtk",lut_range=(0,1))
myv.renwin.z_plus_view()
dvm = myv.get_current_dvm()
dvm.data_src.configure()


