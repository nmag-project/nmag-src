"""This script demonstrates the function 'separate_parts()' compounded with
   'outer_skin()'.
   In the first dataset displayed a triangle and square are present. In the
   second dataset the square has been removed. 

   Author: James Kenny  Last modified: $Date$
"""
import nmesh
import nmesh.visual as viz


# define a simple mesh and submit request to mesher
box=nmesh.box( [0.0,0.0], [1.0,1.0] )
cone = nmesh.conic([3.0,0.0],1.0,[3.0,4.0],0.0)
bbox = [[-1.,-1.],[7.,6.]]
mesh = nmesh.mesh(objects = [box,cone],a0=0.4, bounding_box=bbox)
mesh_info=mesh.tolists()




# create mesh_info lists for parts 1&2 combined and for part 2 alone
[mesh_info1, mesh_info2] = viz.separate_parts(mesh_info, listOfParts=[[1,2],[2]])


# generate a VTK dataset for parts 1&2 combined
vtkData, points, simplices,indices, icradii, ccradii = \
         viz.mesh2vtk(mesh_info1, VTKonly=False)
in2circ=viz.findRatios(icradii, ccradii, factor=2)
vtkData=viz.append2vtk(vtkData, in2circ, "in2circ")
vtkData=viz.append2vtk(vtkData, indices, "part indices")
viz.save_vtk(vtkData, "compound_example_1.vtk")


# create VTK data for part 2 alone and apply outer_skin() to it
# request that only cells with two or more points on the surface are shown
mesh_info2 = viz.outer_skin(mesh_info2, condition='>=2')
vtkData2, points2, simplices2, indices2, icradii2, ccradii2 = \
          viz.mesh2vtk(mesh_info2, VTKonly=False)
in2circ2=viz.findRatios(icradii2, ccradii2, factor=2)
vtkData2=viz.append2vtk(vtkData2, in2circ2, "in2circ")
vtkData2=viz.append2vtk(vtkData2, indices2, "part indices")
viz.save_vtk(vtkData2, "compound_example_2.vtk")


# display in MayaVi, reverse LUT and open slider bar to choose between datasets
myv = viz.mesh2mayavi("compound_example_1.vtk",lut_range=(0,1))
dvm = myv.get_current_dvm()
mm = dvm.get_current_module_mgr()
lut_handler = mm.get_scalar_lut_handler()
lut_handler.set_lut_red_blue()
lut_handler.sc_bar.SetVisibility(1)
lut_handler.sc_bar.GetTitleTextProperty().SetShadow(1)
lut_handler.sc_bar.GetLabelTextProperty().SetShadow(1)

# overlay a wireframe
m1 = myv.load_module('SurfaceMap',0)
m1.actor.GetProperty().SetRepresentationToWireframe()
m1.mapper.SetScalarVisibility(0)

myv.renwin.z_plus_view()
dvm.data_src.configure()




