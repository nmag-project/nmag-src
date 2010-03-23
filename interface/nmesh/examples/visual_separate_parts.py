"""Demonstrates the use of visual.separate_parts() to separate different
   constituent parts of a meshed assembly.

   Attempt to demonstrate the separate_parts() function.

   Author: James Kenny  Last modified: $Date$
"""
import nmesh
import nmesh.visual as viz

# define a simple mesh and submit request to mesher
box=nmesh.box( [0.0,0.0], [1.0,1.0] )
cone=nmesh.conic([3.0,0.0],1.0,[3.0,4.0],0.0)
bbox=[[-1.,-1.],[7.,6.]]
mesh=nmesh.mesh(objects=[box,cone],a0=0.4,bounding_box=bbox)
mesh_info=mesh.tolists()

# separate the triangle and square into different VTK datasets
[mesh_info1, mesh_info2] = viz.separate_parts(mesh_info, listOfParts=[(1,),(2,)])

# calculate inradius/circumradius and append part indices to vtk dataset1
vtkData, points, simplices, indices, icradii, ccradii=viz.mesh2vtk(mesh_info1,VTKonly=False)
in2circ=viz.findRatios(icradii, ccradii, factor=2)
vtkData=viz.append2vtk(vtkData, indices, "part indices")
vtkData=viz.append2vtk(vtkData, in2circ, "in2circ")
viz.save_vtk(vtkData, "temp_1.vtk")

# calculate inradius/circumradius and append part indices to vtk dataset2
vtkData2, points2, simplices2, indices2, icradii2, ccradii2=viz.mesh2vtk(mesh_info2,VTKonly=False)
in2circ2=viz.findRatios(icradii2, ccradii2, factor=2)
vtkData2=viz.append2vtk(vtkData2, indices2, "part indices")
vtkData2=viz.append2vtk(vtkData2, in2circ2, "in2circ")
viz.save_vtk(vtkData2, filename="temp_2.vtk")

# display the data in MayaVi and open the 'Configure Data' dialog
myv = viz.mesh2mayavi(input="temp_2.vtk")
myv.renwin.z_plus_view()
dvm = myv.get_current_dvm()
dvm.data_src.configure()


