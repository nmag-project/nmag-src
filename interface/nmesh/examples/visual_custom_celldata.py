"""Creates a simple 2D mesh, creates a vtkUnstructuredGrid using only the
   surface elements (lines in this case). Demonstrates how to calculate and
   append a custom scalar data set and visualises in MayaVi as a SurfaceMap.

   Additionally, the 'part indices' are appended and the 'Configure Data'
   dialog is opened so the user can experiment choosing between datasets.

   Attempt to demonstrate the surface_only() function.

   Author: James Kenny    Last modified: $Date$
"""
import nmesh
import nmesh.visual as viz

# define a simple mesh and submit request to mesher
box=nmesh.box( [0.0,0.0], [1.0,1.0] )
cone=nmesh.conic([3.0,0.0],1.0,[3.0,4.0],0.0)
bbox=[[-2.,-2.],[7.,6.]]
rod=0.4
mesh=nmesh.mesh(objects=[box,cone],a0=rod,bounding_box=bbox)
mesh_info=mesh.tolists()

# visualise in MayaVi, using the ratio of inradius:circumradius
# as cell_data for a colour scale
mesh_info2 = viz.surface_only(mesh_info)
vtkData, points, simplices, indices, icradii, ccradii=viz.mesh2vtk(mesh_info2, VTKonly=False)

# calculate ratio of actual:requested rod_length for surface elements
rod_length2a0=[]
for i in icradii:
    rod_length2a0.append(i/rod)

# append this as a scalar dataset and also the part indices
vtkData=viz.append2vtk(vtkData, rod_length2a0, "actual/requested rod_length")
vtkData=viz.append2vtk(vtkData, indices, "part indices")

# display mesh from file
viz.save_vtk(vtkData,'temp.vtk')
myv = viz.mesh2mayavi('temp.vtk')
myv.renwin.z_plus_view()

# open the data selection dialog so the user can choose between
# the available scalar datasets.
dvm = myv.get_current_dvm()
dvm.data_src.configure()




