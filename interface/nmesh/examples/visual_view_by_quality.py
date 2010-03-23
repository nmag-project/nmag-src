"""This example should demonstrate how to use the function order_mesh()
   but rather than sorting by position along an axis, the simplices will
   be sorted by their quality. In this case the metric 3*inradius/circumradius
   is used.

   Author: James Kenny       Last modified: $Date$
"""
import nmesh
frustum = nmesh.conic([0.0,0.0,0.0],1.0,[2.0,0.0,0.0],2.0)
bbox = [[-2.,-2.,-2.],[2.,2.,2.]]
mesh = nmesh.mesh(objects=[frustum],bounding_box=bbox)
mesh_info = mesh.tolists()


# now order the mesh by it's quality and display the worst 100 elements only
vtkData, points, simplices, simplexIndices, icradii, ccradii = nmesh.visual.mesh2vtk(mesh_info, VTKonly=False)

# calculate the quality metric
in2circ = nmesh.visual.findRatios(icradii, ccradii, factor=3)

# order the mesh by this metric
mesh_info = nmesh.visual.order_mesh(mesh_info, data=in2circ)

import mayavi
v= mayavi.mayavi()

# display this information and open the ExtractUnstructuredGrid filter
nmesh.visual.solid_in2circ(mesh_info, myv = v)

f = v.load_filter('ExtractUnstructuredGrid',0)

# set the cellMin and cellMax appropriately and then display the filter
# configuration dialog box for the user
f.fil.SetCellClipping(1)
f.fil.SetCellMaximum(100)
f.fil.SetCellMinimum(0)
f.renwin.Render()


# superimpose a wireframe to show the outline of the solid
dvm = v.get_current_dvm()
mm2 = dvm.add_module_mgr_gui()        # add a new module_manager
outline = v.load_module('SurfaceMap',0)
outline.mapper.SetScalarVisibility(0)
outline.actor.GetProperty().SetRepresentationToWireframe()
outline.renwin.Render()

# load dialog box
f.configure()
