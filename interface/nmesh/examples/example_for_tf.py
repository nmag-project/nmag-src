"""This is an example showing how to create a custom set of scalar data
   to be appended as point_data on a mesh.

   Author: James Kenny   Last modified: $Date$

   Version: $Id$
"""
import nmesh
import nmesh.visual as viz


# define a simple mesh and submit request to mesher
#box = nmesh.box([0.0,0.0,0.0],[1.0,1.0,1.0])
cone = nmesh.conic([0.0,0.0,0.0],1.0,[2.0,0.0,0.0],2.0)
bbox = [[-4.,-4.,-4.0],[4.,4.,4.]]
mesh = nmesh.mesh(objects = [cone], bounding_box=bbox)
mesh_info=mesh.tolists()

##add the next line to only look at surface elements
#mesh_info = viz.surface_only(mesh_info)
vtkData, points, simplices, indices, icradii, ccradii=viz.mesh2vtk(mesh_info, VTKonly=False)

# append the point index to each point  -  this will be a scalar field
customScalars = []
for i in range(len(points)):
    customScalars.append(i)
    
# append the data to the mesh and visualise it
vtkData=viz.append2vtk(vtkData, customScalars, "point indices", sites='point')
viz.save_vtk(vtkData, "example_for_tf.vtk")
myv = viz.mesh2mayavi("example_for_tf.vtk")


# extensive customisation of MayaVi visualisation
# users are encouraged to use MayaVi GUI for similar operations
dvm = myv.get_current_dvm()        # data-visualisation-manager
mm = dvm.get_current_module_mgr()  # module manager
m0 = mm.get_module(0)              # this is the SurfaceMap (first module)
m1 = myv.load_module('Glyph',0)

# change SurfaceMap to wireframe, line_width=1.0, no scalar colouring
m0.actor.GetProperty().SetRepresentationToWireframe() #.SetRepresentation(1)
m0.actor.GetProperty().SetLineWidth(1.0)
m0.mapper.SetScalarVisibility(0)
m0.renwin.Render()

# change the Glyph module to use spheres, of radius=0.1 with
# a phi resolution = theta resolution = 8
m1.glyph.SetScaleModeToDataScalingOff()
m1.glyph_var.set(2)               # see *** at bottom of this script for codes
m1.set_glyph_mode()               # updates module to use new glyph source
m1.glyph_src.SetRadius(0.1)       # sets Sphere radius = 0.1
m1.glyph_src.SetPhiResolution(8)
m1.glyph_src.SetThetaResolution(8)
m1.renwin.Render()

# change the lut
lut_handler = mm.get_scalar_lut_handler()
lut_handler.set_lut_red_blue()
lut_handler.sc_bar.SetVisibility(1)
lut_handler.sc_bar.GetTitleTextProperty().SetShadow(1)
lut_handler.sc_bar.GetLabelTextProperty().SetShadow(1)
myv.renwin.z_plus_view()


import time
time.sleep(30)
#raw_input('Hit any key to continue.')




#*** for glyph_var.set(value) the following are the options for 'value':
#        value=0   -> 2D Glyph
#        value=1   -> Cone
#        value=2   -> Sphere
#        value=3   -> Cube
#        value=4   -> Cylinder
#        value=5   -> 3D Arrow
