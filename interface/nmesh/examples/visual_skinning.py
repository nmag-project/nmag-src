"""This example demonstrates the 'skin_mesh()' function which enables the user    to ensure that the mesh simplices are indexed in such a way that the Filter
   'ExtractUnstructuredGrid' can be employed to view the interior of the mesh
   by removing layers, as if peeling skins off an onion.
"""
import nmesh

ellipsoid = nmesh.ellipsoid([0.75,1.25,1])

# create mesh
bbox = [[-0.75,-1.25,-1],[0.75,1.25,1]]

mesh = nmesh.mesh(objects = [ellipsoid], bounding_box=bbox,a0=0.25)
mesh_info = mesh.tolists()

# this command orders the mesh in such a way that ExtractUnstructuredGrid
# can be used to 'peel back skins', if the user can tollerate a longer runtime
# they can uncomment the end of this line to redefine the condition that is
# employed. The default is condition='>=1', so any cell containing one or more
# surface points is included in the first layer, etc...by changing this to
# condition '>=2' each cell must contain at least two points in the list of
# points for that layer, so the layers are thinner, less smooth and greater in
# number (note: this is not a particularly efficient function!)
mesh_info, listOfSkins = nmesh.visual.skin_mesh(mesh_info)#,condition='>=2')

# get a vtkData set for this an append a quality metric to it
vtkData, points, simplices, simplexIndices, icradii, ccradii = \
         nmesh.visual.mesh2vtk(mesh_info, VTKonly=False)

in2circ = nmesh.visual.findRatios(icradii, ccradii, factor=3)
vtkData2 = nmesh.visual.append2vtk(vtkData, in2circ, "in2circ")

# visualise it and manipulate the visualisation to suit this example
v = nmesh.visual.mesh2mayavi(vtkData2,lut_range=(0,1))
dvm = v.get_current_dvm()
mm = dvm.get_current_module_mgr()

# load another surface map to overlay a wireframe on the mesh
m2 = v.load_module('SurfaceMap',0)
m2.actor.GetProperty().SetRepresentationToWireframe()
m2.mapper.SetScalarVisibility(0)

# change some more aesthetic things
luthandler = mm.get_scalar_lut_handler()
luthandler.set_lut_red_blue()
luthandler.sc_bar.SetVisibility(1)
luthandler.sc_bar.GetTitleTextProperty().SetShadow(1)
luthandler.sc_bar.GetLabelTextProperty().SetShadow(1)
v.renwin.y_minus_view()
nmesh.visual.rotate(v,roll=5,pitch=-15,yaw=-60)
v.renwin.camera.Zoom(1.5)

# render the visualisation again to update
v.Render()

# now step through various settings in the filter
f = v.load_filter('ExtractUnstructuredGrid',config=0)
f.fil.CellClippingOn()

# make sure the lut has a title
dvm.data_src.scalar_var.set('in2circ')
dvm.data_src.set_scalar_gui()


# be pretty and animate this
import time
imageCount = 0
for i in listOfSkins:
    f.fil.SetCellMinimum(i[0])
    f.fil.SetCellMaximum(i[1])
    f.renwin.Render()
    nmesh.visual.export_visualisation(v,"skin_"+str(imageCount).zfill(3)+".png")
    imageCount = imageCount + 1
    time.sleep(0.5)

f.fil.SetCellMinimum(0)
f.renwin.Render()

# display the filter configuration GUI for the user to experiment
f.configure()


# show the user the ranges to use for ExtractUnstructuredGrid
print "Skin\tcellMin\tcellMax\n"
for i in range(len(listOfSkins)):
    print "%d\t%d\t%d\n"%(i,listOfSkins[i][0],listOfSkins[i][1])
    




