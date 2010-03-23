import mayavi, sys, Numeric, nmesh
filename = sys.argv[1]
if filename[-5:] != "nmesh" : raise ValueError("The file is not a nmesh file.")
sys.argv=["",""]     

# define a few globals, so that the interval function can use them
globals()['v'] = mayavi.mayavi()
globals()['intervals'] = Numeric.arange(0,1.05,0.05)
globals()['call_counter'] = 0

the_mesh = nmesh.load(filename)
mesh_info = the_mesh.tolists()

# to visualise without writing to disk, uncomment this section
vtkData, points, simplices, simplexIndicies, icradii, ccradii = nmesh.visual.mesh2vtk(mesh_info, VTKonly=False,body_numbers=True)

in2circ = nmesh.visual.findRatios(icradii, ccradii)#, factor=3) #2D mesh

# order the mesh by this metric
mesh_info= nmesh.visual.order_mesh(mesh_info, data=in2circ)

globals()['v'].close_all()
# display this information and open the ExtractUnstructuredGrid filter
nmesh.visual.solid_in2circ(mesh_info, myv = globals()['v'])
m2 = v.load_module('SurfaceMap',0)
m2.actor.GetProperty().SetRepresentationToWireframe()
m2.mapper.SetScalarVisibility(0)
m2.renwin.Render()
dvm = v.get_current_dvm()
mm = dvm.get_current_module_mgr()
luthandler = mm.get_scalar_lut_handler()
luthandler.set_lut_red_blue()
luthandler.sc_bar.SetVisibility(1)
luthandler.sc_bar.GetTitleTextProperty().SetShadow(1)
luthandler.sc_bar.GetLabelTextProperty().SetShadow(1)

# superimpose a wireframe to show the outline of the solid
dvm = globals()['v'].get_current_dvm()
mm2 = dvm.add_module_mgr_gui()        # add a new module_manager
outline = globals()['v'].load_module('SurfaceMap',0)
outline.mapper.SetScalarVisibility(0)
outline.actor.GetProperty().SetRepresentationToWireframe()
outline.renwin.Render()

v.Render()
globals()['v'].renwin.z_plus_view()

raw_input()
