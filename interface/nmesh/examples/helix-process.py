import nmesh, pylab

import mayavi, sys, Numeric
sys.argv=["",""]     

# define a few globals, so that the interval function can use them
globals()['v'] = mayavi.mayavi()
globals()['intervals'] = Numeric.arange(0,1.05,0.05)
globals()['call_counter'] = 0

# define the interval function
def my_function( piece, timestep, mesh_info ):
    """This function can be called to provide MayaVi and pylab
       visualisations of a mesh whilst it is growing.
    """
    dim = len(mesh_info[0][2][0])
    body_nr = 0
    try:                         #case of objects without meshing the bounding box
        body_nr = piece+1
        [mesh_info1] = nmesh.visual.separate_parts(mesh_info, listOfParts=[(body_nr,)])
    except:                      #case of objects + mesh of the bounding box
        body_nr = piece
        [mesh_info1] = nmesh.visual.separate_parts(mesh_info, listOfParts=[(body_nr,)])

    # to visualise without writing to disk, uncomment this section
    vtkData, points, simplices, simplexIndicies, icradii, ccradii = nmesh.visual.mesh2vtk(mesh_info1, VTKonly=False)

    in2circ = nmesh.visual.findRatios(icradii, ccradii, factor=dim)

    # order the mesh by this metric
    mesh_info1= nmesh.visual.order_mesh(mesh_info1, data=in2circ)

    
    globals()['v'].close_all()
    # display this information and open the ExtractUnstructuredGrid filter
    nmesh.visual.solid_in2circ(mesh_info1, myv = globals()['v'])
    globals()['v'].renwin.z_plus_view()
    
    f = globals()['v'].load_filter('ExtractUnstructuredGrid',0)
    
    # set the cellMin and cellMax appropriately and then display the filter
    # configuration dialog box for the user
    f.fil.SetCellClipping(1)
    f.fil.SetCellMaximum(100)
    f.fil.SetCellMinimum(0)
    f.renwin.Render()
    
    # superimpose a wireframe to show the outline of the solid
    dvm = globals()['v'].get_current_dvm()
    mm2 = dvm.add_module_mgr_gui()        # add a new module_manager
    outline = globals()['v'].load_module('SurfaceMap',0)
    outline.mapper.SetScalarVisibility(0)
    outline.actor.GetProperty().SetRepresentationToWireframe()
    outline.renwin.Render()
    
    # load dialog box
    #f.configure()
    

    # plot the quality metric dim*inradius/circumradius as a histogram
    title = "Mesh quality: %d*inradius/circumradius" % (dim)

    pylab.ion()
    pylab.clf()
    n, bins, patches = pylab.hist(in2circ, 10)
    pylab.title(title)
    pylab.xlabel('Quality')
    pylab.ylabel('Number of occurrences')
    pylab.ioff()

    # the following two commands save images from pylab and MayaVi
    # MayaVi captures the screen to do this, so ensure there are no
    # windows on top of MayaVi. (hence the unattended mode should be
    # set up to savefig() but not to draw() 

    pylab.draw()
    raw_input()

C_bottom = [0,0,0]  #center of spiral
R_spiral = 3        #radius of spiral
C_top = [0,0,10]    #top of spiral
R_circle = 3        #radius of max circle along the spiral


helix = nmesh.helix( C_bottom, R_spiral, C_top, R_circle )

bbox = [[-7,-7,-1],[7,7,11]]

N = 5
rod = 0.5
# create mesh of three objects and bounding box
mesh_ex = nmesh.mesh(objects = [helix],
                     a0=rod,
                     bounding_box=bbox,

                     #callback= (my_function,N),
                     max_steps=1000
                     )

mesh_ex.save("helix.nmesh")
