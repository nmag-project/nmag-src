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
    f.configure()
    

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

#=================================== mesh parameters ==============
rod = 0.8

outer_radius = 5.0
inner_radius = 2 
thickness = 1.

import math

bbox = [[-10,-10,-thickness/2.],[10,10,thickness/2.]]

trasl = [0,5.,0]

# external ring
R = nmesh.conic(
    [0.0,0.0,-thickness], outer_radius, [0.0,0.0,thickness], outer_radius, \
    transform=[("shift",trasl)]
    )
#internal ring
r = nmesh.conic(
    [0.0,0.0,-thickness], inner_radius, [0.0,0.0,thickness], inner_radius, \
    transform=[("shift",trasl)],
    )
# take the difference
ring = nmesh.difference(R,[r])

box = nmesh.box([-10,0,-thickness/2.],[10,10,thickness/2.])
system = nmesh.intersect([ring, box])

density = """density = 1.;"""

N = 50

# create mesh of three objects and bounding box
mesh_ex = nmesh.mesh(objects = [system],
                     a0=rod,
                     bounding_box=bbox,
                    
                     density = density,

                     # with these values there is
                     # a bit more pressure than with
                     # the default values (as suggested
                     # by Thomas)
                     #thresh_del = 1.9,
                     #thresh_add = 0.1,


                     #shape_force_scale = 0.5,



                     # it is important that the parameter
                     # "initial_settling_steps" is a
                     # large number so that the neighbour
                     # forces (default scale = 1.0)
                     # act almost alone at first
                     # and the nodes fill all the space,
                     # then towards the end the shape
                     # forces (default scalee = 0.1)
                     # do the the rest
                     initial_settling_steps = 200,

                     max_relaxation = 4,
                     callback= (my_function,N),
                     max_steps=1000
                     )

