"""Simple example of how a mesh can be visualised at regular intervals
   during mesh generation. This version does not write to disk but
   visualises direct from RAM. This is more suitable for fast machines
   when a small interval is being used. (ie N < 20)

   No .PNG images are saved. Pylab plots have been coded, but commented
   out, as have the waiting commands. On a fast machine this example
   should display a fluid mesh generation. 

   Authors: James Kenny 
   Last modified: $Date$
   Version: $Id$
"""

import nmesh, pylab, Numeric

# create cone
#cone = nmesh.conic([3.0,0.0,0.0],1.0,[3.0,0.1,0.0],0.0)
#box = nmesh.box( [0.0,0.0,0.0], [1.0,1.0,1.0], transform=[("rotate3d",[1.,1.,1.],30), \
#                                                          ("shift",[3.0,3.0,3.0])] )
#rod= 0.4
#bbox = [[-1.,-1.,-1.],[7.,6.,6.]]

box1 = nmesh.box( [2,2,2],[3,3,3], transform=[("rotate3d",[-1,-1,-1],50)])
box2 = nmesh.box( [0,0,0],[1,1,1], transform=[("rotate3d",[-1,-1,-1],-30), \
                                             ("shift",[-2,-3,-2])] )
ground = nmesh.box( [-5,-5,-0.3],[5,5,0.3] )
bbox = [[-5,-5,-5],[5,5,5]]
rod = 0.5

# Set up a MayaVi window ready for use during mesher 'pauses'.
# The sys.argv hack is only required when MayaVi is used in
# a 'callback' function as below, it is not needed in static mode.
import mayavi, sys
sys.argv=["",""]     

# define a few globals, so that the interval function can use them
globals()['v'] = mayavi.mayavi()
globals()['intervals'] = Numeric.arange(0,1.05,0.05)
globals()['call_counter'] = 0


# define the interval function
def my_function( piece, timestep, mesh_info ):
    """This function can be called to provide MayaVi and pylab
       visualisations of a mesh whilst it is growing.

       Author: James Kenny
    """

    # to visualise without writing to disk, uncomment this section
    vtkData, points, simplices, simplexIndicies, icradii, ccradii = nmesh.visual.mesh2vtk(mesh_info, VTKonly=False,body_numbers=True)

    in2circ = nmesh.visual.findRatios(icradii, ccradii, factor=3) #2D mesh
    vtkData = nmesh.visual.append2vtk(vtkData, in2circ, "2*inradius/circumradius")
    globals()['v'].close_all()
    globals()['v'] = nmesh.visual.mesh2mayavi(vtkData, myv=globals()['v'], lut_range=(0,1))
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
    v.Render()
    raw_input()

    

# create the mesh and visualise it every n steps
N = 100
mesh = nmesh.mesh(objects = [box1,ground,box2], a0=rod, bounding_box=bbox, 
                  callback=(my_function, N), 
                  max_steps=1000)

# show the finished mesh
mesh_info = mesh.tolists()

# to visualise without writing to disk, uncomment this section
vtkData, points, simplices, simplexIndicies, icradii, ccradii = nmesh.visual.mesh2vtk(mesh_info, VTKonly=False)
in2circ = nmesh.visual.findRatios(icradii, ccradii, factor=2) #2D mesh
vtkData = nmesh.visual.append2vtk(vtkData, in2circ, "2*inradius/circumradius")
globals()['v'].close_all()
globals()['v'] = nmesh.visual.mesh2mayavi(vtkData, myv=globals()['v'], lut_range=(0,1))
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
v.Render()

# in case this has been run from the command line, wait for the user to quit
raw_input('Hit enter to exit.\n')




