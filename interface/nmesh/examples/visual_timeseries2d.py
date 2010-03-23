"""Simple example of how a mesh can be visualised at regular intervals
   during mesh generation. This example saves VTK data to disk before
   visualisation.

   In general one file is continuously overwritten, but if desired
   shutil.copy() can be used to make copies of this file each time
   so that a series of VTK data files are saved which can be viewed in
   MayaVi as a series - selecting each instance by use of a slider bar.

   Authors: James Kenny 
   Last modified: $Date$
   Version: $Id$
"""

import nmesh, Numeric

# create square
box = nmesh.box( [0.0,0.0], [1.0,1.0] )
# create cone
cone = nmesh.conic([3.0,0.0],1.0,[3.0,4.0],0.0)
rod= 0.4
bbox = [[-1.,-1.],[7.,6.]]


# Set up a MayaVi window ready for use during mesher 'pauses'.
# The sys.argv hack is only required when MayaVi is used in
# a 'callback' function as below, it is not needed in static mode.
import mayavi, sys, pylab, shutil
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
    globals()['v'], in2circ = nmesh.visual.solid_in2circ(mesh_info, myv=globals()['v'])
    globals()['v'].renwin.z_plus_view()

    # to save the VTK files as a series rather than overwriting the same
    # file each time the function is called, uncomment the following line
    # (this only works when writing to a file, not when reading from RAM)
    shutil.copy('in2circ_solid.vtk', 'mesh_'+str(globals()['call_counter']).zfill(4)+'.vtk')

    # plot the quality metric 2*inradius/circumradius as a histogram
    pylab.ion()
    pylab.clf()
    n, bins, patches = pylab.hist(in2circ, 10)
    pylab.title('Mesh quality: 2*inradius/circumradius')
    pylab.xlabel('Quality')
    pylab.ylabel('Number of occurrences')
    pylab.ioff()

    # the following two commands save images from pylab and MayaVi
    # MayaVi captures the screen to do this, so ensure there are no
    # windows on top of MayaVi. (hence the unattended mode should be
    # set up to savefig() but not to draw() 

    pylab.draw()
#    pylab.savefig('hist_'+str( globals()['call_counter']).zfill(4)+'.png')
    nmesh.visual.export_visualisation(globals()['v'],'mesh_'+str( globals()['call_counter']).zfill(4)+'.png')

    # increment the call counter
    globals()['call_counter'] += 1

    # to run attended, comment in the following 2 lines

# create the mesh and visualise it every n steps
N = 1000
mesh = nmesh.mesh(objects = [box,cone], a0=rod, bounding_box=bbox, callback=(my_function, N))

# show the finished mesh
mesh_info = mesh.tolists()

# to do this by writing to disk, uncomment the following 2 lines
globals()['v'], in2circ = nmesh.visual.solid_in2circ(mesh_info, myv=globals()['v'])
globals()['v'].renwin.z_plus_view()

# if we have written to disk first (ie the above line is being used)
# then we can save the VTK file by uncommenting the following line
shutil.copy('in2circ_solid.vtk', 'mesh_'+str(globals()['call_counter']).zfill(4)+'.vtk')

# save an image from MayaVi of the finished mesh
nmesh.visual.export_visualisation(globals()['v'],'mesh_'+str(call_counter).zfill(4)+'.png')

# use pylab to plot a histogram of the quality metric 'in2circ'
pylab.ion()
pylab.clf()
n, bins, patches = pylab.hist(in2circ, globals()['intervals'])
pylab.title('Mesh quality: 2*inradius/circumradius')
pylab.xlabel('Quality')
pylab.ylabel('Number of occurrences')
pylab.ioff()
pylab.draw()
aa = globals()['call_counter']
#pylab.savefig('hist_'+str(str(call_counter)).zfill(4)+'.png')

# in case this has been run from the command line, wait for the user to quit




