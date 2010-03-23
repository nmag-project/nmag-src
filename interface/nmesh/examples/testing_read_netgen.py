import nmesh, Numeric, sys


import string
#----------------------------------------------------------------------
# function to read Neutral file from Netgen 
#----------------------------------------------------------------------
def ReadMeshFromNetgenNeutral( fname, debug = 0 ):
    """ 
    In Netgen use export, and set export file format to "neutral".

    """

    f=open( fname, "r" )

    points = []
    
    #get the number of points out of the first line
    help = f.readline()
    points_nr = int( help )

    #read points
    for i in range(points_nr):
        line=f.readline()
        coords = [float(coord) for coord in line.split()]
        points.append(coords)

    if debug:
        print "read",len( points ),"points"

    # read volume elements

    # get the number of volume-elements 
    help = f.readline()
    simplices_nr = int( help )

    #read tetraeders
    simplices_indices = []
    
    #keep track of the subdomain to which tetraeders belong
    simplices_regions = []

    for i in range(simplices_nr):
        line=f.readline()
        vals = [int(val) for val in line.split()]

        # region the simplex belongs to
        simplices_regions.append(vals[0])

        # indices of nodes in each simplex
        simplices_indices.append(vals[1:])


    if debug:
        print "read",len( simplices_regions ),"volume elements"

    return (points,simplices_indices, simplices_regions)    


def visualise_mesh(mesh):
    import mayavi
    sys.argv=["",""]     

    mesh_info = mesh.tolists()
    # define a few globals, so that the interval function can use them
    globals()['v'] = mayavi.mayavi()
    globals()['intervals'] = Numeric.arange(0,1.05,0.05)
    globals()['call_counter'] = 0

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
    globals()['v'].renwin.z_plus_view()  # this line will reduce fluidity
    v.Render()
    raw_input()

#--------------------------------------------------------------
#                    MAIN file
#--------------------------------------------------------------

filename = "cube+sph.stl"

from subprocess import Popen, PIPE
print Popen(["bunzip2", "-k", filename+".bz2"], stdout=PIPE).communicate()[0]

points,simplices_indices,simplices_regions = ReadMeshFromNetgenNeutral(filename)

the_mesh = nmesh.mesh_from_points_and_simplices(points = points,
                                                simplices_indices = simplices_indices,
                                                simplices_regions = simplices_regions,
                                                initial = 1)

visualise_mesh(the_mesh)
