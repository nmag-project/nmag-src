# (C) 2005 Dr. Thomas Fischbacher, Dr Hans Fangohr

import math,sys,Numeric

# a 2d square mesh with 8x8 points, unit spacing, one corner at (0,0), one at (7,7):

#the_mesh = ocaml.simple_lattice_mesh(3,6)

dim=3
mesh_order = 1
r_sphere = 1.0
r_box = 2.5
rod_length = 1.0

mdefaults=ocaml.mesher_defaults

the_mesh = ocaml.mesh_sphere_in_box(mdefaults, dim, r_sphere, r_box, rod_length)


meshed_laplace = ocaml.mesh_laplace(the_mesh,mesh_order)


def harmonic1(xy):
    if len(xy) >= 2:
        return xy[0]*xy[1]+0.0
    else: #this is the 1d case
        return xy[0]

def boundary(xy):
    return 0.3*(3*math.cos(xy[0])+math.sin(xy[1]))*0.0

def rhs (xy):
    print "xy=",xy
    sys.stdout.flush()
    pos = Numeric.array(xy)-Numeric.array([0,0,0],'d')
    r = math.sqrt(reduce((lambda sf,x: sf+x*x),pos,0))
    #if math.sqrt((xy[0]-6)**2+(xy[1]-6)**2) < 1.0:
    if r < 1.2:
        return 1.0
    else:
        return 0.0

sample_harmonic = ocaml.sample_on_mesh(meshed_laplace,harmonic1)
diff_harmonic = ocaml.apply_meshed_diffop(meshed_laplace,sample_harmonic)
solution = ocaml.solve_dirichlet( meshed_laplace, boundary, rhs )
diff_solution = ocaml.apply_meshed_diffop(meshed_laplace,solution)
sample_rhs = ocaml.sample_on_mesh(meshed_laplace,rhs)


geometry = ocaml.meshed_diffop_geometry(meshed_laplace)


#access files in ../debugging
import sys,os,os.path
sys.path.append( os.path.join( os.getcwd(), "../debugging") )
import mesh2vtk

#extract only mesh vertices (not the 'intermediate' points required
#for higher order shape functions):

def meshdata_for_vtk( geometry ):
    """takes a 'geometry' object (which is likely to change soon)
    and returns
    i)  a list of coordinates (these are the x,y,z positions of
        the mesh vertices)

    ii) a list of 'cells'. For 2d data, each cell is a list of three
        integer numbers indexing the coordinates list to describe a
        triangle. For 3d data, each cell is a list of 4 indices
        describing a tetrahedron.

    Notes:

    This is likely to change soon both because the 'geometry' object
    is likely to change and because we might need data on the surface
    elements. (fangohr 26/09/2005)
        
    """
    
    def matrixindex_of_vertex ( i ):
        return geometry[1][1][i]
    def position_of_matrix_index ( i ):
        return geometry[2][1][ i ]
    def make_3d( pos ):
        """used to convert 2d-positions into 3d positions (adding 0
        for the third component. VTK likes to received 3d positions
        (even for 2d data).

        Input data can be any sequence. If input is 2d, then the returned
        object will be of type list. For 3d objects, we return the same
        data type as the input object had.
        """
        d = len(pos) #guess dimensionality
        if d == 2:
            return list(pos)+[0.0]  
        elif d == 3:
            return pos
        elif d == 1:
            return list(pos)+[0.0]+[0.0]  
        else:
            raise ValueError,"expect only 2d and 3d data here"
        
    def find_mesh_point_indices( cells ):
        """cheap trick and hack to find mesh points used
        by the volume elements (=cells)
        -- waiting for feedback from Thomas.

        The purpose of this exercise is to get rid of
        'intermediate' higher order points."""
        
        d = {}    #use hash table to find unique set of vertex indices
        for cell in cells:
            for index in cell:
                d[index]=None         #populate hash table 
        return d.keys()

    #try to learn something about the system:
    dimensions = len( geometry[2][1][0] ) #coordinates of first vertex
    print "Data appears to be %d-dimensional" % dimensions
    if dimensions not in [1,2,3]:
        raise NotImplementedError,"Can only deal with 1d ,2d and 3d data for now, not %d" % (dimensions)


    #create list of lists with indices for triangles.
    #These new indices point to the entries in the "point positions"
    #(see below) for all vertices
    cell_indices = map( lambda x : map( matrixindex_of_vertex, x), geometry[0][1] )
    mesh_point_indices = find_mesh_point_indices( cell_indices )
    pointpos = map( make_3d,  map( position_of_matrix_index, mesh_point_indices ) )

    return pointpos,cell_indices,geometry[3][1]


coordinates, cells, edges = meshdata_for_vtk( geometry )


print "Coordinates:\n ",coordinates,"\nCells:\n ",cells,"\nEdges:\n ",edges,"\n" # DDD


scalars = [sample_harmonic,diff_harmonic,solution,diff_solution,sample_rhs]
scalarsnames = ["harmonic function","\laplace harmonic function","L^(-1) rhs", "LL^(-1) rhs", "rhs"]

mesh2vtk.write_vtk_file_unstructured_mesh( "test.vtk",
                                           coordinates=coordinates,
                                           scalars=scalars,
                                           scalarsnames=scalarsnames,
                                           volumecells=cells,
                                           lines=edges
                                           )
                                           


# Notes:
#
# Not sure where the code shown above should go. It is a conversion from
# our (not quite stable) mesh-geometry format to something VTK expects.
# It could either go to the (more general) VTK routine(s) in debugging/mesh2vtk
# (because it converts the data (partly) into a VTK compatible structure, or
# it could stay close to pyfem because it converts from pyfem to something
# more commonly used for plotting. However, debugging is not a great location for
# mesh2vtk either; so we'll need to tidy this up at some point. (fangohr 26/09/2005)


