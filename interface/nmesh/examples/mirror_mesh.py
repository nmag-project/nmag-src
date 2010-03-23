import sys
import Numeric as N
import math, types

def distance(p1,p2):
    if type(p1) != types.ListType:
        p1 = [p1]
        p2 = [p2]
    d = N.subtract( N.array(p1),N.array(p2) )
    return N.sqrt( N.add.reduce( d*d, 0 ) )
    

def check_mirror_direction(filename,direction):
    """function to check if the dimension of\
    the mesh is compatible with the definition\
    of the mirroring surface\
    """
    f=open( filename, "r" )
    lines = f.readlines()
    f.close()
    
    mesh_variables = lines[1].split()
    dim = int(mesh_variables[3])
    return dim==len(direction)

def read_file(filename):

    try:
        f=open( filename, "r" )
        lines = f.readlines()
        f.close()
    except:
        print "The file %s cannot be opened\n" %filename
        sys.exit()

    return lines

def extract_data(lines):

    mesh_variables = lines[1].split()
    
    points_nr = int( mesh_variables[6] )
    simplices_nr = int( mesh_variables[9] )
    surfaces_nr = int( mesh_variables[12] )
    periodic_nr = int( mesh_variables[15])

    # read points
    points = []
    for i in range(points_nr):
        vals = [float(val) for val in lines[3+i].split()]

        # region the simplex belongs to
        points.append(vals)


    #read tetraedra
    simplices_indices = []
    
    #keep track of the subdomain to which tetraeders belong
    simplices_regions = []

    for i in range(simplices_nr):
        vals = [int(val) for val in lines[3+points_nr+1+i].split()]

        # region the simplex belongs to
        simplices_regions.append(vals[0])

        # indices of nodes in each simplex
        simplices_indices.append(vals[1:])


    # read surfaces
    surfaces = []
    for i in range(surfaces_nr):
        vals = [int(val) for val in lines[3+points_nr+1+simplices_nr+1+i].split()]

        # region the simplex belongs to
        surfaces.append(vals)
    
    # read periodic points
    periodic_pts = []
    for i in range(periodic_nr):
        vals = [int(val) for val in lines[3+points_nr+1+simplices_nr+1+surfaces_nr+1+i].split()]

        # region the simplex belongs to
        periodic_pts.append(vals)

    return points, simplices_indices, simplices_regions, periodic_pts

def mesh_image( data_string, inter_dist_error, surf_dist_error, direction ):
    """
    function that, given a mesh,  mirrors it with respect to "plane" associate to the
    given direction
    """

    if type(data_string) == types.StringType:
        data_string = data_string.split('\n')

    points, simplices_indices, simplices_regions, periodic_pts = extract_data(data_string)

    new_points = [] # image points of the mesh with respect to the mirror "plane"

    relocated = []  # list of "new" points which coincide with the old ones (being on
                    # the mirroring surface)

    periodic = [] # periodic points
                    
    nr_points = len(points)

    
    mapping = dict([(x, x) for x in range(2*nr_points)]) 

    for d,i in zip(direction,range(len(direction))):
        # only one component at a time is considered
        if d == 0: 
            continue

        
        # extract the "mirroring plane"
        min_mirr_comp = points[0][i]
        max_mirr_comp = points[0][i]
        for p in points:
            mirr_component = p[i]
            if mirr_component < min_mirr_comp:
                min_mirr_comp = p[i]
            if mirr_component > max_mirr_comp:
                max_mirr_comp = p[i]

        if d == 1: # mirror along the positive axis
            
            # find the new points
            for p,p_i in zip(points,range(len(points))):
                mirr_component = p[i]
                new_mirr_component = 2*max_mirr_comp - mirr_component
                # substitute the new point with the old one in the mapping
                # if their distance is smaller than the given error
                if distance(mirr_component,new_mirr_component) < inter_dist_error:
                    mapping[nr_points + p_i] = p_i

                    # keep track of the points which coincide with any of the old ones
                    relocated.append(nr_points+p_i)
                
                # build the new point
                elif d<len(direction):
                    new_p = p[:i]+[new_mirr_component]+p[i+1:]
                    new_points.append(new_p)

                    # if the new point is close to the opposite face with
                    # respect to "mirroring" plane, it will have a periodic
                    # counterpart in the mirrored image
                    if distance(mirr_component,min_mirr_comp) < surf_dist_error:
                        periodic.append([p_i,nr_points+p_i])
                else:
                    new_p = p[:i]+[new_mirr_component] 
                    new_points.append(new_p)

                    # if the new point is close to the opposite face with
                    # respect to "mirroring" plane, it will have a periodic
                    # counterpart in the mirrored image
                    if distance(mirr_component,min_mirr_comp) < surf_dist_error:
                        periodic.append([p_i,nr_points+p_i])

        elif d==-1: # mirror along the negative axis
            
            # find the new points
            for p,p_i in zip(points,range(len(points))):
                mirr_component = p[i]
                new_mirr_component = 2*min_mirr_comp - mirr_component

                # substitute the new point with the old one in the mapping
                # if their distance is smaller than the given error
                if distance(mirr_component,new_mirr_component) < inter_dist_error:
                    mapping[nr_points + p_i] = p_i

                    # keep track of the points which coincide with any of the old ones
                    relocated.append(nr_points+p_i)
                    
                # build the new point
                elif d<len(direction):
                    new_p = p[:i]+[new_mirr_component]+p[i+1:]
                    new_points.append(new_p)

                    # if the new point is close to the opposite face with
                    # respect to "mirroring" plane, it will have a periodic
                    # counterpart in the mirrored image
                    if distance(mirr_component,max_mirr_comp) < surf_dist_error:
                        periodic.append([p_i,nr_points+p_i])
                    
                else:
                    new_p = p[:i]+[new_mirr_component] 
                    new_points.append(new_p)

                    # if the new point is close to the opposite face with
                    # respect to "mirroring" plane, it will have a periodic
                    # counterpart in the mirrored image
                    if distance(mirr_component,max_mirr_comp) < surf_dist_error:
                        periodic.append([p_i,nr_points+p_i])

        else:
            print "d != 0,1,-1: impossible!"
            sys.exit(0)

    # shift the indices of the new points to get rid of the "double counted" (common) surface points
    relocated.sort()
    relocated.reverse()
    for p in relocated:
        for k,v in mapping.iteritems():
            if v>p-1:
                mapping[k] = v-1
            else:
                mapping[k] = v

    
    # build the new simplices
    new_simplices_indices = []
    new_simplices_regions = []
    for reg,ind in zip(simplices_regions,simplices_indices):
        
        new_ind = [mapping[i+nr_points] for i in ind]

        new_simplices_indices.append(new_ind)
        new_simplices_regions.append(reg)


    # periodic points on the face opposite to the mirroring plane
    new_periodic = [ [ i, mapping[j]] for (i,j) in periodic ]

    # periodic points on the faces orthogonal to the mirroring plane
    old2new_periodic = [ [ mapping[j+nr_points] for j in i] for i in periodic_pts ]     

    # merge the entries which are periodically equivalent 
    total_periodic = []
    for per in periodic_pts+old2new_periodic+new_periodic:
        already_in = False
        for tot in total_periodic:
            for bit,i in zip(per,range(len(per))):
                if bit in tot:
                    rest = per[:i]+per[i+1:]
                    for j in rest:
                        if j not in tot:
                            tot.append(j)
                    already_in = True
                    break
        if not already_in:
           total_periodic.append(per) 

    # take each entry of the periodic list just once
    reduced_periodic = []
    for tot in total_periodic:
        tot.sort()
        if tot not in reduced_periodic:
            reduced_periodic.append(tot)
        
    return points+new_points, simplices_indices+new_simplices_indices,simplices_regions+new_simplices_regions, \
                  reduced_periodic


def virtual_file(new_points, new_simplices, new_regions, new_periodic):
    """
    function that writes a string containing all the information of an nmesh file
    """
    points_nr = len(new_points)
    simplices_nr = len(new_simplices)
    periodic_nr = len(new_periodic)
    new_file_string = ""
    new_file_string += "# PYFEM mesh file version 1.0\n# dim = %d 	 nodes = %d 	 simplices = %d 	 surfaces = 0 	 periodic = %d\n" %(len(new_points[0]),points_nr,simplices_nr,periodic_nr)
    
    new_file_string += str(points_nr)+"\n"
    
    for point in new_points:
    
        for comp in point:
            new_file_string += "%25.12f " % comp
        new_file_string += "\n"    
    
    new_file_string += str(simplices_nr)+"\n"
    
    for (reg, ind) in zip(new_regions, new_simplices):
        new_file_string += "%6d " % (reg)
        for i in ind:
            new_file_string += "%10d " % i
        new_file_string += "\n"
        
    new_file_string += "0\n"
    
    new_file_string += str(periodic_nr)+"\n"
    
    for per in new_periodic:
        for comp in per:
            new_file_string += "%d " % comp
        new_file_string += "\n"    
    
    return new_file_string
    
def create_periodic_mesh( filename, inter_dist_error, surf_dist_error, direction ):
    """
    function that iterates over the direction components to create the periodic mesh
    """

    data_string = read_file(filename)
    dir_components = []
    dim = len(direction)
    zeros = []
    for i in range(dim):
        zeros.append(0)

    # create a list of lists where each entry has only one component which is 1 or -1
    for d,i in zip(direction,range(len(direction))):
        dir_comp = zeros[:i]+[d]+zeros[i+1:]
        valid_comp = False
        for val in dir_comp:
            if abs(val) == 1:
                valid_comp = True

        if valid_comp:
            dir_components.append(dir_comp)

    # duplicate the initial mesh in an iterative way along all the given directions
    for comp in dir_components:
        points,simplices,regions,periodic = mesh_image(data_string, inter_dist_error, surf_dist_error, comp)
        data_string = virtual_file(points,simplices,regions,periodic)
    return data_string


def print_usage():
    print """
    Usage: python mirror_mesh.py meshfile error_neigh_dist error_surf_dist direction newfile\n
    with:
         * meshfile:            original (periodic-free) nmesh file
         
         * error_neigh_dist:    distance between to points to be considered coincident
         
         * error_surf_dist:     distance between a point and the outer surface to be considered periodic
         * direction:           list of values 0,1 or -1, corresponding to the direction(s)
         
                                over which the mesh is mirrored:
                                1: positive axis
                                0: no mirror
                                -1: negative axis

         * newfile:             name of the file with the new periodic mesh
         
    Example: 3D mesh to be mirrored along the positive x-axis and the negative z-axis

             python mirror_mesh.py meshfile 1e-6 1e-6 1,0,-1 newfile
    
         """
    sys.exit(0)

#print sys.argv
try:
    filename = sys.argv[1]
    neig_dist_error = float(sys.argv[2])
    surf_dist_error = float(sys.argv[3])
    direction = [int(i) for i in sys.argv[4].split(',')]
    newfile = sys.argv[5]
except:
    print_usage()


if not check_mirror_direction(filename,direction):
    print """
    The number of components of the direction vector
    must be equal to the dimension of the mesh.

    """
    sys.exit(0)


newmesh = create_periodic_mesh( filename, neig_dist_error, surf_dist_error, direction )
f=open( newfile, "w" )
f.write(newmesh)
f.close()
    
    
