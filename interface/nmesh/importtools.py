from __future__ import division

import time,os,string,sys

from nmesh_exceptions import *

import nbase
log = nbase.logging.getLogger('nmesh')

def ReadMeshFromNetgenNeutral( fname ):
    """
    Function to read Neutral file from Netgen

    In Netgen use export, and set export file format to "neutral".

    Written by Giuliano and Hans (2006)

    """

    log.debug("About to open file '%s' to read netgen mesh" % fname)

    if fname[-3:] == '.gz': #assume this is gzipped data
        import gzip
        f=gzip.open( fname )
    else:
        f=open( fname, "r" )

    points = []
    
    #get the number of points out of the first line
    help = f.readline()
    try:
        points_nr = int( help )
    except ValueError,msg:
        #didn't get int that we expect. Most likely wrong file format.
        #Check for obvious mistake (i.e. clicked 'save' in netgen,
        #rather than export)
        log.warning("Found '%s' in first line of mesh file '%s' instead of number of points" % (help.strip(),fname))
        if help[0:len("mesh3d")] == "mesh3d":
            log.error("This file has the wrong format (maybe using 'export->neutral' in Netgen [not save] will solve this))")
            import sys
            sys.exit(1)
        log.error("Unknown file format (stopping here)")
        import sys
        sys.exit(1)


    #read points
    for i in range(points_nr):
        line=f.readline()
        coords = [float(coord) for coord in line.split()]
        points.append(coords)

    log.debug("have read %d points" % len( points ))

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

    log.debug("have read %d points" % len( simplices_regions ))

    return (points,simplices_indices, simplices_regions)    




def ReadMeshFromGambitNeutral( fname, debug = 0 ):
    """
    Read Mesh from Gambit.
    
    In Gambit  set solver as generic, use export-mesh, and export file format as "neutral".

    Written by Stephen Powell (2007)

    """

    if fname[-3:] == '.gz': #assume this is gzipped data
        import gzip
        f=gzip.open( fname )
    else:
        f=open( fname, "r" )


    points = []
    
    #get the number of points out of the 7th line
    help = []
    for element in f.xreadlines():
        help.append(element)
        
    number = help[6]
    bits = number.split()
    npoints = int(bits[0])
    #extract the points data 
    d= help[9:int(9+npoints)]
    #Get the coordinates out of each line
    for i in range(npoints):
        line=d[i]
        coords = [float(coord) for coord in line.split()]
        points.append(coords[1:])           
    #get the number of elements from 7th line
    nelem = int(bits[1])
    """To get the elements and to assign the object the elements are from"""
    #create empty list for the elements
    simplices_indices = []

    simplices_regions = []
    #extract the elements data
    e = help[(9+npoints+2):(9+npoints+2+nelem)]

    
    
    #Get the element numbers from the file
    
    k=help[9+npoints+2+nelem+5:]
    
    num_elements = []   #list of the element number in the order given
    num_objects = []    #number of objects
    #t=[0]
    r = int(nelem*0.1)  #create integer of the number of lines/ number of columns
    if (nelem*0.1-float(int(nelem*0.1))) == 0.0:
        rmax = r
    else:
        rmax = r+1

    #make a list of all the elements in the right order.
    for i in range(rmax):
        line=k[i]
        bits = line.split()
        for j in range(len(bits)):
            number =int( bits[j])
            num_elements.append(number)

    last_elem = []  #takes the last element of each object in the list
    
    
    #object_lst= []
    
    
    #tr=('1')   
    #Get the first element out of element list
    line=help[9+npoints+2+nelem+5]
    bits = line.split()
    t0=int(bits[0])
   
    #find the beginning element and end elemnt of each object and the number of elements
    objects = 0     #initial number of objects
   
    for i in range(nelem):
        x=num_elements[i]
        
        if x< t0:
            
            objects = objects +1    #count the number of objects
            last_elem.append(num_elements[i-1])     #append the last number 
           
        t0=x+1
    last_object = num_elements[-1]        #last element of object
    
    last_elem.append(last_object)         #append last object element to last_elem

       
    num_objects = len(last_elem)
    #Create lists for each object append the element numbers to the list
    for i in range (num_objects):
        object_i = []
        x= last_elem[i]
        if x==last_elem[-1]:
           for j in range(1, last_elem[-1]+1):
                object_i.append(j)
        else:
            for j in range(last_elem[i+1]+1, last_elem[i]+1):
                object_i.append(j)
        #w= help[(9+npoints+2)+object_i[0]-1]
        
        #Compare the total list of elements to the list for the object and append the
                #nodes of that element
        for k in range(nelem):
            line=e[k]
            bits=line.split()
            #compare lists
            if int(bits[0])== int(object_i[0]):
                
                #append nodes and object number to simplices_indices and simplices_regions
                for l in range(len(object_i)):
                    line = e[k+l]
                    bits = line.split()
                    g=i+1
                    h=[ int(bits[3]), int(bits[4]), int(bits[5]), int(bits[6])]
                    simplices_regions.append(g)
                    simplices_indices.append(h)
           
           
    return (points, simplices_indices, simplices_regions)


def count_occurences(data_list):
    d={}
    for item in data_list:
        if item in d:
            d[item]+=1
        else:
            d[item]=1
    return d


def report_occurences_string(dic,label="item #"):
    msg = ""
    keys = dic.keys()
    keys.sort()
    for key in keys:
        msg += "%s%d -> %d times\n" % (label,key,dic[key])
    return msg[:-1]



def ReadMeshFromGmeshVersion1_0(filename):
    """
    Function to read Gmesh file version 1.0 

    Written by Hans Fangohr (2008)

    This 1.0 file format was the standard format for GMSH up to
    version 1.6.5 (which is the default version installed on Debian
    etch). Newer versions of gmesh (>2.0) can also save in this format
    (choose "1.0" as the right format, not "2.0")
    """

    log.info("Reading '%s'" % filename)

    if filename[-3:] == '.gz': #assume this is gzipped data
        log.info("File '%s' seems to be gzipped, will uncompress..." % filename)
        import gzip
        f=gzip.open( filename )
    else:
        f=open( filename, "r" )

    lines_bits = map(string.split,f.readlines())


    assert lines_bits[0][0] == "$NOD","Internal Error, expected $NOD but got '%s'" % lines_bits[0][0]

    number_of_nodes = int(lines_bits[1][0])
    log.info("Number of nodes      = %7d" % number_of_nodes)

    #nodes = numpy.zeros((number_of_nodes,3),numpy.float64)
    nodes = [-1]*number_of_nodes
    #the node number is not used in nmesh
    #node_number = numpy.zeros((number_of_nodes,3),numpy.int64)
    node_number = [-1]*number_of_nodes

    new_node_id_by_gmsh_node_id = {}

    for i in range(2,number_of_nodes+2):
        node_nr = int(lines_bits[i][0])  
        node_number[i-2]=node_nr              #information unused in nmesh, but we need to use this
                                              #when parsing the simplex information.
        new_node_id_by_gmsh_node_id[node_nr] = i-2
        pos = map(float,lines_bits[i][1:])
        nodes[i-2] = pos

    assert lines_bits[number_of_nodes+2][0]=="$ENDNOD",\
           "Expected $ENDNOD in line %7d of '%s' but found '%s'" % \
           (number_of_nodes+2,filename,lines_bits[number_of_nodes+2][0])

    assert lines_bits[number_of_nodes+3][0]=="$ELM",\
           "Expected $ELM in line %7d of '%s' but found '%s'" % \
           (number_of_nodes+3,filename,lines_bits[number_of_nodes+3][0])

    number_of_elements = int( lines_bits[number_of_nodes+4][0] )
    log.info("Number of elements   = %7d" % number_of_elements)

    #check (for fun) whether relabelling nodes is necessary
    require_relabelling =0
    for key in new_node_id_by_gmsh_node_id:
        if new_node_id_by_gmsh_node_id[key] == key-1:
            pass
        else:
            require_relabelling += 1
    if require_relabelling:
        log.info("%d nodes out of %d will be relabelled" % (require_relabelling,number_of_nodes))

    simplices = []
    reg_physs = []
    reg_elems = []

    ignored_types = {}

    for k in range(1,1000):
        ignored_types[k]=0
    
    for i in range(number_of_nodes+5,number_of_nodes+5+number_of_elements):

        elm_number = int( lines_bits[i][0] )
        elm_type   = int( lines_bits[i][1] )

        if not elm_type in [4]: #i.e. this is not a tetrahedro
            ignored_types[elm_type] +=1
            continue

        reg_phys = int( lines_bits[i][2] ) #I think this is the region id
        reg_physs.append(reg_phys)

        reg_elem = int( lines_bits[i][3] ) #??
        reg_elems.append(reg_elem)
        
        number_of_elem_nodes = int( lines_bits[i][4] ) #redundant
        assert number_of_elem_nodes == 4, "Can only deal with 1st order "+\
               "tetrahedra, but number of nodes is "+\
               "%d for element %d in line %d in file %s" \
               % (number_of_elem_nodes,elm_number,i+1,filename)

        assert len(lines_bits[i]) == 5+number_of_elem_nodes,\
               "File data inconsistency (%s)" % filename

        simplex = map( int, lines_bits[i][5:5+number_of_elem_nodes] )


        #the node_ids in the gmsh file need to translated into consequtive node ids starting from 0.
        simplex_new_ids = map( lambda oldid : new_node_id_by_gmsh_node_id[oldid], simplex)

        simplices.append(simplex_new_ids)

    assert lines_bits[number_of_nodes+number_of_elements+5][0]=="$ENDELM",\
           "Expected $ENDELM in line %d of '%s' but found '%s'" % \
           (number_of_nodes+number_of_elements+5,filename,lines_bits[number_of_nodes+number_of_elements+5])


    log.info("Number of tetrahedra = %7d" % len(simplices))

    if max( ignored_types.values() ) > 0:
        log.warning(" Some element types have been ignored:")
        msg = ""
        for k in range(1,1000):
            if ignored_types[k] > 0:
                msg += " Type %d found %d times (and ignored)\n" % (k, ignored_types[k])
        log.warning(" Detailed occurences of ignored element types in %s\n%s" % (filename,msg[:-1]))

    reg_phys_dic = count_occurences(reg_physs)
    log.info("Physical Regions:\n%s" % report_occurences_string(reg_phys_dic,label="\t reg_phys "))

    reg_elem_dic = count_occurences(reg_elems)
    log.log(15,"Element Regions:\n%s" % report_occurences_string(reg_elem_dic,label="\t reg_elem "))

    if len(simplices) == 0:
        log.error("No first order tetrahedra found -- stopping. Hint: is this a 3d-mesh?")
        raise IOError,"Can't make sense of data in %s" % filename

    points = nodes

    simplices_regions = reg_physs


    #sanity check for region ids:

    number_of_regions = len(reg_phys_dic.keys())
    max_index_of_regions = max(simplices_regions) 
    min_index_of_regions = min(simplices_regions) 

    logging.info("Number of physical regions: %d" % number_of_regions)
    logging.info("Smallest physical region id: %d" % min_index_of_regions)
    logging.info("Largest physical region id: %d" % max_index_of_regions)

    relabel_str = "Consider re-labelling volume ids in your .geo file or CAD source for gmsh."

    if min_index_of_regions != 1:
        logging.warn("Nmag's load_mesh command (v0.1) is most easily used if the lowest region id is 1 (it is %d in your file).\n%s " % (min_index_of_regions,relabel_str))

    if (max_index_of_regions) != number_of_regions:
        regions_in_this_file = reg_phys_dic.keys()
        regions_in_this_file.sort()
        logging.warn("Nmag's load_mesh command (v0.1) is most easily used if the physical region ids start at 1 and increase consequetively (i.e. [1, 2, 3, ...] not %s).\n%s" % (regions_in_this_file,relabel_str))

    
    assert len(simplices) == len(simplices_regions)

    log.info("Completed reading '%s'" % filename)
    
    paranoia_mode = True
    if paranoia_mode:
	
	    #check that we remove unused nodes
	    elem_by_node = {}
	    for e in simplices:
	        for n in e:
	            if elem_by_node.has_key(n):
	                elem_by_node[n].append(e)
	            else:
	                elem_by_node[n] = [e]
	
	    okay_points = elem_by_node.keys()
	
	    log.info("Paranoia check: Found %d points used " % len(okay_points))
	    for i,point in enumerate(points):
	        if i in okay_points:
	            pass
	        else:
	            log.warn("point %d (at %s) not used" % (i,point))
	
	    return (points, simplices, simplices_regions)
	        
    

def ReadMeshFromGmesh(filename):


    conversion_help = """
To convert a gmsh-mesh 'infile.msh' written in file format 2.0 to file format 1.0
---------------------------------------------------------------------------------


If you already have the gmsh mesh file in format 2.0, then you can use

$> gmsh -3 -format msh1 -o outfile.msh infile.msh

to create 'outfile.msh' which contains the mesh in the gmish file format 1.0.


If you create the mesh interactively, then
  * choose FILE -> SAVE AS,
  * select'Gmsh mesh (*.msh)' from the drop down list,
  * choose filename and click 'OK'
  * When the 'MSH Options' box appears, choose 'Version 1.0' from the
    drop down list in the Format field.
  * click 'OK'

If you create your meshes automatically from the command line, then add '--format msh1' to the command line
to instruct gmsh to write in the old 1.0 format.


--
"""

    if filename[-3:] == '.gz': #assume this is gzipped data
        log.info("File '%s' seems to be gzipped, will uncompress..." % filename)
        import gzip
        f=gzip.open( filename )
    else:
        f=open( filename, "r" )


    firstline = f.readline()

    if firstline[0:4] == "$NOD": #this is the old 1.0 format
        f.close()
        return ReadMeshFromGmeshVersion1_0(filename)
    elif firstline[0:11] == "$MeshFormat": #new format

        secondline = f.readline()
        format_ids = map(int, secondline.split())
        assert format_ids[0] == 2," Expected '2' in line two but got " % secondline

        binascii = ''
        if format_ids[1] == 0:
            binascii = 'ascii'
        elif format_ids[1] == 1:
            binascii = 'binary'
        else:
            raise NotImplementedError,"Expected 0 or 1 as format identifier (second line, second int) but got %s" % format_ids[1]

        log.info("File %s is Gmsh file format version 2.0 (%s)" % (filename,binascii))
        
        log.error("\nReading the gmesh 2.0 format is not support.\n"+
                  "--------------------------------------------\n" +
                  "Please save mesh as 1.0 format following the instructions below.\n"+
                  "(If you relly need the 2.0 format, then please report to nmag@soton.ac.uk).\n"+
                  conversion_help)
        log.error("Can't read gmsh file format 2.0")
        sys.exit(1)
    else:
        raise NotImplementedError,"Don't know how to read file starting with '%s'" % firstline



#Currently unused, maybe be helpful for gmesh 2.0
#
#    def find_section(lines,starttag,endtag=None):
#        section = []
#
#        endtag_found = False
#        starttag_found = False
#        
#        for i in range(len(lines)):
#            if line[0:len(starttag)]==starttag:
#                starttag_found = True
#                for j in range(i+1,len(lines)):
#                    if line[i][0:len(endtag)] == endtag:
#                        endtag_found = True
#                        break
#                    else:
#                        section.append(lines[j])
#                if not endtag_found:
#                    raise IOError,"Couldn't find tag '%s' in '%s'" % \
#                                  (endtag,filename)
#                



