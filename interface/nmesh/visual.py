"""This module contains a series of visualisation tools for the analysis
   of unstructured meshes created by the CED mesher. It has been designed
   for use whilst the CED mesher is running, however it can be used
   independently from a python interpreter. When this is the case the user
   must ensure they conform to the expected data structures.

   Author: James Kenny         
   Last modified: $Date$
   Email: jck103@soton.ac.uk
   
   Filename: visual.py

   Version: $Id$

   Documentation and support:
          Instructions for use have been included in the user's manual.
          More detailed understanding can be found from reading the source
          code. The author will attempt to solve any further issues by email.

   Notes: For a mesh to be considered complete by this set of tools it must
          at least contain the data fields 'SIMPLICES' and 'COORDS'.

   Command line options:
          + -h or --help will print this doc-string
          + -f or --file can be used to specify an input file
                   .dat file formed by Numeric.pickle or nmesh.visual.pickle
                   .vtk file containing vtkUnstructuredGrid
          + for a .dat file it will by default be displayed as a simple solid
            but further options exist:
                  -w or --wireframe will display as a wireframe
                  -s or --surface will display surface elements as a solid
                  -i or --in2circ will calculate the ratio inradius:circumradius
                          and colour mesh elements by this metric

   (fangohr 04/11/2007):

     Addition: we use very few of these routines for nmag/nsim (such
     as for example mesh2vtk).
  
     James' code depends on Numeric; this needs to be changed to numpy
     (if we intend to carry on using the code). I have just changed that
     for mesh2vtk (which is used my nmeshpp --vtk).

			  
"""
from __future__ import division

import nbase
log = nbase.logging.getLogger('nmesh')

import os


def output_file_location(filename,directory=None):
    import nmesh
    return nmesh.output_file_location(filename,directory=directory)
    
def import_vtkpython():
    try:
        import vtk as vtkpython
    except ImportError,msg1:
        try:
            import vtkpython
        except ImportError, msg2:
            raise ImportError, "vtkpython is required (is called vtk on Debian testing). Couldn't find it:\n'%s' and \n'%s'" % (msg1, msg2)
    return vtkpython


def import_pyvtk():
    try:
        import pyvtk
    except ImportError:
        raise ImportError, "pyvtk is required."
    return pyvtk


def pickle(data, filename):
    """Function which stores an array or list such as mesh_info=mesh.tolists()
       in a Numeric.pickle file so that it can be read again later. This is of
       particular use for saving mesh_info lists so that a mesh can be studied
       on a system without ocaml.

       Author: James Kenny                Last modified: 07/05/06

       Input: data            - array or list, such as mesh.tolists()
              filename        - file to save in
    """
    try:
        import Numeric
    except ImportError:
        raise ImportError, "Numeric is required for this function.\n"

    f = open(filename, mode='w')
    Numeric.pickle.dump(data, f)
    f.close()



def unpickle(filename):
    """Function to compliment pickle(). An array or list pickled in file
       <filename> is loaded back into RAM.

       Author: James Kenny                   Last modified: 07/05/06

       Input: filename           - file to unpickle data from

       Output: array or list as it was before pickling
    """
    try:
        import Numeric
    except ImportError:
        raise ImportError, "Numeric is required for this function.\n"

    f = open(filename, mode='r')
    data = Numeric.pickle.load(f)
    f.close()

    return data



def locate_data(mesh_info):
    """This function accepts a mesh_info in the same format as mesh2vtk()
       and searches through to find the location of 'POINT-BODIES', 'COORDS',
       'SURFACES', 'LINKS' and 'SIMPLICES'. It then returns a list which
       describes the location of each.

       Author: James Kenny                Last modified: 05/05/06

       Input: mesh_info
       Output: list of the form
               [<coords>,<links>,<simplices>,<point-bodies>,<surfaces>]

       Notes: if the input is missing 'COORDS' or 'SIMPLICES'
              the function exits because these are the minimum required data to
              describe a pyfem mesh.       
    """
    # find out where in mesh_info required data is stored
    point_bodies = None
    links = None
    coords = None
    surfaces = None
    simplices = None
    parts = []
    
    for i in range(len(mesh_info)):
        if mesh_info[i][0] == 'POINT-BODIES':
            point_bodies = i
        elif mesh_info[i][0] == 'COORDS':
            coords = i
        elif mesh_info[i][0] == 'SURFACES':
            surfaces = i
        elif mesh_info[i][0] == 'SIMPLICES':
            simplices = i

    # check the minimum necessary data is present
    if coords == None:
        raise ValueError, "data 'COORDS' not found in mesh_info.\n"
    if simplices == None:
        raise ValueError, "data 'SIMPLICES' not found in mesh_info.\n"

    # determine how many 'parts' there are in this mesh
    for i in mesh_info[simplices][2]:
        if i[-1] not in parts:
            parts.append(i[-1])

    # find the dimensions of the mesh and the type of simplex
    dims = len(mesh_info[coords][2][0])
    if (dims < 2) or (dims > 3):
        raise ValueError, "nmesh.visual only handles 2D and 3D meshes.\n"

    if len(mesh_info[simplices][2]) == 0: #no simplices in mesh
        simplexType = 'unknown'
    else:
        if len(mesh_info[simplices][2][0][0]) == 3:
            simplexType = 'triangle'
        elif len(mesh_info[simplices][2][0][0]) == 4:
            simplexType = 'tetra'
        elif len(mesh_info[simplices][2][0][0]) == 2:
            simplexType = 'line'
        else:
            raise ValueError, "simplices should be lines, triangles or tetrahedra.\n"

    return [coords, links, simplices, point_bodies, surfaces, dims, simplexType, parts]



def separate_parts(mesh_info, listOfParts):
    """This function separates mesh_info into mesh1, mesh2, ..., meshn
       depending upon the argument 'listOfParts'. This enables a mesh
       to be split into it's constituent parts for separate viewing,
       for example view parts 1&2 as a wireframe and the rest as a solid.

       Author: James Kenny            Last modified: 02/05/06

       Input: mesh_info   - as accepted by mesh2vtk (pyfem format)
              listOfParts - list of lists specifying which parts
                            to be separated, data for body indices not
                            in listOfParts will be omitted.

                            Alternatively, this can be a single integer
                            should only one part be of interest.

                            ie: if listOfParts=[[1,2],[5,6,7]] and there
                                are 9 parts. The data for parts 1&2 will be
                                the first element of returned list, then
                                data for 5&6&7 the second
                                
       Output: list of mesh parts, each element in similar format as mesh_info
    """
    import types

    try:
        import Numeric
    except ImportError:
        raise ImportError, "separate_parts() requires Numeric.\n"
    
    # if we're passed an integer convert it so a list of tuples
    if type(listOfParts) == types.IntType:
        listOfParts = [[listOfParts]]
    elif type(listOfParts) != types.ListType:
        raise TypeError, "listOfParts must be integer or list of tuples.\n"

    # find out some basic information about the mesh
    [coords, links, simplices, point_bodies, surfaces, dims, simplexType, parts] \
             = locate_data(mesh_info)

    # check that the user has not requested a part which is not present
    for i in listOfParts:
        for j in i:
            if j not in parts:
                raise ValueError, "there is no part %d in mesh.\n" % (i)
    
    # initialise a list to contain the returned data sets
    # and find out which indices correspond to parts of mesh
    partsData = []

    # if we couldn't find all of the required data, notify the user
    if point_bodies == None:
        raise ValueError, "this function requires 'POINT-BODIES' data.\n"
    if surfaces == None:
        print "separate_parts(): Warning, surface elements not present in input.\n"

    # for each requested combination of parts copy the mesh information
#    print mesh_info[3][2]
    for i in listOfParts:

        # initialise a temporary list for this set of parts and
        # also a dictionary and counter to be used for translating
        # point indices from the original mesh_info to the new data lists
        iData = []
        coordsTranslate = {}
        cTransCounter = 0

        # copy the structure of the mesh_info provided
        for j in range(len(mesh_info)):
            iData.append([])
            iData[j].append(mesh_info[j][0])
            iData[j].append(mesh_info[j][1])
            iData[j].append([])

        # run through mesh_info[point_bodies][2] and append "COORDS"
        # and "POINT-BODIES" to the new data lists        
        for j in range(len(mesh_info[point_bodies][2])):
            for k in i:
                if k in mesh_info[point_bodies][2][j]:
                    iData[coords][2].append(mesh_info[coords][2][j])
                    iData[point_bodies][2].append(mesh_info[point_bodies][2][j])
                    coordsTranslate[j]=cTransCounter
                    cTransCounter = cTransCounter + 1

        # run through mesh_info[simplices][2] and append "SIMPLICES" also ensure
        # point indices are correctly translated to work with the new point lists
        for j in range(len(mesh_info[simplices][2])):
            if mesh_info[simplices][2][j][-1] in i:
                translate = []
                for k in mesh_info[simplices][2][j][0]:
                    translate.append(coordsTranslate[k])
                    
                iData[simplices][2].append([translate,
                                            mesh_info[simplices][2][j][1],
                                            mesh_info[simplices][2][j][2],
                                            mesh_info[simplices][2][j][3]])

        # if 'SURFACES' data is present copy this for the parts required
        if surfaces:
            for j in range(len(mesh_info[surfaces][2])):
                # vary our approach depending upon format of part index
                if type(mesh_info[surfaces][2][j][-1]) == types.IntType:
                    if mesh_info[surfaces][2][j][-1] in i:
                        translate = []                    
                        for k in mesh_info[surfaces][2][j][0]:
                            translate.append(coordsTranslate[k])                    
                        iData[surfaces][2].append([translate,
                                                   mesh_info[surfaces][2][j][1],
                                                   mesh_info[surfaces][2][j][2],
                                                   mesh_info[surfaces][2][j][3]])
                        
                elif type(mesh_info[surfaces][2][j][-1] == types.TupleType):
                    for t in mesh_info[surfaces][2][j][-1]:
                        if t in i:
                            translate = []
                            for k in mesh_info[surfaces][2][j][0]:
                                translate.append(coordsTranslate[k])                    
                            iData[surfaces][2].append([translate,
                                                       mesh_info[surfaces][2][j][1],
                                                       mesh_info[surfaces][2][j][2],
                                                       mesh_info[surfaces][2][j][3]])

                else:
                    raise ValueError, "'SURFACES' must have Integer or Tuple part index.\n"
        
        partsData.append(iData)

    return partsData



def surface_only(mesh_info, listOfSurfaces=None):
    """This function takes a 'mesh_info' in similar form to mesh2vtk and
       returns reduced version(s) only containing information on the surface
       elements. If no surfaces are requested explicitly, all surfaces will
       be returned.

       The user should note that the icradii and ccradii data available to a
       surface element will not be the data of the volume element. For 3D a
       surface element would be a triangle and the icradii and ccradii for
       the surface element have been recalculated to reflect this.

       Author: James Kenny            Last modified: 02/05/06

       Input: mesh_info      - as required by separate_bodies or mesh2vtk

              listOfSurfaces - list of surfaces to be contained in the output
                               using same format as pyfem 'POINT-BODIES'

              ie: if listOfSurfaces=[(0,1),(2,)] then the surfaces of objects
                  0 and 1 will be included in the first data returned and
                  the surface of object 2 will be in the second returned dataset
                  The output is a list of lists, so returned[0] will be the
                  data for 0&1 and returned[1] will be the data for 2. The
                  datasets for 0&1 and 2 are separate and can be used to make
                  separate VTK datasets.
                  
       Output: a list structure only containing information for the surface
               elements. Suitable for input to mesh2vtk()

       Similar functions: outer_skin()
    """
    log.debug("Entering 'surface_only'")
    # find out some basic information about the mesh
    [coords, links, simplices, point_bodies, surfaces, dims, simplexType, parts] \
             = locate_data(mesh_info)

    # check the required data is available
    if point_bodies == None:
        raise ValueError, "this function requires 'POINT-BODIES' data.\n"
    if surfaces == None:
        raise ValueError, "data 'SURFACES' not found in mesh_info.\n"
    
    # return some or all surfaces depending upon what the user requested
    if listOfSurfaces == None:
        
        # initialise a list to contain the returned dataset
        # and a dictionary to help translate point indices later
        surfacesData = []
        coordsTranslate = {}
        cTransCounter = 0

        # Make empty copy of mesh_info 
        for i in range(len(mesh_info)-1):
            surfacesData.append([])
            surfacesData[i].append(mesh_info[i][0])
            surfacesData[i].append(mesh_info[i][1])
            surfacesData[i].append([])  #data missing


        # make a list of all points on any surface and populate surfacesData
        for i in range(len(mesh_info[point_bodies][2])):
            if len(mesh_info[point_bodies][2][i]) == 2:  # point is on a surface

                #Hans: I think the above line is a problem. The body may only exist on its own, i.e.
                #it has only a body number '1' but is still a surface point.
                
                coordsTranslate[i] = cTransCounter       # add to lookup hashtable
                cTransCounter = cTransCounter + 1       
                surfacesData[coords][2].append(mesh_info[coords][2][i])
                surfacesData[point_bodies][2].append(mesh_info[point_bodies][2][i])

        # make a list of all surface simplices according to mesh_info[4][2]
        for i in range(len(mesh_info[surfaces][2])):
            translate = []                
            for j in mesh_info[surfaces][2][i][0]:
                #if j==61:
                #    print "Just came across i=2, will start ipython"
                #    from IPython.Shell import IPShellEmbed
                #    ipshell = IPShellEmbed()
                #    ipshell() # this call
                translate.append(coordsTranslate[j])
                
            surfacesData[simplices][2].append([translate, mesh_info[surfaces][2][i][1],
                                               mesh_info[surfaces][2][i][2],
                                               mesh_info[surfaces][2][i][3]])
            
    else:
        # initialise a list to contain the returned datasets
        surfacesData = []
        
        # separate mesh_info into the required blocks
        for i in listOfSurfaces:

            # initialise a temporary list for this i
            # and a dictionary to convert the point indices for this mesh part
            iData = []
            coordsTranslate = {}
            cTransCounter = 0
        
            for j in range(len(mesh_info)-1):
                iData.append([])
                iData[j].append(mesh_info[j][0])
                iData[j].append(mesh_info[j][1])
                iData[j].append([])

            # check which points in the mesh lie on the required surfaces
            # and populate the point translation dictionary accordingly
            for j in range(len(mesh_info[point_bodies][2])):
                if len(mesh_info[point_bodies][2][j]) == 2:
                    for k in mesh_info[point_bodies][2][j]:
                        if k in i:
                            coordsTranslate[j] = cTransCounter
                            cTransCounter = cTransCounter + 1
                            iData[coords][2].append(mesh_info[coords][2][j])
                            iData[point_bodies][2].append(mesh_info[point_bodies][2][j])

            # include the surface elements for required surfaces
            for j in range(len(mesh_info[surfaces][2])):            
                if mesh_info[surfaces][2][j][-1] in i:
                    translate = []                
                    for k in mesh_info[surfaces][2][j][0]:
                        translate.append(coordsTranslate[k])
                    
                    iData[simplices][2].append([translate, mesh_info[surfaces][2][j][1],
                                                mesh_info[surfaces][2][j][2],
                                                mesh_info[surfaces][2][j][3]])

        surfacesData.append(iData)

    return surfacesData



def outer_skin(mesh_info, condition=">= 1"):
    """This function reduces mesh_info to contain only the outer skin
       of simplices. Can provide useful insight particularly for 3D
       meshes when used in conjunction with order_mesh(). The rule used
       can be controlled by the optional argument 'condition' which must
       be a string with a comparison and an integer. So the default ">= 1"
       will mean that only cells with 1 or more points on the surface will
       be shown.

       Author: James Kenny             Last modified: 10/05/06

       Input: mesh_info - as required by mesh2vtk

       Output: reduced mesh_info containing only simplices on surfaces
               ready for mesh2vtk but missing information on 'LINKS'
    """
    import types
    
    # find out some basic information about the mesh
    [coords, links, simplices, point_bodies, surfaces, dims, simplexType, parts] \
             = locate_data(mesh_info)
    if point_bodies == None:
        raise ValueError, "this function requires 'POINT-BODIES' data.\n"

    # initialise a list to contain the returned dataset
    # and a dictionary to help translate point indices later
    skin = []
    coordsTranslate = {}
    cTransCounter = 0

    # copy the structure of the input mesh_info
    for i in range(len(mesh_info)):
        skin.append([])
        skin[i].append(mesh_info[i][0])
        skin[i].append(mesh_info[i][1])
        skin[i].append([])

    # make a list of all points on any surface
    for i in range(len(mesh_info[point_bodies][2])):
        if len(mesh_info[point_bodies][2][i]) == 2:  # point is on a surface
            coordsTranslate[i] = cTransCounter
            cTransCounter = cTransCounter + 1
            skin[coords][2].append(mesh_info[coords][2][i])
            skin[point_bodies][2].append(mesh_info[point_bodies][2][i])

    # using the list of points on surfaces, create a new 'mesh_info'
    # containing those simplices which contain 'n(+)' points on a surface
    for i in range(len(mesh_info[simplices][2])):
        skin_simplex = False
        pointCounter = 0
        for j in mesh_info[simplices][2][i][0]:
            if len(mesh_info[point_bodies][2][j]) == 2:
                pointCounter = pointCounter + 1

        # cheat by compiling comparison code dependent upon arguments
        if eval('pointCounter '+condition) == True:
            skin_simplex=[]

        # if the simplex contains a point on a surface update our new dataset
        if type(skin_simplex) == types.ListType:
            for j in mesh_info[simplices][2][i][0]:
                # if the point is in our translation dictionary, translate it
                if j in coordsTranslate.keys():
                    skin_simplex.append(coordsTranslate[j])
                else:
                    # otherwise, add it to the dictionary and translate it
                    coordsTranslate[j] = cTransCounter
                    skin_simplex.append(cTransCounter)
                    cTransCounter = cTransCounter + 1
                    skin[coords][2].append(mesh_info[coords][2][j])
                    skin[point_bodies][2].append(mesh_info[point_bodies][2][j])
                
            skin[simplices][2].append([skin_simplex, mesh_info[simplices][2][i][1],
                                       mesh_info[simplices][2][i][2],
                                       mesh_info[simplices][2][i][3]])

    # whilst it seems unlikely that the user would require the 'SURFACES' data
    # AND use this function, it is possible and may prevent errors later
    if surfaces:
        for i in range(len(mesh_info[surfaces][2])):
            surface_simplex = []
            for j in mesh_info[surfaces][2][i][0]:
                surface_simplex.append(coordsTranslate[j])

            skin[surfaces][2].append([surface_simplex, mesh_info[surfaces][2][i][1],
                                      mesh_info[surfaces][2][i][2],
                                      mesh_info[surfaces][2][i][3]])

    return skin



def isRightTetra(tetra, points):
    """This function should determine whether a tetrahedron is 'right-handed'
       or not. If it is right-handed it will return 1 else it will return 0.

       Author: James Kenny (adapted from code by Matteo Franchin)

       Input: list of four point indices (list of integers)
              list of points             (list of tuples of floats or integers)
       Output: 1 if right-handed
               0 if not
    """
    a = points[tetra[0]]
    b = points[tetra[1]]
    c = points[tetra[2]]
    d = points[tetra[3]]

    # find the three vectors a-b, b-c, c-d
    # computes volume as det([a-b,b-c,c-d])
    # as in http://en.wikipedia.org/wiki/Tetrahedron
    v1 = [a[0]-b[0], a[1]-b[1], a[2]-b[2]]
    v2 = [b[0]-c[0], b[1]-c[1], b[2]-c[2]]
    v3 = [c[0]-d[0], c[1]-d[1], c[2]-d[2]]
   
    volume = v1[0]*(v2[1]*v3[2] - v2[2]*v3[1]) \
             - v1[1]*(v2[0]*v3[2] - v2[2]*v3[0]) \
             + v1[2]*(v2[0]*v3[1] - v2[1]*v3[0])

    # if it's positive the tetrahedron is right-handed
    if volume > 0:
        return 1
    else:
        return 0

    

def order_mesh(mesh_info, axis=0, data=None):
    """This function should rearrange the order of simplices in a 3D mesh
       such that they are indexed by the position of their incentre along the
       axis specified by the user.

       The user can alternatively provide a dataset by which the mesh will be
       ordered. This dataset should have one entry for each simplex of the mesh.
       It will then be possible to use the filter ExtractUnstructuredGrid to
       view only those simplices in the range of interest.

       As part of the process for 3D volume meshes all tetrahedra are forced
       to conform with the right hand rule required by most graphical rendering
       systems. This is necessary to ensure that all faces are drawn.

       Author: James Kenny             Last modified: 10/05/06

       Input: mesh_info
              axis = 0, 1 or 2
              data = list, tuple or linear array of length = # simplices
              
       Output: mesh_info with the 'SIMPLICES' data rearranged accordingly
    """
    # try importing Numeric 
    try:
        import Numeric
    except ImportError:
        raise ImportError, "Numeric is required by this function.\n"
    
    # find out some basic information about the mesh
    [coords, links, simplices, point_bodies, surfaces, dims, simplexType, parts] \
             = locate_data(mesh_info)
    if (simplexType != 'triangle') and (simplexType != 'tetra'):
        raise ValueError, "can only sort meshes of triangles or tetrahedra.\n"

    # if 3rd party data has been provided check it's suitable
    if data != None:
        try:
            if len(data) != len(mesh_info[simplices][2]):
                raise ValueError, "data must have one entry for each simplex.\n"
        except ValueError:
            raise ValueError, "data must be tuple, list or linear array.\n"        

    # copy mesh_info except for simplices data which will be appended later
    mesh_info2 = []
    for i in range(len(mesh_info)):
        mesh_info2.append([])
        mesh_info2[i].append(mesh_info[i][0])
        mesh_info2[i].append(mesh_info[i][1])
    mesh_info2[coords].append(mesh_info[coords][2])
    if links:
        mesh_info2[links].append(mesh_info[links][2])
    if surfaces:        
        mesh_info2[surfaces].append(mesh_info[surfaces][2])
    if point_bodies:
        mesh_info2[point_bodies].append(mesh_info[point_bodies][2])

    # unless the user has advised that the tetra are already right-handed
    # employ isRightTetra() to ensure that all tetra are correctly defined
    newsimplices = []
    for i in mesh_info[simplices][2]:
        if simplexType == 'tetra':
            if isRightTetra(i[0],mesh_info[coords][2]) == 1:
                newsimplices.append(i)
            else:
                # swap the first two tetrahedron points
                newsimplices.append([[i[0][1],i[0][0],i[0][2],i[0][3]], \
                                     i[1],i[2],i[3]])
        else:
            newsimplices.append(i)

    # start by creating a dictionary and sorting its keys, do this based on
    # the position of the incentre or by dataset provided by user if appropriate
    # each dictionary entry is a list, otherwise simplices with the same key
    # would be lost
    mesh_info2[simplices].append([])
    simplexIndex = {}

    if data == None:
        for i in range(len(mesh_info[simplices][2])):
            if mesh_info[simplices][2][i][2][0][axis] in simplexIndex.keys():
                simplexIndex[mesh_info[simplices][2][i][2][0][axis]].append(i)
            else:
                simplexIndex[mesh_info[simplices][2][i][2][0][axis]] = [i]
    else:
        for i in range(len(mesh_info[simplices][2])):
            if data[i] in simplexIndex.keys():
                simplexIndex[data[i]].append(i)
            else:
                simplexIndex[data[i]] = [i]
    sortedKeys = Numeric.sort(simplexIndex.keys())

    # now run through the sorted dictionary of lists 
    # and append simplices to mesh_info2 in the required order
    for i in range(len(sortedKeys)):
        for j in simplexIndex[sortedKeys[i]]:
            mesh_info2[simplices][2].append(newsimplices[j])

    return mesh_info2
    
    

def skin_mesh(mesh_info, condition='>=1'):
    """This function, similar to order_mesh, rearranges the order in which
       simplices are listed in a mesh. Thus any vtkUnstructuredGrid created
       from it will have cell indices such that by using the filter
       ExtractUnstructuredGrid cells can be clipped in a manner which peels
       layers off the mesh, much like skins off an onion (hence the name).

       In a similar way that the condition can be specified by the user for
       outer_skin() so it can here. This is the condition for how many points
       must be in a simplex for it to be considered part of this layer. With
       the default condition '>=1' whenever a cell has one or more points from
       the list of interest for this layer, it will be included in that layer.
       So by changing condition = '>=3' at least one entire face of the tetra
       must be made up of these points, so there will be a larger number of
       thinner (less smooth) layers. (This will significantly increase runtime)

       Author: James Kenny             Last modified: 10/05/06

       Input: mesh_info

       Output: a copy of mesh_info where the simplices have been ordered.

       Notes: it is recommended that this function is only used on single
              parts - for example if the mesh contains a box and a sphere,
              this should be used for either the box or the sphere. Whilst
              it should work for both at the same time the effect is not
              particularly useful; and the function has not been tested
              with this in mind, so the risk of failure is higher.
    """
    try:
        import Numeric
    except ImportError:
        raise ImportError, "Numeric is required for this function.\n"
    
    # find out some basic information about the mesh
    [coords, links, simplices, point_bodies, surfaces, dims, simplexType, parts] \
             = locate_data(mesh_info)
    if point_bodies == 0:
        raise ValueError, "data 'POINT-BODIES' is required.\n"
    if dims != 3:
        raise ValueError, "this function is solely for 3D meshes.\n"

    # copy mesh_info except for simplices data which will be appended later
    mesh_info2 = []
    for i in range(len(mesh_info)):
        mesh_info2.append([])
        mesh_info2[i].append(mesh_info[i][0])
        mesh_info2[i].append(mesh_info[i][1])
    mesh_info2[coords].append(mesh_info[coords][2])
    mesh_info2[point_bodies].append(mesh_info[point_bodies][2])    
    if links:
        mesh_info2[links].append(mesh_info[links][2])
    if surfaces:        
        mesh_info2[surfaces].append(mesh_info[surfaces][2])
    mesh_info2[simplices].append([])

    # find list of points on the surface of our mesh
    points2Search = []
    for i in range(len(mesh_info[point_bodies][2])):
        if len(mesh_info[point_bodies][2][i]) == 2:
            points2Search.append(i)
    
    # now recursively find a skin of tetrahedra and build a list of tuples
    # giving the cellMin and cellMax of each skin (ready for ExtractUnstructuredGrid)
    listOfSkins = []
    findLayer = True
    cellMax = 0
    lastCellMax = 0

    while findLayer == True:
        nextPoints2Search = []
        simplicesToDelete = []
        
        # run through points2Search and see which simplices use these points
        # this is not as inefficient as it first appears, because simplices are
        # deleted from mesh_info once they've been placed in mesh_info2
        for i in range(len(mesh_info[simplices][2])):
            pointsCounter = 0
            for j in mesh_info[simplices][2][i][0]:
                if j in points2Search:
                    pointsCounter = pointsCounter + 1

            # if this simplex contains a point in points2Search
            # add it to mesh_info2 and prepare to remove it from mesh_info
            if eval('pointsCounter '+condition) == True:
                mesh_info2[simplices][2].append(mesh_info[simplices][2][i])
                simplicesToDelete.append(i)
                cellMax = cellMax + 1

                # find out which points in this simplex are not in points2Search
                for j in mesh_info[simplices][2][i][0]:
                    if j not in points2Search:
                        nextPoints2Search.append(j)

        # remove the simplices copied to far
        Numeric.sort(simplicesToDelete)
        delCount = 0
        for i in simplicesToDelete:
            mesh_info[simplices][2].__delitem__(i-delCount)
            delCount = delCount + 1

        # make a note of the indices for these simplices
        listOfSkins.append((lastCellMax,cellMax))
        lastCellMax = cellMax
        points2Search = nextPoints2Search

        # if there are no points left, exit
        if nextPoints2Search == []:
            findLayer = False
            

    return mesh_info2, listOfSkins
    


def mesh2vtk(mesh_info,VTKonly=True,body_numbers=True,node_numbers=False):
    """This function takes a mesh and creates a VTK data set from it using
       pyvtk. Additionally it returns the principal mesh data in a convenient
       series of lists. 
    
       Author: James Kenny             Last modified: 23/04/06

       Input: raw mesh data   - output of mesh.tolists()
              VTKonly         - if the user requires the additional outputs of
                                this function, VTKonly should be set to 'False'
              body_numbers    - if True, then a scalar data set is attached to every
                                simplex corresponding to the body id that this node belongs to.
              node_numbers    - if True, then each node carries its id as a scalar value.

              



       Output:
       if VTKonly==True, then return
               vtkData     - pyvtk dataset

       else return:

               vtkData     - pyvtk dataset
               points      - list of points
               simplices   - list of simplices (triangles or tetrahedra)
               indices     - list of part indices (ready for append2vtk)
               icradii     - list of incircle/insphere radii
               ccradii     - list of circumcircle/circumshpere radii
       
       Requires: pyvtk, Numeric

       Notes: any simplex with a body index of -9 is ignored. This can be
              useful for user-defined functions which wish to 'mask' simplices.
              The bounding box (index -1) will still be included.
    """
    try:
        import numpy
    except ImportError:
        raise ImportError, "numpy is required.\n"

    try:
        import pyvtk
    except ImportError:
        raise ImportError, "pyvtk is required.\n"


    log.debug("Entering mesh2vtk")
 

    # determine whether we have 2D or 3D data
    dim = len(mesh_info[0][2][0])
    if (dim < 2) or (dim > 3):
        raise ValueError, "Mesh should be 2D or 3D." 
    else:
        log.info( "Dimension of the space is %d" % (dim))

    # find out some basic information about the mesh
    [coords, links, simplexes, point_bodies, surfaces, dims, simplexType, parts] \
             = locate_data(mesh_info)

    # load points and bodies and initialize lists for other variables
    points = mesh_info[coords][2]
    ccradii = []
    icradii = []

    # 2D case
    if dim == 2:
        simplices = []
        simplexIndices = []
            
        # read appropriate parts of the mesh ready for PyVTK
        for i in mesh_info[simplexes][2]:
            if i[-1] != -9:       # flagged cells ignored
                simplices.append(i[0])
                simplexIndices.append(i[-1])
                ccradii.append(i[1][1])
                icradii.append(i[2][1])            

        # now create the vtkUnstructuredGrid using PyVTK
        # extend the 2D points with a null coordinate in the 3rd
        # direction to avoid vtk complaints
        size = len(points)
        zeros = numpy.zeros(size)
        zeros = numpy.reshape(zeros,(zeros.shape[0],1))
        points = numpy.concatenate((points,zeros), axis=1)

        # use pyvtk to convert the mesh data into a vtkUnstructuredGrid
        #if len(simplices[0]) == 2:
        if simplexType=='line':
            ug = pyvtk.UnstructuredGrid(points, line=simplices)
        elif simplexType=='triangle':
            ug = pyvtk.UnstructuredGrid(points, triangle=simplices)
        else:
            raise NotImplementedError,"Cannot deal with simplexType='%s' in 2 dimensions" % simplexType

        header = "2D Unstructured Mesh"
    # 3D case
    elif dim == 3:
        simplices = []
        simplexIndices = []
            
        # read appropriate parts of the mesh ready for PyVTK
        for i in mesh_info[simplexes][2]:
            if i[-1] != -9:           # flagged cells ignored
                simplices.append(i[0])
                simplexIndices.append(i[-1])
                ccradii.append(i[1][1])
                icradii.append(i[2][1])

        # use pyvtk to convert the mesh data into a vtkUnstructuredGrid
        #if len(simplices[0]) == 3:
        if simplexType=='triangle':
            ug = pyvtk.UnstructuredGrid(points, triangle=simplices)
        elif simplexType=='tetra':
            ug = pyvtk.UnstructuredGrid(points, tetra=simplices)
        else:
            raise NotImplementedError,"Cannot deal with simplexType='%s' in 3 dimensions" % simplexType

        header = "3D Unstructured Mesh"

    if body_numbers:
        vtkData = pyvtk.VtkData(ug,
                                pyvtk.CellData(pyvtk.Scalars( simplexIndices, "Body number" ) ),
                                header,format="binary")
    else:
        vtkData = pyvtk.VtkData(ug,
                                header,format="binary")

    if node_numbers:
        vtkData = append2vtk(vtkData,data=range(len(mesh_info[0][2])), title="Node id",sites='point' ) 

    log.debug("About to leave mesh2vtk")

    # if only the vtkData was requested, then return that alone
    if VTKonly == False:    
        return vtkData, points, simplices, simplexIndices, icradii, ccradii
    else:
        return vtkData


def append2vtk(vtkData, data, title, sites='cell'):
    """This function appends vector or scalar data to a VTK data set
       in pyvtk format. The data can be applied as cell or point data.
       Application as point data will require the points of a mesh to
       be included in the VTK data set in addition to the normal
       simplices.
       
       Author: James Kenny             Last modified: 22/04/06
               Hans Fangohr            26/06/2006

       Input:
         vtkData:  (vtkData structure to which the data should be added)
         data   :  the actual scalar or vector data to be added
         title  :  name of the data (as it appears in the vtk data set, Legend, MayaVi)
         sites  :  Either 'cell' or 'point', depending on wether the provided data
                   is associated with cells or nodes(=vertices).
                   
       Output: pyvtk mesh with data set appended

       Requires: pyvtk
    """
    import types, copy
    try:
        import pyvtk
    except ImportError:
        raise ImportError, "pyvtk could not be imported.\n"

    vtkData2 = copy.deepcopy(vtkData)

    # if cell_data is to be appended
    if sites == 'cell':
        if type(data[0]) in (types.FloatType, types.IntType):
            vtkData2.cell_data.append(pyvtk.Scalars(data, title))
        else:
            raise ValueError, "cell_data must be scalar.\n"

    # if point_data is to be appended    
    elif sites == 'point':
        if type(data[0]) in (types.FloatType, types.IntType):
            vtkData2.point_data.append(pyvtk.Scalars(data, title))
        elif len(data[0]) == 3:                
            vtkData2.point_data.append(pyvtk.Vectors(data, title))
        else:
            raise ValueError, "point_data must either be scalar or 3d vector.\n"

    else:
        raise IOError, "data sites must be 'cell' or 'points'.\n"
        
    return vtkData2



def findRatios(list1, list2, factor=1):
    """This function computes the ratio:  factor*(list1[i] / list2[i]).

       Author: James Kenny             Last modified: 22/04/06
       
       Output: Numeric.Float32 array containing the ratios requested

       Notes: If being used to calculate inradius : circumradius ratio
              factor=2 for 2D and factor=3 for 3D will ensure that ideal
              simplices have a quality of 1.
    """
    import Numeric
    
    # check the two lists are of equal length
    if len(list1) != len(list2):
        raise ValueError, "findRatios(): lists must be of equal length.\n"

    # initialise an appropriately sized array
    ratios = []

    # populate array with required ratios
    for i in range(len(list1)):
        if (list2[i] == 0) :
            ratios.append(0.0)
        else:
            ratios.append(abs( factor*(list1[i])/list2[i]))
    return ratios


def compute_triangle(points, tri):
    """This is a function which computes the angles in a triangle
       or on the face of a tetrahedron.

       Author: James Kenny              Last modified: 10/05/06

       Input: list of points in mesh
              list of indices for a triangle
       Output: three angles in degrees (list)
    """
    try:
        import Numeric, math
    except ImportError:
        raise ImportError, "this function requires Numeric and math.\n"
    
    # define three rotations of the same triangle so that the same
    # algorithm can be used to find each angle
    a = points[tri[0]]; b = points[tri[1]]; c = points[tri[2]]
    tris = [[a, b, c], [b, c, a], [c, b, a]]
    angles = []

    for i in tris:
        # define 2 vectors representing 2 sides of the triangle
        # either side of the angle to be calculated
        r_ab = Numeric.transpose(i[1]) - i[0]
        r_ac = Numeric.transpose(i[2]) - i[0]
        
        # calculate the angle between the two sides
        rdotr = Numeric.dot(r_ab, r_ac)
        modrr = math.sqrt(sum(r_ab**2))*math.sqrt(sum(r_ac**2))

        angle = math.degrees(math.acos(rdotr / modrr))
        angles.append(angle)
    
    tangles = Numeric.zeros([3],Numeric.Float32)
    tangles = [angles[0], angles[1], angles[2]]
    
    return tangles



def findAngles(mesh_info):
    """Method to calculate internal angles of triangles, or triangular
       faces.

       Author: James Kenny             Last modified: 10/05/06

       Input: points and cells of a mesh
       Output: list of angles
    """
    # find out some basic information about the mesh
    [coords, links, simplices, point_bodies, surfaces, dims, simplexType, parts] \
             = locate_data(mesh_info)

    # check we have what we need
    if simplexType != 'triangle':
        raise ValueError, "this function is for triangular meshes only.\n"

    # find the internal angles of each triangle
    cells = mesh_info[simplices][2]
    angles = []       
    for i in range(len(cells)):
        angles.append( compute_triangle(mesh_info[0][2], cells[i][0]) )

    return angles



def save_vtk(vtkData, filename, format='binary',directory=None):    
    """This function simply provides a hook into the .tofile() method of a
       pyvtk data object.    
    """
    try:
        import pyvtk
    except ImportError:
        raise ImportError, "pyvtk is required.\n"    

    path = output_file_location(filename,directory)

    log.debug("Saving vtk file to '%s'" % path)
    vtkData.tofile(path, format=format)

    return path




def VTKfromRAM(pyvtkData):
    """This method uses vtkpython to read the vtkData from a string in
       memory rather than from a file - thus writing to disk is not necessary.

       Author: James Kenny             Last modified: 02/05/06
       
       Input: pyvtk mesh data
       Output: vtkpython mesh data

       Limitations: the current implementation only works for vtkUnstructuredGrid
       
       Requires: vtkpython, pyvtk

       Notes: apparently there is a memory leak associated with reader.GetOuput()
              This does not present a severe problem, but the user should be
              aware that this could potentially cause system slow-down through
              repeated use. From experience it would seem to require of the
              order of 500 calls of this function to cause noticeable problems.
    """
    # try to import vtkpython and pyvtk

    vtkpython = import_vtkpython()
    pyvtk = import_pyvtk()
    

    # required modules have been imported, convert pyvtk data to a string
    # and use the vtkpython reader to process it
    reader = vtkpython.vtkUnstructuredGridReader()
    reader.ReadFromInputStringOn()
    temp_string = pyvtkData.to_string()
    reader.SetInputString(temp_string)
    reader.Update()

    return reader.GetOutput()



def mesh2mayavi(input, myv=None, module='SurfaceMap', lut_range=None):
    """Displays a mesh in VTK form using MayaVi.

       Author: James Kenny            Last modified: 09/05/06

       Input: - input is either  a vtk filename (string)
                or a vtk object (for example created with mesh2vtk).
              - if myv is specified it will be used as the handle
                for an existing MayaVi instance. (ie if myv is given
                an existing window will be used, if it is not then a new
                one will be opened)
              - by specifying module as a string the user can force which
                module MayaVi uses, the recommended options are:
                    'SurfaceMap'     (which is the default)
                    'Glyph'          (useful for points and vectors)
                    'Axes'
                    'Outline'        (draws a box round the visualisation)
                The user should be careful that the correct type of data
                has been provided, any of MayaVi's modules can be requested.
              - lut_range is an optional argument (tuple) for specifying
                the range of the scalar_look_up_table in MayaVi. For example
                if the lut_range should be from 0 to 1 then lut_range=(0,1)
                
       Output: new or updated MayaVi visualisation of mesh
               returns the MayaVi instance handler

       Requires: MayaVi
    """     
    #Shouldn't need this anymore, Hans 26 April
    ## temporary fix to overcome implementation issues with pyfem
    import sys
    sys.argv=[""]

    import types
    
    try:
        import mayavi
    except ImportError:
        raise ImportError, "mayavi module not avaiable.\n"
            
    # check that the module requested is sane
    if type(module) != types.StringType:
        raise ValueError, "module must be specified as a string.\n"

    # detect whether we've been given a vtkdataobject from VTKfromRAM()
    # or a vtk filename
    if type(input) == types.StringType:
        if input[-4:] != ".vtk":
            raise IOError, "filename must include .vtk extension.\n"
        else:
            fromRAM = False
    else:
        # unfortunately there is no explicit capture for vtkdataobjects
        fromRAM = True
        input = VTKfromRAM(input)

    # if we've been passed a MayaVi instance then update it
    # if not, create a new one
    if myv == None:
        myv = mayavi.mayavi()
        
    # load input VTK dataset from file or RAM?
    if fromRAM == False:
        v = myv.open_vtk(input,config=0)
    else:
        v = myv.open_vtk_data(input)

    # load requested module
    m = myv.load_module(module,0) 

    # if lut_range given apply to default scalar set
    if lut_range:
        dvm = myv.get_current_dvm()
        mm = dvm.get_current_module_mgr()
        mm.scalar_lut_h.range_on_var.set(1)
        mm.scalar_lut_h.range_var.set(lut_range)
        mm.scalar_lut_h.set_range_var()    

    # return the MayaVi instance handler
    return myv



def rotate(myv, roll=0, pitch=0, yaw=0):
    """This function enables intuitive user control of the viewing angle
       used in MayaVi instance myv.

       Author: James Kenny            Last modified: 26/04/06

       Input: MayaVi instance handler
              position vector - (blah, blah, blah)
    """
    myv.renwin.camera.Roll(roll)
    myv.renwin.camera.Pitch(pitch)
    myv.renwin.camera.Yaw(yaw)
    myv.renwin.ren.ResetCamera()
    myv.Render()



def export_visualisation(myv, filename,directory=None):
    """This function saves the visualisation in the MayaVi window handled
       by myv, to a PortableNetworkGraphic or EncapsulatedPostScript

       Author: James Kenny              Last modified: 26/04/06

       Input: myv      - handle on a MayaVi instance
              filename - filename to save to, the type of file extension
                         in filename will influence the format saved
                         
                         If no file extension given, nothing is done.

       Notes: The output image is obtained by a screen capture method.
              The user should therefore ensure that the MayaVi visualisation
              is not obscured by other windows when an image is saved. It would
              not be a simple modification to force off screen rendering of
              images. Whilst possible, it requires a new render window to be
              created or a different X display to be specified. The MayaVi
              mailing list has some useful thoughts on this.
    """
    vtkpython = import_vtkpython()
    
    import mayavi

    if "." not in filename:
        raise ValueError, "must specify file extension.\n"
    else:
        filename = output_file_location(filename,directory)
        import sys
        print "DDD sys.argv=",sys.argv
        
        if ".png" in filename.lower():
            # save a PNG file
            log.debug("About to save mayavi visualisation to %s" % filename)
            myv.renwin.save_png(filename)
            
        elif ".eps" in filename.lower():
            # save an EPS file
            ex = vtkpython.vtkGL2PSExporter()
            ex.SetFilePrefix(filename[:-4])
            ex.SetFileFormatToEPS()
            ex.SetSortToBSP()
            log.debug("About to save mayavi visualisation to %s" % filename)
            myv.renwin.save_gl2ps(filename, exp=ex)

            # naughty cheat to rename the file appropriately because maya saves as gzipped eps
            import os
            os.rename(filename, filename+'.gz')
            #and uncompress
            log.debug("Will attempt to uncompress %s.gz" % filename)
            
            returnval = os.system("gunzip -f "+filename)
            if returnval != 0:
                log.error("Tried to uncompress %s.gz but something went wrong" % filename)
                log.error("Is in possible there is no 'gunzip' program in the search PATH?")
                log.error("Will carry on leaving %s.gz compressed" % filename)
        else:
            # file extension specified was invalid
            raise IOError, "file extension specified is not supported.\n"





def part_scalars(mesh_info, listOfParts, sites='cell'):
    """This function enables the user to create a new scalar dataset
       ready for appending to a VTK data set. The data is dependent upon
       the argument 'listOfParts'.

       Author: James Kenny            Last modified: 02/05/06

       Input: mesh_info   - same as input to separate_bodies()
              listOfParts - list of tuples, specifying a scalar value
                            to be appended to each element

              sites       - ='cell' will create cell_data
                            ='points' will create point_data

          ie:  if listOfParts=[(1,10),(5,7,1)] and sites='cell' then
               every cell belonging to body 1 will have a scalar of 10
               and cells belonging to bodies 5 and 7 will have a scalar
               of 1.

       Output: a 1D array in the correct format for append2vtk as either
               cell_data or point_data dependent upon how it was
               specified. Cells whose value has not been specified will
               assume a value of zero.
    """
    try:
        import Numeric
    except ImportError:
        raise ImportError, "could not import the Numeric libraries.\n"

    # find out some basic information about the mesh
    [coords, links, simplices, point_bodies, surfaces, dims, simplexType, parts] \
             = locate_data(mesh_info)

    # initialise an array in the correct format for append2vtk
    if sites=='cell':
        partData = Numeric.zeros([len(mesh_info[simplices][2])],Numeric.Float32)

        # run through mesh_info and add scalars for each cell
        for i in range(len(mesh_info[simplices][2])):
            for j in listOfParts:
                if mesh_info[simplices][2][i][-1] in j[:-1]:
                    partData[i] = j[-1]
        return partData
                    
    elif sites=='point':
        partData = Numeric.zeros([len(mesh_info[point_bodies][2])],Numeric.Float32)

        # run through mesh_info and add scalars for each point
        for i in range(len(mesh_info[point_bodies][2])):
            for j in listOfParts:
                if mesh_info[point_bodies][2][i][-1] in j[:-1]:
                    partData[i] = j[-1]
        return partData
        
    else:
        raise ValueError, "please define sites='cell' or sites='point'.\n"
    


##################### HIGH-LEVEL CONVENIENCE FUNCTIONS #########################



def basic_solid(mesh_info, part=None, order=False, myv=None, rgb=None):
    """This function simplifies the process of visualising a mesh as a solid
       with some basic appearance settings automated. Optionally the user can
       specify to only display a selection of mesh parts (if there are several)
       and if only one mesh part is asked for, this can be 'ordered' so that
       by use of the filter 'ExtractUnstructuredGrid' the user can view the
       interior of a 3D mesh. Finally the user may provide the handle for a
       MayaVi instance, this enables the function to use an existing MayaVi
       window for the visualisation.

       Multiple mesh parts may be specified as a tuple, ie: parts 1,2&3 would
       be specified  by part=(1,2,3).

       When order is specified, it should be an integer between 0 and 2
       to determine which axis the part is ordered along.

       A file 'basic_solid.vtk' will be created during run.

       The function returns it's handle on a MayaVi instance, this way these
       convenience functions can be used in conjunction with each other in the
       same visualisation window.

       If a tuple rgb=(R,G,B) is provided, then all objects will be couloured
       in that colour. Otherwise, the part number is used to colour simplices.
       (R,G and B should be floats between 0 and 1.)

       Author: James Kenny
       Last modified: (fangohr 05/07/2006 13:50)
    """
    import types
    try:
        import mayavi
    except ImportError:
        raise ImportError, "MayaVi is required for this function.\n"

    # find out some basic information about the mesh
    [coords, links, simplices, point_bodies, surfaces, dims, simplexType, parts] \
             = locate_data(mesh_info)

    # check that the request(s) made by the user were sensible
    if part != None:
        if type(part) not in [types.IntType, types.TupleType, types.ListType]:
            raise ValueError, "part must be integer, tuple or list of tuples.\n"
        if point_bodies == None:
            raise ValueError, "data 'POINT-BODIES' is required.\n"
    if (type(part) == types.TupleType) and (order != None):
        if len(part) > 1:
            raise ValueError, "order != False not valid with multiple parts.\n"
    if type(part) == types.TupleType:
        # wrap it in a list to keep separate_parts() happy
        part = [part]
    if order not in [False, 0, 1, 2]:
        raise ValueError, "order must be 0, 1 or 2 to specify ordering axis.\n"

    # things seem sensible, continue...
    if part != None:
        [mesh_info] = separate_parts(mesh_info, listOfParts=part)
    if order != False:
        mesh_info = order_mesh(mesh_info, axis=order)

    # create a VTK dataset from it and visualise by writing to a file first
    vtkData, points, simplices, indices, icradii, ccradii = mesh2vtk(mesh_info, VTKonly=False,body_numbers=False)
    vtkData = append2vtk(vtkData, indices, "part indices")
    save_vtk(vtkData, 'basic_solid.vtk')

    # if we've been given a MayaVi window to use, make sure we empty it first
    if myv != None:
        myv.close_all()    
    v = mesh2mayavi('basic_solid.vtk', myv=myv)

    # now change some of the aesthetics manually
    dvm = v.get_current_dvm()
    mm = dvm.get_current_module_mgr()

    if rgb != None:
        # make this a rgb-coloured solid    
        m1 = mm.get_module(0)                # now have handle on the 'SurfaceMap'
        m1.mapper.SetScalarVisibility(0)
        m1.act.GetProperty().SetColor((0,1,0))  # make it green (R,G,B)
        m1.renwin.Render()
    
    # overlay a black wireframe
    m2 = v.load_module('SurfaceMap',0)
    m2.actor.GetProperty().SetRepresentationToWireframe()
    m2.mapper.SetScalarVisibility(0)
    m2.renwin.Render()

    # if the user requested the mesh be ordered, open up the configuration box
    # for filter 'ExtractUnstructuredGrid' and set CellClipping to 'On'
    if type(order) == types.IntType:
        f = v.load_filter('ExtractUnstructuredGrid',0)
        f.fil.SetCellClipping(1)
        f.configure()
    
    # return the handle on the current MayaVi instance
    return v
    
    
        
def basic_surface(mesh_info, part=None, myv=None):
    """This function should offer a similar representation to basic_solid()
       but only displays the surface elements, this is more time efficient if
       only the external topology is of interest to the user.

       Author: James Kenny                  Last modified: 09/05/06
    """
    import types
    try:
        import mayavi
    except ImportError:
        raise ImportError, "MayaVi is required for this function.\n"

    # find out some basic information about the mesh
    [coords, links, simplices, point_bodies, surfaces, dims, simplexType, parts] \
             = locate_data(mesh_info)

    # check that the request(s) made by the user were sensible
    if part != None:
        if type(part) not in [types.IntType, types.TupleType, types.ListType]:
            raise ValueError, "part must be integer, tuple or list of tuples.\n"
        if point_bodies == None:
            raise ValueError, "data 'POINT-BODIES' is required.\n"
    if type(part) == types.TupleType:
        # wrap it in a list to keep separate_parts() happy
        part = [part]
    if (type(part) == types.TupleType) and (order != None):
        if len(part) > 1:
            raise ValueError, "order=True not valid with multiple parts.\n"

    # things seem sensible, continue...
    if part != None:
        [mesh_info] = separate_parts(mesh_info, listOfParts=part)
    mesh_info = surface_only(mesh_info)

    # create a VTK dataset from it and visualise by writing to a file first
    vtkData = mesh2vtk(mesh_info)
    save_vtk(vtkData, 'basic_surface.vtk')

    # if we've been given a MayaVi window to use, make sure we empty it first
    if myv != None:
        myv.close_all()    
    v = mesh2mayavi('basic_surface.vtk', myv=myv)

    # now change some of the aesthetics manually
    dvm = v.get_current_dvm()
    mm = dvm.get_current_module_mgr()

    # make this a green solid    
    m1 = mm.get_module(0)                # now have handle on the 'SurfaceMap'
    m1.mapper.SetScalarVisibility(1)
    m1.act.GetProperty().SetColor((0,1,0))  # make it green (R,G,B)
    
    # overlay a black wireframe
    m2 = v.load_module('SurfaceMap',0)
    m2.actor.GetProperty().SetRepresentationToWireframe()
    m2.mapper.SetScalarVisibility(0)
    m2.renwin.Render()
   
    # return the handle on the current MayaVi instance
    return v



def wireframe(mesh_info, part=None, myv=None):
    """This function simplifies the process of visualisaing a mesh or group of
       mesh parts as a wireframe. Optionally an existing MayaVi window can be
       used.

       Author: James Kenny                  Last modified: 09/05/06
    """
    import types
    try:
        import mayavi
    except ImportError:
        raise ImportError, "MayaVi is required for this function.\n"

    # find out some basic information about the mesh
    [coords, links, simplices, point_bodies, surfaces, dims, simplexType, parts] \
             = locate_data(mesh_info)

    # check that the request(s) made by the user were sensible
    if part != None:
        if type(part) not in [types.IntType, types.TupleType, types.ListType]:
            raise ValueError, "part must be integer, tuple or list of tuples.\n"
        if point_bodies == None:
            raise ValueError, "data 'POINT-BODIES' is required.\n"
    if type(part) == types.TupleType:
        # wrap tuple in a list to keep separate_parts() happy
        part = [part]

    # things seem sensible, continue...
    if part != None:
        [mesh_info] = separate_parts(mesh_info, listOfParts=part)

    # create a VTK dataset from it and visualise by writing to a file first
    vtkData = mesh2vtk(mesh_info)
    save_vtk(vtkData, 'wireframe.vtk')

    # if we've been given a MayaVi window to use, make sure we empty it first
    if myv != None:
        myv.close_all()
    v = mesh2mayavi('wireframe.vtk', myv=myv)

    # now change some of the aesthetics manually
    dvm = v.get_current_dvm()
    mm = dvm.get_current_module_mgr()

    # make this a green solid    
    m1 = mm.get_module(0)                # now have handle on the 'SurfaceMap'
    m1.actor.GetProperty().SetRepresentationToWireframe()
    m1.mapper.SetScalarVisibility(0)
    m1.renwin.Render()
    
    # return the handle on the current MayaVi instance
    return v



def solid_in2circ(mesh_info, part=None, order=False, myv=None, directory=None):
    """This function simplifies the process of calculating the ratio of inradius
       to circumradius and applying this visually as a quality metric to a
       mesh. The ratio will be unity for an ideal element and elements will be
       coloured according to their quality.

       Author: James Kenny                  Last modified: 09/05/06
    """
    import types
    try:
        import mayavi
    except ImportError:
        raise ImportError, "MayaVi is required for this function.\n"

    # find out some basic information about the mesh
    [coords, links, simplices, point_bodies, surfaces, dims, simplexType, parts] \
             = locate_data(mesh_info)

    # check that the request(s) made by the user were sensible
    if part != None:
        if type(part) not in [types.IntType, types.TupleType, types.ListType]:
            raise ValueError, "part must be integer, tuple or list of tuples.\n"
        if point_bodies == None:
            raise ValueError, "data 'POINT-BODIES' is required.\n"
    if (type(part) == types.TupleType) and (order != None):
        if len(part) > 1:
            raise ValueError, "order != False not valid with multiple parts.\n"
    if type(part) == types.TupleType:
        # wrap it in a list to keep separate_parts() happy
        part = [part]
    if order not in [False, 0, 1, 2]:
        raise ValueError, "order must be 0, 1 or 2 to specify ordering axis.\n"

    # things seem sensible, continue...
    if part != None:
        [mesh_info] = separate_parts(mesh_info, listOfParts=part)
    if order != False:
        mesh_info = order_mesh(mesh_info, axis=order)

    # create a VTK dataset from it and visualise by writing to a file first
    vtkData, points, simplices, simplexIndices, icradii, ccradii = \
             mesh2vtk(mesh_info, VTKonly=False,body_numbers=False)
    if simplexType == 'triangle':
        fac = 2
    if simplexType == 'tetra':
        fac = 3
    in2circ = findRatios(icradii, ccradii, factor=fac)    
    vtkData = append2vtk(vtkData, in2circ, title="%d*inradius/circumradius" \
                         % (dims))
    tmpfilepath = 'tmp_nmesh_visual_in2circ_solid.vtk'
    save_vtk(vtkData, tmpfilepath)

    # if we've been given a MayaVi window to use, make sure we empty it first
    if myv != None:
        myv.close_all()
    v = mesh2mayavi(tmpfilepath, myv=myv, lut_range=(0,1))

    # now change some of the aesthetics manually
    dvm = v.get_current_dvm()
    mm = dvm.get_current_module_mgr()
    lut_handler = mm.get_scalar_lut_handler()
    lut_handler.set_lut_red_blue()
    lut_handler.sc_bar.SetVisibility(1)
    lut_handler.sc_bar.GetTitleTextProperty().SetShadow(1)
    lut_handler.sc_bar.GetLabelTextProperty().SetShadow(1)    
  
    # overlay a black wireframe
    m2 = v.load_module('SurfaceMap',0)
    m2.actor.GetProperty().SetRepresentationToWireframe()
    m2.mapper.SetScalarVisibility(0)
    m2.renwin.Render()

    # if the user requested the mesh be ordered, open up the configuration box
    # for filter 'ExtractUnstructuredGrid' and set CellClipping to 'On'
    if type(order) == types.IntType:
        f = v.load_filter('ExtractUnstructuredGrid',0)
        f.fil.SetCellClipping(1)
        f.configure()

    # remove tmp file
    #log.debug("About to remove" +str(tmpfilepath) )
    #os.remove( tmpfilepath )

    # return the handle on the current MayaVi instance
    return v, in2circ




    


#High-level convenience functions:

def show_surfaces_mayavi(mesh):
    #James: moved the 'import nmesh' statement within this function and
    #       updated function calls to reflect changes in nmesh.visual
     
    #Hans: introduce this method to quickly visualise the parts in a mesh.
    #Used in some examples.

    #Would be useful to check whether dimensions are 2 or 3 here
    #but I don't know how to do that.
    #(Remember that when we load from a file there is not mesh.dim available)
    #(fangohr 26/04/2006)
    log.debug("Entering 'show_surfaces_mayavi'")
    import nmesh
    mesh_info2 = surface_only(mesh.tolists())
    vtkData, points, simplices, indices, icradii, ccradii=mesh2vtk(mesh_info2, VTKonly=False)
    vtkData=append2vtk(vtkData, indices, "body indices")
    myv = mesh2mayavi(vtkData)
    return myv


def show_bodies_mayavi(mesh):
    log.debug("Entering 'show_bodies_mayavi'")
    meshinfo = mesh.tolists()
    vtkData, points, simplices, indices, icradii, ccradii=mesh2vtk(meshinfo, VTKonly=False)
    vtkData=append2vtk(vtkData, indices, "body indices")
    myv = mesh2mayavi(vtkData)
    return myv


def save2vtk(mesh,filename,directory=None,format='binary'):
    """Given a mesh and a filename, this will write the whole mesh into a vtk file. """
    #Used in some examples.

    log.debug("Entering 'save2vtk'")
    mesh_info = mesh.tolists()
    vtkdata = mesh2vtk(mesh_info,VTKonly=True,body_numbers=True)
    filepath = save_vtk(vtkdata, filename, format=format,directory=directory)    


#Functions inherited from Ocaml
def plot2d_ps(mesh, file_name, directory=None,autoscale=True,scale_x=72,scale_y=72,offset_x=72,offset_y=72):
    """
    Function to create a postscipt file of the mesh (2D meshes only).

    Parameters are:
      mesh        : the mesh object to plot
      file_name   : the file name of the postscript file
      autoscale   : BOOLEAN,      see SCALING
      scale_x,    : (default 72), see SCALING
      scale_y,    : (default 72), see SCALING
      offset_x,   : (default 72) , see SCALING
      offset_y    : (default 72) , see SCALING


    SCALING:
      If 'autoscale' is TRUE, then the mesh will be scaled to fill
      an A4 page as good as possible (while keeping the aspect
      ratio). It is expected that this is usually what the users
      would like to do.

      If necessary, the scaling can be done manually by
       (i) setting autoscale=FALSE and
       (ii) overriding the defaults for scale_x,scale_y,offset_x and offset_y

      Background information required for manual scaling:
    
      The plot is created by translating the positions of the nodes
      of the mesh to positions on the postscript page. In postscript,
      the origin (0,0) is at the lower left corner of the page, x is
      increasing to the right, y is increasing to the top.

      The postscript units are 'points' and 72 points correspond to
      the distance of one inch (=2.54cm).

      The postscipt position p=(px,py) is computed from the actual
      position r=(rx,ry) as follows:

      px = rx*scale_x + offset_x
      py = ry*scale_y + offset_y

      Note that arguments for  scale_x,scale_y,offset_x and offset_y
      are ignored unless autoscale=False is provided as well.

      Examples for manual scaling:

      Suppose the mesh extends (in real space) from 0 to 3 in x and
      from 0 to 4 in y-direction.

      The command:

      plot2d_ps(mesh, file_name, autoscale=False, scale_x=100, scale_y=100)

      will therefore plot the mesh starting one inch from the bottom
      and one inch from the left, and it will extend over an area of
      length in x of 3*100/72*2.54cm=10.58cm, and of length in y of
      4*100/72*2.54cm=14.11cm.

      plot2d_ps(mesh, file_name, autoscale=False, scale_x=50, scale_y=50,
                                                  offset_x=0,offset_y=0)

      will create a plot in the lower left corner of the page, with
      dimensions 3*50/72*2.54cm=5.29cm in x and 4*50/72*2.54cm=7.06
      in y.
      
    """
    import ocaml, nmesh
    if autoscale == True:
        from nmeshlib import outer_corners
        mincorner, maxcorner  = outer_corners(mesh)
        log.debug("maxcorner = %s, mincorner = %s" % (str(maxcorner),str(mincorner)))
        meshrange = map( lambda mini,maxi : maxi-mini, mincorner,maxcorner)

        log.debug("The range of the mesh is %s" % meshrange)

        #Assume A4 dimensions: 595 points in x, 841 points in y
        pointsx = 595
        pointsy = 841

        border = 72

        #at this point we limit the code for 2d data only
        assert len(meshrange) == 2, "Can only plot 2d-meshes"

        #compute 'ideal' scaling to fit page
        scale_x = (pointsx-2*border)/meshrange[0]
        scale_y = (pointsy-2*border)/meshrange[1]

        log.debug("Optimal scale_x=%f, scale_y=%f" % (scale_x,scale_y))

        #choose smaller scale (so that plot fits on page)
        scale = min(scale_x,scale_y)
        scale_x = scale_y = scale
        log.debug("Choose scale = %g" % scale)

        #need to compute offset
        margin_x = (pointsx - scale*meshrange[0])/2.0
        margin_y = (pointsy - scale*meshrange[1])/2.0
        offset_x = margin_x - scale*mincorner[0]
        offset_y = margin_y - scale*mincorner[1]

        log.debug("Choose offset x,y = (%f,%f)" % (offset_x,offset_y))

    sc_x = float(scale_x)
    sc_y = float(scale_y)
    os_x = float(offset_x)
    os_y = float(offset_y)


    path = output_file_location(file_name,directory)

    ocaml.mesh2d_ps(mesh.raw_mesh,path,[sc_x,sc_y,os_x,os_y])
    log.info("mesh saved in file %s" % path)
    
    

############# EXTENSIONS (fangohr) to plot vector data #########################
# Quite dirty hacks for now, but we need some visualisation capabilities now.
# Hans, 26 June 2006


def plot_vectorfield_on_mesh( meshinfo, vectordata, legendname="myvectordata", _staticdata = [ None, None] ):
    """
    Function that takes

     -mesh info lists (as return by mesh.tolists())
     -vectordata defined on nodes (in a list of lists)
     -a legend

    The function can be called repeatedly. When called the first time,
    it will open a MayaVi window and plot the vector data.
    When called again, it will update the vector data in this window.

    There are a number of improvements that should be done (allowing
    more vectorfields, customise arrows etc) but this should get us started.

    If you need any other feature, please let Hans know.

    The function returns the mayavi object. (This is only useful
    if you want to play with the visualisation outside this function.)

    Issues/Bugs:
    
    - currently every vector field is written into a temporary
     file. This can most likely be avoided. However, it is not so bad
     as the file probably only exists in the disk cache.
    - the legend name must not change from call to call

    - the mesh info is only required for the first call

    Hans, 26 June 2006

    - it so turns out that the Mayvi interface cannot be manipulated with the mouse
    while the computation is running (i.e. no interactivity). This was the whole
    point of the excercise (so the exercise is [nearly] pointless!).

    Need to look at proper threading to make this work. (fangohr 05/07/2006 11:31)
     
    """


    log.critical("This function has been superceeded by nfem.visual.fields2vtkfile")
    raise StandardError,"this functions is out of date. Use nfem.visual.fields2vtkfile"

    #Abuse default list to store 'static' data in the folliwng way

    # tmpfile = _staticdata[0]
    # mayavi_handle=_staticdata[1]

    import os,tempfile

    try:
        import mayavi
    except ImportError:
        raise ImportError, "MayaVi is required for this function.\n"
        
    if _staticdata[0]==None:
        fh,_staticdata[0] = tempfile.mkstemp('.vtk')

    vtkdata=mesh2vtk(meshinfo)

    vtkdata=append2vtk(vtkdata, vectordata, legendname, sites='point')
    save_vtk(vtkdata,_staticdata[0],format='ascii')

    if _staticdata[1]==None:
        _staticdata[1] = mayavi.mayavi()
        _staticdata[1].open_vtk(_staticdata[0])
        _staticdata[1].load_module('VelocityVector',0) 
    else: #i.e. this is not the first call to this function

        #get all DataVisualisationManagers 
        dvms = _staticdata[1].get_dvm_names()

        for i in dvms:
            #see example http://mayavi.sourceforge.net/mwiki/AnimationScripting for outline of this
            #Note that what we do here is different because we are loading the data
            #from the same file in subsequent calls. Therefore, we need to use the
            #ds.reread_file() method, and don't need ds.Update and ds.update_references()
            #Hans, 26 June 2006
            
            # grab a handle to the DVM
            dvm = _staticdata[1].mayavi.data_viz_mgr[i]
            ds  = dvm.get_data_source()
            ##rdr = ds.get_reader()
            ##rdr.SetFileName(_staticdata[0])
            ##ds.Update()
            ds.reread_file()
            ##ds.update_references()


    _staticdata[1].Render()

    #not sure whether it is safe todo this here, but seems to work.
    os.remove(_staticdata[0])

    return _staticdata[1] #only if the user would like to use the mayavi instance

def plot_vectorfield_on_mesh2( meshinfo, vectordata, legendname="myvectordata", _staticdata = [ None, None] ):
    """
    Function that takes

     -mesh info lists (as return by mesh.tolists())
     -vectordata defined on nodes (in a list of lists)
     -a legend

    The function can be called repeatedly. When called the first time,
    it will open a MayaVi window and plot the vector data.
    When called again, it will update the vector data in this window.

    There are a number of improvements that should be done (allowing
    more vectorfields, customise arrows etc) but this should get us started.

    If you need any other feature, please let Hans know.

    The function returns the mayavi object. (This is only useful
    if you want to play with the visualisation outside this function.)

    Issues/Bugs:
    
    - currently the first vector field is written into a temporary
     file. This can most likely be avoided. However, it is not so bad
     as the file probably only exists in the disk cache.
    - the legend name must not change from call to call

    - the mesh info is only required for the first call

    Hans, 26 June 2006

    - it so turns out that the Mayvi interface cannot be manipulated with the mouse
    while the computation is running (i.e. no interactivity). This was the whole
    point of the excercise (so the exercise is [nearly] pointless!).

    Need to look at proper threading to make this work. (fangohr 05/07/2006 11:31)

    This is the improved version, that does only write the first frame to disk. I'll
    keep this for now to be able to look this up later. (fangohr 05/07/2006 14:11)
     
    """

    #Abuse default list to store 'static' data in the folliwng way

    # tmpfile = _staticdata[0]
    # mayavi_handle=_staticdata[1]

    log.critical("This function has been superceeded by nfem.visual.fields2vtkfile")
    raise StandardError,"this functions is out of date. Use nfem.visual.fields2vtkfile"


    import os,tempfile

    try:
        import mayavi
    except ImportError:
        raise ImportError, "MayaVi is required for this function.\n"
        
    vtkdata=mesh2vtk(meshinfo)

    vtkdata=append2vtk(vtkdata, vectordata, legendname, sites='point')

    if _staticdata[1]==None:
        #in the first iteration, we need to fire up Mayavi.
        #It also seems we have to create a vtk file (surely this can be
        #overcome). For subsequent operations, no files are used.
        
        if _staticdata[0]==None:
            fh,_staticdata[0] = tempfile.mkstemp('.vtk')
        else:
            raise StandardError,"This is impossible"
        save_vtk(vtkdata,_staticdata[0],format='ascii')
        _staticdata[1] = mayavi.mayavi()

        #vtkram = VTKfromRAM( vtkdata )
        #_staticdata[1].open_vtk_data(vtkram)
        _staticdata[1].open_vtk(_staticdata[0])
        _staticdata[1].load_module('VelocityVector',0)

        #not sure whether it is safe todo this here, but seems to work.
        os.remove(_staticdata[0])

    else: #i.e. this is not the first call to this function

        #get all DataVisualisationManagers 
        dvms = _staticdata[1].get_dvm_names()

        for i in dvms:
            #see example http://mayavi.sourceforge.net/mwiki/AnimationScripting for outline of this
            #Note that what we do here is different because we are loading the data
            #from the same file in subsequent calls. Therefore, we need to use the
            #ds.reread_file() method, and don't need ds.Update and ds.update_references()
            #Hans, 26 June 2006
            
            # grab a handle to the DVM
            dvm = _staticdata[1].mayavi.data_viz_mgr[i]
            ds  = dvm.get_data_source()
            rdr = ds.get_reader()
            rdr.ReadFromInputStringOn()
            rdr.SetInputString(vtkdata.to_string())
            rdr.Update()
            ##rdr.SetFileName(_staticdata[0])
            ##ds.Update()
            #ds.reread_file()
            ds.update_references()


    _staticdata[1].Render()


    return _staticdata[1] #only if the user would like to use the mayavi instance


      
######################### COMMAND LINE ARGUMENT HANDLING #######################


def main():
    import sys, getopt
    
    try:           # find information from command line 
        opts, args = getopt.getopt(sys.argv[1:], "hf:isw", ["help", "file=",
                                                            "in2circ", "surface",
                                                            "wireframe"])
    except getopt.GetoptError:
        # options given were not recognised, display doc-string
        print __doc__
        sys.exit(2)

    # state descriptor
    doSomething = False

    for o, a in opts:
        if o in ("-h", "--help"):
            print __doc__
            sys.exit()
        if o in ("-f", "--file"):
            inputfile = a
            doSomething = True

    if doSomething == False:        # no options specified
        print __doc__
        sys.exit(2)
    else:
        if (".vtk" not in inputfile) and (".dat" not in inputfile):
            raise IOError, "file specified must have .dat or .vtk extension.\n"

        if inputfile[-4:] == ".vtk":       # display the VTK file
            try:
                import mayavi
            except ImportError:
                raise ImportError, "MayaVi is required.\n"
            v = mayavi.mayavi()
            v.open_vtk(inputfile,config=0)
            m1 = v.load_module('SurfaceMap',0)
            m1.mapper.SetScalarVisibility(1)
            m1.act.GetProperty().SetColor((0,1,0))
            m2 = v.load_module('SurfaceMap',0)
            m2.actor.GetProperty().SetRepresentationToWireframe()
            m2.mapper.SetScalarVisibility(0)
            m1.renwin.Render()

            m2.renwin.Render()
            raw_input('Hit any key to exit.')
        
        else:                              # process the pickled .dat file
            mesh_info = unpickle(inputfile)
            for o, a in opts:
                if o in ("-s", "--surface"):
                    basic_surface(mesh_info)
                    doSomething = False
                elif o in ("-i", "--in2circ"):
                    solid_in2circ(mesh_info)
                    doSomething = False
                elif o in ("-w", "--wireframe"):
                    wireframe(mesh_info)
                    doSomething = False

            if doSomething == True:
                # no specific visual style requested, use default
                basic_solid(mesh_info)
            raw_input('Hit any key to exit.')



if __name__=="__main__":
    # if this script has been called stand-alone from the command line
    # test which command line options were given
    main()




