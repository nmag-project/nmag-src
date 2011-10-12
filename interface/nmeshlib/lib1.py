from __future__ import division

try:
    import ocaml
except ImportError,msg:
    error = "We can't import the ocaml package. This usually means that you\n"
    error +="are running this code in a normal python interpreter, not within\n"
    error +="the nmesh or nsim executable.\n\n"
    raise ImportError,error+str(msg)

try:
    import nsim
except ImportError,msg:
    error = "Can't import nsim library\n"
    raise ImportError,error+str(msg)

import types, math, os, sys, logging

log = logging.getLogger('nmesh')

def memory_report(tag):
    t,vmem,rss = ocaml.time_vmem_rss()
    log.log(15,"Memory report: T=  %f VMEM=   %d KB RSS=   %d KB %s" % (t,int(vmem),int(rss),tag))

memory_report("during importing meshlib/lib1.py")

#provides exceptions
from nmesh_exceptions import *

#import configtools

import nsim

from nsim.snippets import output_file_location


def get_default_meshing_parameters():
        """Provides the default meshing parameters object. These can, for example, be retrieved as a string
        (.to_string)."""

        default_configfile = os.path.join(nsim.snippets.get_absolute_librarypath(__file__)[0],'nmesh.config')
        mp = MeshingParameters(file=default_configfile)

        return mp

from nsim.features import Features

class MeshingParameters(Features):

    def __init__(self,string=None,file=None):
        log.debug("entering MeshingParameterrs.__init__")
        Features.__init__(self,local=True)

        self.dim = None

        if file:
            self.from_file(file)

        if string:
            if type(string)==types.StringType:
                self.from_string(string)
            else:
                NMeshTypeError,"expected TypeString, not type: %s" % type(string)

        self.add_section('user-modifications')


    def _getsectionname(self):
        
        if self.dim == None:
            raise NmeshStandardError,"MeshingParameters object doesn't know"+\
                                     "dimension yet. This is a programming problem."
        if self.dim in [2,3]:
            section = 'nmesh-%dD' % self.dim
        else:
            section = 'nmesh-ND'

        return section
            

    def __getitem__(self,name):
        section = self._getsectionname()

        print "About to call Features.get, name=%s,section=%s" % (name,section)

        print "Raw data:", self.get(section,name,raw=True)
        print "Non-Raw data:", self.get(section,name)#XXXX
        print "done"
        return self.get(section,name)
    

    def __setitem__(self,key,value):
        self.set('user-modifications',key,value)

    def pass_parameters_to_ocaml(self,mesher,dim):

        self.dim = dim

        #override default values with modifications from user

        for key,value in self.items('user-modifications'):
            log.info("Modifying default parameter %s=%s to be %s (as requested by user)"
                     % (key,self[key],str(value)))

            section = self._getsectionname()
            self.set(section,key,str(value))

        log.log(15,"final configuration for this run (only section nmesh-%dD matters) is\n%s"
                % (self.dim,self.to_string()))

        #now pass modified version to ocaml
        scale = float(self["shape_force_scale"])
        ocaml.mesher_defaults_set_shape_force_scale(mesher,scale)

        scale = float(self["volume_force_scale"])
        ocaml.mesher_defaults_set_volume_force_scale(mesher,scale)

        scale = float(self["neigh_force_scale"])
        ocaml.mesher_defaults_set_neigh_force_scale(mesher,scale)

        scale = float(self["irrel_elem_force_scale"])
        ocaml.mesher_defaults_set_irrel_elem_force_scale(mesher,scale)

        scale = float(self["time_step_scale"])
        ocaml.mesher_defaults_set_time_step_scale(mesher,scale)

        thresh = float(self["thresh_add"])
        ocaml.mesher_defaults_set_thresh_add(mesher,thresh)

        thresh = float(self["thresh_del"])
        ocaml.mesher_defaults_set_thresh_del(mesher,thresh)

        thresh = float(self["topology_threshold"])
        ocaml.mesher_defaults_set_topology_threshold(mesher,thresh)

        scale = float(self["tolerated_rel_move"])
        ocaml.mesher_defaults_set_tolerated_rel_movement(mesher, scale)

        max_steps = int(self["max_steps"])
        ocaml.mesher_defaults_set_max_relaxation_steps(mesher, max_steps)

        steps = int(self["initial_settling_steps"])
        ocaml.mesher_defaults_set_initial_settling_steps(mesher,steps)

        scale = float(self["sliver_correction"])
        ocaml.mesher_defaults_set_sliver_correction(mesher,scale)

        scale = float(self["smallest_volume_ratio"])
        ocaml.mesher_defaults_set_smallest_allowed_volume_ratio(mesher,scale)

        scale = float(self["max_relaxation"])
        ocaml.mesher_defaults_set_movement_max_freedom(mesher,scale)


    def set_shape_force_scale(self, scale):
        """set_shape_force_scale( scale:float ).

        Function to change the default value of
        the scaling factor for the shape contribution
        to the force on the nodes.
        For each simplex, the force is computed as follows:
        - Each vertex coordinates are expressed in the centre of mass
          reference system.
        - The variance-covariance matrix is built from the sum of
          the outerproduct of each vertex with itself,
          scaled by (number of vertices -1).
        - A Singular value decomposition (SVD) is applied
          to this matrix and the results is a matrix of eigenvectors
          and a matrix of eigenvalues like (2D case)

            | eigval_1     0    |
        M = |                   |
            |    0     eigval_2 |

        - These eigenvalues are scaled in order to keep their
          product = 1.

        - From the M matrix the force matrix F is created.
          Its entries are the logarithm of the corresponding
          eigenvalue in M, bounded between -3. and 3., scaled
          by the shape_force_scale factor & the contribution from
          the linear function associated to the
          initial_settling_steps factor Ki, which goes from
          0 to 1 in initial_settling_steps steps .

            |shape_force_scale* Ki * (-log(eigval_1))                0             |
        F = |                                                                 |
            |      0                       shape_force_scale* Ki *(-log(eigval_2)) |

        - Such a force matrix is then expressed
          in the original reference system (the one
          used before the SVD transformation of the axes)
          and applied to each vertex expressed in
          the centre of mass reference system.

        """
        self["shape_force_scale"]=float(scale)

    def set_volume_force_scale(self, scale):
        """set_volume_force_scale( scale:float ).

        Function to change the default value of
        the scaling factor for the volume contribution
        to the force on the nodes.
        For each simplex, the force is computed as follows:
        - The volume of the simplex is computed
        - The volume of a regular simplex is computed
          taking the edge length equal to 1.1 * the effective
          rod length computed in its centre of mass.
        - The volume factor is computed as the logarithm of
          the ratio (current simplex vol)/(ideal simplex vol)
          scaled by the scaling factor & the contribution from
          the linear function associated to the
          initial_settling_steps factor, which goes from
          0 to 1 in initial_settling_steps steps.
        - The force on each node is computed multiplying
          the volume factor by the vector connecting the
          node to the centre of mass of the simplex
          (lines of gravity).

        """
        self["volume_force_scale"]=float(scale)

    def set_neigh_force_scale(self, scale):
        """set_neigh_force_scale( scale:float ).

        Function to change the default value of
        the scaling factor for the neighbour contribution
        to the force on the nodes.
        For each node-neighbour pair, the force is computed as follows:
        - Take the relative distance between the node
          and the neighbour (absolute distance scaled with
          the rod length and the relative density).
        - Use this distance to compute a force
          along the direction of the connection
          between the node and the neighbour
          of the form

            | neigh_force_scale* Ki *(1.0 - distance)  if distance < 1.0
            |
        F = <
            |
            | 0.0                                      otherwise

          where Ki is the contribution from
          the linear function associated to the
          initial_settling_steps factor, which goes from
          0 to 1 in initial_settling_steps steps.

        """
        self["neigh_force_scale"]=float(scale)

    def set_irrel_elem_force_scale(self, scale):
        """set_irrel_elem_force_scale( scale:float ).

        Function to change the default value of
        the scaling factor for the contribution
        of irrelevant elements to the force on the
        nodes. Such irrelevant elements are the ones
        that are mainly outside the body but could have
        a small part inside. The force acts from the
        "inside" node to the centre of mass and is
        proportional to their distance. The effect is
        to push the "inside" node on the boundary of
        the body.

        """
        self["irrel_elem_force_scale"]= float(scale)
 

    def set_time_step_scale(self, scale):
        """set_time_step( scale:float ).

        Function to change
        the default value of the scaling factor for the time
        step. Using the expression

        dT = (p_(n+1) - p_(n))/F

        with

        * p_(n) = position of the node at n-th iteration
        * F = sum of forces acting on that node

        Calling Ki the contribution from the linear function
        associated to the initial_settling_steps factor, which goes
        from max_relaxation to 1 in initial_settling_steps steps, the
        time step dt is time_step_scale * ki * min_i(dT_i), the
        minimum over all the nodes of the value dT.  The new position
        for each node is then

        p^(t+dt) = p^(t) + F * dt
        """
        self["time_step_scale"] = float(scale)

    def set_thresh_add(self, thresh):
        """set_thresh_add( thresh:float ).

        Function to change the default value of the threshold for the
        insertion of a new point. A new point is likely to be inserted
        when the ratio between the volume of a sphere centred on the
        node coordinates with ratio = 1/2 * effective a0 and its
        Voronoi cell is lower than the threshold minus 0.1 * the
        contribution from the linear function associated to the
        initial_settling_steps factor, which goes from max_relaxation
        to 0 in initial_settling_steps steps.  The insertion occurs
        with a probability P(x) = 10 percent
        
        """
        self["thresh_add"] = float(scale)



    def set_thresh_del(self, thresh):
        """set_thresh_add( thresh:float ).

        Function to change the default value of
        the threshold for the deletion of a
        new point. A point is likely to be deleted when
        the ratio between the volume of a sphere centred on
        the node coordinates with ratio = 1/2 * effective a0
        and its Voronoi cell exceeds the threshold plus
        0.1 * the contribution  from the linear function
        associated to the initial_settling_steps factor,
        which goes from max_relaxation to 0 in initial_settling_steps steps.
        The deletion occurs with a probability
        P(x) = 10 percent
        """
        self["thresh_del"] = float(thresh)

    def set_topology_threshold(self, thresh):
        """set_topology_threshold( thresh:float ).

        Function to change
        the default value of the topology threshold. The mesher
        re-triangulates if the movement of one of the points exceeds
        this value * the contribution  from the linear function
        associated to the initial_settling_steps factor,
        which goes from max_relaxation to 1 in initial_settling_steps steps..
        """

        self["topology_threshold"] = float(thresh)


    def set_tolerated_rel_move(self, scale):
        """set_tolerated_rel_move( scale:float ).

        Function to change
        the default value of the tolerated relative movement.
        The mesher stops when the relative movement of all the points
        is within this value.
        """

        self["tolerated_rel_move"] = float(scale)


    def set_max_steps(self, max_steps):
        """set_max_relaxation_steps(max_steps).

        Function to change the maximum number of relaxation steps
        for the mesh.
        """

        self["max_steps"] = int(max_steps)


    def set_initial_settling_steps(self, steps):
        """set_initial_setling_steps( steps:int ).

        Function to change the default value of
        the initial steps when the "settling" behaviour
        of the mesh occurs. Used together with the
        max_relaxation parameter.
        """
        self["initial_settling_steps"] =  int(steps)


    def set_sliver_correction(self, scale):
        """set_sliver_correction( scale:float ).

        By default the shape force uses only the angular component
        of the force obtained from the principal component transformation
        and related "regularity" of each simplex. This function allows
        to use also the longitudinal component, as long as its effect is
        to move a point away from the center of mass.

        """
        self["sliver_correction"] = float(scale)

    def set_smallest_volume_ratio(self, scale):
        """set_smallest_volume_ratio( scale:float ).

        The flat simplices on the surface of our
        objects are a consequence of the qhull
        triangulator. We remove them setting a
        maximum value for the ratio between their
        volume and the volume we expect from
        the length of their lines of gravity.

        """
        self["smallest_volume_ratio"] = float(scale)


    def set_max_relaxation(self, scale):
        """set_max_relaxation( scale:float ).

        Function to change
        the behaviour of the mesh algorithm at the beginning of the
        mesh relaxation. In the first initial_settling_points
        it linearly scales the parameters (look at their explanation
        for more details):
        thresholds for adding&deleting points,
        time_step_scale,
        topology_threshold,
        (NOT>>tolerated_rel_move<<NOT (condition to stop the relaxation) )

        """
        self["max_relaxation"] = scale
        

class meshBaseClass(object):
    """This is the base class for mesh object. It is currently inherited
    from by the "mesh" class which computes the mesh and the "load"
    class which loads the mesh. These are different ways of creating
    the mesh-object and therefore have different properties.

    This base class contains all methods and attributes that are
    common to all these classes.  """

    def __init__(self,raw_mesh):
        memory_report("beginning meshBaseClass constructor")
        log.debug('in meshBaseClass constructor, assigning raw_mesh to self')
        self.raw_mesh = raw_mesh
        memory_report("Adding the raw_mesh to class")
        log.debug('%s' % self.__str__())

        #'old' approach of caching meshinfo data
        self.__listdata = None 

        #'new' approach of fetching only what is requested
        self.__meshinfo_points = None
        self.__meshinfo_simplices = None
        self.__meshinfo_simplicesregions = None
        self.__meshinfo_links = None
        self.__meshinfo_pointsregions = None
        self.__meshinfo_surfaces_and_surfacesregions = None
        self.__regionvolumes = None
        self.__meshinfo_periodic_point_indices = None
        memory_report("End __init__")


    def scale_node_positions(self, scale):
        """scale_node_positions( scale:float ).
        Function that takes a float and scales the positions
        of all the points of the mesh by this common factor.
        Example: if mesh node 0 is at [1,2,0] initially and the
        scale factor is 10, then it will be located at [10,20,0]
        afterwards.
        """
        scale = float(scale)
        log.debug("scale_node_positions: Multiply all node positions by %g" % scale)
        ocaml.mesh_scale_node_positions(self.raw_mesh,scale)

        # Make sure we fetch new node data if requested
        self.__meshinfo_points = None
        #self.__meshinfo_simplices = None
        #self.__meshinfo_simplicesregions = None
        #self.__meshinfo_links = None
        #self.__meshinfo_pointsregions = None
        #self.__meshinfo_surfaces_and_surfacesregions = None

        #and new region volumes as these will change.
        self.__regionvolumes = None


    def save_hdf5(self, filename):
        """save_mesh(filename).

        Function to save a mesh to the file named filename. The suggested
        fileextension is 'nmesh.h5' (but other extensions can be used).
        """


        import nfem
        import nfem.hdf5_v01 as h5
        tables=nfem.hdf5_v01.importtables()

        f=h5.open_pytables_file(filename,'w') 

        h5.tagfile(f,'nmesh','1.0')

        h5.add_mesh(f,self,mesh_unit_length=None)

        h5.close_pytables_file(f)


    def save(self, file_name, directory=None, format=None):
        """save_mesh(filename).

        Function to save a mesh to the file named filename. The suggested
        fileextension is 'nmesh' (but other extensions can be used).

        The format is determined from the filename: anything other than .h5 will
        be saved in ascii file format. Files ending in .h5 will be saved in compressed
        hdf5 format. The format can be set with the format flag (use 'ascii' or 'hdf5').
        """
        path = output_file_location(file_name,directory=directory)

        if os.path.splitext(path)[1].lower() == '.h5':
            self.save_hdf5(path)
            log.info("Have written mesh to '%s' (hdf5 format)" % path)
        else:
            ocaml.mesh_writefile(path,self.raw_mesh)
            log.info("Have written mesh to '%s' (nmesh ascii format)" % path)



    def __str__(self):
        """Return string that summarises the mesh object.
        This includes number of points and simplices."""
        simplices = ocaml.mesh_nr_simplices(self.raw_mesh)
        points = ocaml.mesh_nr_points(self.raw_mesh)
        return "Mesh with %d points and %d simplices" % (points,simplices)


    def __update_listdata(self):
        if self.__listdata == None:
            log.debug("Retrieved tolists data once, we'll cache this for further queries")
            self.__listdata = ocaml.mesh_plotinfo(self.raw_mesh)
        else:
            log.debug("Using cached tolists data")

    def tolists(self):
        """
        Function to get Python lists with
        mesh information out of a pyobject (OCaml pill)
        containing the mesh. Study entry 0 and entry 1 in all lists
        for exmplanation of the data, entry 2 contains the actual data.

        TODO: update documentation here
        """

        log.debug("Provided info about mesh to python via tolists()")
        self.__update_listdata()
        return self.__listdata

        
    def __get_points(self):
        if self.__meshinfo_points == None:
            log.debug("Fetching points from Ocaml")
            self.__meshinfo_points = ocaml.mesh_plotinfo_points( self.raw_mesh )
        return self.__meshinfo_points


    points = property(__get_points,doc="List of lists keeping the positions of the nodes of the mesh")


    def __get_pointsregions(self):
        if self.__meshinfo_pointsregions == None:
            log.debug("Fetching pointsregions from Ocaml")
            self.__meshinfo_pointsregions = ocaml.mesh_plotinfo_pointsregions( self.raw_mesh )
        return self.__meshinfo_pointsregions


    pointsregions = property(__get_pointsregions,doc="List of lists keeping the regions (coded as integers) to which a point belongs. (Note that a point can belong to more than one region.)")

    def __get_simplices(self):
        if self.__meshinfo_simplices == None:
            log.debug("Fetching simplices from Ocaml")
            self.__meshinfo_simplices = ocaml.mesh_plotinfo_simplices( self.raw_mesh )
        return self.__meshinfo_simplices


        self.__update_listdata()
        simplices = map( lambda a : a[0], self.__listdata[2][2])
        return simplices

    simplices = property(__get_simplices,doc="List of lists keeping the point indices of the points that form simplices in the mesh")

    def __get_simplicesregions(self):
        if self.__meshinfo_simplicesregions == None:
            log.debug("Fetching simplicesregions from Ocaml")
            self.__meshinfo_simplicesregions = ocaml.mesh_plotinfo_simplicesregions( self.raw_mesh )
        return self.__meshinfo_simplicesregions

    simplicesregions = property(__get_simplicesregions,doc="List of regions (coded as integers) showing to which region a simplex belongs.")

    def __get_surfaces_and_surfacesregions(self):
        if self.__meshinfo_surfaces_and_surfacesregions == None:
            log.debug("Fetching surface simplices and surfacesregions from Ocaml")
            self.__meshinfo_surfaces_and_surfacesregions = ocaml.mesh_plotinfo_surfaces_and_surfacesregions( self.raw_mesh )
        return self.__meshinfo_surfaces_and_surfacesregions

    def __get_surfaces(self):
        return self.__get_surfaces_and_surfacesregions()[0]

    def __get_surfacesregions(self):
        return self.__get_surfaces_and_surfacesregions()[1]

    surfaces = property(__get_surfaces,doc="List of lists keeping the point indices of the points that form surface elements.")


    surfacesregions = property(__get_surfacesregions,doc="List of regions (coded as integers) showing to which region a simplex belongs.")

    def __get_links(self):
        if self.__meshinfo_links == None:
            log.debug("Fetching links from Ocaml")
            self.__meshinfo_links = ocaml.mesh_plotinfo_links( self.raw_mesh )
        return self.__meshinfo_links

    links = property(__get_links,doc="List of lists of pairs of point indices that make up all the links (pairs) in the mesh")

    def __get_dim(self):
      return ocaml.mesh_dim(self.raw_mesh)

    dim = property(__get_dim, doc="The dimension of the space where the mesh lives")

    def __get_regionvolumes(self):
        if self.__regionvolumes == None:
            log.debug("Fetching regionvolumes from Ocaml")
            self.__regionvolumes = ocaml.mesh_plotinfo_regionvolumes(self.raw_mesh)
        return self.__regionvolumes

    regionvolumes = property(__get_regionvolumes,doc="List of volumes, one number for each region.")

    def __get_numregions(self):
        return len(self.regionvolumes)

    numregions = property(__get_numregions,
                          doc="The number of regions of the mesh, " \
                              "including the empty space.")

    def __get_periodic_point_indices(self):
        if self.__meshinfo_periodic_point_indices == None:
            log.debug("Fetching periodic point inforamtion from Ocaml")
            self.__meshinfo_periodic_point_indices = ocaml.mesh_plotinfo_periodic_points_indices( self.raw_mesh )
        return self.__meshinfo_periodic_point_indices

    periodicpointindices = property(__get_periodic_point_indices,
                                    doc=("List of lists keeping the indices of "
                                         "the periodic nodes"))

    def set_vertex_distribution(self,dist):
        raw_mesh=self.raw_mesh
        ocaml.mesh_set_vertex_distribution(raw_mesh,dist)

    def __get_permutation(self):
        return ocaml.mesh_get_permutation(self.raw_mesh)

    permutation = property(__get_permutation,
                           doc=("The permutation mapping the original mesh "
                                "(the one provided by the user) to the "
                                "current mesh (reordered with parmetis)."))


def _is_nmesh_ascii_file(filename):

    #check that file seems to be the right thing:
    #Expect a string like this:
    # PYFEM mesh file version 1.0

    firstline = open(filename,'r').readline()
    keyword = "# PYFEM"
    if firstline[0:len(keyword)] != keyword:
        return False
    else:
        return True


def _is_nmesh_hdf5_file(filename):
    import tables

    if tables.isPyTablesFile(filename):
        is_h5 = True
    else:
        is_h5 = False
    log.debug("Checking whether '%s' is pytables file -> answer is %s" % (filename,str(is_h5)))
    return is_h5


class load_ascii(meshBaseClass):
    """load(file_name:string).

       Given a file_name of an nmesh-mesh, this constructor will read the file and return
       a mesh instance.

       Constructor of mesh from file.
       """

    def __init__(self, filename, do_reorder=False, do_distribute=True):

        log.debug('in load constructor with filename = %s' % str(filename))

        self.filename = filename

        if _is_nmesh_ascii_file(filename):
            raw_mesh = ocaml.mesh_readfile(filename, do_reorder,do_distribute)
            log.info("Have read mesh from %s. " %   filename )
        else:
            log.warn("Input file '%s' does not seem to be nmesh file" % filename)
            log.info("First line of file reads:\n%s" % firstline.rstrip())
            log.debug("First line of input file %s is\n%s" % (filename,firstline.rstrip()))

            raise NmeshUserError,"file '%s' is not ascii nmesh file" % filename

        meshBaseClass.__init__(self,raw_mesh)


#def load_ascii_python(filename,do_reorder=False):
#    """This code is not used --- have this function available in OCaml"""
#    f=open(filename,'r')
#    lines = map(lambda a : a[:-1], f.readlines()) #map strips off newlines character
#    tag="# PYFEM mesh file version"
#    filetag = lines[0][0:len(tag)]
#    if filetag!=tag:
#        print "first line of file '%s' reads\n%s" % (filename, lines[0])
#        print "Identified as tag:\n%s" % filetag
#        print "but expect tag is\n%s" % tag
#        raise NmeshIOError,"file '%s' is not nmesh ascii format (tag='%s')" % (filename,tag)
#
#    if len(lines[0])>len(filetag):
#        version = lines[0][len(filetag):].split()[0]
#    else:
#        raise NmeshIOError,"file '%s' doesn't have version information" % filename
#
#    if version!="1.0":
#        raise NmeshIOError,"file '%s' is of unknown version (%s)" % (filename,version)
#
#    #next line contains only nice data:
#    # dim = 2 	 nodes = 129 	 simplices = 219 	 surfaces = 37
#    log.debug("roading mesh file, 2nd line reads: %s" % lines[2])
#
#    #use this to count line numbers
#    i = 2
#    nrnodes=int(lines[i]); i+=1
#
#    points = [None]*nrnodes
#    log.debug("Found %d nodes" % nrnodes)
#    
#    for i0 in range(nrnodes):
#        j=i0+i
#        points[i0] = map(float,lines[j].split())
#
#    i += nrnodes
#
#    nrsimplices = int(lines[i]); i+=1
#
#    log.debug("Found %d simplices" % nrsimplices)
#
#    simplices = [None]*nrsimplices
#    simplices_regions = [None]*nrsimplices
#
#    for i0 in range(nrsimplices):
#        j=i0+i
#        tmp = map(int,lines[j].split())
#        simplices[i0] = tmp[1:]
#        simplices_regions[i0] = tmp[0]
#
#    log.debug("Giving points, simplices and regions to ocaml to create mesh")
#    return(mesh_from_points_and_simplices(points,simplices,simplices_regions,do_reorder=do_reorder))
    

def raise_if_not_hdf5(filename):
    """Raise an exception if filename is not a Pytables file."""
    from nfem.hdf5_v01 import importtables
    tables = importtables()

    if not tables.isPyTablesFile(filename):
        is_not = ("Pytables file" if tables.isHDF5File(filename)
                  else "HDF5 file")
        raise IOError("'%s' is not a %s" % (filename, is_not))

def mesh_get_attrs(filename, *attrs):
    raise_if_not_hdf5(filename)

    import nfem.hdf5_v01 as h5
    tables = h5.importtables()

    # Start with some consistency checks...
    f = h5.open_pytables_file(filename, 'r')

    if not hasattr(f.root, 'mesh'):
        raise IOError("File '%s' does not contain a mesh" % filename)

    if not hasattr(f.root, 'etc'):
        raise IOError("File '%s' has no /etc node: "
                      "cannot be nmesh file" % filename)

    h5.checktag(f, 'nmesh', '1.0', alt=[('nsimdata', '0.1')])

    # Retrieve mesh attributes
    mesh_pool = f.root.mesh
    read = lambda x: (x.read() if x != None else x)
    attr_data = [read(getattr(mesh_pool, attr, None))
                 for attr in attrs]

    h5.close_pytables_file(f)
    return attr_data

def mesh_get_permutation(filename):
    """Retrieve the permutation associated to the given mesh, or None
    if there isn't any."""
    return mesh_get_attrs(filename, "permutation")[0]

def load_hdf5(filename, do_reorder=False, do_distribute=True):
    attrs = ["points", "simplices", "simplicesregions",
             "periodicpointindices"]

    vals = mesh_get_attrs(filename, *attrs)
    if None in vals[:3]:
        raise IOError("File '%s' is not a valid mesh file: some mesh "
                      "attributes are missing")

    points, simplices, simplices_regions = map(lambda x: x.tolist(), vals[:3])
    periodicpointindices = vals[3]

    periodic_point_indices = []
    if periodicpointindices != None:
	# This list still includes many -1 which represent None. This
	# is because (in contrast to points, simplices and
	# regionindices) the periodicpointindices will (in general)
	# have a different number of entries per line. See
	# hdf5_v01.py:add_mesh().

	for line in periodicpointindices:
            # We could use filter((-1).__cmp__, line) above: is faster
            # but less readable.
	    periodic_point_indices.append([index for index in line if index!=-1])

    return mesh_from_points_and_simplices(points, simplices, simplices_regions,
                                          periodic_point_indices, 
                                          do_reorder=do_reorder,
                                          do_distribute=do_distribute)

def load(filename,reorder=False,do_distribute=True):

    """Load nmesh file with name filename. If reorder=True is given,
    then the nodes will be re-ordered (using metis) to decrease width
    of connectivity matrix.

    If do_distribute=True, the mesh will be distributed over the
    number of processes (if number of processes is greater than 1,
    otherwise this will be ignored).

    Files can be read from nmesh ascii (extension .nmesh) and nmesh.h5
    files (extension .h5).
    """

    memory_report("beginning of load(%s)" % filename)

    #check that file name exists;
    if not os.path.exists(filename):
        raise NmeshUserError,"file '%s' does not exist" % filename

    if _is_nmesh_ascii_file(filename):
        log.debug("mesh file '%s' seems to be ascii file" % filename)
        return load_ascii(filename,reorder,do_distribute)
    elif _is_nmesh_hdf5_file(filename):
        log.debug("mesh file '%s' seems to be hdf5 file" % filename)
        return load_hdf5(filename,reorder,do_distribute)
    else:
        log.debug("mesh file '%s' seems to be neither hdf5 nor ascii file --> error" % filename)
        raise NmeshUserError,"%s is unknow file format (not ascii, not hdf5)" % filename

    memory_report("end of load(%s)" % filename)



class mesh_from_points_and_simplices(meshBaseClass):
    """mesh_from_points_and_simplices(points: float list of lists,
                                      simplices_indices: int list of lists,
                                      simplices_regions: int list of lists,
                                      periodic_point_indices: int list of lists,
                                      initial: int,                                      
                                      do_reorder=bool [default:False]).

       Given a set of points and the list of simplices indices as well
       as the region they belong to, this constructor will create a
       mesh and return a mesh instance. The variable initial, set to 0
       by default, takes the value 0 or 1 depending on the minimum
       value in the set of indices used to define a simplex.

       If do_reorder is specified and True, then the nodes will be reordered
       using parmetis.

       Constructor of mesh from points and simplices.
       """

    def __init__(self, points=[], simplices_indices=[], simplices_regions=[],
                 periodic_point_indices=[], initial=0, do_reorder=False,
                 do_distribute=True):

        log.debug('in load constructor with given points and simplices')

        float_points = map(lambda point: map(float,point), points)
        int_simplices_regions = map(lambda region: int(region), simplices_regions)

  	#correct **NETGEN** point counting: they start from 1, we start from 0.
        #it seems that Gmsh and Gambit also start from 1. (fangohr 13/11/2008)
        if initial == 1:

            def reduce_by_one(x): return x-1
            simplices_indices = map(lambda simplex: map(reduce_by_one,simplex), simplices_indices)


        int_simplices_indices = map(lambda simplex: map(int,simplex), simplices_indices)


        dim = len(points[0])
        memory_report("in mesh_from_points_and_simplices, just before creating ocaml mesh")

        raw_mesh = ocaml.mesh_from_points_and_simplices(dim, float_points,
                                                        simplices_indices, simplices_regions,
                                                        periodic_point_indices,
                                                        do_reorder,do_distribute)

        log.log(15,nsim.snippets.get_ocaml_pill_size_string(raw_mesh,'raw_mesh'))
        
        memory_report("in mesh_from_points_and_simplices, just after creating ocaml mesh")
        log.log(15,"Have read mesh from points and simplices")

        meshBaseClass.__init__(self,raw_mesh)


class mesh(meshBaseClass):
    """ Class to define the parameters for the meshing algorithm.
        mesh(
        bounding_box:[float list,float list]
                                      - outer bounding box  (compulsory)
        objects:obj list              - objects to be meshed
        a0:float                      - desired length of connection between nodes
                                        (where density == 1.0)
        density:string                - density function of nodes ditribution
        periodic:bool list            - periodic boundary conditions in the outer box:
                                        in the list the True entries correspond to the
                                        directions with periodic boundaries
        mesh_bounding_box:bool        - whether bounding box should be meshed
        fixed_points: float list list - set of fixed points to be used in the mesh
        mobile_points: float list list- set of mobile points to be used in the mesh: no random point is generated
        simply_points: float list list- set of points to be used meshed straight away
        callback:tuple                - see below
        max_relaxation:float          - mesher parameter: see mesh.set_max_relaxation
        topology_threshold:float      - mesher parameter: see mesh.set_topology_threshold

        #KKK#
        cache_name:string             - file name with which we may cache a mesh (XXX document me better!)
        shape_force_scale:float       - mesher parameter: see mesh.set_shape_force_scale
        volume_force_scale:float      - mesher parameter: see mesh.set_volume_force_scale
        neigh_force_scale:float       - mesher parameter: see mesh.set_neigh_force_scale
        irrel_elem_force_scale:float  - mesher parameter: see mesh.set_irrel_elem_force_scale

        thresh_add:float              - mesher parameter: see mesh.set_thresh_add
        thresh_del:float              - mesher parameter: see mesh.set_thresh_del
        initial_settling_steps:float  - mesher parameter: see mesh.set_initial_settling_steps
        sliver_correction: float      - mesher parameter: see mesh.set_sliver_correction
        smallest_volume_ratio: float  - mesher parameter: see mesh.set_smaller_volume_ratio

        time_step_scale:float         - mesher parameter: see mesh.set_time_step_scale
        tolerated_rel_move:float      - mesher parameter: see mesh.set_tolerated_rel_move
        max_steps:int                 - mesher parameter: see mesh.set_max_steps

        meshing_parameters             - collection of mesher parameters.
        ).

        The a0 is the desired length of the connections between
        the mesh nodes, a value which is weighted with the density function
        (given in C code).

        The bounding_box defines the outer boundary box of the mesh.

        The callback argument allows to provide a function f that is executed
        every n steps like this

        callback = (f,n)

        The function f will be passed the following arguments

        nr_piece       =  index of the meshing object
        iteration_nr   =  number of iteration in the relaxation process
        mesh           =  mesh objecct (use nmesh.tolists(mesh) to obtain
                          python list of lists of nodes, vertices)

        The periodic list impose periodic boundary conditions
        on the outer box in the corresponding directions.
        If periodic boundary conditions are used,
        the density function must respect the related contraints.

        """
    def __init__(self, bounding_box=None, objects = [], a0 = 1.,
                 density="", periodic = [], fixed_points = [], mobile_points = [],
                 simply_points=[], callback = None, mesh_bounding_box=False,
                 meshing_parameters=None,
                 cache_name = "", hints=[], **keywords):
#                 #KKK#
#                 shape_force_scale=None, volume_force_scale=None,
#                 neigh_force_scale=None, irrel_elem_force_scale = None,
#                 thresh_add=None, thresh_del=None,
#                 initial_settling_steps=None, sliver_correction = None,
#                 max_relaxation=None, topology_threshold=None,
#                 time_step_scale=None, tolerated_rel_move=None,
#                 max_steps=None,



        log.debug('in mesh constructor with objects = %s' % str(objects))

        if bounding_box == None:
            msg = "A bounding box must be provided. It doesn't have to be accurate but should "
            msg +="enclose all objects to be meshed. If the bounding box is chosen to big, "
            msg +="this will slow down the meshing process (somewhat)."
            log.critical("No bounding box")
            raise NmeshUserError(msg)

        if mesh_bounding_box==True:
            self.mesh_exterior = 1
        else: #this means we don't want to mesh the outside
            self.mesh_exterior = 0
            log.debug("Don't mesh bounding box")

            #some sanity checks
            if len(objects)==0:
                msg = "No objects to mesh, no exterior to mesh. Stopping here"
                log.critical(msg)
                raise NmeshUserError, msg

            if periodic.count(True) >= 1:
                msg = "Can only produce periodic meshes when meshing the bounding box"
                log.critical(msg)
                raise NmeshUserError, msg


        #make sure entries in bounding_box are floats (we basically
        #allow the user to use integers here if they like):
        self.bounding_box = map(lambda point: map(float,point), bounding_box)


        dim = len(self.bounding_box[0])
        if meshing_parameters == None:
            self.meshing_parameters = get_default_meshing_parameters()
        else:
            
            self.meshing_parameters = meshing_parameters

        #XXX
        #from IPython.Shell import IPShellEmbed
        #
        #ipshell = IPShellEmbed()
        #
        #ipshell() # this c

        #self.default_params = self.default_parameters(dim)


        # load objects to be meshed
        self.obj = []
        if type(objects) == types.ListType :
            for obj in objects:
                self.obj.append(obj.obj)
        else:
            msg = "Expect a list of objects but instead of a list I got %s" % type(objects)
            log.critical(msg)
            raise ValueError(msg)

        # load hints
        self.hints = []
        if type(hints) == types.ListType:
            # check if the hints list contains something before loading it
            if len(hints) >= 1:
                for hint in hints:
                    mesh = hint[0]
                    obj = hint[1]
                    self.hints.append([mesh.raw_mesh, obj.obj])
        else:
            msg = "Expect a list of hints but instead of a list I got %s" % type(hints)
            log.critical(msg)
            raise ValueError(msg)

        self.a0 = float(a0)


        self.cache_name = cache_name

        self.density = density

        def true2one(x):
            if x: return 1.0
            else: return 0.0

        # periodic boundary conditions
        if periodic == []:
            self.periodic = dim*[0.0] 
        else:
            if len(periodic) == len(bounding_box[0]):
                self.periodic = map(lambda x: true2one(x),periodic)
            else:
                msg = "The list length for periodic boundaries must match the dimension of the space."
                log.critical(msg)
                raise NmeshUserError, msg

        self.fixed_points = map(lambda point: map(float,point), fixed_points)
        self.mobile_points = map(lambda point: map(float,point), mobile_points)
        self.simply_points = map(lambda point: map(float,point), simply_points)
        # add fixed&mobile points coming from objects
        for obj in objects:
            self.fixed_points += map(lambda point: map(float,point), obj.fxd_pts)
            self.mobile_points += map(lambda point: map(float,point), obj.mbl_pts)

        if callback == None:
            callback_function = lambda a,b,c : None  #dummy
            callback_interval = 1000000              #call rarely
        else:
            callback_function, callback_interval = callback

        self.driver = ocaml.make_mg_gendriver(callback_interval, callback_function)

        # default mesher
        default_mesher = ocaml.mesher_defaults
        # modifiable version (so we can set parameters)
        self.my_mesher = ocaml.copy_mesher_defaults(default_mesher)

        # take user requests in keywords into account for meshing parameters:
        for name,value in keywords.items():
            log.debug("Parsing keywords given in mesh constructor. We assume\n"+
                      "these will only be meshing parameters. Checking this now:")

            self.meshing_parameters[name]=value

        # change mesher parameters
        self.meshing_parameters.pass_parameters_to_ocaml(self.my_mesher,dim)

        # some debugging output
	log.log(15,"Fixed points: %s" % str(self.fixed_points))
	log.log(15,"Mobile points: %s" % str(self.mobile_points))
	log.log(15,"Simply points: %s" % str(self.simply_points))

        # compute mesh, and store it in pyobject (OCaml pill)
        log.debug("Starting computation of mesh")
    	raw_mesh = ocaml.mesh_bodies_raw(self.driver,self.my_mesher, self.bounding_box[0],self.bounding_box[1],
                                         self.mesh_exterior,self.obj,self.a0,self.density, self.fixed_points,
                                         self.mobile_points, self.simply_points,self.periodic,self.cache_name, self.hints)

        if type(raw_mesh) == types.NoneType:
            raise StandardError("The mesh has not been created, something went wrong!\n Please contact developers'")

        #call constructor of baseclass
        meshBaseClass.__init__(self,raw_mesh)

        log.debug("Finished computation of mesh, leaving constructor of mesh")


#    def default_parameters(self, dim):
#
#        default_2d_parameters = {}
#        default_2d_parameters['shape_force_scale'] = 0.1
#        default_2d_parameters['volume_force_scale'] = 0.0
#        default_2d_parameters['neigh_force_scale'] = 1.0
#        default_2d_parameters['irrel_elem_force_scale'] = 0.0
#        default_2d_parameters['thresh_add'] = 0.6
#        default_2d_parameters['thresh_del'] = 1.6
#        default_2d_parameters['initial_settling_steps'] = 200
#        default_2d_parameters['sliver_correction'] = 1.0
#        default_2d_parameters['max_relaxation'] = 3.0
#        default_2d_parameters['topology_threshold'] = 0.2
#        default_2d_parameters['time_step_scale'] = 0.1
#        default_2d_parameters['tolerated_rel_move'] = 0.002
#        default_2d_parameters['max_steps'] = 1000
#
#        default_3d_parameters = {}
#        default_3d_parameters['shape_force_scale'] = 0.2
#        default_3d_parameters['volume_force_scale'] = 0.0
#        default_3d_parameters['neigh_force_scale'] = 1.0
#        default_3d_parameters['irrel_elem_force_scale'] = 0.0
#        default_3d_parameters['thresh_add'] = 1.0
#        default_3d_parameters['thresh_del'] = 2.0
#        default_3d_parameters['initial_settling_steps'] = 200
#        default_3d_parameters['sliver_correction'] = 1.0
#        default_3d_parameters['max_relaxation'] = 3.0
#        default_3d_parameters['topology_threshold'] = 0.2
#        default_3d_parameters['time_step_scale'] = 0.1
#        default_3d_parameters['tolerated_rel_move'] = 0.002
#        default_3d_parameters['max_steps'] = 2000
#
#        default_Nd_parameters = {}
#        default_Nd_parameters['shape_force_scale'] = 0.2
#        default_Nd_parameters['volume_force_scale'] = 0.0
#        default_Nd_parameters['neigh_force_scale'] = 1.0
#        default_Nd_parameters['irrel_elem_force_scale'] = 0.0
#        default_Nd_parameters['thresh_add'] = 1.0
#        default_Nd_parameters['thresh_del'] = 2.0
#        default_Nd_parameters['initial_settling_steps'] = 200
#        default_Nd_parameters['sliver_correction'] = 1.0
#        default_Nd_parameters['max_relaxation'] = 3.0
#        default_Nd_parameters['topology_threshold'] = 0.2
#        default_Nd_parameters['time_step_scale'] = 0.1
#        default_Nd_parameters['tolerated_rel_move'] = 0.002
#        default_Nd_parameters['max_steps'] = 2000
#
#        if dim == 2:
#            return default_2d_parameters
#        elif dim == 3:
#            return default_3d_parameters
#        else:
#            return default_Nd_parameters

    def fixed_points(self, fixed = []):
        """List of points to be added to the list of fixed points.
        """
        if fixed != []:
            self.fixed_points += fixed

    def mobile_points(self, mobile = []):
        """List of points to be added to the list of mobile points.
        """
        if mobile != []:
            self.mobile_points += mobile

    def simply_points(self, simply_pts = []):
        """List of points to be added to the list of fixed points to be meshed straigth away.
        """
        if simply_pts != []:
            self.simply_points += simply_pts

    def default_fun(self,nr_piece,n,mesh):
        """Default function for the driver
        """
        pass

    def extended_fun_driver(self,nr_piece,iteration_nr,mesh):
        """
        Change the function to be executed every fun_call_interval steps.
        """
        self.fun_driver(nr_piece,iteration_nr,mesh)

        # call to the visual module or pass
        if self.visual:
            self.visual_fun()
        else:
            pass



def tolists(mesh):
    """
    Function to get a Python list with
    mesh information out of a pyobject (OCaml pill)
    containing the mesh.
    """

    log.warning("Deprication warning: mesh.tolists() should be replaced by self.tolists()")

    return mesh.tolists()

def union(obj2uni):
    """
    Function to create the union of a group of
    objects
    """
    if type(obj2uni) == types.ListType :
        if len(obj2uni)<2:
            msg = "The function union needs at least two objects"
            log.critical(msg)
            raise NmeshUserError(msg)
        else:
            united_obj = []
            fixed_points = []
            mobile_points = []
            for obj in obj2uni:
                united_obj.append(obj.obj)
                if len(obj.fxd_pts) > 0:
                    fixed_points += obj.fxd_pts
                if len(obj.mbl_pts) > 0:
                    mobile_points += obj.mbl_pts

            obj_union = mesh_obj(obj2uni[0].dim,fixed_points,mobile_points)
            obj_union.obj = ocaml.body_union(united_obj)
            return obj_union

    else:
        msg = "Objects that should form an union need to be provided in a list.\n"
        msg +="The type provided was '%s'.\n" % str(type(obj2uni))
        log.critical(msg)
        raise NmeshUserError(msg)

def difference(mother_obj, obj2sub=[]):
    """
    Function to take the difference between the
    mother object and obj2sub.
    """
    if type(mother_obj.obj) == type(obj2sub[0].obj):

        sub_obj = []
        fixed_points = []
        mobile_points = []
        for obj in obj2sub:
            sub_obj.append(obj.obj)
            if len(obj.fxd_pts) > 0:
                fixed_points += obj.fxd_pts
            if len(obj.mbl_pts) > 0:
                mobile_points += obj.mbl_pts

        obj_diff = mesh_obj(mother_obj.dim,fixed_points,mobile_points)
        obj_diff.obj = ocaml.body_difference(mother_obj.obj,sub_obj)
        return obj_diff
    else:
        raise TypeError("Type mismatch for object difference")

def intersect(obj2int):
    """
    Function to take the intersection of the
    mesh object objects obj2int.
    """

    if type(obj2int) == types.ListType :

        if len(obj2int)<2:
            msg = "the intersection needs at least two objects"
            log.critical(msg)
            raise NmeshUserError(msg)
        else:
            intsct_obj = []
            fixed_points = []
            mobile_points = []
            for obj in obj2int:
                intsct_obj.append(obj.obj)
                if len(obj.fxd_pts) > 0:
                    fixed_points += obj.fxd_pts
                if len(obj.mbl_pts) > 0:
                    mobile_points += obj.mbl_pts

            obj_intsct = mesh_obj(obj2int[0].dim,fixed_points,mobile_points)
            obj_intsct.obj = ocaml.body_intersection(intsct_obj)
            return obj_intsct

    else:
        msg = "Objects that should be intersected need to be provided in a list.\n"
        msg +="The type provided was '%s'.\n" % str(type(obj2int))
        log.critical(msg)
        raise NmeshUserError(msg)

def save(mesh, file_name):
    """save_mesh(mesh, file_name:string).

    Function to save a mesh on the file file_name. Suggested
    fileextension is 'nmesh' (but other extensions can be
    used).
    """

    msg="Deprication: use mesh.save() instead of nmesh.save (where mesh is a mesh object)."
    raise NmeshUserError(msg)
    #mesh.save(file_name)




class mesh_obj:
    def __init__(self, dim, fixed_points, mobile_points):
        """Base class of a mesh object
        """
        #-------------------------------------
        # self.obj IS an OCaml PILL
        #-------------------------------------

        self.dim = dim
        self.fxd_pts = fixed_points
        self.mbl_pts = mobile_points
        self.obj = []

    def shift(self, shift, sc=True ):
        """shift(
        shift:float list   -  shifting vector to add to the current position
        sc:bool            -  reference system for shifting (body or system coordinates)
        ).

        Function to shift the object of the amount shift along
        the axes. If sc = False the shift is made wrt the body
        coordinates, wrt the system coordinates otherwise.
        """

        if sc:
            self.obj = ocaml.body_shifted_sc(self.obj,shift)
        else:
            self.obj = ocaml.body_shifted_bc(self.obj,shift)

    def scale(self, scale):
        """scale(
        scale:float list  - scaling factors along the axes of the object
        ).

        Function that scales the object along the axes
        with the factors given by the scale vector
        """
        self.obj = ocaml.body_scaled(self.obj,scale)

    def rotate(self, a1_ix, a2_ix, ang=0.0, sc=True):
        """rotate(
        a1_ix:int -  index of first rotation axis
        a2_ix:int -  index of second rotation axis
        ang:float -  angle of rotation (degrees)
        sc:bool   -  reference system for rotation (body or system coordinates)
        ).

        Function that rotates the object of an angle ang
        given the indices of the rotation axes.
        If sc = False the rotation is made wrt the
        body coordinates, wrt the system coordinates otherwise.
        """

        if sc:
            self.obj = ocaml.body_rotated_sc(self.obj,a1_ix,a2_ix,math.radians(ang))
        else:
            self.obj = ocaml.body_rotated_bc(self.obj,a1_ix,a2_ix,math.radians(ang))

    def rotate2d(self, ang=0.0, sc=True):
        """rotate(
        ang:float -  angle of rotation (degrees)
        sc:bool   -  reference system for rotation (body or system coordinates)
        ).

        Function that rotates the 2D object of an angle ang.
        If sc = False the rotation is made wrt the
        body coordinates, wrt the system coordinates otherwise.
        """
        if sc:
            self.obj = ocaml.body_rotated_sc(self.obj,0,1,math.radians(ang))
        else:
            self.obj = ocaml.body_rotated_bc(self.obj,0,1,math.radians(ang))

    def rotate3d(self, axis, ang=0.0, sc=True):
        """rotate(
        axis:float list  -  rotation axis
        ang:float        -  angle of rotation (degrees)
        sc:bool          -  reference system for rotation (body or system coordinates)
        ).

        Function that rotates the 3D object of an angle ang
        wrt to the given axis.
        If sc = False the rotation is made wrt the
        body coordinates, wrt the system coordinates otherwise.
        """
        if sc:
            self.obj = ocaml.body_rotated_axis_sc(self.obj,axis,math.radians(ang))
        else:
            self.obj = ocaml.body_rotated_axis_bc(self.obj,axis,math.radians(ang))


    def transform(self, transformationlist, sc):
        """ Function to perform transformation on the object
        following the order specified in the transform list.
        If sc = False the transformations are made wrt the
        body coordinates, wrt the system coordinates otherwise.
        """

        knowntransformations = {}
        knowntransformations["shift"]=['list_of_floats']
        knowntransformations["scale"]=['list_of_floats']
        knowntransformations["rotate"]=['pair_of_axes(list)','rotation_angle']
        knowntransformations["rotate2d"]=['rotation_angle']
        knowntransformations["rotate3d"]=['rotatation_axis','rotation_angle']

        if type(transformationlist) != types.ListType:
            raise NmeshUserError,"Transformations need to be provided in a list (type is %s)" % str(type(transformationlist))

        for transformation in transformationlist:
            #check whether the transformation is build of a tuple
            if not type(transformation) == types.TupleType:
                error_msg="While processing %s, we encountered an error" % str(transformation)
                error_msg+="Each transformation need to be provided in a tuple, where the first element\n"
                error_msg+="is a string (=name of transformation) and the second (and possibly third) element\n"
                error_msg+="are arguments to that transformation.\n"
                error_msg+="Instead of a tuple, we got the type %s)" % str(type(transformation))
                log.critical(error_msg)
                raise NmeshUserError,error_msg

            #check whether we know the transformation
            if not transformation[0] in knowntransformations.keys():
                error_msg = "Don't know '.%s'\n" % str(transformation[0])
                error_msg += "Known transformations are %s\n" % str(knowntransformations.keys())
                log.critical(error_msg)
                raise NmeshUserError, "Unknown transformation:"+error_msg

            #check whether the transformation has the right number of arguments:
            if len(transformation)==1:
                msg = "Expect parameters %s for transformation '%s'" % \
                      (str(knowntransformations[transformation[0]]),transformation[0])
                log.critical(msg)
                raise NmeshUserError, msg


            tra = transformation[0]
            arg1 = transformation[1]

            if tra == "shift":
                self.shift(map(float,arg1))
            elif tra == "scale":
                self.scale(map(float,arg1))
            elif tra == "rotate":
                arg2 = transformation[2]
                self.rotate(arg1[0],arg1[1],arg2)
            elif tra == "rotate2d":
                self.rotate2d(arg1)
            elif tra == "rotate3d":
                arg2 = transformation[2]
                self.rotate3d(map(float,arg1),arg2)
            else:
                #because we have checked for the transformation string already,
                #this should never happen
                raise NmeshUserError, "Internal Error -- this should never happen"


class box(mesh_obj):
    """Class to create a bounded box
    """

    def __init__(self, point1, point2, transform=[], fixed_points=[],
                 mobile_points=[], simply_points = [], sc = True,
                 use_fixed_corner_points=False):
        """box(
        point1: float list,
        point2: float list,
        transform:(string, transformation_data) list,
        fixed_points: float list list ,
        mobile_points: float list list ,
        simply_points: float list list ,
        sc:bool,
        use_fixed_corner_points:bool
        ).

        Creates a box given the coordinates of the lower-left (point1) and
        upper_right (point2) vertices. The box is transformed following the order
        of the transformations specified in the transform list.
        Calling ai (i = 1,2,...) the axis of the space where the object is
        defined, examples of these transformations are:
        - ("shift", [a1,a2,a3,..])
        - ("scale", [a1,a2,a3,...])
        - ("rotate", [a1,a2], phi)
        - ("rotate2d", phi)
        - ("rotate3d", [a1,a2,a3], phi)
        If sc = False the transformations are made wrt the body coordinates,
        wrt the system coordinates otherwise.

        If use_fixed_corner_points is True, then the corners of the box wil
        be 'immobile points' and therefore garanteed to be in this position.
        """
        dim = len(point1)
        assert dim == len(point2), "Point1 and Point2 have different dimensions"

        if use_fixed_corner_points:
            def combinations( component_pairs_left, corners_found ):
                if len(component_pairs_left)<=0:
                    raise StandardError, "This is impossible"
                #at least one component left
                new_corners = []
                for item in corners_found:
                    new_corners.append( item + [component_pairs_left[0][0]] )
                    new_corners.append( item + [component_pairs_left[0][1]] )

                if len(component_pairs_left)==1:
                    return new_corners
                else: # more than one coordinate left
                    return combinations( component_pairs_left[1:],new_corners )

            corner_points = combinations( zip(point1,point2), [[]] )
            log.log(15,"Adding corners of this box as fixed points: %s" % str(corner_points))
            fixed_points.extend( corner_points )

        mesh_obj.__init__(self, dim, fixed_points, mobile_points)

        log.debug("creating box with P1=%s P2=%s" % (str(point1),str(point2)))

        self.obj = ocaml.body_box(map(float,point1),map(float,point2))

        log.debug("transformations are %s" % (str(transform)))

        if len(transform) > 0: #i.e. we have transformations
            if len(fixed_points) > 0: #i.e.we have fixed points
                log.warning("Fixed points will not be transformed -- has to be done manually by user")

        self.transform(transform,sc)

class ellipsoid(mesh_obj):
    """Class to create an ellipsoid
    """

    def __init__(self, length, transform=[], fixed_points=[], mobile_points=[], sc = True):
        """ellipsoid(length: float list, transform:(string, transformation_data) list,
        fixed_points: float list list , mobile_points: float list list,  sc:bool).

        Creates an ellipsoid given the length of
        the main axes. The ellipsoid is transformed following the order
        of the transformations specified in the transform list.
        Calling ai (i = 1,2,...) the axis of the space where the object is
        defined, examples of these transformations are:
        - ("shift", [a1,a2,a3,..])
        - ("scale", [a1,a2,a3,...])
        - ("rotate", [a1,a2], phi)
        - ("rotate2d", phi)
        - ("rotate3d", [a1,a2,a3], phi)
        If sc = False the transformations are made wrt the body coordinates,
        wrt the system coordinates otherwise.
        """




        dim = len(length)
        mesh_obj.__init__(self,dim, fixed_points, mobile_points)

        log.debug("create ellipsoid")
        self.obj = ocaml.body_ellipsoid(map(float,length))

        self.transform(transform,sc)

class conic(mesh_obj):
    """Class to create a conical frustum
    """

    def __init__(self, coords1, rad1, coords2, rad2, transform=[], fixed_points=[], mobile_points=[], sc = True):
        """conical( coords1:float list, rad1:float, coords2: float list, rad2: float,
        transform:(string, transformation_data) list, fixed_points: float list list ,
        mobile_points: float list list, sc:bool).

        Creates a conical frustum given the centre of the lower circumference,
        its radius, the centre of the upper circumference and its radius.
        The frustum is transformed following the order
        of the transformations specified in the transform list.
        Calling ai (i = 1,2,...) the axis of the space where the object is
        defined, examples of these transformations are:
        - ("shift", [a1,a2,a3,..])
        - ("scale", [a1,a2,a3,...])
        - ("rotate", [a1,a2], phi)
        - ("rotate2d", phi)
        - ("rotate3d", [a1,a2,a3], phi)
        If sc = False the transformations are made wrt the body coordinates,
        wrt the system coordinates otherwise.
        """

        dim = len(coords1)
        mesh_obj.__init__(self,dim, fixed_points, mobile_points)

        log.debug("create conical frustum with P1,R1=(%s,%s), P2,R2=(%s,%s)"
                  % (str(coords1),str(rad1),str(coords2),str(rad2)))
        self.obj = ocaml.body_frustum(map(float,coords1),float(rad1),map(float,coords2),float(rad2))

        self.transform(transform,sc)



class helix(mesh_obj):
    """Class to create a helix
    """

    def __init__(self, coords1, rad1, coords2, rad2, transform=[], fixed_points=[],
                 mobile_points=[], sc = True):
        """helix( coords1:float list, rad1:float, coords2: float list, rad2: float,
        transform:(string, transformation_data) list, fixed_points: float list list ,
        mobile_points: float list list, sc:bool).

        Creates a helix given the centre of its base, the radius of
        the spiral, the top point of the spiral and the radius of
        the circle at the base of the helix. The number of turns is
        set to 4 (8 pi radians). The helix is transformed following the order
        of the transformations specified in the transform list.
        Calling ai (i = 1,2,...) the axis of the space where the object is
        defined, examples of these transformations are:
        - ("shift", [a1,a2,a3,..])
        - ("scale", [a1,a2,a3,...])
        - ("rotate", [a1,a2], phi)
        - ("rotate2d", phi)
        - ("rotate3d", [a1,a2,a3], phi)
        If sc = False the transformations are made wrt the body coordinates,
        wrt the system coordinates otherwise.
        """

        dim = len(coords1)
        mesh_obj.__init__(self,dim, fixed_points, mobile_points)

        log.debug("create helix with P_spiral,R_spiral=(%s,%s), P_circle,R_circle=(%s,%s)"
                  % (str(coords1),str(rad1),str(coords2),str(rad2)))
        self.obj = ocaml.body_helix(map(float,coords1),float(rad1),map(float,coords2),float(rad2))

        self.transform(transform,sc)

def outer_corners(mesh):

    """Given a mesh object, this will determine the spatial extension of the
    mesh. The function returns two points in space which (if they would span
    a cuboid) enclose all mesh points.

    Currently used to scale postscript plots automatically.
    """

    #We find the min and max coordinates for each dimension.  Note
    #that strictly this should not be necessary if we have a
    #meshed bounding box (because then we know that information).
    #We compute this here because (i) if the bounding box is not meshed
    #and the user over estimated the size of the bounding box, then
    #this will zoom into the box and only show meshed parts.
    #(ii) if we load the mesh from a file, we don't have access
    #to the bounding box information.
    meshlists = mesh.tolists( )
    coords = meshlists[0][2]
    coordtranspose = zip(*coords)
    maxcorner = map(max,coordtranspose)
    mincorner = map(min,coordtranspose)

    return mincorner,maxcorner
