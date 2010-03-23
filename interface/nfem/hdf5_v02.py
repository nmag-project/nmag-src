"""

Second iteration of writing data files.

Basic ideas
===========

Versioning
----------

/etc (Group) 'Configuration and version data'
/etc/filetype (Array(1L,)) 'data file type (string)'
/etc/fileversion (Array(1L,)) 'data file type version'

Mesh
----

/mesh (Group) 'Mesh data'
/mesh/dofsites (Table(1L,), 'Sites for all degrees of freedom'
/mesh/points (CArray(540L, 3L), 'Positions of mesh nodes (=points)'
/mesh/simplices (CArray(2115L, 4L), 'Indices of nodes (starting from zero). Each row is one simplex.'
/mesh/simplicesregions (CArray(2115L,), ) 'Region ids (each row is one  simplex).'


If a mesh length scale has been provided, then the following nodes exist:

/mesh/scale (Group) 'Mesh coordinate length scale group'
/mesh/scale/sifactor (Array()) 'Multiply mesh coordinates with this numer to obtain meters'
/mesh/scale/unit (Array()) 'SI object for coordinate to position conversion'

If the mesh is periodic, it has this leaf node:
/mesh/periodicpointindices (to be added later)




Plan:

- need to store for each field:
  - dofsites
  - units

- do this as follows:

  - if field is written, check whether we know metadata (I think this already works like that)
    and dof sites

  - if either is missing, add this.

  - this means (in contrast to old implementation) that for the dofsites, we have a row per field (and not a column per field).

    Advantage:
      Can add dofsites data as we go along (and therefore we can add fields later which we didn't even know we had before

    Disadvantage:
    
      I think (check this) that all rows have to have elements of
      identical size. Some fields may not be defined everywhere, so
      the actual dofsite data will be smaller. We can work around this
      by adding a None-token (such as (-1,)). This will compress very
      well.

      However, when we create the table, we need to know how large it
      can possible become.



"""



__docformat__='restructuredtext'




import os

import logging
log = logging.getLogger('nfem')

from hdf5 import H5DataFile, importtables

tables = importtables()

import numpy

class H5DataFile_v02(H5DataFile):
    
    def __init__(self,filename,mode,tag,version,file_exists=None):
        """
        :Parameters:
          `filename` : string
            The name of the hdf5 file

          `mode` : string
            Mode for opening the file:
            
              - ``r`` is read only (default)
              
              - ``w`` write; a new file is created (old files if they
                exist are deleted)

              - ``a`` append; an existing file is opened for reading
                and writing, and the file is created if it doesn't exist.

              - ``r+`` similar to ``a`` but the file must exist already

        """
        
        self.version = "0.2"
        self.filename = filename
        self.mode = mode

        self.fh = tables.openFile(filename,mode)

        log.debug("Attempting to open '%s' in mode '%s' (code is for file version %s)" %\
                  (self.filename,self.mode,self.version))

        #check tag from caller is nsim
        if tag != 'nsim':
            raise TypeError,"Can only read 'nsim' files ('%s' is unknown)"\
                  % tag

        #check version from caller -- if given -- is 0.2
        if version:
            if version != self.version:
                raise TypeError,"Can only deal with version %s "\
                      "(you requesteded '%s' -- internal error calling the wrong class)" \
                      % (self.version,version)

        if file_exists:
            self.checktag('nsim',self.version)
        else:
            self.tagfile('nsim',self.version)

        #measure size of file. Start by taking into account metadata

        self.fh.flush()

        self.rawdatasize = 0*os.path.getsize(self.filename)
        log.log(15,"Initial file size (pytables metadata) is %d bytes" % self.rawdatasize)

        log.debug("Opened '%s' in mode '%s' (code is for file version %s)" % (self.filename,self.mode,self.version))


    def __str__(self):
        msg = "HDF5 Data File Object:"
        msg += "\n  Filename   : %s" % self.filename
        msg += "\n  mode       : %s" % self.mode
        self.fh.flush()
        actualsize = os.path.getsize(self.filename)

        if actualsize < 100*1024:
            pass #if files are so small, data is meaningless

            rawdatasize = "<100kB"
            actualsize = "<100kB"
            compression = "?"
        else:
            rawdatasize = "%d kiloByte" % (self.rawdatasize/1024)
            compression = "%2d%%" % int(round(self.rawdatasize - actualsize)\
                                        /float(self.rawdatasize)*100)
            actualsize = "%d kiloByte" % (actualsize/1024)
            
        msg += "\n  rawdatasize: %s " % rawdatasize
        msg += "\n  actual size: %s " % actualsize
        msg += "\n  compression: %s"   % str(compression)
        
        return msg


    def close(self):
        self.fh.flush()
        self.fh.close()

    #def __del__(self):
    #    print "In Deconstructor"
    #    self.fh.flush()
    #    self.fh.close()


    def add_mesh(self,mesh,mesh_unit_length=None):
        """
        Add mesh data structure to open hdf5 file.

        Raises error if /mesh exists already in that file.

        :Parameters:
          `mesh` : Python mesh object
            The Python object that contains the mesh 

          `unit_length` : SI object or None
            The SI object that defines what a length of 1.0 in mesh coordinates
            corresponds to in the real world.

            If we save the mesh from the mesher (using this function), then
            we do not want to add a unit_length scale (so this parameter is optional). 

        """

        size = 0

        meshgroup = self.fh.createGroup("/", 'mesh', 'Mesh data')

        filter = tables.Filters(complevel=5, complib="zlib")

        #positions
        pos_shape = (len(mesh.points),mesh.dim)
        pos_chunk = tables.Float64Atom(shape = pos_shape)
        
        pos_c_array = self.fh.createCArray(meshgroup, 'points', pos_shape, pos_chunk, \
                                     filters=filter,title='Positions of mesh nodes (=points)')

        tmpsize = 8*len(mesh.points)*mesh.dim
        log.log(15,"Adding %d byte mesh node positions" % tmpsize)
        size += tmpsize

        pos_c_array[:] = numpy.array(mesh.points)


        simp_dim = mesh.dim+1 #simplices are n+1 dimensional

        #Compression note: I have tested different integer data types
        #for simplex indices and region indices. Between
        #tables.Int16Bit and tables.Int32BitAtom there is less than 1%
        #difference in file size (using zlib compression level 5) for
        #a file with 2000 points and 10000 simplices.
        #For simplicity, we only use Int32Bit at the moment.
        #(fangohr 19/01/2007)
        
        #simplices 
        simplex_shape = (len(mesh.simplices),simp_dim)
        simplex_chunk = tables.Int32Atom(shape = simplex_shape)
        simplex_c_array = self.fh.createCArray(meshgroup, 'simplices',simplex_shape, simplex_chunk, \
                                         title='Indices of nodes (starting from zero). Each row is one simplex.',\
                                         filters=filter)

        tmpsize = 4*len(mesh.simplices)*simp_dim
        log.log(15,"Adding %d byte simplex data" % tmpsize)
        size += tmpsize

        simplex_c_array[:,:] = numpy.array(mesh.simplices)

        #simplicex regions
        simplexregion_shape = (len(mesh.simplicesregions),)
        simplexregion_chunk = tables.Int32Atom(shape=simplexregion_shape)
        simplexregion_c_array = self.fh.createCArray(meshgroup, 'simplicesregions',\
                                                   simplexregion_shape, simplexregion_chunk, \
                                                   title='Region ids (one for each simplex).',filters=filter)

        tmpsize = 4*len(mesh.simplicesregions)
        log.log(15,"Adding %d byte simplex region data" % tmpsize)
        size += tmpsize

        simplexregion_c_array[:] = numpy.array(mesh.simplicesregions)
            
        #Add lengthscale if given
        if mesh_unit_length:
            #check type
            if not isinstance(mesh_unit_length,SI):
                raise NsimValueError,"mesh_unit_length (%s) is of type %s but needs to be SI object" %\
                      (mesh_unit_length,type(mesh_unit_length))

            #check whether this is for meters
            if not mesh_unit_length.is_compatible_with(SI('m')):
                raise NsimUserError,"Unit length provided for mesh (%s) is not compatible with '%s'" %\
                      (mesh_unit_length,SI('m'))

            #all tests done. Add data.
            lengthgroup = self.fh.createGroup("/mesh", 'scale', 'Mesh coordinate length scale group')
            self.fh.createArray('/mesh/scale','sifactor',\
                          mesh_unit_length.value,\
                          title='Multiply mesh coordinates with this numer to obtain meters')

            self.fh.createArray('/mesh/scale','unit',repr(mesh_unit_length),\
                          title='SI object for coordinate to position conversion')

            log.log(15,"Added mesh_unit_length scale (%s) to %s:/mesh/scale" %\
                    (mesh_unit_length,self.filename))
        else:
            log.log(15,"Not saving mesh_unit_length (non given)")

        self.rawdatasize += size

        return None





    
