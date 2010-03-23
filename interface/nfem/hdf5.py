
raise NotImplementedError,"This is currently not used"

import logging
log = logging.getLogger('nfem')

#logging.getLogger('').setLevel(logging.debug)

import os

log.log(10,'test in import hdf510')

log.log(20,'test in import hdf520')

log.log(30,'test in import hdf530')


def importtables():
    version = '1.3.2'
    try:
        import tables
    except ImportError:
        msg = "You need to install pytables http://www.pytables.org/ (version %s or above." % version
        raise ImportError,msg

    if tables.__version__ < version:
        msg = "Have found pytables version %s but we need version %s or above." % (tables.__version__,version)
        raise ImportError,msg
    return tables

tables = importtables()


class H5DataFile:
    def __init__(self,filename,mode='r',tag='nsim',version=None,*furtherarguments):
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

        #add tag depending on whether read, write, append
        file_exists = 'unknown'

        fileversion = None

        if mode == 'a':
            #if file exists already, then find out the version and and
            #call corresponding class
            if os.path.exists(filename):
                log.debug("Have found '%s' to exist" % filename)
                file_exists = True
            else:
                file_exists = False
                log.debug("Have not found '%s'" % filename)
        elif mode in ['r', 'r+']:
            file_exists = True
            if not os.path.exists(filename):
                raise IOError,"file '%s' doesn't exist" % filename
        elif mode == 'w':
            file_exists = False
        else:
            raise ValueError, "Unknown value '%s' for 'mode'" % mode

        
        if file_exists:
            fileversion = self._get_version_from_filename(filename)
            #call corresponding class
        else:
            fileversion = version #not specified, use user provided
                                  #(which defaults to None. None is
                                  #used to signal 'most recent')

        if fileversion == '0.2' or fileversion == None:
            import hdf5_v02
            self.__class__ = hdf5_v02.H5DataFile_v02
            hdf5_v02.H5DataFile_v02.__init__(self,filename,mode=mode,\
                                             tag=tag,version=fileversion,\
                                             file_exists=file_exists)
#            file_exists=file_exists,*furtherarguments)
        else:
            raise NotImplementedError,"Can only read nsim h5 files version 0.2 "\
                                      "(you need %s)" % fileversion
            

    def _get_version_from_filename(self,filename,mode='r'):
        fh=tables.openFile(filename,mode)
        data = fh.root.etc.fileversion[0]
        fh.close()
        return data

    def _get_filetype_from_filename(self,filename,mode='r'):
        fh = tables.openFile(filename,mode)
        data = fh.root.etc.filetype[0]
        fh.close()
        return data
    

    def _get_version(self):
        return self.fh.root.etc.fileversion[0]

    def _get_filetype(self):
        return self.fh.root.etc.filetype[0]


    def _checktag_version(self,version):
        version = str(version)
        version_okay = False
        fileversion = self._get_version()
        log.debug("checktag_version, filename=%s, file version=%s, "\
                  "required version=%s" % (self.fh.filename,fileversion,version))
        if fileversion == version:
            version_okay = True
        else:
            raise NfemUserError,"File is of version %s but you try to use with"+\
                  "code for version %s" % (self.fh.root.etc.fileversion[0],version)

        log.debug("_checktag_version: file-version=%s, required version=%s -> matching=%s" %\
                  (fileversion,version,str(version_okay)))

        return version_okay

    def _checktag_filetype(self,filetype):
        filetype = str(filetype)
        filetype_okay = False
        filefiletype=self._get_filetype()
        log.debug("checktag_filetype, filename=%s, filetype=%s,"\
                  " required filetype=%s" % \
                  (self.fh.filename,filefiletype,filetype))
        if  filefiletype == filetype:
            filetype_okay = True
        else:
            raise IOError,"File %s is of type '%s' but you try to use"+\
                  "it as '%s'" % (f.filename,f.root.etc.filetype[0],filetype)

        return filetype_okay

    def checktag(self,filetype,version):
        """
        Given a filetype string and a version string, this checks whether the file
        we open is of the same type and version."""

        return self._checktag_version(version) and self._checktag_filetype(filetype)


    def tagfile(self,filetype,version):
        """Add 'tag' in /etc/filetype and /etc/fileversion.

        We use this to identify the different types of h5 files we have.

        So far these are

        nmesh (only the mesh: points, simplices, simplex regions are compulsory) 1.0
        nsimdata (several time dependend fields) 0.1

        Superseeded by

        nsim 0.2 (which is used for meshes, restart files and data)

        """

        version = str(version)
        filetype= str(filetype)

        if not hasattr(self.fh.root,'etc'):
            etcgroup = self.fh.createGroup("/", 'etc', 'Configuration and version data')

        if not hasattr(self.fh.root.etc,'filetype'):
            self.fh.createArray("/etc", 'filetype', [filetype],title='data file type')
            log.debug("Have tagged file %s with filetype '%s'" % (self.fh.filename,filetype))
        else:
            raise RunTimeError, "tag /etc/filetype exists already! (can't add '%s')" % filetype
        
        if not hasattr(self.fh.root.etc,'fileversion'):
            self.fh.createArray("/etc", 'fileversion', [version],title='data file type version')
            log.debug("Have tagged file %s with version %s" % (self.fh.filename,version))
        else: 
            raise RunTimeError, "tag /etc/fileversion exists already! (can't add '%s')" % version



    

            
                
            
