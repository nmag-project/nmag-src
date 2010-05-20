import os
import nsim
from nsim.snippets import get_absolute_librarypath

def read_first_line(fname):
     try:
          f = open(fname, 'r')
          lines = f.readlines()
          if len(lines) > 1:
               print "Warning: file %s \nhas %d lines but we expect only 1." \
                     % (fname, len(lines)),
               print "The first 10 lines read"
               for line in lines[0:10]:
                    print "\t> ",line,
          line = lines[0]
          return line.strip()
     except IOError, msg: #can't find file
          return None

def get_version_string():
    return svnversion.svnversion

def _get_nmag_path():
    nsimpath = get_absolute_librarypath(nsim)[0]
    nmagpath = os.path.join(get_absolute_librarypath(nsim)[0],'../nmag')
    return nmagpath

def get_nmag_release():
    nmagpath = _get_nmag_path()

    RELEASE = read_first_line(os.path.join(nmagpath,'RELEASE'))

    if RELEASE:
        return RELEASE
    else:
        return "(unknown)"

def get_nmag_distmode():
    nmagpath = _get_nmag_path()
    DISTMODE = read_first_line(os.path.join(nmagpath,'DISTMODE'))

    if DISTMODE:
         return DISTMODE
    else:
         return "(unknown)"

def get_nmag_release_date():
    nmagpath = _get_nmag_path()
    RELEASEDATE = read_first_line(os.path.join(nmagpath,'RELEASEDATE'))

    if RELEASEDATE:
         return RELEASEDATE
    else:
         return "(unknown)"

def get_nmag_release_dist_svn_string():
    msg = "Versions:"
    msg += "\n\tnsim.svnversion        =" + str(nsim.svnversion.svnversion)
    msg += "\n\tnmag-release           =" + str(get_nmag_release())
    msg += "\n\tnmag-distribution-mode =" + str(get_nmag_distmode())
    msg += "\n\tnmag-release-date      =" +  str(get_nmag_release_date())
    return msg
    
def get_nmag_paths():
    paths = {}
    nmagpath = _get_nmag_path()
    paths['nsimpath'] = get_absolute_librarypath(nsim)[0]
    paths['nmagpath'] = os.path.join(get_absolute_librarypath(nsim)[0],'../nmag')
    paths['realnmagpath'] = os.path.realpath(nmagpath)
    return paths

def get_nmag_paths_string():
    paths = get_nmag_paths()
    msg  = "Paths:"
    msg += "\n\tnsimpath       : %s" % paths['nsimpath']
    msg += "\n\tnmagpath       : %s" % paths['nmagpath']
    msg += "\n\tnmagpath (real): %s" % paths['realnmagpath']
    return msg

