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
    import version
    return version.version_str

def _get_nmag_path():
    nsimpath = get_absolute_librarypath(nsim)[0]
    nmagpath = os.path.join(get_absolute_librarypath(nsim)[0],'../nmag')
    return nmagpath

def get_nmag_release():
    import version
    if version.vcinfo != None:
        return version.vcinfo.split()[1]
    else:
        return "unreleased"

def get_nmag_distmode():
    import version
    return ("unknown" if version.dist_mode == None
            else version.dist_mode)

def get_nmag_release_date():
    import version
    return ("n/a" if version.release_date == None
            else version.release_date)

def get_nmag_dist_date():
    import version
    return ("unknown" if version.dist_date == None
            else version.dist_date)

def get_nmag_release_dist_svn_string():
    """Obsolete: use get_nmag_release_info."""
    return get_nmag_release_info()

def get_nmag_release_info():
    """This function returns a string with:
    1. Nsim version (with the -dev suffix in case of versions not released
       officially,
    2. The release date (in case of officially released version),
    3. The version control information (revision number) of the code,
    4. The distribution mode,
    5. The distribution date."""
    msg = "Versions:"
    msg += "\n\t     nsim version = " + str(get_version_string())
    msg += "\n\t     release date = " + str(get_nmag_release_date())
    msg += "\n\t          VC info = " + str(get_nmag_release())
    msg += "\n\tdistribution mode = " + str(get_nmag_distmode())
    msg += "\n\tdistribution date = " + str(get_nmag_dist_date())
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
    msg += "\n\tnsimpath = %s" % paths['nsimpath']
    msg += "\n\tnmagpath = %s" % paths['realnmagpath']
    return msg

