""" (C) 2007, 2009 Matteo Franchin, University of Southampton (UK)
Script to configure Nsim. Try

  python configure.py --help

to get a list of configuration options.
"""

import os
import sys
import re

###############################################################################
# WE START BY RETRIEVING CONFIGURATION INFORMATION FROM PYTHON

# We gather other configuration infos from the distutils python module.
try:
    import distutils.sysconfig
    pyconfig = distutils.sysconfig.get_config_vars()

except:
    pyconfig = {}

# Python version and name
pyversion = pyconfig.get("VERSION", sys.version[:3])
pynamever = "python" + pyversion

# These paths are reconstructed following indications from main python
# documentation for module sys (http://docs.python.org/lib/module-sys.html)
py_config_files_dir = sys.exec_prefix + '/lib/%s/config' % pynamever
py_cmodules_dir = sys.exec_prefix + '/lib/%s/lib-dynload' % pynamever
py_modules_dir = sys.exec_prefix + '/lib/%s' % pynamever
py_headers_dir = sys.exec_prefix + '/include/%s' % pynamever

# We gather further info on NumPy include directories from NumPy itself
try:
    from numpy.distutils.misc_util import get_numpy_include_dirs
    numpy_include_dirs = get_numpy_include_dirs()

except:
    numpy_include_dirs = []

#----------------------------------------------------------------------------
# Some ugly architecture-dependent choices

# We just check if this machine has Mac OS X library flags (sort of :-/)
is_macos = "bundle" in pyconfig.get('LDSHARED', []) 

# To build dynamic loadable modules (used by fastfield)
LDSHARED = ("-fno-common -bundle" if is_macos else "-fPIC -shared")

###############################################################################
# UTILITY FUNCTIONS AND CLASSES

# Here we define some functions for configuration of external libraries
# (finding directories and files)

def myexit(msg, exit_status=1):
    """Print a message and exit."""
    sys.stderr.write(msg)
    sys.exit(exit_status)

class Msg(object):
    def __init__(self, show_debug=False):
        self.immediate = {}
        self.buffers = {}
        if show_debug:
            self.immediate["debug"] = sys.stdout

    def prn(self, msg, log="msg"):
        if log in self.immediate:
            self.immediate[log].write(msg)
        elif log in self.buffers:
            self.buffers[log] += msg
        else:
            self.buffers[log] = msg

    def prnln(self, msg, log="msg"):
        self.prn(msg + "\n", log)

    def debug(self, msg):
        self.prnln(msg, log="debug")

    def summary(self, task, status):
        msg = task.rjust(30) + ": " + status
        self.prnln(msg, log="summary")

    def show(self, log, title):
        if log not in self.buffers:
            return
        b = self.buffers[log]
        if len(b) > 0:
            bar = "-"*(len(title) + 2)
            print "%s\n%s\n%s\n%s" % (bar, title, bar, b)

msg = Msg()

def bool_to_yesno(yes):
    return ("Yes" if yes else "No")

def possible_names(names, prefixes=[''], suffixes=['']):
    '''Given sets of names, prefixes and suffixes return the list
       of all the possible file names obtained by combining
       names, prefixes and suffixes in all the possible ways.
    '''
    pf = []
    for prefix in prefixes:
        for name in names:
            for suffix in suffixes:
                pf.append(prefix + name + suffix)
    return pf

def find_at_least_one(names, path):
    '''Returns the first file found when searching for the given names
       inside the directory provided in 'path'
    '''
    for name in names:
        msg.debug("(Searching %s)" % os.path.join(path, name))
        if os.path.isfile(os.path.join(path, name)):
            return name
    return None

def find_files(names, paths, prefixes=[''], suffixes=['']):
    '''Find a path in 'paths' which contains all the files specified
       in names. Possible prefixes or suffixes for the names are considered
       during the search.
    '''
    for path in paths:
        all_found = True
        found_files = []
        msg.debug("Searching %s in %s..." % (names, path))
        for name in names:
            msg.debug("Searching '%s'" % name)
            names_of_same_file = possible_names([name], prefixes, suffixes)
            found_file = find_at_least_one(names_of_same_file, path)
            found_files.append(found_file)
            if found_file == None:
                msg.debug("Not found!")
                all_found = False
                break
            msg.debug("Found!")
        if all_found: return path, found_files
    return None, None

def find_path_of_file(name, paths):
    for path in paths:
        if os.path.isfile(os.path.join(path, name)):
            return path
    return None

def find_file(names, prefixes, suffixes, paths):
    for full_name in possible_names(names, prefixes, suffixes):
        full_path = find_path_of_file(full_name, paths)
        if full_path != None:
          return (full_name, full_path)
    return None

def get_std_paths(additional_paths=[], env=None, sep=":"):
  paths = []
  if env != None:
    iterable = hasattr(env, "__iter__")
    assert not (iterable and type(env) == str)
    # ^^^ as far as I know str doesn't have __iter__
    envvars = env if iterable else [env]
    for envvar in envvars:
      envvar_content = os.getenv(envvar)
      if envvar_content:
        paths.extend(envvar_content.split(sep))

  for additional_path in additional_paths:
    if additional_path not in paths:
      paths.append(additional_path)
  return paths

# Replacement utilities
_configvar_re = re.compile(r"[@][a-zA-Z_]*[@]")

def configvar_replace(config, text):
    """"Substitute configuration variables with form @NAME@ with their
    values."""

    def replacer(match_object):
        try:
            cv = match_object.group(0)[1:-1]
        except:
            raise ValueError("Error when substituting the configuration "
                             "variable.")
        if len(cv) == 0:
            return "@"

        else:
            val = config.get(cv, None)
            return str(val) if val != None else ""

    return re.sub(_configvar_re, replacer, text)

def configvar_replace_file(config, filename_in, filename_out=None):
    """Copy file with name filename_in to a file with name filename_out,
    replacing all configuration variables with form "@NAME@" with their values.
    """
    if filename_out == None:
        filename_out = os.path.splitext(filename_in)[0]

    with open(filename_in, "r") as f:
        text_in = f.read()

    text_out = configvar_replace(config, text_in)

    with open(filename_out, "w") as f:
        f.write(text_out)
    
  
###############################################################################
# FIND BINARIES

std_bin_paths = \
  get_std_paths(['/bin', '/usr/bin', '/usr/local/bin'], "PATH")
std_lib_paths = \
  get_std_paths(['/usr/lib', '/usr/local/lib'], "LD_LIBRARY_PATH")
std_inc_paths = \
  get_std_paths(['/usr/include', '/usr/local/include'],
                ["CPATH", "C_INCLUDE_PATH"])

def find_binary(name, additional_paths=[], env_var=None, use_env=True):
  paths = std_bin_paths + additional_paths
  name_and_path = find_file([name], [''], [''], paths)
  if name_and_path == None:
    msg.prnln("Warning: cannot locate binary '%s'" % name, "warning")
    return name
  name, path = name_and_path
  return os.path.join(path, name)

###############################################################################
# PER-LIBRARY CONFIGURATION

config_file = "configuration"
lib_option = "-l"
libpath_option = "-L"
inc_option = "-I"
use_full_lib_path = False
gen_install_scripts = False

#import glob
#print glob('/usr/lib/petscdir/*')

#print _paths_by_libname

configs = {}

macos_blas_lapack_dirs = ['/System/Library/' \
                          'Frameworks/Accelerate.framework/Versions/A/' \
                          'Frameworks/vecLib.framework/Versions/A']
configs["lapack-libdir"] = (
  "Path containing the LAPACK library file",
  "LAPACK_LIB_PATH",
  "LAPACK",
  ["lapack"],
  std_lib_paths + macos_blas_lapack_dirs
)

configs["blas-libdir"] = (
  "Path containing the BLAS library file",
  "BLAS_LIB_PATH",
  "BLAS",
  ["blas"],
  std_lib_paths + macos_blas_lapack_dirs
)

configs["sundials-libdir"] = (
  "Path containing the Sundials library files",
  "SUNDIALS_LIB_PATH",
  "SUNDIALS",
  ["sundials_cvode", "sundials_nvecserial", "sundials_nvecparallel"],
  ["/usr/local/lib", "/usr/lib"]
)

configs["qhull-includedir"] = (
  "Path containing the C-headers for qhull",
  "QHULL_INCLUDE_PATH",
  "QHULL",
  [os.path.join("qhull", name) for name in ["qhull.h", "qset.h", "user.h"]],
  std_inc_paths
)

configs["qhull-libdir"] = (
  "Path containing the library files for qhull",
  "QHULL_LIBRARY_PATH",
  "QHULL",
  ["qhull"],
  std_lib_paths
)

mpich_lib_path = std_lib_paths + \
  ['/usr/local/mpi/openmpi/lib',
   '/usr/lib/mpich/lib',
   '/usr/lib/mpich/lib/shared',
   '/usr/local/mpich2/1.0.3/lib']

configs["mpi-libdir"] = (
  "Path containing the library files for MPI",
  "MPICH2_LIBRARY_PATH",
  "MPICH",
  ["mpi"],
  mpich_lib_path
)

configs["pmpich-libdir"] = (
  "Path containing the library files for pmpich",
  "PMPICH_LIB_PATH",
  "PMPICH",
  ["pmpich"],
  mpich_lib_path
)

configs["mpi-includedir"] = (
  "Path containing the C-header files for MPI",
  "EXTRA_INCLUDE_PATH",
  "MPICH",
  ["mpi.h"],
  std_inc_paths+['/usr/local/mpi/openmpi/include','/usr/include/mpi']
)

configs["petsc-libdir"] = [
  "Path containing the library files for petsc",
  "PETSC_LIBRARY_PATH",
  "PETSC",
  ["petsc", "petscvec", "petscmat", "petscksp", "petscts", "petscsnes", "petscdm"],
  std_lib_paths + ["/usr/lib/petscdir/2.3.2/lib/linux-gnu-c-real-opt"]
]

configs["petsc-includedir"] = (
  "Path containing the C-headers files for petsc",
  "PETSC_INCLUDE_PATH",
  "PETSC",
  ["petsc.h"],
  std_inc_paths + ["/usr/include/petsc", "/usr/local/petsc/include"]
)

configs["metis-libdir"] = (
  "Path containing the library files for METIS and ParMETIS",
  "METIS_LIB_PATH",
  "METIS",
  ["metis", "parmetis"],
  std_lib_paths + ["/usr/local/lib/parmetis"]
)

configs["metis-includedir"] = (
  "Path containing the C-headers files for METIS",
  "METIS_INCLUDE_PATH",
  "METIS",
  ["parmetis.h"],
  std_inc_paths + ["/usr/include/parmetis", "/usr/local/include/parmetis"]
)

configs["numpy-includedir"] = (
  "Path where to find NumPy headers (usually figured out automatically)",
  "NUMPY_INCLUDE_PATH",
  "NUMPY",
  ["numpy/arrayobject.h"],
  numpy_include_dirs
)

other_opts = [
  ("help", "Shows this help screen"),
  ("libdir=", "Path to be used when searching for all the libraries"),
  ("includedir=", "Path to be used when searching for all the headers"),
  ("full-lib-name", "Use 'path/libname.so' instead of '-Lpath -lname'"),
  ("with-single-petsc-lib", "Link only against libpetsc (PETSc > 3.1)"),
  ("cflags=", "CFLAGS to use (override automatically detected values)"),
  ("prefix=", "Target root installation directory for Nmag"),
  ("bindir=", "User executables (where executables are installed)"),
  ("datarootdir=", "Data root (where *.pyc files are installed)")
]

def help_msg(description_offset=28):
  '''Generate the help message with all options'''
  msg = ("This is the configuration script for Nsim.\n"
         "A number of configuration options can be specified to help "
         "the configuration process. Here is a list:\n")
  for option, description in other_opts:
      if option[-1] == '=':
          left = " --%sdir" % option
      else:
          left = " --%s" % option
      msg += left.ljust(description_offset) + description + "\n"
  for option in configs:
      if option[0] == '_': continue
      config = configs[option]
      description = config[0]
      left = (" --%s=dir" % option).ljust(description_offset)
      msg +=  left + description + "\n"
  return msg

###############################################################################
# COMMAND LINE PARSING

import getopt, sys
long_opts = [option + '=' for option in configs]
long_opts.extend([other_opt[0] for other_opt in other_opts])
short_opts = 'h'

recognized, others = getopt.getopt(sys.argv[1:], short_opts, long_opts)

recognized_opts = map(lambda x: x[0], recognized)
processed_opts = {}

# Show help message if requested
if '-h' in recognized_opts or '--help' in recognized_opts:
    myexit(help_msg() + "\n", 0)

# Use the full name of the libraries (including path, prefix and suffix)
# when linking. Example: '/usr/lib/libmetis.so' instead of '-L/usr/lib -lmetis'
if '--full-lib-name' in recognized_opts:
    use_full_lib_path = True
    processed_opts['--full-lib-name'] = None

if '--with-single-petsc-lib' in recognized_opts:
    configs['petsc-libdir'][3] = ['petsc']
    processed_opts['--full-lib-name'] = True


configuration = {}   # Configuration dictionary
user_CFLAGS = None   # Default assumption is that the user does not provide
                     # CFLAGS via a --cflags option:

# Add other paths
for opt, arg in recognized:
    if opt.startswith("--"):
        key = opt[2:]
        if key in configs:
            config = configs[key]
            config[4].insert(0, arg)
            continue

        else:
            if key == 'libdir':
                # Add this paths to all the config entries
                # which contain libdir (Such as qhull-libdir, sundials-libdir)
                for key in configs:
                    if 'libdir' in key:
                        configs[key][4].insert(0, arg)
                continue

            elif key == 'includedir':
                # Add this paths to all the config entries
                # which contain libdir (Such as qhull-libdir, sundials-libdir)
                for key in configs:
                    if 'includedir' in key:
                        configs[key][4].insert(0, arg)
                continue

    if opt == '--cflags':
        user_CFLAGS = arg
        processed_opts[opt] = None

    elif opt == '--prefix':
        configuration["BINDIR"] = os.path.join(arg, "bin")
        configuration["DATAROOTDIR"] = os.path.join(arg, "share")

    elif opt == '--bindir':
        configuration["BINDIR"] = arg

    elif opt == '--datarootdir':
        configuration["DATAROOTDIR"] = arg

    elif opt not in processed_opts:
        msg.prnln("Warning: ignored option '%s'" % opt, "warning")

###############################################################################
# Determine the installation mode and directories

# Set the path to the sources (the directory containing this script)
src_dir = os.path.realpath(os.path.split(sys.argv[0])[0])
configuration["SRCDIR"] = src_dir

# Info about all directories involved in the installation process
varname_reldir = \
  (("BINDIR", ("bin",)),
   ("DATAROOTDIR", ("share",)))

# Determine the root directory
root_dir = None
for varname, reldir in varname_reldir:
    varval = configuration.get(varname, None)
    if varval != None:
        if root_dir == None:
            dotdot = len(reldir)*("..",)
            root_dir = os.path.realpath(os.path.join(varval, *dotdot))

# Whether Nmag should be installed or used locally
do_install = (root_dir != None)

# If root_dir is not set, then set it to the current Nmag source directory
# (i.e. the directory containing this script).
if root_dir == None:
    configuration["BINDIR"] = os.path.join(src_dir, "bin")
    configuration["DATAROOTDIR"] = src_dir

else:
    # Resolve all the paths
    for varname, reldir in varname_reldir:
        configuration.setdefault(os.path.join(root_dir, *reldir))

#----------------------------------------------------------------------------
# Print out some preliminary info
print "Searching for binaries in:  " + ", ".join(std_bin_paths)
print "Searching for libraries in: " + ", ".join(std_lib_paths)
print "Searching for headers in:   " + ", ".join(std_inc_paths)

#----------------------------------------------------------------------------
# Now we find all the libraries nsim needs

# Now we search for libraries
found_files = {}
for key in configs:
    config = configs[key]
    var = config[1]
    id_name = config[2]
    files = config[3]
    paths = config[4]

    libs = ""
    incs = ""
    path = None
    found = None
    if 'libdir' in key:
        msg.debug("Searching library files for the library '%s'" % key)
        path, found = find_files(files,
                                 paths,
                                 prefixes=['lib'],
                                 suffixes=['.0','.1','.so', '.dylib'])
        if files:
            if use_full_lib_path and found:
                libs += " ".join([os.path.join(path, name) for name in found])
                msg.debug("Using full path '%s'" % libs)
            else:
                libs = "%s%s " % (libpath_option, path)
                libs += " ".join(["%s%s" % (lib_option, f) for f in files])
                msg.debug("Using '%s'" % libs)

    else:
        msg.debug("Searching C-header files for the library '%s'" % key)
        path, found = find_files(files, paths)
        if path:
            incs = "%s%s" % (inc_option, path)

    if path != None:
        if incs: configuration["%s_INCFLAGS" % id_name] = incs
        if libs: configuration["%s_LDFLAGS" % id_name] = libs
        msg.debug("Adding variable x_INCFLAGS='%s' and x_LDFLAGS='%s' for x='%s'"
                  % (incs, libs, id_name))
        configuration[var] = path
        found_files[var] = found
    else:
        msg.prnln("Warning: configuration failed for '%s'" % key, "warning")

###############################################################################
# Retrieve PyCaml specific flags (taken from 'python-config', option --libs)

#if not pyconfig.get("Py_ENABLE_SHARED"):
#    myexit("The Python executable you are trying to use has not been "
#           "compiled with support for shared libraries. Exiting...")

pycaml_ldflags = ["-L%s/lib" % sys.exec_prefix]
pycaml_ldflags += pyconfig.get("LIBS", "").split()
pycaml_ldflags += pyconfig.get("SYSLIBS", "").split()
pycaml_ldflags.append("-l" + pynamever)
configuration["PYTHON_LDFLAGS"] = " ".join(pycaml_ldflags)

#----------------------------------------------------------------------------
# Other configurations

configuration["OCAMLNCFLAGS"] = "" # "-ffast-math" # I should check that this is supported
configuration["PETSC_ARCH"] = "" # I should check that this is supported
configuration["PYCAML_OCAMLLDFLAGS"] = "" # I should check that this is supported
configuration["OCAMLDEBUGFLAGS"] = "-g" # I should check that this is supported
configuration["DEBUGFLAGS"] = "" # I should check that this is supported
configuration["GCC_FLAGS_SHLIB"] = LDSHARED
configuration["PYCAML_CLIBS"] = pynamever
configuration["PYTHON_LIBRARY_PATH"] = py_config_files_dir
configuration["PYTHON_INCLUDE_PATH"] = py_headers_dir
configuration["DLFLAGS"] = '-ldl'
configuration["DLLIB"] = 'ldl'
configuration["PETSC_INCFLAGS"] = "%s %s/usr/local/petsc/bmake/freebsd" % (configuration["PETSC_INCFLAGS"], inc_option)

if os.uname()[0] == 'FreeBSD':
    configuration["DLFLAGS"] = '-lc' # dlopen, etc are built into libc on FreeBSD
    configuration["DLLIB"] = 'c'

###############################################################################
# FIND BINARIES OF REQUIRED UTILITIES

required_binaries = \
  [("BASH", "bash",[], "Bash shell"),
   ("CC", "cc",[], "C compiler"),
   ("CPP", "cpp",[], "C preprocessor"), 
   ("PERL", "perl",[], "Perl interpreter"), 
   ("MPICC", "mpicc", ["/usr/local/mpi/openmpi/bin"],
    "Mpi C compiler wrapper"),
   ("OCAMLFIND", "ocamlfind",[], "Findlib utility"),
   ("OCAMLLEX", "ocamllex",[], "OCaml Lex utility"),
   ("OCAMLYACC", "ocamlyacc",[], "OCaml Yacc utility"),
   ("INSTALL", "install", [], "install utility")]

for env_var, command, paths, desc in required_binaries:
    # First we should try to scan the command line for something like VAR=VAL
    val = None

    # Then we check whether there is something in the environment
    if val == None:
        val = os.getenv(env_var)

    # Finally, if the user didn't give us instructions of where to find the
    # binary, then we will search it using the PATH environment variable
    if val == None:
        val = find_binary(command, paths)

    assert val != None
    configuration[env_var] = val
    
# We have to ensure that libpmpich and libmpich are in the same path!
try:
    if configuration["PMPICH_LIB_PATH"] != configuration["MPICH2_LIBRARY_PATH"]:
        configuration.pop("PMPICH_LDFLAGS")
except:
    pass

# We need to find the library files for sundials
# These will dynamically loaded using dlopen
# therefore we need to know them.
try:
    lib_path = configuration["SUNDIALS_LIB_PATH"]
    lib_names = [os.path.join(lib_path, lib_name)
                 for lib_name in found_files["SUNDIALS_LIB_PATH"]]
    for lib_name in lib_names:
        if "cvode" in lib_name:
            configuration["SUNDIALS_CVODE_LIB"] = lib_name
        if "nvecserial" in lib_name:
            configuration["SUNDIALS_NVECSERIAL_LIB"] = lib_name
        if "nvecparallel" in lib_name:
            configuration["SUNDIALS_NVECPARALLEL_LIB"] = lib_name
except:
    configuration["SUNDIALS_CVODE_LIB"] = None
    configuration["SUNDIALS_NVECSERIAL_LIB"] = None
    configuration["SUNDIALS_NVECPARALLEL_LIB"] = None

###############################################################################
# Calling the configure script generated by autoconf: this script will
# create the python file arch.py, which will be imported.
# This file contains all the relevant information that we need
# to obtain from autoconf.
# This mixed python-autoconf configuration system is somewhat ugly.
# We may want to change it in the future, fully opting for autoconf.

print "Invoking autoconf configure script to fine-tune the system..."

exit_status = os.system("cd config/ac; ./configure")
print ("Exited from ./config/ac/configure script with status %d"
       % exit_status)

if exit_status !=  0:
    sys.exit(1)

if user_CFLAGS != None:
    CFLAGS = user_CFLAGS

else:
    opt_CFLAGS = ""
    arch_CFLAGS = ""
    warn_CFLAGS = "" # I should check that this is supported

    sizeof_int = 4 # often the case (64 bits cpus are lp64, typically)

    try:
        sys.path.append("./config")
        import arch
        warn_CFLAGS = arch.warn_cflags
        opt_CFLAGS = arch.cflags
        arch_CFLAGS = arch.arch_cflags
        sizeof_int = int(arch.sizeof_int)
        pylibs = arch.pylibs

    except:
        msg.prnln("WARNING: Cannot import arch. Optimisation flags disabled!")
        msg.prnln("WARNING: Cannot obtain sizeof(int): "
                  "assuming sizeof(int) = 4.")

    CFLAGS = " ".join([warn_CFLAGS, arch_CFLAGS, opt_CFLAGS])

configuration["CFLAGS_ARCH"] = CFLAGS
configuration["NSIM_CFLAGS"] = CFLAGS

###############################################################################
# We finally write the configuration to file(s): we produce four files
# one for each language used in nsim sources: ocaml, C, python and a file
# which can be included by Makefile-s

class ConfigFile(object):
    def __init__(self):
        self.content = []
        makefile = {}
        makefile['comment'] = lambda args: '# %s\n' % args[0]
        makefile['value'] = lambda args: '%s=%s\n' % (args[0], args[1])
        c_header = {}
        c_header['comment'] = lambda args: '/* %s */\n' % args[0]
        c_header['value'] = lambda args: '#define %s "%s"\n' % (args[0], args[1])
        ocaml = {}
        ocaml['comment'] = lambda args: '(* %s *)\n' % args[0]
        ocaml['value'] = lambda args: 'let %s = "%s";;\n' % (args[0].lower(), args[1])
        py = {}
        py['comment'] = lambda args: '# %s\n' % args[0]
        py['value'] = lambda args: '%s = "%s"\n' % (args[0].lower(), args[1])
        self.executors = {'makefile':makefile, 'c_header':c_header,
                          'ocaml':ocaml, 'python':py}
    def add_comment(self, string):
        self.content.append(['comment', string])
    def add_value(self, name, value):
        self.content.append(['value', name, value])
    def add_extra(self, language, extra):
        self.content.append(['extra', language, extra])
    def save(self, file_name, language='makefile'):
        executor = self.executors[language]
        f = open(file_name, "w")
        for line in self.content:
            command = line[0]
            args = line[1:]
            if command == "extra":
                if args[0] == language: f.write(args[1])
            else:
                command_executor = executor[command]
                f.write(command_executor(args))
        f.close()
    def have(self, name):
        def f(item):
            return item[0] == "value" and item[1] == name
        return len(filter(f, self.content)) > 0

# Now we write the configuration file
cf = ConfigFile()

# We generate the configuration
cf.add_comment("Configuration file for Nsim")
cf.add_comment("generated automatically by configure.py")
bigarray_c_int = {4:'32', 8:'64'}[sizeof_int]
extra = """
(* We provide means to define bigarrays whose elements are integers with
   the same size of C integers declared with 'int' *)

(* This is the ocaml type associated with the bigarray *)
type c_int_mltype = int$$;;

(* This is the element type and determines the actual size of the bigarray *)
type c_int_elt = Bigarray.int$$_elt;;

(* This is the kind as used in the creation functions Array1.create, ... *)
let c_int_kind = Bigarray.int$$;;

(* Example: to create a one dimensional bigarray:

     let a1 = Bigarray.Array1.create Nsimconf.c_int_kind Bigarray.c_layout 5;;

   which would then have type:

     (Nsimconf.c_int_mltype, Nsimconf.c_int_elt, Bigarray.c_layout) Bigarray.Array1.t

   Here we define this type, for convenience.
 *)

type c_int_bigarray1 = (c_int_mltype,
                        c_int_elt,
                        Bigarray.c_layout) Bigarray.Array1.t;;

(* Functions to convert OCaml integers to c_int and back *)
let c_int_of_int oi = Int$$.of_int oi;;
let c_int_to_int ci = Int$$.to_int ci;;
(* functions to manipulate c_int-s *)
let c_int_add x y = Int$$.add x y;;
let c_int_sub x y = Int$$.sub x y;;
let c_int_bigarray1_create size =
  Bigarray.Array1.create
    c_int_kind Bigarray.c_layout size;;

"""
extra = extra.replace("$$", bigarray_c_int)
cf.add_extra('ocaml', extra)

for var in configuration:
    cf.add_value(var, configuration[var])

# Write the same configuration in different languages
cf.save('./src/configuration.h', language='c_header')
cf.save('./src/nsimconf.py', language='python')
cf.save('./src/nsimconf.ml', language='ocaml')

###############################################################################
# Replace all files

my_configuration = configuration.copy()
my_configuration["CONFIGURATION"] = repr(configuration)

in_files = \
  ["./src/Makefile.in",
   "./bin/Makefile.in",
   "./subst.py.in",
   "./Makefile.in"]

for in_file in in_files:
    print "Substituting", in_file
    configvar_replace_file(my_configuration, in_file)

del my_configuration

###############################################################################
# Recap configuration settings on the screen
for env_var, _, _, desc in required_binaries:
    msg.summary("%s (%s)" % (desc, env_var), configuration[env_var])

# Installation settings
msg.summary("Install Nmag on this system", bool_to_yesno(do_install))
for varname, _ in varname_reldir:
    msg.summary(varname, configuration[varname])

msg.summary("NumPy array support",
            bool_to_yesno(cf.have("NUMPY_INCLUDE_PATH")))
msg.summary("CFLAGS", CFLAGS)

msg.show("warning", "Warning messages")
msg.show("summary", "Configuration summary")

