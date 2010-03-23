""" (C) 2007, 2009 Matteo Franchin, University of Southampton (UK)
Script to configure Nsim. Try

  python configure.py --help

to get a list of configuration options.
"""

import os, sys

#----------------------------------------------------------------------------
# We detect the python version and obtain the path where libpython is.

this_version = sys.version[:3]
this_python = "python" + this_version
# These paths are reconstructed following indications from main python
# documentation for module sys (http://docs.python.org/lib/module-sys.html)
py_config_files_dir = sys.exec_prefix + '/lib/%s/config' % this_python
py_cmodules_dir = sys.exec_prefix + '/lib/%s/lib-dynload' % this_python
py_modules_dir = sys.exec_prefix + '/lib/%s' % this_python
py_headers_dir = sys.exec_prefix + '/include/%s' % this_python

# We gather other configuration infos from the distutils python module.
try:
    import distutils.sysconfig
    pyconfig = distutils.sysconfig.get_config_vars()
except:
    pyconfig = {}

#----------------------------------------------------------------------------
# Some ugly architecture-dependent choices

# We just check if this machine has Mac OS X library flags (sort of :-/)
is_macos = False 
                 
if pyconfig.has_key('LDSHARED'): 
    sh_cmnd = pyconfig['LDSHARED']
    if "bundle" in sh_cmnd:
        is_macos = True # Mac OS X

if is_macos:
    # To build dynamic loadable modules (used by fastfield)
    LDSHARED = "-fno-common -bundle"
else:
    # Linux & co
    LDSHARED = "-fPIC -shared"

#----------------------------------------------------------------------------
# Here we define some functions for configuration of external libraries
# (finding directories and files)

class Msg:
    def __init__(self, show_debug=False):
        self.immediate = {}
        self.buffers = {}
        if show_debug:
            self.immediate["debug"] = sys.stdout

    def prn(self, msg, log="msg"):
        if self.immediate.has_key(log):
            self.immediate[log].write(msg)
        elif self.buffers.has_key(log):
            self.buffers[log] += msg
        else:
            self.buffers[log] = msg

    def prnln(self, msg, log="msg"):
        self.prn(msg + "\n", log)

    def debug(self, msg):
        self.prnln(msg, log="debug")

    def summary(self, msg):
        self.prnln(msg, log="summary")

    def show(self, log, title):
        if not self.buffers.has_key(log):
            return
        b = self.buffers[log]
        if len(b) > 0:
            bar = "-"*(len(title) + 2)
            print "%s\n%s\n%s\n%s" % (bar, title, bar, b)

msg = Msg()

def bool_to_yesno(b):
    if b:
        return "Yes"
    else:
        return "No"

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
        if full_path != None: return (full_name, full_path)
    return None

#-----------------------------------------------------------------------------
std_lib_paths = ['/usr/lib', '/usr/local/lib']
std_inc_paths = ['/usr/include', '/usr/local/include']
config_file = "configuration"
lib_option = "-l"
libpath_option = "-L"
inc_option = "-I"
use_full_lib_path = False

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
  ['/usr/lib/mpich/lib',
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
  std_inc_paths+['/usr/include/mpi']
)

configs["petsc-libdir"] = (
  "Path containing the library files for petsc",
  "PETSC_LIBRARY_PATH",
  "PETSC",
  ["petsc", "petscvec", "petscmat", "petscksp", "petscts", "petscsnes", "petscdm"],
  std_lib_paths + ["/usr/lib/petscdir/2.3.2/lib/linux-gnu-c-real-opt"]
)

configs["petsc-includedir"] = (
  "Path containing the C-headers files for petsc",
  "PETSC_INCLUDE_PATH",
  "PETSC",
  ["petsc.h", "petscksp.h", "petscts.h", "petscvec.h", "petscmat.h", "petscsys.h"],
  std_inc_paths + ["/usr/include/petsc"]
)

configs["metis-libdir"] = (
  "Path containing the library files for METIS and ParMETIS",
  "METIS_LIB_PATH",
  "METIS",
  ["metis", "parmetis"],
  std_lib_paths
)

configs["metis-includedir"] = (
  "Path containing the C-headers files for METIS",
  "METIS_INCLUDE_PATH",
  "METIS",
  ["metis.h"],
  std_inc_paths + ["/usr/include/metis"]
)

configs["numpy-includedir"] = (
  "Path where to find NumPy headers (usually figured out automatically)",
  "NUMPY_INCLUDE_PATH",
  "NUMPY",
  ["numpy/arrayobject.h"],
  [py_headers_dir]
)

configs["_util-libdir"] = (
  "Hidden",
  "UTIL",
  "UTIL",
  ["util"],
  std_lib_paths
)

other_opts = [
  ("help", "Shows this help screen"),
  ("libdir=", "Path to be used when searching for all the libraries"),
  ("includedir=", "Path to be used when searching for all the headers"),
  ("full-lib-name", "Use 'path/libname.so' instead of '-Lpath -lname'"),
  ("cflags=", "CFLAGS to use (override automatically detected values)")
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

#----------------------------------------------------------------------------
# Option parsing

import getopt, sys
long_opts = [option + '=' for option in configs]
long_opts.extend([other_opt[0] for other_opt in other_opts])
short_opts = 'h'

recognized, others = getopt.getopt(sys.argv[1:], short_opts, long_opts)

recognized_opts = map(lambda x: x[0], recognized)
processed_opts = {}

# Show help message if requested
if '-h' in recognized_opts or '--help' in recognized_opts:
    print help_msg(),
    sys.exit(0)

# Use the full name of the libraries (including path, prefix and suffix)
# when linking. Example: '/usr/lib/libmetis.so' instead of '-L/usr/lib -lmetis'
if '--full-lib-name' in recognized_opts:
    use_full_lib_path = True
    processed_opts['--full-lib-name'] = None

# Default assumption is that the user does not provide CFLAGS via a --cflags option:
CFLAGS=None 

# Add other paths
for opt, arg in recognized:
    if len(opt)>2 and opt[0:2] == "--":
        key = opt[2:]
        if configs.has_key(key):
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
    if opt=='--cflags':
        CFLAGS=arg
        processed_opts[opt] = None
    
    if not processed_opts.has_key(opt):
        msg.prnln("Warning: ignored option '%s'" % opt, "warning")

#----------------------------------------------------------------------------
# Now we find all the libraries nsim needs

# Now we search for libraries
configuration = {}
configuration_extra = {}
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
                                 suffixes=['.so', '.dylib'])
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

    if key[0] == '_': # Hidden option
        configuration_extra[var] = (path, found)
        continue

    if path != None:
        if incs: configuration["%s_INCFLAGS" % id_name] = incs
        if libs: configuration["%s_LDFLAGS" % id_name] = libs
        msg.debug("Adding variable x_INCFLAGS='%s' and x_LDFLAGS='%s' for x='%s'"
                  % (incs, libs, id_name))
        configuration[var] = path
        found_files[var] = found
    else:
        msg.prnln("Warning: configuration failed for '%s'" % key, "warning")

#----------------------------------------------------------------------------

if configuration_extra["UTIL"][1] != None:
    configuration["UTIL_CLIBS"] = "util"
else:
    configuration["UTIL_CLIBS"] = ""

#----------------------------------------------------------------------------
# Other configurations

configuration["OCAMLNCFLAGS"] = "" # "-ffast-math" # I should check that this is supported
configuration["PETSC_ARCH"] = "" # I should check that this is supported
configuration["PYCAML_OCAMLLDFLAGS"] = "" # I should check that this is supported
configuration["OCAMLDEBUGFLAGS"] = "-g" # I should check that this is supported
configuration["DEBUGFLAGS"] = "" # I should check that this is supported
configuration["NSIM_CFLAGS"] = "-Wall" # I should check that this is supported
configuration["PYCAML_OPT_DARWIN"] = "" # Remove?
configuration["EXTRA_LIBRARY_PATH"] = "" # Remove?
configuration["GCC_FLAGS_SHLIB"] = LDSHARED
configuration["PYCAML_CLIBS"] = this_python
configuration["PYTHON_LIBRARY_PATH"] = py_config_files_dir
configuration["PYTHON_INCLUDE_PATH"] = py_headers_dir

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

#----------------------------------------------------------------------------
# Calling the configure script generated by autoconf: this script will
# create the python file arch.py, which will be imported.
# This file contains all the relevant information that we need
# to obtain from autoconf.
# This mixed python-autoconf configuration system is somewhat ugly.
# We may want to change it in the future, fully opting for autoconf.

print "Invoking autoconf configure script to fine-tune the system..."
import os
exit_status = os.system("cd ac; ./configure")
print "Exited from ./ac/configure script with status %d" % exit_status
if exit_status !=  0: sys.exit(1)

try:
    import arch
    CFLAGS_ARCH = arch.arch_cflags
    sizeof_int = int(arch.sizeof_int)

except:
    msg.prnln("WARNING: Cannot import arch. Optimisation flags disabled!")
    msg.prnln("WARNING: Cannot obtain sizeof(int): assuming sizeof(int) = 4.")
    sizeof_int = 4
    CFLAGS_ARCH=""

if CFLAGS!=None:
    CFLAGS_ARCH=CFLAGS

configuration["CFLAGS_ARCH"] = CFLAGS_ARCH

#----------------------------------------------------------------------------
# We finally write the configuration to file(s): we produce four files
# one for each language used in nsim sources: ocaml, C, python and a file
# which can be included by Makefile-s

class ConfigFile:
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

for var in configuration: cf.add_value(var, configuration[var])

# Write the same configuration in different languages
cf.save('configuration.inc', language='makefile')
cf.save('configuration.h', language='c_header')
cf.save('nsimconf.py', language='python')
cf.save('nsimconf.ml', language='ocaml')

# Recap configuration settings on the screen
msg.summary("NumPy array support: %s" %
            bool_to_yesno(cf.have("NUMPY_INCLUDE_PATH")))

msg.show("warning", "Warning messages")
msg.show("summary", "Configuration summary")

