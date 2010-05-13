"""A collection of useful utilities"""

__docformat__ = "restructuredtext"

import os, types, sys, time, inspect

import features

import logging

log = logging.getLogger('nsim')

def get_absolute_librarypath(library):
    """
    Get absolute path of package or file.

    Given a package (such as nmesh, nsim, ...), return a tuple
    containing (PATH, FILENAME) where FILENAME is the name of the
    __init__.py file that defines the package and PATH the location of
    that file.

    Useful to get the path to configuration files that are stored in
    the same place.
    """
    if type(library) == types.ModuleType:
        this_file_path = os.path.realpath(library.__file__)

    elif type(library) == types.StringType:
        # Assume this is just a filename
        this_file_path = os.path.realpath(library)

    library_directory, filename = os.path.split(this_file_path)
    return (library_directory, filename)

def get_runid(argv):
    """Extract runID from name of script

    This mostly strips off any leading subdirectories from the name of
    the script, and removes the .py extension if given.
    """

    if len(argv) == 0:
        return 'interactive-session'
        #raise StandardError,"sys.argv = %s -- can't determine name of program (and runid)" % sys.argv

    if len(argv) == 1 and argv[0]=='':
        raise StandardError,"sys.argv = %s -- can't determine name of program (and runid)" % sys.argv

    filename = sys.argv[0]

    if "/" in filename:
        filename = filename.split("/")[-1]
    #get rid of '.py' if neccessary
    if '.py' == filename[-3:]:
        run_id = filename[:-3]  # all is well
    else:
        run_id = filename

    #runid,extension = os.path.splitext(os.path.split(argv[0])[1])

    #if extension.lower() != ".py":
    #    raise ValueError, "Nmesh scripts need to have the .py extension"

    return run_id


def output_file_location(filename,directory=None):
    """Return path to save file, including 'etc/savedir' if given

    If 'directory'==None, and if globalfeatures (see
    nsim.features.Features) has section 'etc' with entry 'savedir',
    then this will be prepended to the filename.

    If 'directory' is given (string), then this will be prepended
    to the 'filename'

    Warning: This has not been used for some time and needs further
    testing. In particular, we need to double check whether this
    function is called whereever files are created.

    12/02/2007 08:25

    """

    if directory==None:

        globalfeatures=features.Features()
        if globalfeatures.has_sectionkey('etc','savedir'):
            directory=globalfeatures.get('etc','savedir')
        else:
            directory=''

    path = os.path.join(directory,filename)
    log.debug('composing path to save data to be %s' % path)
    return path


def get_version_string():
    """Return version string of all ocaml modules we use in nsim."""
    import ocaml
    versionstring=ocaml.version().replace("$:$","\n").replace("$","")
    version="$Name$\nUsing modules: \n%s" % versionstring
    return version


def get_ocaml_pill_size(pill):
    import ocaml
    data, header, depth = ocaml.memory_footprint(pill)
    return data, header, depth

def get_ocaml_pill_size_string(pill,name):
    kB=1024.
    data,header,depth = get_ocaml_pill_size(pill)
    msg = "Pill '%s' has size %dkB (data=%dB, head=%dB, depth=%d)" % \
          (name,(data+header)/kB,data,header,depth)
    return msg


def get_ml_heap_words():
    """Returns the number of words that OCaml thinks it is using.
    This needs to be multiplied by 4 (on a 32bit machine) or 8 (on a
    64bit machine) to get the number of bytes."""

    import ocaml
    size_str = ocaml.ml_heap_footprint()
    mwords = -1
    for line in size_str.split('\n'):
        if 'heap_words:' == line[0:len('head_words:')]:
            mwords = int(line.split(':')[1].strip())
            return mwords

def get_callers_string(levels=100):
    """Returns a string that lists the n calling levels, where n is LEVELS, in a string.

    Example:

        called from FUNC '            whereami' LINE 50 FILE 'test.py'
        called from FUNC '              callit' LINE 69 FILE 'test.py'
        called from FUNC '                   ?' LINE 72 FILE 'test.py'

    """
    callers = inspect.getouterframes(inspect.currentframe())[1:] #throw away this function

    caller_string=""

    for level in range(0,min(levels,len(callers))):
        caller = callers[level]
        caller_string += "\tcalled from FUNC '%20s' LINE %d FILE '%s'\n" % (caller[3],caller[2],caller[1])
    return caller_string[:-1] #do not return last \n

def rename_old_files(old_files):
    for file in old_files:
        if os.path.exists(file):
            newname = file + '.old'
            log.info("Found old file %s, rename it to %s" % (file, newname))
            os.rename(file, newname)

def funky_wait(wait, dt=0.2):
    "Pauses for wait seconds, printing a dot every dt seconds."
    waited_so_far = 0
    while waited_so_far < wait:
        time.sleep(dt)
        print ".",
        sys.stdout.flush()
        waited_so_far += dt
    print

from subprocess import Popen, PIPE, STDOUT

def exec_cmd(cmd_args):
    """Execute cmd_args[0] passing cmd_args[1:] as command line arguments.
    Returns a tuple (output, exit_status).
    """
    p = Popen(cmd_args, stdout=PIPE, stderr=STDOUT)
    output = p.communicate()[0]
    return (output, p.returncode)

def contains_all(the_list, the_items):
    """Returns true only and only if 'the_list' contains all the elements
    of the list 'the_items'."""
    for item in the_items:
       if item not in the_list:
           return False
    return True

def rec_apply(f, x):
    """Apply recursively f to all the element of the given list/typle, or,
    if the list/tuple contains other list/tuples apply f to their elements
    in a recursive fashion."""
    if isinstance(x, (types.TupleType, types.ListType)):
        return tuple([rec_apply(f, xi) for xi in x])
    else:
        return f(x)

def rec_scale(scale, x):
    """Scale recursively the given list (of lists, eventually)."""
    return rec_apply(lambda y: scale*y, x)

#def get_main_program_path():
#    """Returns a tuple (PATH,NAME) where PATH is the full path name of
#    the directory in which the main Python program file lives, and
#    NAME is the name of this Python program file.
#
#    (The 'main Python program file' is the one that is given to pyfem3
#    for execution, i.e. typically the X.py in 'nsim X.py'.)
#
#    This maybe useful to determine the path in which the main program
#    lives, which we use for regression tests.
#
#    Once we move to embedding OCaml in Python (rather than Python in
#    OCaml), the need for this function should disappear.
#
#    (fangohr, 27/3/09, 8:30)
#
#    Addition: if we use this function in the tested scripts, it reports
#    the location of the tests/pytest_main.py, so this doesn't really help.
#
#
#    """
#    import ocaml, os, sys
#
#    iwd = ocaml.get_initial_working_directory()
#    if len( sys.argv ) > 0:
#        main_file = sys.argv[0]
#    else:
#        main_file = ''
#    full_path = os.path.join(iwd,main_file)
#    return os.path.split(full_path)

