"""
File to be called in the initial set up phase of nmag.

Tasks:

 - extract run_id
 - provide globally visible feature object for high-level python settings
    (features) which provides run_id
 - provide data for ocaml features
 - scan for command line parameters

   - logging level
   - loggingconfiguration file
   - ocaml feature configuration file

 - read configuration files for

   - logging

     - set up loggers

   - other nsim/nmag settings

     - set ocaml features from config files

 - write configuration in files so we know what we have done (this is
   _ocaml.conf, _log.conf, _nmag.conf)

"""

import sys, os, logging, optparse, time

log = logging.getLogger('') # Use root logger as long as we don't have our own

# The feature stores, used to propagate setup information to all the parts of
# the program, both the Python and OCaml code.
import nsim
from nsim.versions import get_version_string, get_nmag_release_info,\
                          get_nmag_paths_string
pyfeatures = None
ocamlfeatures = None

# A dictionary to keep track of what setup stage has been done already
task_done = {'completed': False,
             'features': False,
             'cmdline': False,
             'logging': False,
             'welcome': False}

# The OptionParser object which will be used in the setup stage to parse the
# command line of the nsim executable
cmdline_parser = None

def get_root_path(relative_path=None):
    """
    Get the absolute path where the Nsim Python executable is located.
    """
    p = os.path.join(os.path.split(nsim.__file__)[0], "..", "..")
    if relative_path != None:
        p = os.path.join(p, *relative_path)
    return os.path.realpath(p)

    nsim_module_path = os.path.split(os.path.realpath(nsim.__file__))[0]
    return os.path.split(nsim_module_path)[0]

def get_interface_path():
    """
    Get the absolute path where the Nsim Python interface is located
    (this is the directory containing the directories nsim, nmag, nmesh, ...
    which contain the implementation of the corresponding modules).
    """
    return get_root_path(['interface'])

def get_exec_path(executable='nsim'):
    """Return the full path to the given executable of the Nsim package."""
    return get_root_path(['bin', executable])

# Get the full path of the nsim executable
nsim_exec_path = get_exec_path('nsim')

def tune(act_as_library=False):
    """This function can be used to tune the way the setup is done (remember
    that the setup is done by the function ``setup`` define in this same
    module). If you whish to change the setup behaviour you should import
    the module setup and use this function as soon as you can (which typically
    means before importing nmag or any other module using invoking
    ``setup.setup``)."""
    if act_as_library:
        for task in ['cmdline', 'welcome', 'logging']:
            task_done[task] = True

def get_nmag_runid(argv):
    if argv == None or len(argv) == 0:
        return 'interactive-session'
    runid, _ = os.path.splitext(os.path.split(argv[0])[1])
    return runid

def generate_cmdline_parser():
    """Create an OptionParse object to parse the Nsim command line."""

    # Setup OptionParser to (semi-conveniently) parse input options
    usage = ("usage: %prog [options] \n\n"
             "(C) University of Southampton, United Kingdom, 2005, 2006")

    import nsim.versions
    version = "%s\n%s\n" % (get_nmag_release_info(),
                            get_nmag_paths_string())

    #version = (nsim.snippets.get_version_string() +
    #           "\n\nUse 'nsimversion' to find the version of your "
    #           "nmag release.")

    parser = optparse.OptionParser(usage=usage, version=version)

    help = ('verbosity level (root logger): '
            'critical|error|warn|info|info2|debug')
    parser.add_option('--loglevel',  '-l', help=help,
                      type='string', metavar='LEVEL')

    help = 'Force logging even in interactive mode'
    parser.add_option('--log', help=help,
                      dest='forcelog', action='store_true')

    help = 'remove all existing data files for this simulation script'
    parser.add_option('--clean', help=help,
                      dest='clean', action='store_true')

    help = 'Continue hysteris loop calculation from last snapshot'
    parser.add_option('--restart', help=help,
                      dest='restart', action='store_true')

    help = 'Debug-feature: use particular config file for logging (advanced)'
    parser.add_option('--logconfigfile', help=help,
                      type='string', metavar='FILE')

    help = 'Debug-feature: use particular ocaml config file (experts only)'
    parser.add_option('--ocamlconfigfile', help=help,
                      type='string',metavar='FILE')

    help = ("Debug-feature: will report size of OCamlPill at various places. "
            "Do only use for small systems (i.e. not too many mesh points) "
            "as this requires a lot of stack memory. (Can also use "
            "'ulimit -s 65536' to extend stack to 64MB, for example). Most "
            "of the pill size messages are only reported at --loglevel=debug")
    parser.add_option('--pillsizereports', help=help,
                      dest='debugpillsizereports', action='store_true')

    help = ("Debug-feature: will show any logging messages issued on the "
            "slave nodes (normally supressed for efficiency). These are "
            "label with 'S0X-' where X is the rank of the node that issued "
            "the message.")
    parser.add_option('--slavelog', help=help,
                      dest='slavelog',action='store_true')

    help = ("Debug-feature: will write configuration files that reflect the "
            "settings of internal 'feature' structures, and logging "
            "configurations.")
    parser.add_option('--dumpconf', help=help,
                      dest='dumpconf', action='store_true')

    return parser

def cmdline_to_pyfeatures(options, arguments):
    """Feed information from command-line options into pyfeatures."""

    # Features with values
    for ft in ('loglevel', 'logconfigfile'):
        if getattr(options, ft):
            pyfeatures.set('nmag', ft, getattr(options, ft))

    # Boolean features determined by option presence
    for ft in ('clean', 'restart', 'slavelog', 'dumpconf'):
        pyfeatures.set('nmag', ft, getattr(options, ft))


def setup_things(options, arguments):
    # We need to do this after we have read the configuration files.

    # debug switches for ocaml
    if getattr(options, 'debugpillsizereports'):
        ocamlfeatures.set('debug', 'do_ocaml_pill_memory_reports', 'true')
    else:
        ocamlfeatures.set('debug', 'do_ocaml_pill_memory_reports', 'false')

    #if getattr(options, 'slavelog'):
    #    ocamlfeatures.set('nmag','slavelog','true')
    #else:
    #    ocamlfeatures.set('nmag','slavelog','false')

    log.debug("Ocamlfeatures after dealing with command line arguments:\n%s"
              % str(ocamlfeatures))

    # Do we actually need this?
    # fangohr 26/08/2008: Yes, this is useful for debugging and to understand
    #                     the configuration code.

    # Write current configurations if required
    if pyfeatures.get('nmag', 'dumpconf', raw=True):
        log.info("Found --dumpconf switch, dumping configurations...")

        runidbase = os.path.join(pyfeatures.get('etc', 'savedir'),
                                 pyfeatures.get('etc', 'runid'))

        conf_filename_1 = runidbase + '_nmag.conf'
        pyfeatures.to_file(conf_filename_1)
        log.info("Found --dumpconf switch: pyfeatures -> '%s'"
                 % conf_filename_1)

        conf_filename_2 = runidbase + '_ocaml.conf'
        open(conf_filename_2, 'w').write(str(ocamlfeatures))
        log.info("Found --dumpconf switch: ocamlfeatures -> '%s'"
                 % conf_filename_2)

        logfilepath = pyfeatures.config.defaults()['logfilepath']
        logconf = nsim.features.Features(local=True,
                                         defaults={'logfilepath':logfilepath})
        logconfigfilename = pyfeatures.get('nmag', 'logconfigfile')
        logconf.from_file(logconfigfilename)

        conf_filename_3 = runidbase + '_log.conf'
        logconf.to_file(conf_filename_3)
        log.info("Found --dumpconf switch: logging.conf -> '%s'"
                 % conf_filename_3)

    if pyfeatures.get('nmag', 'slavelog', raw=True):
        import ocaml
        ocaml.nlog_log_mpi('ocaml.ocaml', 20,
                           'Log messages from slaves are reported to stdout')

def setup_ocaml_loggers():
    """Set up logging for ocaml: will provide hooks to Python logging
    into Ocaml's Nlog loggers."""

    def logging_nofileinfo(logger,level,msg):
        # Calls from Ocaml don't provide filename and line number information.
        # Need to make sure that logging doesn't attempt to get these, so we
        # disable this temporarily:
        logging._srcfile_org = logging._srcfile
        logging._srcfile = None
        # then do the call
        logger.log(level, msg)
        #logger.log(50,"About to flush")
        #...and restore. (Not Python-thread safe).
        logging._srcfile = logging._srcfile_org

    import ocaml

    # ocaml logger names are ending in '.ocaml' to distinguish in logs
    # whether the message comes from Python or Ocaml

    # These are the names of the ocaml loggers as seen from Python (and the user)
    pythonloggernames = ["nmesh.ocaml", "nfem.ocaml", "ocaml.ocaml"]

    loggers = {}

    for loggername in pythonloggernames:
        loggers[loggername] = logging.getLogger(loggername)

    # Can't create these closures in a loop, so need to have one per logger
    def ocamllognmesh(level, msg):
        logging_nofileinfo(loggers["nmesh.ocaml"], level, msg)

    # First make sure a logger has been registered with Nlog.
    # (This will -- if the logger doesn't exist -- create one.)

    log.debug("Calling ocaml.nlog_setupLogger for nmesh.ocaml")
    ocaml.nlog_setupLogger("nmesh.ocaml")

    loggers['nmesh.ocaml'].debug("Test 1")
    # Now that the logger exists, we can modify it's default handler:
    ocaml.nlog_register_handler("nmesh.ocaml", ocamllognmesh)
    loggers['nmesh.ocaml'].debug("Test 2")

    def ocamllognfem(level, msg):
        logging_nofileinfo(loggers["nfem.ocaml"], level, msg)

    ocaml.nlog_setupLogger("nfem.ocaml")
    ocaml.nlog_register_handler("nfem.ocaml", ocamllognfem)

    def ocamllogocaml(level, msg):
        logging_nofileinfo(loggers["ocaml.ocaml"], level, msg)

    ocaml.nlog_setupLogger("ocaml.ocaml")
    ocaml.nlog_register_handler("ocaml.ocaml", ocamllogocaml)

    log.debug("Done registering loggers nmesh.ocaml, nfem.ocaml "
              "and ocaml.ocaml for ocaml (available in ocaml via Nlog)")

def setup_complete(value=True):
    """Mark the setup as completed, forbidding any further setup operation to be
    carried out in the rest of the program execution."""
    task_done['completed'] = value

def setup_one(argv=None, do_features=False, do_logging=False,
              do_welcome=False, log_to_console_only=False):
    """Similar to the function ``setup`` but does default to False for all the
    setup stages."""
    return \
      setup(argv=argv, do_features=do_features, do_logging=do_logging,
            do_welcome=do_welcome, log_to_console_only=log_to_console_only)

def setup(argv=None, do_features=True, do_logging=True,
          do_welcome=None, log_to_console_only=None,
          warn_about_py_ext=True):
    """Carry out the various stages of the setup of Nsim.

    do_cmdline: the command line is parsed using the OptionParser object stored
    inside cmdline_parser variable. If it has not been set by the user, then
    it is automatically generated in this function.
    """

    # If the setup has been marked as completed then exit immediately
    if task_done['completed']:
        return

    # Check once for all what really needs to be done
    do_features = do_features and not task_done['features']
    do_cmdline =  (argv != None) and not task_done['cmdline']
    do_logging =  do_logging and not task_done['logging']
    do_welcome = (do_welcome == True
                  or (do_welcome == None and not task_done['welcome']))

    # We first parse the command line
    (options, arguments) = (None, None)
    if do_cmdline:
        global cmdline_parser
        if cmdline_parser == None:
            cmdline_parser = generate_cmdline_parser()
        (options, arguments) = cmdline_parser.parse_args(argv)
        is_interactive = (len(arguments) == 0)

        # Deal here with some of the command line args
        if options.forcelog:
            log_to_console_only = False
        else:
            log_to_console_only = is_interactive

        task_done['cmdline'] = True

    # We would like now to setup the ocaml and python feature objects.
    # First, however, we need to determine the name of the logging file, since
    # it is required in order to construct correctly the pyfeature object.
    savedir = '.'
    runid = get_nmag_runid(arguments)
    logfilename = runid + '_log.log'
    logfilepath = os.path.join(savedir, logfilename)

    # We now find out where we should read the configuration for logging
    logconfigfile = None
    if options != None and options.logconfigfile != None:
        logconfigfile = options.logconfigfile

    # We can now construct the feature objects
    global pyfeatures, ocamlfeatures
    if do_features:
        pyfeatures = \
          nsim.features.Features(defaults={'logfilepath':logfilepath})
        ocamlfeatures = nsim.features.OcamlFeatures()

        # We now determine the exact name of the config file which we should
        # load in order to setup the logger
        nmaglibrarypath = nsim.snippets.get_absolute_librarypath(__file__)[0]

        if logconfigfile == None:
            # The user has not provided his own configuration file for logging
            # we then have to use one of the default files.
            if log_to_console_only:
                logconfigfile = "logging-console.conf"

            else:
                logconfigfile = "logging.conf"

            # We need to prepend the path where this files can be found
            # (which doesn't make sense if the user has provided his own file)
            logconfigfile = os.path.join(nmaglibrarypath, logconfigfile)

        else:
            # The option log_to_console_only does not make sense in this case.
            # We may 'assert log_to_console_only == None', but we don't.
            # We just ignore the issue, this is the best thing to do!
            pass


        # We finally fill the pyfeatures object with default values (to be
        # overridden by command line options)
        ocamlconfigfile = os.path.join(nmaglibrarypath, 'ocaml.conf')
        pyfeatures.set('nmag', 'ocamlconfigfile', ocamlconfigfile)
        pyfeatures.set('nmag', 'logconfigfile', logconfigfile)
        pyfeatures.set('nmag', 'loglevel', 'info')
        pyfeatures.set('etc', 'runid', runid)
        pyfeatures.set('etc', 'mainprogram', 'nmag')
        pyfeatures.set('etc', 'savedir', savedir)   # Location of output files

        # Move any settings from pyfeatures to OCaml's features living in
        # 'snippets'
        pyfeatures.to_ocaml_features()
        task_done['features'] = True

    # Interpret the command line arguments and override settings in pyfeatures
    if do_cmdline:
        cmdline_to_pyfeatures(options, arguments)

        # We rename the files here for two reasons:
        # - we have to do it before the log file is created
        #   (before the 'do_logging' section)
        # - we have to do it after the command line has been parsed
        #   (and only if the command line has been parsed?), after the config
        #   file has been read and we hence know what is the name of the log
        #   file.
        if pyfeatures.get('nmag', 'clean', raw=True):
            nsim.snippets.rename_old_files([logfilename])

    # We are now ready to setup the logger
    global log
    log = logging.getLogger('nsim')

    if do_logging:
        # Setup logging from default logging configuration file
        nsim.logtools.setup_loggers(logconfigfile, logfilepath)

        # Last, we need to set the global log level
        nsim.logtools.set_global_loglevel(pyfeatures.get('nmag', 'loglevel'))

        # All loggers are set up.
        setup_ocaml_loggers()

        log.debug('current logging status is \n%s'
                  % nsim.logtools.logging_status_str())
        task_done['logging'] = True

    # Just a warning message about the extension of the file
    if warn_about_py_ext and do_cmdline and len(argv) > 0:
        _, script_extension = os.path.splitext(os.path.split(argv[0])[1])
        if script_extension.lower() != ".py":
            msg = ("Nmag scripts need to have the .py extension. Will wait "
                   "2 seconds before continuing...")
            log.warn(msg)
            nsim.snippets.funky_wait(2)

    if do_cmdline:
        setup_things(options, arguments)

    if do_welcome:
        import ocaml
        log.debug("Sundials library path ='%s'"
                  % ocaml.get_nsim_sundials_library_path())

        nr_cpus = ocaml.petsc_mpi_nr_nodes()
        log.info("Nsim %s" % get_version_string())
        log.info("Runid is '%s'" % (pyfeatures.get('etc', 'runid')))
        log.info("Using %d CPUs" % (nr_cpus))

        if nr_cpus > 1:
            log.info("Waiting 1 seconds for messages from slaves to arrive "
                     "(experimental)")
            ocaml.mpi_hello()
            time.sleep(1)

        task_done['welcome'] = True

    return (options, arguments)

def get_features():
    return (pyfeatures, ocamlfeatures)

