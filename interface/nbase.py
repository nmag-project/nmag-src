import sys, os, logging, optparse, types

import nsim

rootlog = logging.getLogger('')

class Config:
    """Very simple class to keep configuration of system.
    Can make this more sophisticated later if required"""
    def __init__(self):
        self._conf = {}
    def get(self,key):
        value = self._conf[key]
        return value
    def set(self,key,value):
        self._conf[key]=value
    def has_key(self,key):
        return self._conf.has_key(key)

    def __str__(self):
        s = "Debug info: Data in Configuration class\n"
        keys=self._conf.keys()
        keys.sort()
        for key in keys:
            s = s+"   %25s -> %s\n" % (str(key),str(self._conf[key]))
        return s

#    def __del__(self):
#        print "in destructor Config"
#        if self.has_key('ocaml_profiling'):
#            if self._conf['ocaml_profiling'] == True:
#
#                import ocaml
#                report = ocaml.sys_profiling("report")
#                print "%10f %7d %s" % ("time","calls","name")
#                for item in report:
#                    print "%10f %7d %s" % (line[0],int(line[1]),line[2])

def get_version_string():
    import ocaml
    versionstring = ocaml.version().replace("$:$","\n").replace("$","")
    version="$Name$\nUsing modules: \n%s" % versionstring
    return version

def _get_run_id_of_scriptname(argvs):
    """Take name of main python script and chop off .py"""
    if len(argvs) == 0:
        return 'interactive-session'
        #or we could do this:
        #raise StandardError, "len(sys.argv)==0 -- this should be impossible?"

    # Get the current script name without the leading path
    filename = os.path.basename(argvs[0])

    # Get rid of '.py' if neccessary
    if filename.endswith('.py'):  
        return filename[:-3]  # all is well
    else:
        return filename

def _config_default_parameters(conf):
    # Default parameters
    conf.set('loglevel', None)
    return conf

def _config_finalise_configuration(conf):
    """Here we set any default values that (at the moment) the user
    cannot change."""

    #other settings depending on run_dir
    conf.set('logfile_standard', os.path.join(conf.get('run_dir'),'log.log'))
    conf.set('logfile_user', os.path.join(conf.get('run_dir'),'user.log'))
    conf.set('file-cache-directory' ,
             os.path.join(conf.get('run_dir'),'cache'))
    return conf

def _configure(argvs):
    # Create (empty) configuration object
    conf = Config()

    # Start with default parameters
    conf = _config_default_parameters(conf)

    # and finalise (add further defaults that may depend on user input
    # via commandline)
    conf = _config_finalise_configuration(conf)
    return conf

def setup_run_dir(run_dir):
    """Create subdirectory where all the simulation output is stored"""
    # Check whether directory exists:
    if os.path.exists(run_dir):
        if not os.path.isdir(run_dir):
            raise IOError("expected '%s' to be a directory" % run_dir)
    else:
        os.mkdir(run_dir)
    return run_dir

def _create_logconfigfile_if_missing(conf):
    """Create a config file for logging (see
    http://docs.python.org/lib/logging-config-fileformat.html)

    If it exists already, then we don't touch it (as the user
    can use this to finetune the logging).

    Otherwise, we write a default config file to disk.
    """
    logconfigfilepathname = conf.get('logconfigfile')
        
    if os.path.exists(logconfigfilepathname):
        # Don't touch existing file -- may have been modified by user
        return logconfigfilepathname

    else:
        # Work out default values
        run_dir = conf.get('run_dir')
        logfilename = conf.get('logfile_standard')
        logfilepathname = os.path.join(run_dir, logfilename)
        userlogfilename = conf.get('logfile_user')
        userlogfilepathname = os.path.join(run_dir, userlogfilename)

        # Copy template with new default values
        logconf = \
          nsim.features.Features({'userlogfilepath':userlogfilepathname,
                                  'logfilepath':logfilepathname},
                                 local=True)

        from nsim.snippets import get_absolute_librarypath
        default_path = nsim.snippets.get_absolute_librarypath(nsim)[0]
        logconf.from_file(os.path.join(default_path, 'defaultlogging.conf'))
        # Write this to its new location in run_dir (usually)
        logconf.to_file(logconfigfilepathname)
    return logconfigfilepathname

def setup_logger(conf):
    """Set up logging"""
    logconfigfile = _create_logconfigfile_if_missing(conf)

    # Read configuration
    import logging.config
    logging.config.fileConfig(logconfigfile)

    # Register our extra level INFO2 (more output than INFO but less than
    # DEBUG) the idea being that the user may want to learn a bit more but
    # not all (then use INFO2)
    logging.addLevelName(15, 'INFO2')
    
    # Get root logger 
    rootlog = logging.getLogger('')

    level = conf.get('loglevel')
    # Modify level of root logger if has been specified at command line
    if level: # use None to not modify the settings from logconfigfile here
        rootlog.setLevel(level)
        rootlog.info('Setting global loglevel to %s',
                     logging.getLevelName(level))
    rootlog.debug("Setting up logging (fine tune in %s)" % logconfigfile)
    return rootlog

def setup_ocaml_loggers():
    """Set up logging for ocaml: will provide hooks to Python logging
    into Ocaml's Nlog loggers."""

    def logging_nofileinfo(logger,level,msg):
        #Calls from Ocaml don't provide filename and line number information.
        #Need to make sure that logging doesn't attempt to get these, so we
        #disable this temporarily:
        logging._srcfile_org = logging._srcfile
        logging._srcfile = None
        #then do the call
        logger.log(level, msg)
        #...and restore. (Not Python-thread safe).
        logging._srcfile = logging._srcfile_org
        
    import ocaml

    # Names in Python are ending in '.ocaml' to distinguish in logs
    # whether the message comes from Python or Ocaml

    pythonloggernames = ["nmesh.ocaml", "nfem.ocaml", "ocaml.ocaml"]

    loggers = {}

    for loggername in pythonloggernames:
        loggers[loggername] = logging.getLogger(loggername)

    # Can't create these closures in a loop, so need to have one per logger
    def ocamllognmesh(level, msg):
        logging_nofileinfo(loggers["nmesh.ocaml"], level, msg)

    # First make sure a logger has been registered with Nlog.
    # (This will -- if the logger doesn't exist -- create one.)
    ocaml.nlog_setupLogger("nmesh.ocaml")
    # Now that the logger exists, we can modify it's default handler:
    ocaml.nlog_register_handler("nmesh.ocaml", ocamllognmesh)

    def ocamllognfem(level, msg):
        logging_nofileinfo(loggers["nfem.ocaml"], level, msg)
    ocaml.nlog_setupLogger("nfem.ocaml")
    ocaml.nlog_register_handler("nfem.ocaml", ocamllognfem)

    def ocamllogocaml(level,msg):
        logging_nofileinfo(loggers["ocaml.ocaml"], level, msg)
    ocaml.nlog_setupLogger("ocaml.ocaml")
    ocaml.nlog_register_handler("ocaml.ocaml", ocamllogocaml)

    rootlog.debug("Done registering loggers nmesh.ocaml, nfem.ocaml and "
                  "ocaml.ocaml for ocaml (available via Nlog)")

def update_features(conf):
    import ocaml
    ocaml.snippets_register_feature('file-cache', 'directory',
                                    conf.get('file-cache-directory'))

def debug_python_logging_status_str():
    rootlog=logging.getLogger('')
    msg = "Current logging status: "
    msg +="rootLogger level=%2d\n" % rootlog.level
    #this keeeps the loggers (with the exception of root)
    loggers = logging.Logger.manager.loggerDict
    for loggername,logger in [('root',rootlog)]+loggers.items():
        if logger.__class__ == logging.PlaceHolder:
            continue #these are non-existing parent loggers
                     #(i.e. "ocaml" if only "ocaml.ocaml" was defined)
        for i in range(len(logger.handlers)):
            handler=logger.handlers[i]
            #add output depending on handler class
            if handler.__class__ == logging.StreamHandler:
                handlerstr = str(handler.stream)
            elif handler.__class__ == logging.FileHandler:
                handlerstr = str(handler.baseFilename)
            else:
                handlerstr = str(handler)
            
            msg += " %15s (lev=%2d, eff.lev=%2d) -> handler %d: lev=%2d %s\n" % (loggername,logger.level,logger.getEffectiveLevel(),i,handler.level,handlerstr)

    return msg

# This is the main function which activates the logging both for Python and
# OCaml. Here the run_* directories and files are created, etc.
def configure():
    # Here we wrap up the functionality of this script. Purpose is as follows:
    # If a simulation is to be run, then we need to call configure after
    # importing nbase (say in nmesh/main.py or nfem2/main.py.
    # If, howewer, we don't want this (say for nmesh2vtk), then we can import
    # nbase and set _need_to_run_configure to false (to avoid getting a run
    # directory set up etc)
    #
    #There is possibly a better way to do this.
    import nbase
    global _need_to_run_configure
    if _need_to_run_configure:
        # Combine default values and command line input into a
        # configure object that describes paths to logname, data files etc
        conf = _configure(sys.argv)
        nbase.conf = conf

        # Create data directory
        setup_run_dir(conf.get("run_dir"))

        # Set up python loggers
        rootlog  = setup_logger(conf)

        # Set up ocaml loggers
        setup_ocaml_loggers();

        # Set file cache directory (i.e. override default settings of 'features'
        # coming from pyfem2.ml
        update_features(conf);

        # Provide some info
        rootlog.info("run_id is '%s'" % conf.get("run_id"))
        rootlog.info("run_dir is '%s'"% conf.get("run_dir"))
        rootlog.debug("Complete configuration is \n'%s'"% conf.__str__())
        rootlog.debug("debug_python_logging_status_str()\n%s" 
                      % debug_python_logging_status_str())
        rootlog.debug("Current version is\n%s" % get_version_string())
        rootlog.debug("leaving nbase configure")
        _need_to_run_configure=False

    return nbase.conf

_need_to_run_configure = False

conf = Config()

