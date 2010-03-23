"""
A collection of tools built on top of Python's `logging` package.
"""

__docformat__ = 'restructuredtext'

import logging, types

INFO2 = (logging.INFO + logging.DEBUG) >> 1

def loglevel_int_of_string(levelstring):
    """Convert string into logging-level int.

    Given the name of a loglevel, return the numerical value.
    Includes all levels from logging plus 'info2' which is between
    'info' and 'debug'.

    :Parameters:
      `levelstring` : string
        Possible values are: 'error', 'critical', 'warn', 'warning',
                             'info', 'info2', 'debug', 'notset'

    :Return:

      Values are identical to values from Python's
      ``logging`` module. ``logging`` does not have the ``info2``
      level, which has value 15 (halfway between ``info`` and
      ``debug``.

    """
    # Match levelstring to level
    levels = {'error'   : logging.ERROR,
              'critical': logging.CRITICAL,
              'warn'    : logging.WARN,
              'warning' : logging.WARNING,
              'info'    : logging.INFO,
              'info2'   : INFO2,
              'debug'   : logging.DEBUG,
              'notset'  : logging.NOTSET}

    assert levelstring != None
    assert levelstring != ''

    levelstring = levelstring.lower()
    for levelname in levels.keys():
        if levelstring in levelname:
            return levels[levelname]

    if levelstring.isdigit():
        return int(levelstring)

    # At this point we have had a levelname but couldn't match it.
    raise ValueError("Can only handle levels: %s or integer values, but "
                     "received '%s'" % (levels.keys(), levelstring))

def set_loglevel_comfort(logger, level):
    """Set log level for logger; level can be int or string."""

    if type(level) == types.StringType:
        level = loglevel_int_of_string(level)

    logger.setLevel(level)
    logger.debug("Setting logger level to %s" % level)

def set_global_loglevel(level):
    """Set log level of root logger."""
    return set_loglevel_comfort(logging.getLogger(''), level)

def logging_status_str():
    """Return a string that shows all knows loggers and their current levels

    This is useful for debugging of the logging module.
    """
    rootlog = logging.getLogger('')
    msg = ("Current logging status: "
           "rootLogger level=%2d\n" % rootlog.level)

    # This keeeps the loggers (with the exception of root)
    loggers = logging.Logger.manager.loggerDict
    for loggername, logger in [('root', rootlog)] + loggers.items():
        if logger.__class__ == logging.PlaceHolder:
            continue # these are non-existing parent loggers
                     # (i.e. "ocaml" if only "ocaml.ocaml" was defined)
        for i in range(len(logger.handlers)):
            handler = logger.handlers[i]
            #add output depending on handler class
            if handler.__class__ == logging.StreamHandler:
                handlerstr = str(handler.stream)
            elif handler.__class__ == logging.FileHandler:
                handlerstr = str(handler.baseFilename)
            else:
                handlerstr = str(handler)
            
            msg += (" %15s (lev=%2d, eff.lev=%2d) -> handler %d: lev=%2d %s\n"
                    % (loggername, logger.level, logger.getEffectiveLevel(),
                       i, handler.level, handlerstr))
    return msg

def setup_loggers(logconfigfilename, logfilepath):
    """Set up logging"""

    #read configuration
    import logging.config
    logging.config.fileConfig(logconfigfilename,
                              defaults={'logfilepath':logfilepath})

    # Register our extra level INFO2 (more output than INFO but less than
    # DEBUG) the idea being that the user may want to learn a bit more 
    # but not all (then use INFO2)
    logging.addLevelName(INFO2, 'INFO2')

    # We also support 15<->Info in Nlog (Nlog is the OCaml logger)

def get_handlers_file_names():
    """Return the file names associates to all the active file handlers in the
    logging."""
    root_logger = logging.getLogger('')
    all_handlers = root_logger.handlers
    for h in root_logger.handlers:
        print h
    
    return all_handlers

