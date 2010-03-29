# Nmag micromagnetic simulator
# Copyright (C) 2010 University of Southampton
# Hans Fangohr, Thomas Fischbacher, Matteo Franchin and others
#
# WEB:     http://nmag.soton.ac.uk 
# CONTACT: nmag@soton.ac.uk
#
# AUTHOR(S) OF THIS FILE: XXX
# LICENSE: GNU General Public License 2.0
#          (see <http://www.gnu.org/licenses/>)

'''
Provide means of starting a new Python interactive loop from Python
and provides the main startup code for nsim.
'''

import sys
from code import InteractiveConsole

class Shell(InteractiveConsole):
    def __init__(self, locals=None, filename='<console>'):
        InteractiveConsole.__init__(self, locals=locals, filename=filename)

    def interact(self, banner=''):
        return InteractiveConsole.interact(self, banner=banner)

def ipython(globals=None, locals=None):
    """Interactive python prompt (see `Example: IPython`_)."""
    # We use an embedded ipython session
    # (http://ipython.scipy.org/doc/manual/node9.html)
    # to inspect the current state. The calling_frame magic is necessary
    # to get the context of the place where this ipython() function is called
    # (and not where IPShellEmded([]) is invoked.
    calling_frame = sys._getframe(1)
    if globals == None:
        globals = calling_frame.f_globals
    if locals == None:
        locals = calling_frame.f_locals
    from IPython.Shell import IPShellEmbed
    IPShellEmbed([])(local_ns=locals, global_ns=globals)

def main(args, locals=None, globals=None, use_ipython=True):
    from nsim.setup import setup
    options, arguments = setup(argv=args[1:], warn_about_py_ext=False)
    sys.argv = arguments

    if len(arguments) > 0:
        # Execute a file
        execfile(arguments[0], globals, locals)

    else:
        # Run the interactive loop

        # We should use ipython, when possible
        try:
            import IPython

        except:
            use_ipython = False

        if use_ipython:
            # Better to use ipython from nsim.snippets rather than the one
            # in nmag. Indeed, importing nmag, causes a lot of other modules
            # to be imported which can result in slow startup of the shell
            # on older machines.
            ipython(locals=locals, globals=globals)

        else:
            sh = Shell()
            sh.interact()

