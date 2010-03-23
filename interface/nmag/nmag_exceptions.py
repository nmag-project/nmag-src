import logging

class NmagBaseError(Exception):
    # This docstring originally contained 'NmagBaseException'. While I
    # prefer that name, I changed the docstring to match the class
    # name rather than the other way around, as that is less likely to
    # break clent code. However, we should consider changing both to
    # 'NmagBaseException'.
    """NmagBaseError"""

    def __init__(self, msg=''):
        # The following line should really be 
        #    super(NmagBaseError, self).__init__(msg) 
        # but we might like to keep the old-fashioned version for
        # backwards compatibility. super was introduced in Python 2.2.
        Exception.__init__(self, msg)
        self.logger = logging.getLogger('nmag')
        self.logger.critical("Exception type: %s " % self.__doc__)
        self.logger.critical("Exception msg : %s " % msg)

# The following is equivalnt to repeating
#
# class SomeException(NmagBaseError):
#    "SomeException"
#
# for each exception listed in nmag_exception_names. While the new
# version is more cryptic, it does make adding new exception types
# easier, and it is makes sure that the mismatches between the class
# name and the docstring, which appeared in the original hand-written
# code, will never occur again. I wasn't going to commit this code as
# I judged that it wasn't worth making the code harder to understand
# in this way ... until I discovered the mismatches in the
# hand-written code, which I took to be justification enough. I won't
# be in the slightest bit offended if you hate this, and revert the
# change. There is, of course, also a slight possibility that this
# version of the code has some subtle bugs, which is almost impossible
# to imagine in the hand-written version. (jacek 2007/09/29)

nmag_exception_names = '''
    NmagDimError
    NmagImportError
    NmagUserError
    NmagStandardError
    NmagInternalError
    NmagTypeError
    NmagChangedInterfaceError
'''.split()


# Get a handle on this module's namespace, to allow programmatic
# binding of local names. This is slightly complicated by the odd
# behaviour of __import__. Here is the relevant part of __import__'s
# docstring:

#   When importing a module from a package, note that
#   __import__('A.B', ...)  returns package A when fromlist is empty,
#   but its submodule B when fromlist is not empty.

if __name__.find('.') > -1:
    fromlist = [None]
else:
    fromlist = []

this_module = __import__(__name__, globals(), locals(), fromlist)

# Surely there must be a simpler way to achieve the above !


# In Python2.5 exceptions became new-style classes, so we must detect
# the Python version and generate either a new-style or classic class
# accordingly
import sys
if sys.version_info < (2, 5):
    import types
    classtype = types.ClassType
else:
    classtype = type

# Programatically generate the exception classes and bind them in this
# module's namespace.
for name in nmag_exception_names:
    setattr(this_module, name,
            classtype(name, (NmagBaseError,), {'__doc__': name}))

__all__ = nmag_exception_names + ['NmagBaseError']
