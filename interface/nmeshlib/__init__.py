# Define what gets imported with a 'from nmesh import *'
__all__ = ['lib1','nmesh_exceptions']

# Load __all__ in nmesh namespace so that a simple 'import nmesh' gives
# access to them via nmesh.<name>
glob,loc = globals(),locals()
for name in __all__:
    __import__(name,glob,loc,[])

# Namespace cleanup
del name,glob,loc

from lib1 import *
