"""fem -- Finite Element Simulations

XXX TODO: Document Me!

The underlying engine is written in OCaml. This python packages
provide a high-level interface.

(C) University of Southampton 2005,2006
T Fischbacher, G Bordignon, J Kenny, H Fangohr

Licensed under GNU Public License (GPL) v2

$Id$

"""


# Define what gets imported with a 'from fem import *'
__all__ = ['main','fields']

# Load __all__ in nmesh namespace so that a simple 'import nmesh' gives
# access to them via nmesh.<name>
glob,loc = globals(),locals()
for name in __all__:
    __import__(name,glob,loc,[])

# Namespace cleanup
del name,glob,loc

# import all objects from main.py
from main import *

#__version__ = "$Id$"
