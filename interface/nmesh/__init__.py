"""nmesh -- a n-dimensional mesher

The underlying engine is written in OCaml. This python packages
provide a high-level interface.

(C) University of Southampton 2005-
T Fischbacher, G Bordignon, J Kenny, H Fangohr

Licensed under GNU Public License (GPL)

subpackages:

nmesh.visual -- visualisation of meshes
nmesh.export -- file format conversions export
nmesh.importtools  -- file format conversions import (can't use name 'import')

$Id$

"""

# Define what gets imported with a 'from nmesh import *'
__all__ = ['main','visual','export','tools','importtools']

# Load __all__ in nmesh namespace so that a simple 'import nmesh' gives
# access to them via nmesh.<name>
glob,loc = globals(),locals()
for name in __all__:
    __import__(name,glob,loc,[])

# Namespace cleanup
del name,glob,loc

# import all objects from main.py
from nmeshlib.lib1 import *

# This may be useful in tests and 1D simulations
from nmeshlib.unidmesher import generate_1d_mesh

