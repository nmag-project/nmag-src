"""
A multi physics simulation framework based on finite elements.

The underlying engine is written in OCaml. 

This python packages provide a high-level interface to this
functionality, and general purpose tools.

Particular high-level user interfaces (such as *nmag*) use nsim, as
well as advanced users that need functionality not provided in nmag.

.. include:: <isonum.txt>

Copyright |copy| 2005-2007 by University of Southampton 

:Author: T Fischbacher, G Bordignon, M Frachin, H Fangohr

:License: GNU Public License (GPL)

:Version: $Id$

"""

__docformat__="restructuredtext"

# Define what gets imported with a 'from nmesh import *'
__all__ = ['snippets', 'features', 'logtools', 'when', 'versions', 'reporttools']

# Load __all__ in namespace so that a simple 'import nsim' gives
# access to them via nsim.<name>
glob,loc = globals(),locals()
for name in __all__:
    __import__(name,glob,loc,[])

# Namespace cleanup
del name,glob,loc


# get subversion build id (output from 'svnversion')
try:
    import svnversion
    __version__ = svnversion.svnversion
except:
    __version__ = 'not available'
