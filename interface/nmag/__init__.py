"""nmag -- Micromagnetics Simulations

XXX TODO: Document Me!

The underlying engine is written in OCaml. This python packages
provide a high-level interface.

(C) University of Southampton 2005,2006
T Fischbacher, G Bordignon, M Franchin, H Fangohr

Licensed under GNU Public License (GPL) v2

$Id: __init__.py 3723 2007-07-16 16:22:46Z tf $

"""

__docformat__="restructuredtext"

## import all objects from main.py
from main import *

import nmeshlib as mesh
