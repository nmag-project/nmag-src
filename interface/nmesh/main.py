from __future__ import division

import sys

try:
    import ocaml
except ImportError,msg:
    error = "We can't import the ocaml package. This usually means that you\n"
    error +="are running this code in a normal python interpreter, not within\n"
    error +="the nmesh or nsim executable.\n\n"
    raise ImportError,error+str(msg)

#provides logging setup 
import nbase

nbase.configure()

log = nbase.logging.getLogger('nmesh')


try:
    import nsim
except ImportError,msg:
    error = "Can't import nsim library\n"
    raise ImportError,error+str(msg)

globalfeatures = nsim.features.Features()
globalfeatures.set('etc','runid',nsim.snippets.get_runid(sys.argv))




