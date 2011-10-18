""" Python-defined Example Op-Codes for pyddpla

(C) 2009 Dr. Thomas Fischbacher

"""

# Note: curiously, "import ocaml" seems to be important here:
# Otherwise, ocaml functions in opcodes are not handled properly...

import numpy, sys,ocaml
#import numpy, sys

def op_greeting(message):
    """PYDDPLA op-code PY-GREETING(:MESSAGE)"""
    print ("[PYDDPLA-OPCODE] Greeting=%s\n" % repr(message)),
    sys.stdout.flush()

def op_inspect_msvec(tag,v):
    """PYDDPLA op-code PY-INSPECT-MSVEC(:TAG,:VEC)"""

    print "===INSPECTING VEC ==="
    def fun_show(array):
        fmt_str = "%%s %%%dd: %%f"%len(str(array.size))
        for i in range(array.size):
            print fmt_str%(tag,i,array[i])
        sys.stdout.flush()
            
    ocaml.ddpla_pyeval_on_drh_msvec(fun_show,v)
    
