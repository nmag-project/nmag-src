from quantity import *
from physics import *
from optree import *

import nmesh

from nmag import MagMaterial

# Constants
damping_const = Constant("alpha", def_on_material=True)
negJ = Constant("negJ", def_on_material=True)
M_sat = Constant("M_sat", def_on_material=True)

# All fields
m = SpaceField("m", [3], def_on_material=True)
H_ext = SpaceField("H_ext", [3])
E_total = SpaceField("E_total", [], def_on_material=True)
rho = SpaceField("rho", [])
dmdt = SpaceField("dmdt", [3], def_on_material=True)
M = SpaceField("M", [3], def_on_material=True)
H_exch = SpaceField("H_exch", [3], def_on_material=True)
H_anis = SpaceField("H_anis", [3], def_on_material=True)
H_total = SpaceField("H_total", [3], def_on_material=True)
E_demag = SpaceField("E_demag", [], def_on_material=True)
E_ext = SpaceField("E_ext", [], def_on_material=True)
E_anis = SpaceField("E_anis", [], def_on_material=True)

tree = \
  OverMatNode(
    SumNode(MulNode(QuantityNode(M_sat),
                    UnparsedNode("<${rho}||d/dxj ${m(j)}>")),
            MulNode(QuantityNode(M_sat),
                    UnparsedNode("<${rho}||D/Dxj ${m(j)}>"))))
oc = OperatorContext(tree, inputs=[rho], outputs=[m])
print "quants:", tree.get_used_quants({})
print "str:", oc.to_str()
print


print "Simplifying"
simp_tree = tree.simplify()
oc = OperatorContext(simp_tree, inputs=[rho], outputs=[m])
print "quants:", simp_tree.get_used_quants({})
print "str:", oc.to_str()
print

tree = OverMatNode(
       MulNode(QuantityNode(negJ),
               UnparsedNode("<d/dxj ${H_exch(k)}||d/dxj ${m(k)}>")))
oc = OperatorContext(tree, inputs=[m], outputs=[H_exch])
print "quants:", tree.get_used_quants({})
print "str:", oc.to_str()