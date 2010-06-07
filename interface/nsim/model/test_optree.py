# Nmag micromagnetic simulator
# Copyright (C) 2010 University of Southampton
# Hans Fangohr, Thomas Fischbacher, Matteo Franchin and others
#
# WEB:     http://nmag.soton.ac.uk
# CONTACT: nmag@soton.ac.uk
#
# AUTHOR(S) OF THIS FILE: Matteo Franchin
# LICENSE: GNU General Public License 2.0
#          (see <http://www.gnu.org/licenses/>)

from quantity import *
from model import *
from optree_old import *
from computation import Operator

import nmesh

from nmag import MagMaterial

# Constants
damping_const = Constant("alpha", subfields=True)
negJ = Constant("negJ", subfields=True)
M_sat = Constant("M_sat", subfields=True)

# All fields
m = SpaceField("m", [3], subfields=True)
H_ext = SpaceField("H_ext", [3])
E_total = SpaceField("E_total", [], subfields=True)
rho = SpaceField("rho", [])
dmdt = SpaceField("dmdt", [3], subfields=True)
M = SpaceField("M", [3], subfields=True)
H_exch = SpaceField("H_exch", [3], subfields=True)
H_anis = SpaceField("H_anis", [3], subfields=True)
H_total = SpaceField("H_total", [3], subfields=True)
E_demag = SpaceField("E_demag", [], subfields=True)
E_ext = SpaceField("E_ext", [], subfields=True)
E_anis = SpaceField("E_anis", [], subfields=True)

#tree = \
  #OverMatNode(
    #SumNode(MulNode(QuantityNode(M_sat),
                    #UnparsedNode("<${rho}||d/dxj ${m(j)}>")),
            #MulNode(QuantityNode(M_sat),
                    #UnparsedNode("<${rho}||D/Dxj ${m(j)}>"))))
#oc = OperatorContext(tree, inputs=[rho], outputs=[m])
#print "quants:", tree.get_used_quants({})
#print "str:", oc.to_str()
#print

op_div_m = \
  Operator("div_m", "(1.23)*<rho||d/dxj m(j)> + (4.56)*<rho||D/Dxj m(j)>, j:3")

print str(op_div_m.operator_tree)

#print "Simplifying"
#simp_tree = tree.simplify()
#oc = OperatorContext(simp_tree, inputs=[rho], outputs=[m])
#print "quants:", simp_tree.get_used_quants({})
#print "str:", oc.to_str()
#print

#tree = OverMatNode(
       #MulNode(QuantityNode(negJ),
               #UnparsedNode("<d/dxj ${H_exch(k)}||d/dxj ${m(k)}>")))
#oc = OperatorContext(tree, inputs=[m], outputs=[H_exch])
#print "quants:", tree.get_used_quants({})
#print "str:", oc.to_str()
