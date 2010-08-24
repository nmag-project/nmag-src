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

"""Provides a function to parse an operator string and return a tree
representation of the operator which can be used to simplify it,
examine the quantities involved and finally rewrite it as text."""

__all__ = ['OpSimplifyContext',
           'OperatorNode', 'ContribsNode', 'ContribNode', 'UContribNode',
           'ScalarNode', 'ScaledBraKetNode',
           'DiffFieldNode', 'DiffNode', 'DiffIndexNode', 'BSpecsNode',
           'FieldNode', 'FieldIndexNode', 'FieldIndicesNode',
           'MiddleFieldNode', 'BraKetNode', 'SumSpecsNode', 'SumSpecNode',
           'SignSym', 'RegionNode', 'RegLogSomeNode', 'RegLogAllNode',
           'RegLogNRegsNode', 'RegLogParenthesisNode', 'RegLogNotNode',
           'RegLogAndNode', 'RegLogOrNode', 'AmendSpecNode', 'DiagAmendNode',
           'AmendMxDimNode', 'OptFieldsNode', 'FieldsNode'
           ]

from tree import *
import collections

class OpSimplifyContext:
    def __init__(self, quantities=None, material=None):
        self.quantities = quantities
        self.material = material

default_simplify_context = OpSimplifyContext()

class SignSym(GenericSym):
    def __str__(self):
        v = self.value
        if v == 1.0:
            return "+"
        elif v == -1.0:
            return "-"
        else:
            assert v == None
            return ""

class Node(GenericNode):
    def __init__(self, children=[], data=[]):
        if type(data) == str:
            data = [data]
        elif isinstance(data, collections.Sequence):
            data = list(data)
        else:
            data = [data]
        GenericNode.__init__(self, children=children, data=data)

class OperatorNode(Node):
    fmt = minimal_list_formatter

    def __init__(self, children=None, data=[],
                 contribs=None, amendments=None, sums=None):
        if children == None:
            children = list([None, None, None])
            # ^^^ list() is necessary in order to get a new list for every
            #  call to the method.
        if contribs != None:
            children[0] = contribs
        if amendments != None:
            children[1] = amendments
        if sums != None:
            children[2] = sums
        Node.__init__(self, children=children, data=data)

    def __str__(self):
        contribs, amendments, sums = self.children
        s_contribs = str(contribs)
        s_amendments = str(amendments)
        s_sums = ", %s" % str(sums) if sums != None else ""
        return s_contribs + s_amendments + s_sums

    def is_zero(self):
        contribs = self.children[0]
        return contribs.is_zero()

class ContribsNode(AssocNode):
    def simplify(self, context=None):
        # This node is responsible for expanding the operator string for all
        # the materials: transforming one expression in 'm' into many
        # expressions in 'm_material1', 'm_material2', etc.
        material_list = [""]
        set_material = lambda mn: None
        material = None
        if context != None and context.material != None:
            material = context.material
            if isinstance(material, str):
                material_list = [material]
            else:
                material_list = material

            def set_material(mn):
                context.material = mn

        opstrings = self.children
        expanded_opstrings = ContribsNode()
        plus = SignSym(1.0)
        sign = SignSym()
        for material_name in material_list:
            set_material(material_name)

            for opstring in opstrings:
                simplified = opstring.simplify(context=context)
                if not simplified.is_zero():
                    expanded_opstrings.add2(simplified, sign)
                    sign = plus

        set_material(material)
        return expanded_opstrings

    def is_zero(self):
        for child in self.children:
            if not child.is_zero():
                return False
        return True

class ContribNode(Node):
    fmt = minimal_list_formatter

    def is_zero(self):
        return self.children[0].is_zero()

    def __str__(self):
        return str(self.data[0]) + str(self.children[0])

class UContribNode(AssocNode):
    fmt = ListFormatter("", "", "*")

    def __str__(self):
        pieces = []
        for term in self.children:
            braket, factor = term.children
            if factor != None and not factor.is_one():
                pieces.append(str(factor))
            pieces.append(str(braket))
        return self.fmt.stringify(pieces)

    def is_zero(self):
        scaled_braket = self.children[0]
        scale_factor = scaled_braket.children[1]
        return scale_factor.is_zero()

class ScalarNode(Node):
    fmt = minimal_list_formatter

    def simplify(self, context=None):
        # We should go through the quantities, find this one and - if it is
        # constant - just simplify the tensor with such constant.
        # For example, if pi(i, j) is a tensor which turns out to be always
        # equal to 3.14, then we just have to substitute pi(i, j) -> 3.14
        v = self.data[0]
        if type(v) == str and context != None:
            q = context.quantities.get(v)
            if q.is_constant():
                #assert self.children == [None], \
                #  "Prefactors of <...> should be scalars, not vectors!"
                if q.is_constant():
                    return ScalarNode(data=q.as_float(context.material))

                elif (context.material != None
                      and q.is_defined_on_material(context.material)):
                    field_node = Node.simplify(self, context=context)
                    field_node.data[0] += "_%s" % context.material
                    return field_node

        # Keep it as a it is
        return Node.simplify(self, context=context)

    def __str__(self):
        n = self.data[0]
        if type(n) == str:
            return n
        else:
            return repr(n) if n >= 0.0 else "(%s)" % repr(n)

    def is_one(self):
        return self.data[0] == 1.0

    def is_zero(self):
        return self.data[0] == 0.0

class DiffFieldNode(AssocNode):
    def __str__(self):
        if self.data == None:
            return str(self.children[0])
        else:
            return "%s %s" % (self.data, str(self.children[0]))

class DiffNode(AssocNode):
    fmt = minimal_list_formatter

class BSpecsNode(Node):
    def __str__(self):
        return "[%s]" % self.children[0] if len(self.children) > 0 else ""

class DiffIndexNode(Node):
    def __str__(self):
        return self.data[0]

class FieldNode(Node):
    def __init__(self, children=[], data=[]):
        Node.__init__(self, children=children, data=data)

        if len(self.data) == 1:
            n = self.data[0]
            self.data = [n, n]
            #             ^^^ the first gets decorated with the material name,
            #             (example: m --> m_Py), the second doesn't change

    def simplify(self, context=None):
        if context != None:
            q = context.quantities.get(self.data[0])
            assert not q.is_constant(), \
              "Constant Quantities cannot appear inside <...>."
            if (context.material != None
                and q.is_defined_on_material(context.material)):
                field_node = Node.simplify(self, context=context)
                field_node.data[0] += "_%s" % context.material
                return field_node

        return Node.simplify(self, context=context)

    def __str__(self):
        bspecs, indices = self.children
        if indices != None:
            return "%s%s(%s)" % (self.data[0], bspecs, indices)
        else:
            return self.data[0] + str(bspecs)

    def _collect_quantities(self, collections, parsing):
        print self.data
        #raw_input()
        qn = self.data[1]
        collections[parsing][qn] = True

class FieldIndexNode(Node):
    def __str__(self):
        return self.data[0]

class FieldIndicesNode(Node):
    fmt = plain_list_formatter

class MiddleFieldNode(Node):
    fmt = minimal_list_formatter

class ScaledBraKetNode(Node):
    fmt = minimal_list_formatter

class BraKetNode(Node):
    fmt = ListFormatter("<", ">", "|")

    def _collect_quantities(self, collections, parsing):
        print self.children
        assert len(self.children) == 3
        self.children[0]._collect_quantities(collections, "outputs")
        self.children[1]._collect_quantities(collections, "inputs")
        self.children[2]._collect_quantities(collections, "inputs")

class SumSpecsNode(Node):
    fmt = plain_list_formatter

class SumSpecNode(Node):
    def __str__(self):
        return "%s:%s" % (self.data[0], self.data[1])

class RegionLogicNode(Node):
    pass

class RegionNode(RegionLogicNode):
    def __str__(self):
        return str(self.data[0])

class RegLogSomeNode(RegionLogicNode):
    fmt = plain_list_formatter

class RegLogAllNode(RegionLogicNode):
    fmt = ListFormatter("all=", "", "")

class RegLogNRegsNode(RegionLogicNode):
    fmt = ListFormatter("#=", "", "")

class RegLogParenthesisNode(RegionLogicNode):
    fmt = ListFormatter("(", ")", "")

class RegLogNotNode(RegionLogicNode):
    fmt = ListFormatter("not ", "", "")

class RegLogAndNode(RegionLogicNode):
    fmt = ListFormatter("", "", " and ")

class RegLogOrNode(RegionLogicNode):
    fmt = ListFormatter("", "", " or ")

class AmendSpecNode(Node):
    fmt = ListFormatter("; ", "", "; ")
    def __str__(self):
        if len(self.children) == 0:
            return ""
        else:
            return Node.__str__(self)

class DiagAmendNode(Node):
    fmt = ListFormatter("", "", "=")

class AmendMxDimNode(Node):
    fmt = ListFormatter("(L||R)=(", ")", "||")

class OptFieldsNode(Node):
    def __str__(self):
        if len(self.children) == 0:
            return "*"
        else:
            return str(self.children[0])

class FieldsNode(Node):
    fmt = plain_list_formatter

