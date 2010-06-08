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
           'ScalarNode',
           'DiffFieldNode', 'DiffNode', 'DiffIndexNode', 'BSpecsNode',
           'FieldNode', 'FieldIndexNode', 'FieldIndicesNode',
           'MiddleFieldNode', 'BraKetNode', 'SumSpecsNode', 'SumSpecNode',
           'SignSym']

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
        s_amendments = ""
        s_sums = ", %s" % str(sums) if sums != None else ""
        return s_contribs + s_amendments + s_sums

class ContribsNode(AssocNode):
    def simplify(self, context=None):
        # This node is responsible for expanding the operator string for all
        # the materials: transforming one expression in 'm' into many
        # expressions in 'm_material1', 'm_material2', etc.
        if context != None and context.material != None:
            material = context.material
            if isinstance(material, str):
                material_list = [material]
            else:
                material_list = material

            opstrings = self.children
            expanded_opstrings = ContribsNode()
            plus = SignSym(1.0)
            sign = SignSym()
            for material_name in material_list:
                context.material = material_name
                for opstring in opstrings:
                    simplified = opstring.simplify(context=context)
                    expanded_opstrings.add2(simplified, sign)
                    sign = plus

            context.material = material
            return expanded_opstrings

        else:
            return GenericNode.simplify(self, context=context)

class ContribNode(Node):
    fmt = minimal_list_formatter

class UContribNode(AssocNode):
    fmt = ListFormatter("", "", "*")

    def __str__(self):
        pieces = []
        for operator, operand in zip(self.data, self.children):
            if operator != None and not operator.is_one():
                pieces.append(str(operator))
            pieces.append(str(operand))
        return self.fmt.stringify(pieces)

class ScalarNode(Node):
    fmt = minimal_list_formatter

    def __str__(self):
        n = self.data[0]
        return repr(n) if n >= 0.0 else "(%s)" % repr(n)

    def is_one(self):
        return self.data[0] == 1.0

class DiffFieldNode(AssocNode):
    def __str__(self):
        if self.data == None:
            return str(self.children[0])
        else:
            return "%s %s" % (self.data, str(self.children[0]))

class DiffNode(AssocNode):
    fmt = minimal_list_formatter

class BSpecsNode(Node):
    fmt = minimal_list_formatter

class DiffIndexNode(Node):
    def __str__(self):
        return self.data[0]

class FieldNode(Node):
    def simplify(self, context=None):
        # We should go through the quantities, find this one and - if it is
        # constant - just simplify the tensor with such constant.
        # For example, if pi(i, j) is a tensor which turns out to be always
        # equal to 3.14, then we just have to substitute pi(i, j) -> 3.14
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

class FieldIndexNode(Node):
    def __str__(self):
        return self.data[0]

class FieldIndicesNode(Node):
    fmt = plain_list_formatter

class MiddleFieldNode(Node):
    fmt = minimal_list_formatter

class BraKetNode(Node):
    fmt = ListFormatter("<", ">", "|")

class SumSpecsNode(Node):
    fmt = plain_list_formatter

class SumSpecNode(Node):
    def __str__(self):
        return "%s:%s" % (self.data[0], self.data[1])
