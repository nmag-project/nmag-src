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

__all__ = ['OperatorNode', 'ContribsNode', 'ContribNode', 'UContribNode',
           'ScalarNode',
           'DiffFieldNode', 'DiffNode', 'DiffIndexNode', 'BSpecsNode',
           'FieldNode', 'FieldIndexNode', 'FieldIndicesNode',
           'MiddleFieldNode', 'BraKetNode', 'SumSpecsNode', 'SumSpecNode',
           'SignSym']

from tree import *
import collections

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

    def __init__(self, children=[None, None, None], data=[],
                 contribs=None, amendments=None, sums=None):
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
    pass

class ContribNode(Node):
    fmt = minimal_list_formatter

class UContribNode(AssocNode):
    fmt = ListFormatter("", "", "*")

class ScalarNode(Node):
    fmt = minimal_list_formatter

    def __str__(self):
        n = self.data[0]
        return repr(n) if n >= 0.0 else "(%s)" % repr(n)

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
