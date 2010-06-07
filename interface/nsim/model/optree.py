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
           'DiffFieldNode', 'DiffNode', 'DiffIndexNode', 'BSpecsNode',
           'FieldNode', 'BraKetNode', 'SignSym']

from tree import *

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
        GenericNode.__init__(self, children=children, data=data)

class OperatorNode(Node):
    fmt = minimal_list_formatter

    def __init__(self, children=[], data=[],
                 contribs=None, amendments=None, sums=None):
        children.extend(filter(None, [contribs, amendments, sums]))
        Node.__init__(self, children=children, data=data)

class ContribsNode(AssocNode):
    pass

class ContribNode(Node):
    pass

class UContribNode(AssocNode):
    fmt = minimal_list_formatter

    def __init__(self, children=[], data=[], prefactor=1.0):
        if prefactor:
            data = [prefactor]
        AssocNode.__init__(self, children=children, data=data)

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
            return "%s%s(%s)" % (self.data, bspecs, indices)
        else:
            return self.data[0] + str(bspecs)

class BraKetNode(Node):
    fmt = ListFormatter("<", ">", "|")
