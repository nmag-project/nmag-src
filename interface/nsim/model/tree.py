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

import collections, copy

__all__ = ["ListFormatter", "default_list_formatter", "plain_list_formatter",
           "minimal_list_formatter", "spaced_list_formatter",
           "GenericSym",
           "GenericNode", "AssocNode"]

class ListFormatter:
    def __init__(self, open_str="[", close_str="]", separator=", "):
        self.open_str = open_str
        self.close_str = close_str
        self.separator = separator

    def stringify(self, l):
        if l == None:
            return ""
        return (self.open_str
                + self.separator.join([str(item) for item in l])
                + self.close_str)

default_list_formatter = ListFormatter("(", ")", ", ")
plain_list_formatter = ListFormatter("", "", ", ")
minimal_list_formatter = ListFormatter("", "", "")
spaced_list_formatter = ListFormatter("", "", " ")

class GenericSym(object):
    def __init__(self, value=None):
        self.value = value

class GenericNode(object):
    """Generic class for a node of the diffop parser.
    All the derived classes have to agree on one convention:
    self.children should be just a list of objects of type Node or of any
    other derived class. Alternatively they can just be None.
    All non-Node objects should go into self.data.
    This allows to easily write routines to go throughout the tree.
    """

    node_type = "Node"
    fmt = ListFormatter()
    prefix_in_str = False

    def __init__(self, children=[], data=[]):
        if not isinstance(children, collections.Sequence):
            children = [children]
        else:
            children = list(children)
        self.children = children
        if not type(data) == str and isinstance(data, collections.Sequence):
            data = list(data)
        self.data = data

    def __str__(self):
        if self.prefix_in_str:
            return self.node_type + self.fmt.stringify(self.children)
        else:
            return self.fmt.stringify(self.children)

    def add(self, l):
        """Add l as one subnode."""
        self.children.append(l)
        return selfopt_sum_specs

    def add2(self, l, d):
        """Add one subnode l with the associated data d."""
        self.children.append(l)
        if self.data == None:
            self.data = [d]
        else:
            self.data.append(d)
        return self

    def get_inner_item(self):
        """If the container has just one item, return that, otherwise return
        None."""
        return None

    def is_float(self, context=None):
        """Return True when the method 'as_float' can be used successfully."""
        return False

    def as_float(self, context=None):
        """If the node is a constant floating point number, then return its
        value, otherwise raise an exception."""
        raise ValueError("%s node cannot be converted to float."
                         % self.node_type)

    def is_zero(self, context=None):
        """Whether the node is constantly and uniformly equal to zero.
        Used only in optimisations (it is safe to return always False)."""
        return False

    def is_one(self, context=None):
        """Whether the node is constantly and uniformly equal to one.
        Used only in optimisations (it is safe to return always False)."""
        return False

    def simplify(self, context=None):
        """Simplify the parse tree using algebraic rules such as
        0*(...) -> 0 and similar ones."""
        def simplify(c):
            if c == None:
                return None
            else:
                return c.simplify(context=context)

        simplified_children = [simplify(c) for c in self.children]
        new_obj = self.__class__()
        new_obj.children = simplified_children
        new_obj.data = copy.copy(self.data)
        return new_obj

    def get_inputs_and_outputs(self):
        """Traverse the parse tree to collect inputs and outputs
        quantities."""
        inputs = []
        outputs = []
        self._collect_quantities(inputs, outputs, parsing='root')
        return (inputs, outputs)

    def _collect_quantities(self, inputs, outputs, parsing):
        for child in self.children:
            if child != None:
                child._collect_quantities(inputs, outputs, parsing)

class AssocNode(GenericNode):
    fmt = spaced_list_formatter

    def __str__(self):
        pieces = []
        for operator, operand in zip(self.data, self.children):
            if operator != None:
                pieces.append(str(operator))
            pieces.append(str(operand))
        return self.fmt.stringify(pieces)
