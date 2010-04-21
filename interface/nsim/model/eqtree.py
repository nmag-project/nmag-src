import collections, types

__all__ = ['LocalEqnNode', 'LocalAndRangeDefsNode',
           'NumTensorNode', 'NumTensorsNode', 'IntsNode',
           'IxRangeNode', 'AssignmentNode', 'AssignmentsNode',
           'NumIndexNode', 'VarIndexNode', 'IndicesNode',
           'TensorSumNode', 'TensorProductNode',
           'SignedTensorAtomNode',
           'FloatNode', 'ParenthesisNode', 'TensorNode', 'FunctionNode']

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

class Node:
    '''Generic class for a node of the local_eqn parser.
    All the derived classes have to agree on one convention:
    self.children should be just a list of objects of type Node or of any
    other derived class. Alternatively they can just be None.
    All non-Node objects should go into self.data.
    This allows to easily write routines to go throughout the tree.
    '''

    node_type = "Node"
    fmt = ListFormatter()
    prefix_in_str = True

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

    def debug(self):
        data = self.data
        children = self.children
        if data == None:
            data = []
        if children == None:
            children = []
        print "INSPECTING", self.node_type
        raw_input()
        print ("%s has %d nodes and %d data items"
               % (self.node_type,  len(children), len(data)))
        for i, c in enumerate(children):
            if c == None:
                print "CHILD %d: is None" % i

            else:
                print "CHILD %d: %s" % (i, c.node_type)

        print
        for i, c in enumerate(children):
            if c == None:
                continue

            try:
                print "CHILD %d: type %s" % (i, str(type(c)))
                c.debug()
            except:
                print "Error: back to", self.node_type

    def add(self, l):
        self.children.append(l)
        return self

    def add2(self, l, d):
        self.children.append(l)
        if self.data == None:
            self.data = [d]
        else:
            self.data.append(d)
        return self

    def is_zero(self, quantities=None):
        return False

    def is_one(self, quantities=None):
        return False

    def simplify(self, quantities=None):
        def simplify(c):
            if c == None:
                return None
            else:
                return c.simplify(quantities)

        simplified_children = [simplify(c) for c in self.children]
        return self.__class__(simplified_children, self.data)

class UnaryNode(Node):
    node_type = "UnaryNode"
    fmt = plain_list_formatter

    def __init__(self, value):
        Node.__init__(self, data=value)

    def __str__(self):
        return self.fmt.stringify([self.data])

class ListNode(Node):
    node_type = "ListNode"
    prefix_in_str = False

class AssocOpNode(ListNode):
    node_type = "AssocOpNode"
    ops = {}

    def __str__(self):
        s = ""
        for op, term in zip(self.data, self.children):
            s += "%s%s" % (self.ops[op], term)
        return s

class LocalEqnNode(ListNode):
    fmt = ListFormatter("", "", "\n")
    node_type = "LocalEqn"

class LocalAndRangeDefsNode(ListNode):
    node_type = "LocalAndRangeDefs"

    def __init__(self):
        ListNode.__init__(self)
        self.children = [ListNode(), ListNode()]
        self.children[0].fmt = self.children[1].fmt = \
          ListFormatter("", "; ", ", ")

    def add_local(self, l):
        self.children[0].add(l)
        return self

    def add_range(self, r):
        self.children[1].add(r)
        return self

    def __str__(self):
        s = ""
        ls, rs = self.children
        for l in ls.children:
            s += "%%local %s; " % l
        for r in rs.children:
            s += "%%range %s; " % r
        return s

class NumTensorNode(ListNode):
    node_type = "NumTensor"

    def __str__(self):
        return "%s(%s)" % (self.data, self.children[0])

class NumTensorsNode(ListNode):
    node_type = "NumTensors"
    fmt = plain_list_formatter

class IntsNode(ListNode):
    node_type = "Ints"
    fmt = plain_list_formatter

class IxRangeNode(ListNode):
    node_type = "IxRange"
    fmt = plain_list_formatter

    def __str__(self):
        return self.fmt.stringify(["%s:%s" % ir
                                   for ir in self.data])

class AssignmentNode(ListNode):
    node_type = "Assignment"
    fmt = ListFormatter("", ";", " <- ")

class AssignmentsNode(ListNode):
    node_type = "Assignments"
    fmt = minimal_list_formatter

class NumIndexNode(UnaryNode):
    node_type = "NumIndex"

class VarIndexNode(UnaryNode):
    node_type = "VarIndex"

class IndicesNode(ListNode):
    node_type = "Indices"
    fmt = plain_list_formatter

class TensorSumNode(AssocOpNode):
    node_type = "TensorSum"
    fmt = ListFormatter("", "", " + ")
    ops = {None: '', 1.0: ' + ', -1.0: ' - '}

class TensorProductNode(AssocOpNode):
    node_type = "TensorProduct"
    ops = {None: '', '*': '*', '/': '/'}

class SignedTensorAtomNode(Node):
    node_type = "SignedTensorAtom"

    def __init__(self, value, sign=1.0):
        Node.__init__(self, value, data=float(sign))

    def sign(self, s):
        self.data *= s
        return self

    def __str__(self):
        if self.data == 1.0:
            return str(self.children[0])
        else:
            assert self.data == -1.0
            return "-%s" % self.children[0]

class FloatNode(UnaryNode):
    node_type = "Number"

    def __init__(self, value):
        UnaryNode.__init__(self, float(value))

class ParenthesisNode(Node):
    node_type = "Parenthesis"

    def __init__(self, content):
        Node.__init__(self, (content,))

    def __str__(self):
        return "(%s)" % self.children[0]

class TensorNode(Node):
    node_type = "Tensor"

    def __init__(self, name, arg=None):
        Node.__init__(self, arg, data=name)

    def __str__(self):
        if self.children[0] == None:
            return self.data[0]
        else:
            return "%s(%s)" % (self.data, self.children[0])

class FunctionNode(Node):
    node_type = "Function"

    def __init__(self, name, arg=None):
        Node.__init__(self, arg, data=name)

    def __str__(self):
        if self.children[0] == None:
            return self.data[0]
        else:
            return "%s[%s]" % (self.data, self.children[0])
