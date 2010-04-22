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

    def is_float(self, quantities=None):
        '''Return True when the method 'as_float' can be used successfully.'''
        return False

    def as_float(self, quantities=None):
        '''If the node is a constant floating point number, then return its
        value, otherwise raise an exception.'''
        raise ValueError("%s node cannot be converted to float."
                         % self.node_type)

    def is_zero(self, quantities=None):
        '''Whether the node is constantly and uniformly equal to zero.
        Used only in optimisations (it is safe to return always False).'''
        return False

    def is_one(self, quantities=None):
        '''Whether the node is constantly and uniformly equal to one.
        Used only in optimisations (it is safe to return always False).'''
        return False

    def simplify(self, quantities=None):
        '''Simplify the parse tree using algebraic rules such as
        0*(...) -> 0 and similar ones.'''
        def simplify(c):
            if c == None:
                return None
            else:
                return c.simplify(quantities)

        simplified_children = [simplify(c) for c in self.children]
        new_obj = self.__class__()
        new_obj.children = simplified_children
        new_obj.data = self.data
        return new_obj

class UnaryNode(Node):
    node_type = "UnaryNode"
    fmt = plain_list_formatter

    def __init__(self, data=[], children=[]):
        Node.__init__(self, children=children, data=data)

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

    def simplify(self, quantities=None):
        # First let's look for zeros in the factors of the multiplication
        # If one of the factors is zero, then we just return the float 0.0
        # We otherwise include the factor in the simplified one, but only if
        # it is not one.
        new_children = []
        new_data = []
        constant_factor = 1.0
        for factor, op in zip(self.children, self.data):
            new_factor = factor.simplify(quantities=quantities)
            if new_factor.is_zero(quantities=quantities):
                if op == '/':
                    raise ValueError("Found division by zero during "
                                     "simplification")
                return FloatNode(0.0)

            elif new_factor.is_float(quantities=quantities):
                v = new_factor.as_float(quantities=quantities)
                if op in ['*', None]:
                    constant_factor *= v
                else:
                    assert op == '/'
                    constant_factor /= v

            elif not new_factor.is_one(quantities=quantities):
                new_children.append(new_factor)
                new_data.append(op)

        insert_factor = (constant_factor != 1.0)
        # We can omit the factor when it is one and is followed by a
        # multiplicative term
        omit_prefactor = (constant_factor == 1.0
                          and len(new_data) >= 1
                          and new_data[0] in [None, '*'])
        if omit_prefactor:
            new_data[0] = None

        else:
            new_children.insert(0, FloatNode(constant_factor))
            new_data.insert(0, None)

        assert(len(new_data) == len(new_children))
        new_obj = TensorProductNode()
        new_obj.children = new_children
        new_obj.data = new_data
        return new_obj

class SignedTensorAtomNode(Node):
    node_type = "SignedTensorAtom"

    def __init__(self, value=None, sign=1.0):
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

    def is_float(self, quantities=None):
        return self.children[0].is_float(quantities=quantities)

    def as_float(self, quantities=None):
        return self.data*self.children[0].as_float(quantities=quantities)

    def is_zero(self, quantities=None):
        return self.children[0].is_zero(quantities=quantities)

    def is_one(self, quantities=None):
        return (self.data == 1.0
                and self.children[0].is_one(quantities=quantities))

class FloatNode(UnaryNode):
    node_type = "Number"

    def __init__(self, value=0.0):
        UnaryNode.__init__(self, float(value))

    def is_zero(self, quantities=None):
        return self.data == 0.0

    def is_one(self, quantities=None):
        return self.data == 1.0

    def is_float(self, quantities=None):
        return True

    def as_float(self, quantities=None):
        return self.data

class ParenthesisNode(ListNode):
    node_type = "Parenthesis"
    fmt = default_list_formatter

class TensorNode(Node):
    node_type = "Tensor"

    def __init__(self, name="?", arg=None):
        Node.__init__(self, arg, data=name)

    def __str__(self):
        if self.children[0] == None:
            return self.data[0]
        else:
            return "%s(%s)" % (self.data, self.children[0])

class FunctionNode(Node):
    node_type = "Function"

    def __init__(self, name="?", arg=None):
        Node.__init__(self, arg, data=name)

    def __str__(self):
        if self.children[0] == None:
            return self.data[0]
        else:
            return "%s[%s]" % (self.data, self.children[0])
