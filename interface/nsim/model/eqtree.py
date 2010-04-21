
__all__ = ['LocalEqnNode', 'LocalAndRangeDefsNode',
           'NumTensorNode', 'NumTensorsNode', 'IntsNode',
           'IxRangeNode', 'AssignmentNode',
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

default_list_formatter = ListFormatter()
plain_list_formatter = ListFormatter("", "", ", ")

class Node:
    def __init__(self, node_type, children=[], data=None):
        self.node_type = node_type
        self.children = children
        self.data = data

    def __str__(self):
        children_str = ", ".join([str(c) for c in self.children])
        return "%s(%s)" % (self.node_type, children_str)

    def __repr__(self):
        return self.__str__()

    def simplify(self, quantities):
        return self

class UnaryNode(Node):
    def __init__(self, node_type, children=[], data=None):
        Node.__init__(self, node_type, children=children, data=data)
        self.fmt1 = plain_list_formatter

    def __str__(self):
        return self.fmt1.stringify([self.data])

class ListNode(Node):
    def __init__(self, node_type, copy_from=None, num_lists=1, data=None):
        if copy_from != None:
           assert copy_from.__class__ == self.__class__
           children = list(copy_from.children)
        else:
            children = [[] for _ in range(num_lists)]
        Node.__init__(self, node_type, children=children, data=data)
        self.fmt1 = default_list_formatter
        self.fmt2 = default_list_formatter

    def add(self, l, list_idx=0):
        self.children[list_idx].append(l)
        return self

    def __str__(self):
        fmt1, fmt2 = (self.fmt1, self.fmt2)
        if len(self.children) == 1:
            return fmt1.stringify(self.children[0])
        else:
            return fmt2.stringify([fmt1.stringify(c) for c in self.children])

class AssocOpNode(ListNode):
    def __str__(self):
        s = ""
        for op, term in self.children[0]:
            s += "%s%s" % (self.ops[op], term)
        return s

class LocalEqnNode(Node):
    def __init__(self, local_and_range_defs=None, assignments=None):
        Node.__init__(self, "LocalEqn",
                      (local_and_range_defs, assignments))

    def __str__(self):
        return "%s\n%s" % (self.children[0], self.children[1])

class LocalAndRangeDefsNode(ListNode):
    def __init__(self, copy_from=None):
        ListNode.__init__(self, "LocalAndRangeDefs",
                          copy_from=copy_from, num_lists=2)
        self.fmt1 = ListFormatter("", "; ", ", ")
        self.fmt2 = ListFormatter("", "; ", ", ")

    def add_local(self, l):
        return self.add(l, list_idx=0)

    def add_range(self, r):
        return self.add(r, list_idx=1)

    def __str__(self):
        s = ""
        for l in self.children[0]:
            s += "%local " + self.fmt1.stringify(self.children[0])
        for r in self.children[1]:
            s += "%range " + self.fmt2.stringify(self.children[1])
        return s

class NumTensorNode(Node):
    def __init__(self, name, indices=None):
        Node.__init__(self, "NumTensor", (name, indices))

class NumTensorsNode(ListNode):
    def __init__(self, copy_from=None):
        ListNode.__init__(self, "NumTensors",  copy_from=copy_from)

class IntsNode(ListNode):
    def __init__(self, copy_from=None):
        ListNode.__init__(self, "Ints", copy_from=copy_from)

class IxRangeNode(ListNode):
    def __init__(self, copy_from=None):
        ListNode.__init__(self, "IxRange", copy_from=copy_from)
        self.fmt1 = ListFormatter("", "", ", ")

    def __str__(self):
        return self.fmt1.stringify(["%s:%s" % ir for ir in self.children[0]])

class AssignmentNode(ListNode):
    def __init__(self, copy_from=None):
        ListNode.__init__(self, "Assignment", copy_from=copy_from)
        self.fmt1 = ListFormatter("", "", "; ")

    def __str__(self):
        return self.fmt1.stringify(["%s <- %s;" % (l, r)
                                    for l, r in self.children[0]])

class NumIndexNode(UnaryNode):
    def __init__(self, value):
        UnaryNode.__init__(self, "NumIndex", data=value)

class VarIndexNode(UnaryNode):
    def __init__(self, value):
        UnaryNode.__init__(self, "VarIndex", data=value)

class IndicesNode(ListNode):
    def __init__(self, copy_from=None):
        ListNode.__init__(self, "Indices", copy_from=copy_from)
        self.fmt1 = ListFormatter("", "", ", ")

class TensorSumNode(AssocOpNode):
    def __init__(self, copy_from=None):
        ListNode.__init__(self, "TensorSum", copy_from=copy_from)
        self.fmt1 = ListFormatter("", "", " + ")
        self.ops = {None: '', 1.0: ' + ', -1.0: ' - '}

class TensorProductNode(AssocOpNode):
    def __init__(self, copy_from=None):
        ListNode.__init__(self, "TensorProduct", copy_from=copy_from)
        self.ops = {None: '', '*': '*', '/': '/'}

class SignedTensorAtomNode(Node):
    def __init__(self, value, sign=1.0):
        Node.__init__(self, "SignedTensorAtom", [value], data=float(sign))

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
    def __init__(self, value):
        UnaryNode.__init__(self, "Number", data=float(value))

class ParenthesisNode(Node):
    def __init__(self, content):
        Node.__init__(self, "Parenthesis", (content,))

    def __str__(self):
        return "(%s)" % self.children[0]

class TensorNode(Node):
    def __init__(self, name, arg=None):
        Node.__init__(self, "Tensor", (name, arg))

    def __str__(self):
        if self.children[1] == None:
            return self.children[0]
        else:
            return "%s(%s)" % self.children

class FunctionNode(Node):
    def __init__(self, name, arg=None):
        Node.__init__(self, "Function", (name, arg))

    def __str__(self):
        if self.children[1] == None:
            return self.children[0]
        else:
            return "%s[%s]" % self.children
