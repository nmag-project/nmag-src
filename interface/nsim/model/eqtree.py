
__all__ = ['LocalEqnNode', 'LocalAndRangeDefsNode',
           'NumTensorNode', 'NumTensorsNode', 'IntsNode',
           'IxRangeNode', 'AssignmentNode',
           'NumIndexNode', 'VarIndexNode', 'IndicesNode',
           'TensorSumNode', 'TensorProductNode',
           'SignedTensorAtomNode',
           'ParenthesisNode', 'TensorNode', 'FunctionNode']

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

class ListNode(Node):
    def __init__(self, node_type, copy_from=None, num_lists=1, data=None):
        if copy_from != None:
            assert copy_from.__class__ == self.__class__
            children = list(copy_from.children)
        else:
            children = [[] for _ in range(num_lists)]
        Node.__init__(self, node_type, children=children, data=data)

    def add(self, l, list_idx=0):
        self.children[list_idx].append(l)
        return self

class LocalEqnNode(Node):
    def __init__(self, local_and_range_defs=None, assignments=None):
        Node.__init__(self, "LocalEqn",
                      (local_and_range_defs, assignments))

class LocalAndRangeDefsNode(ListNode):
    def __init__(self, copy_from=None):
        ListNode.__init__(self, "LocalAndRangeDefs",
                          copy_from=copy_from, num_lists=2)

    def add_local(self, l):
        return self.add(l, list_idx=0)

    def add_range(self, r):
        return self.add(r, list_idx=1)

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

class AssignmentNode(ListNode):
    def __init__(self, copy_from=None):
        ListNode.__init__(self, "Assignment", copy_from=copy_from)

class NumIndexNode(Node):
    def __init__(self, value):
        Node.__init__(self, "NumIndex", value)

class VarIndexNode(Node):
    def __init__(self, value):
        Node.__init__(self, "VarIndex", value)

class IndicesNode(ListNode):
    def __init__(self, copy_from=None):
        ListNode.__init__(self, "Indices", copy_from=copy_from)


class TensorSumNode(ListNode):
    def __init__(self, copy_from=None):
        ListNode.__init__(self, "TensorSum", copy_from=copy_from)

class TensorProductNode(ListNode):
    def __init__(self, copy_from=None):
        ListNode.__init__(self, "TensorProduct", copy_from=copy_from)

class SignedTensorAtomNode(Node):
    def __init__(self, value, sign=1.0):
        Node.__init__(self, "SignedTensorAtom", (value,), data=float(sign))

    def sign(self, s):
        self.data *= s

class FloatNode(Node):
    def __init__(self, value):
        Node.__init__(self, "Number", data=float(value))

class ParenthesisNode(Node):
    def __init__(self, content):
        Node.__init__(self, "Parenthesis", (content,))

class TensorNode(Node):
    def __init__(self, name, arg=None):
        Node.__init__(self, "Tensor", (name, arg))

class FunctionNode(Node):
    def __init__(self, name, arg=None):
        Node.__init__(self, "Function", (name, arg))
