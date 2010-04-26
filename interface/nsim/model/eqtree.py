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

__all__ = ['LocalEqnNode', 'LocalAndRangeDefsNode',
           'NumTensorNode', 'NumTensorsNode', 'IntsNode',
           'IxRangeNode', 'AssignmentNode', 'AssignmentsNode',
           'NumIndexNode', 'VarIndexNode', 'IndicesNode',
           'TensorSumNode', 'TensorProductNode',
           'SignedTensorAtomNode',
           'FloatNode', 'ParenthesisNode', 'TensorNode', 'FunctionNode']

import collections, types, copy

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

class SimplifyContext:
    def __init__(self, quantities=None, material=None):
        self.quantities = quantities
        self.material = material
        self.simplify_tensors = True
        self.simplify_parentheses = True
        self.simplify_sums = True
        self.simplify_products = True

default_simplify_context = SimplifyContext()

def _sc(context):
    """If a simplify-context is None, then the default context should be used.
    This function facilitate the implementation of such a concept. Let's see
    how:

      x = _sc(context).simplify_tensors

    replaces:

      if context == None:
          x = default_simplify_context.simplify_tensors
      else:
          x = context.simplify_tensors
    """
    if context != None:
        return context
    else:
        return default_simplify_context

class Node:
    """Generic class for a node of the local_eqn parser.
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
        return self

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

class UnaryNode(Node):
    node_type = "UnaryNode"
    fmt = plain_list_formatter

    def __init__(self, data=[], children=[]):
        Node.__init__(self, children=children, data=data)

    def __str__(self):
        return self.fmt.stringify([self.data])

    def get_inner_item(self):
        return self

class ListNode(Node):
    node_type = "ListNode"

    def get_inner_item(self):
        if len(self.children) == 1:
            return self.children[0].get_inner_item()
        else:
            return None

    def is_float(self, context=None):
        c = self.get_inner_item()
        if c != None:
            return c.is_float(context=context)
        return Node.is_float(self, context=context)

    def as_float(self, context=None):
        c = self.get_inner_item()
        if c != None:
            return c.as_float(context=context)
        return Node.as_float(self, context=context)

    def is_zero(self, context=None):
        c = self.get_inner_item()
        if c != None:
            return c.is_zero(context=context)
        return Node.is_zero(self, context=context)

    def is_one(self, context=None):
        c = self.get_inner_item()
        if c != None:
            return c.is_one(context=context)
        return Node.is_one(self, context=context)

class AssocOpNode(ListNode):
    node_type = "AssocOpNode"
    ops = {}

    def __str__(self):
        s = ""
        for op, term in zip(self.data, self.children):
            s += "%s%s" % (self.ops[op], term)
        return s

    def get_inner_item(self):
        if len(self.data) == 1 and self.data[0] == None:
            return self.children[0].get_inner_item()
        else:
            return None

class LocalEqnNode(Node):
    fmt = ListFormatter("", "", "\n")
    node_type = "LocalEqn"

class LocalAndRangeDefsNode(Node):
    node_type = "LocalAndRangeDefs"

    def __init__(self):
        Node.__init__(self)
        self.children = [Node(), Node()]
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

class NumTensorNode(Node):
    node_type = "NumTensor"

    def __str__(self):
        return "%s(%s)" % (self.data, self.children[0])

class NumTensorsNode(Node):
    node_type = "NumTensors"
    fmt = plain_list_formatter

class IntsNode(Node):
    node_type = "Ints"
    fmt = plain_list_formatter

class IxRangeNode(Node):
    node_type = "IxRange"
    fmt = plain_list_formatter

    def __str__(self):
        return self.fmt.stringify(["%s:%s" % ir
                                   for ir in self.data])

class AssignmentNode(Node):
    node_type = "Assignment"
    fmt = ListFormatter("", ";", " <- ")

    def _collect_quantities(self, inputs, outputs, parsing):
        assert parsing == 'root'
        assert len(self.children) == 2
        self.children[0]._collect_quantities(inputs, outputs, 'inputs')
        self.children[1]._collect_quantities(inputs, outputs, 'outputs')

class AssignmentsNode(Node):
    node_type = "Assignments"
    fmt = minimal_list_formatter

    def simplify(self, context=None):
        # This node is responsible for expanding the equation for all the
        # materials: transforming one equation in 'm' into many equations in
        # 'm_material1', 'm_material2', etc.
        if context != None and context.material != None:
            material = context.material
            if isinstance(material, str):
                material_list = [material]
            else:
                material_list = material

            eqs = self.children
            expanded_assignments = AssignmentsNode()
            for material_name in material_list:
                context.material = material_name
                for eq in eqs:
                    expanded_assignments.add(eq.simplify(context=context))

            context.material = material
            return expanded_assignments

        else:
            return Node.simplify(self, context=context)

class NumIndexNode(UnaryNode):
    node_type = "NumIndex"

class VarIndexNode(UnaryNode):
    node_type = "VarIndex"

class IndicesNode(Node):
    node_type = "Indices"
    fmt = plain_list_formatter

class TensorSumNode(AssocOpNode):
    node_type = "TensorSum"
    fmt = ListFormatter("", "", " + ")
    ops = {None: '', 1.0: ' + ', -1.0: ' - '}

    def simplify(self, context=None):
        # We go through the operands, exclude the zeros and sum the constants
        # together.
        new_children = []
        new_data = []
        constant_sum = 0.0
        for operand, op in zip(self.children, self.data):
            new_operand = operand.simplify(context=context)
            if new_operand.is_float(context=context):
                v = new_operand.as_float(context=context)
                if op in [1.0, None]:
                    constant_sum += v
                else:
                    assert op == -1.0
                    constant_sum -= v

            elif not new_operand.is_zero(context=context):
                new_children.append(new_operand)
                new_data.append(op)

        if constant_sum != 0.0 or len(new_children) == 0:
            if len(new_data) > 0 and new_data[0] == None:
                new_data[0] = 1.0
            new_children.insert(0, FloatNode(constant_sum))
            new_data.insert(0, None)

        else:
            if len(new_data) > 0 and new_data[0] == 1.0:
                new_data[0] = None

        assert(len(new_data) == len(new_children))
        new_obj = TensorSumNode()
        new_obj.children = new_children
        new_obj.data = new_data
        return new_obj

class TensorProductNode(AssocOpNode):
    node_type = "TensorProduct"
    ops = {None: '', '*': '*', '/': '/'}

    def simplify(self, context=None):
        # First let's look for zeros in the factors of the multiplication
        # If one of the factors is zero, then we just return the float 0.0
        # We otherwise include the factor in the simplified one, but only if
        # it is not one.
        new_children = []
        new_data = []
        constant_factor = 1.0
        for factor, op in zip(self.children, self.data):
            new_factor = factor.simplify(context=context)
            if new_factor.is_zero(context=context):
                if op == '/':
                    raise ValueError("Found division by zero during "
                                     "simplification")
                return FloatNode(0.0)

            elif new_factor.is_float(context=context):
                v = new_factor.as_float(context=context)
                if op in ['*', None]:
                    constant_factor *= v
                else:
                    assert op == '/'
                    constant_factor /= v

            elif not new_factor.is_one(context=context):
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

    def get_inner_item(self):
        if self.data == 1.0:
            return self.children[0].get_inner_item()
        else:
            return None

    def is_float(self, context=None):
        return self.children[0].is_float(context=context)

    def as_float(self, context=None):
        return self.data*self.children[0].as_float(context=context)

    def is_zero(self, context=None):
        return self.children[0].is_zero(context=context)

    def is_one(self, context=None):
        return (self.data == 1.0
                and self.children[0].is_one(context=context))

class FloatNode(UnaryNode):
    node_type = "Number"

    def __init__(self, value=0.0):
        UnaryNode.__init__(self, float(value))

    def is_zero(self, context=None):
        return self.data == 0.0

    def is_one(self, context=None):
        return self.data == 1.0

    def is_float(self, context=None):
        return True

    def as_float(self, context=None):
        return self.data

class ParenthesisNode(ListNode):
    node_type = "Parenthesis"
    fmt = default_list_formatter

    def simplify(self, context=None):
        simplified = ListNode.simplify(self, context=context)
        if not _sc(context).simplify_parentheses:
            return simplified
        inner_item = simplified.get_inner_item()
        if isinstance(inner_item, (FloatNode, TensorNode)):
            return inner_item
        return simplified

class TensorNode(UnaryNode):
    node_type = "Tensor"
    special_tensors = {"eps":None}

    def __init__(self, name="?", arg=None):
        Node.__init__(self, arg, data=(name, name))

    def __str__(self):
        if self.children[0] == None:
            return self.data[0]
        else:
            return "%s(%s)" % (self.data[0], self.children[0])

    def simplify(self, context=None):
        # First, let's check whether this is a special tensor. If that is the
        # case, then we should just treat it specially.
        if self.special_tensors.has_key(self.data[0]):
            return Node.simplify(self, context=context)

        # We should go through the quantities, find this one and - if it is
        # constant - just simplify the tensor with such constant.
        # For example, if pi(i, j) is a tensor which turns out to be always
        # equal to 3.14, then we just have to substitute pi(i, j) -> 3.14
        if context != None:
            q = context.quantities.get(self.data[1])
            if q.is_constant():
                assert self.children == [None], \
                  "Vector Constant quantities are not supported, yet!"
                return FloatNode(q.as_constant(material=context.material,
                                               in_units=True))
            else:
                if (context.material != None
                    and q.is_defined_on_material(context.material)):
                    tensor_node = Node.simplify(self, context=context)
                    tensor_node.data[0] += "_%s" % context.material
                    return tensor_node

        return Node.simplify(self, context=context)

    def _collect_quantities(self, inputs, outputs, parsing):
        name = self.data[1]
        if self.special_tensors.has_key(name):
            return
        if parsing == "inputs":
            qs_list = inputs
        else:
            assert parsing == "outputs"
            qs_list = outputs
        if not name in qs_list:
            qs_list.append(name)

class FunctionNode(UnaryNode):
    node_type = "Function"

    def __init__(self, name="?", arg=None):
        Node.__init__(self, arg, data=name)

    def __str__(self):
        if self.children[0] == None:
            return self.data[0]
        else:
            return "%s[%s]" % (self.data, self.children[0])
