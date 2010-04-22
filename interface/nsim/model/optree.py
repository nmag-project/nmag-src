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

import exceptions

__all__ = ['Node', 'UnaryNode', 'BinaryNode',
           'UnparsedNode', 'QuantityNode', 'SumNode', 'MulNode',
           'OverMatNode', 'OperatorContext']

def _find_next_quant(s, pos=0):
    start_pos = s.find("${", pos)
    if start_pos < 0:
        return None
    end_pos = s.find("}", start_pos + 2)
    if end_pos < 0:
        raise ValueError("Error while parsing '%s': '${' without '}'" % s)
    return (s[start_pos+2:end_pos], (start_pos, end_pos))

def _parse_quant(s):
    if "(" in s:
        start_pos = s.find("(")
        end_pos = s.find(")")
        if end_pos < 0:
            raise ValueError("Error while parsing '%s': '(' without ')'" % s)
        indices = [idx_str.strip()
                   for idx_str in s[start_pos+1:end_pos].split(",")]
        return (s[:start_pos], indices)

    else:
        return (s, [])

# At the moment we implement a hybrid parsed/nonparsed tree.
# One day, we may have Python to parse operator strings and to generate
# a parse tree which is then - again within Python - parsed to produce
# an operator matrix.
class Node:
    def __init__(self):
        pass

    def to_str(self, op_context):
        return "<Node>"

    def is_zero(self):
        """Returns whether the node (let's call it x) represents a number
        or tensor which is always zero, meaning - in particular - that
        x + y can be written as y and x * y as x.
        NOTE: it is safe to always return False (no optimisations will be
          carried out in such a case."""
        return False

    def is_one(self):
        """Returns whether the node (let's call it x) represents a number
        or tensor which is always one, meaning - in particular - that
        x * y can be written as y.
        NOTE: it is safe to always return False (no optimisations will be
          carried out in such a case."""
        return False

    def simplify(self):
        """Simplify the operator parse tree and return the simplified version.
        Simplify applies rules such as 0 + a = a, 1*a = a, 0*a = 0, etc.
        """
        self._not_implemented()

    def get_used_quants(self, d):
        """Fills the given dictionary 'd' with the quantities used inside the
        operator expression."""
        self._not_implemented()

    def _not_implemented(self):
        raise NotImplementedError("Node method not implemented, yet.")

class UnaryNode(Node):
    def __init__(self, value):
        Node.__init__(self)
        self.value = value

    def simplify(self):
        return self.__class__(self.value)

    def to_str(self, op_context):
        return str(self.value)

    def get_used_quants(self, d):
        return d

class BinaryNode(Node):
    def __init__(self, left, right):
        Node.__init__(self)
        self.left = left
        self.right = right

    def simplify(self):
        return self.__class__(self.left, self.right)

    def to_str(self, op_context):
        l = self.left.to_str(op_context)
        r = self.right.to_str(op_context)
        return "%s (?) %s" % (l, r)

    def get_used_quants(self, d):
        if self.left != None:
            self.left.get_used_quants(d)
        if self.right != None:
            self.right.get_used_quants(d)
        return d

    # Propagate documentation
    get_used_quants.__doc__ = Node.get_used_quants.__doc__

class UnparsedNode(UnaryNode):
    def get_used_quants(self, d):
        s = self.value
        pos = 0
        while True:
            quant_str_pos = _find_next_quant(s, pos)
            if quant_str_pos == None:
                break

            quant_str, (_, pos) = quant_str_pos
            quant_name, quant_indices = _parse_quant(quant_str)
            if d.has_key(quant_name):
                prev_rank = len(d[quant_name])
                if len(quant_indices) != prev_rank:
                    raise ValueError("Found '%s' both used as %s and %s"
                                     % (quant_name,
                                        prev_quant_type, quant_type))

            else:
                d[quant_name] = quant_indices

        return d

    def to_str(self, op_context):
        s = self.value
        pos = 0
        out_s = ""
        while True:
            quant_str_pos = _find_next_quant(s, pos)
            if quant_str_pos == None:
                break

            quant_str, (start_pos, end_pos) = quant_str_pos
            out_s += s[pos:start_pos]
            pos = end_pos + 1
            quant_name, quant_indices = _parse_quant(quant_str)
            if op_context.inouts_dict.has_key(quant_name):
                q = op_context.inouts_dict[quant_name]
                if q.def_on_mat and not op_context.for_material:
                    raise ValueError("'%s' is a quantity defined on material "
                                     "but this operator expression is not "
                                     "summed over materials. Please use "
                                     "OverMatNode." % quant_name)
                if q.def_on_mat:
                    quant_name += "_" + op_context.for_material
            else:
                raise ValueError("'%s' has not been found in the list of "
                                 "input/output quantities for this operator"
                                 % quant_name)

            if len(quant_indices) > 0:
                out_s += "%s(%s)" % (quant_name, ", ".join(quant_indices))
            else:
                out_s += quant_name

        out_s += s[pos:]
        return out_s

    # Propagate documentation
    get_used_quants.__doc__ = Node.get_used_quants.__doc__

class QuantityNode(UnaryNode):
    def is_zero(self):
        return self.value == 0.0

    def is_one(self):
        return self.value == 1.0

    # Propagate documentation
    is_zero.__doc__ = UnaryNode.is_zero.__doc__
    is_one.__doc__ = UnaryNode.is_one.__doc__

class SumNode(BinaryNode):
    def simplify(self):
        l, r = (self.left, self.right)
        if l.is_zero():
            return r.simplify()
        elif r.is_zero():
            return l.simplify()
        else:
            return self.__class__(l.simplify(), r.simplify())

    def to_str(self, op_context):
        return "%s + %s" % (self.left.to_str(op_context),
                            self.right.to_str(op_context))

    def is_zero(self):
        return self.left.is_zero() and self.left.is_zero()
        # ^^^ remember that this is for optimisation, it is acceptable
        #     to have that x is zero even if x.is_zero() == False

    # Propagate documentation
    is_zero.__doc__ = UnaryNode.is_zero.__doc__

class MulNode(BinaryNode):
    def simplify(self):
        l = self.left
        if l.is_zero():
            return l.simplify()
        elif l.is_one():
            return self.right.simplify()
        else:
            return self.__class__(self.left.simplify(), self.right.simplify())

    def to_str(self, op_context):
        return "(%s)*%s" % (self.left.to_str(op_context),
                            self.right.to_str(op_context))

    def is_zero(self):
        return self.left.is_zero() or self.left.is_zero()
        # ^^^ remember that this is for optimisation, it is acceptable
        #     to have that x is zero even if x.is_zero() == False

    def is_one(self):
        return self.left.is_one() and self.left.is_one()
        # ^^^ remember that this is for optimisation, it is acceptable
        #     to have that x is one even if x.is_one() == False

    # Propagate documentation
    is_zero.__doc__ = UnaryNode.is_zero.__doc__
    is_one.__doc__ = UnaryNode.is_one.__doc__

class OperatorContext:
    def __init__(self, tree=None, inputs=[], outputs=[],
                 running_indices=[]):
        self.tree = tree
        self.for_material = None
        self.inputs = inputs
        self.outputs = outputs
        inouts = inputs + outputs
        self.inouts = inouts
        self.inouts_dict = {}
        for q in inouts:
            self.inouts_dict[q.name] = q

    def to_str(self):
        return self.tree.to_str(self)

class OverMatNode(UnaryNode):
    def to_str(self, op_context):
        return "<Node>"

    def is_zero(self):
        return False

    def is_one(self):
        return False

    def to_str(self, op_context):
        if op_context.for_material != None:
            raise ValueError("Recursive usage of OverMatNode: OverMatNode "
                             "should be used only once!")
        ss = []
        for mat_name in ['Py']:
            op_context.for_material = mat_name
            ss.append(self.value.to_str(op_context))

        op_context.for_material = None
        return " + ".join(ss)

    def get_used_quants(self, d):
        return self.value.get_used_quants(d)
