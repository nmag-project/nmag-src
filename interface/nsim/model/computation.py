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

__all__ = ['Equation', 'Operator', 'CCode', 'LAMProgram', 'Computations']

import eqparser, opparser
from eqtree import EqSimplifyContext
from optree import OpSimplifyContext
from group import Group
from obj import ModelObj

class Computation(ModelObj):
    """A computation can be regarded as a black box which takes some
    quantities as input and produces some quantities as output."""

    type_str = "Computation"

    def __init__(self, name, inputs=[], outputs=[]):
        ModelObj.__init__(self, name)
        self.inputs_and_outputs = None

    def get_inputs_and_outputs(self, context=None):
        """Get the input and output quantities involved in the computation."""
        if self.inputs_and_outputs == None:
            return ([], [])
        else:
            return self.inputs_and_outputs

    def get_inouts(self, context=None):
        """Get all the quantities involved in the computation.
        If the optional argument is not specified, then return the last
        computed value, if possible."""
        ins, outs = self.get_inputs_and_outputs(context=context)
        return ins + outs

class LAMProgram(Computation):
    type_str = "LAMProgram"

    def __init__(self, name, commands=[]):
        Computation.__init__(self, name)
        self.commands = commands

    def add_commands(self, commands):
        if type(commands[0]) == str:
            commands = [commands]
        self.commands += commands

    def get_prog_name(self):
        return self.get_full_name()

class Equation(LAMProgram):
    type_str = "Equation"

    def __init__(self, name, equation_string):
        LAMProgram.__init__(self, name)
        self.equation_str = equation_string
        self.equation_tree = eqparser.parse(equation_string)
        self.simplified_tree = None
        self.text = None

    def get_prog_name(self):
        return "EqProg_%s" % self.name

    def get_text(self, context=None):
        """Get the final (processed) text of the equation."""
        if self.simplified_tree == None:
            self.simplified_tree = self.equation_tree.simplify(context=context)
        if self.text == None:
            self.text = str(self.simplified_tree)
        return self.text

    def get_inputs_and_outputs(self, context=None):
        """Get the input and output quantities involved in the equation.
        If the optional argument is not specified, then return the last
        computed value, if possible."""
        if self.simplified_tree == None:
            self.simplified_tree = self.equation_tree.simplify(context=context)
        if self.inputs_and_outputs == None:
            self.inputs_and_outputs = \
              self.simplified_tree.get_inputs_and_outputs()
        return self.inputs_and_outputs

class Operator(Computation):
    type_str = "Operator"

    def __init__(self, name, operator_string,
                 input=None, output=None):
        Computation.__init__(self, name)
        self.operator_str = operator_string
        self.operator_tree = opparser.parse(operator_string)
        self.simplified_tree = None
        self.text = None

        #self.running_indices = None
        #self.operator_tree = operator_tree
        #self.allow_incongruent_shapes = False
        #self.seen_indices = {}

    def _parse_quant_str(self, quant_str):
        """Parse the given Quantity string representation and return the
        a tuple of (quantity name, list of index variables)."""
        if "(" in quant_str:
            field_s, indices_s = quant_str.split("(", 1)
            indices_s = indices_s.strip()
            assert indices_s[-1] == ")", ("Index specification in '%s' "
              "should end with a final parenthesis" % quant_str)
            indices_s = indices_s[:-1]
            indices = [i_s for i_s in indices_s.split(",")]

        else:
            field_s = quant_str
            indices = []

        return (field_s, indices)

    def _check_quant_str(self, quant_str):
        """Check the given Quantity string representation and take note about
        the indices and how they are used (basically, infer the index ranges).
        """
        name, indices = self._parse_quant_str(quant_str)
        for q in self.input + self.output:
            if q.name == name:
                for i, idx_name in enumerate(indices):
                    cur_shape = self.seen_indices.get(idx_name, None)
                    new_shape = q.shape[i]
                    if (not allow_incongruent_shapes
                        and cur_shape != None
                        and cur_shape != new_shape):
                        raise ValueError("Found incongruency in range for "
                                         "index variable %s." % idx_name)
                    self.seen_indices[idx_name] = new_shape
                return q

        raise ValueError("Cannot find field '%s' in the list of input or "
                         "output fields." % quant_str)

class OldOperator(Computation):
    type_str = "Operator"

    def __init__(self, name, operator_tree, running_indices=None,
                 input=None, output=None):
        Computation.__init__(self, name)
        self.running_indices = running_indices
        self.operator_tree = operator_tree
        self.allow_incongruent_shapes = False
        self.seen_indices = {}

    def _parse_quant_str(self, quant_str):
        """Parse the given Quantity string representation and return the
        a tuple of (quantity name, list of index variables)."""
        if "(" in quant_str:
            field_s, indices_s = quant_str.split("(", 1)
            indices_s = indices_s.strip()
            assert indices_s[-1] == ")", ("Index specification in '%s' "
              "should end with a final parenthesis" % quant_str)
            indices_s = indices_s[:-1]
            indices = [i_s for i_s in indices_s.split(",")]

        else:
            field_s = quant_str
            indices = []

        return (field_s, indices)

    def _check_quant_str(self, quant_str):
        """Check the given Quantity string representation and take note about
        the indices and how they are used (basically, infer the index ranges).
        """
        name, indices = self._parse_quant_str(quant_str)
        for q in self.input + self.output:
            if q.name == name:
                for i, idx_name in enumerate(indices):
                    cur_shape = self.seen_indices.get(idx_name, None)
                    new_shape = q.shape[i]
                    if (not allow_incongruent_shapes
                        and cur_shape != None
                        and cur_shape != new_shape):
                        raise ValueError("Found incongruency in range for "
                                         "index variable %s." % idx_name)
                    self.seen_indices[idx_name] = new_shape
                return q

        raise ValueError("Cannot find field '%s' in the list of input or "
                         "output fields." % quant_str)

class CCode(Computation):
    type_str = "CCode"

class Computations(Group):
    pass
