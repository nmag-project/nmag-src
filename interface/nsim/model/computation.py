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

__all__ = ['Equation', 'Operator', 'CCode', 'LAMCode', 'Computations']

import eqparser
from group import Group

class Computation:
    """A computation can be regarded as a black box which takes some
    quantities as input and produces some quantities as output."""

    type_str = "Computation"

    def __init__(self, inputs=[], outputs=[]):
        self.inputs = inputs
        self.outputs = outputs
        inouts = inputs + outputs
        self.inouts = inouts
        inouts_dict = {}
        for q in inouts:
            inouts_dict[q.name] = q
        self.inouts_dict = inouts_dict

class Equation(Computation):
    type_str = "Equation"

    def __init__(self, equation_string):
        Computation.__init__(self)
        self.equation_str = equation_string
        self.equation_tree = eqparser.parse(equation_string)

class Operator(Computation):
    type_str = "Operator"

    def __init__(self, operator_tree, running_indices=None,
                 input=None, output=None):
        Computation.__init__(self)
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

class LAMCode(Computation):
    type_str = "LAMCode"

class Computations(Group):
    pass
