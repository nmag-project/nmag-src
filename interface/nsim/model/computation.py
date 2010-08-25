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

__all__ = ['Equation', 'Operator', 'CCode', 'LAMProgram', 'KSP', 'BEM',
           'Computations']

import eqparser, opparser
from eqtree import EqSimplifyContext
from optree import OpSimplifyContext
from group import Group
from obj import ModelObj

class Computation(ModelObj):
    """A computation can be regarded as a black box which takes some
    quantities as input and produces some quantities as output."""

    type_str = "Computation"

    def __init__(self, name, inputs=[], outputs=[], auto_dep=None):
        ModelObj.__init__(self, name)
        self.inputs_and_outputs = None
        self.auto_dep = auto_dep if auto_dep != None else True

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

def _expand_commands(cmds):
    def _expand_arg(arg):
        return arg.get_full_name() if isinstance(arg, Computation) else arg
    return [[_expand_arg(arg) for arg in cmd] for cmd in cmds]

class LAMProgram(Computation):
    type_str = "LAMProgram"

    def __init__(self, name, commands=None, auto_dep=None):
        Computation.__init__(self, name, auto_dep=auto_dep)
        self.commands = _expand_commands(commands) if commands else []

    def add_commands(self, commands):
        if type(commands[0]) == str:
            commands = [commands]
        self.commands += _expand_commands(commands)

    def get_prog_name(self):
        return self.get_full_name()

class ParsedComputation(LAMProgram):
    def __init__(self, computation_name, computation_prog_name,
                 computation_tree, computation_string, auto_dep=None):
        LAMProgram.__init__(self, computation_name, auto_dep=auto_dep)
        self.prog_name = computation_prog_name
        self.text = computation_string
        self.tree = computation_tree
        self.simplified_tree = None
        self.final_text = None

    def get_prog_name(self):
        return "%s_%s" % (self.prog_name, self.name)

    def get_text(self, context=None):
        """Get the final (processed) text of the equation."""
        if self.simplified_tree == None:
            self.simplified_tree = self.tree.simplify(context=context)
        if self.final_text == None:
            self.final_text = str(self.simplified_tree)
        return self.final_text

    def get_inputs_and_outputs(self, context=None):
        """Get the input and output quantities involved in the computation.
        If the optional argument is not specified, then return the last
        computed value, if possible."""
        if self.simplified_tree == None:
            self.simplified_tree = self.tree.simplify(context=context)
        if self.inputs_and_outputs == None:
            self.inputs_and_outputs = \
              self.simplified_tree.get_inputs_and_outputs()
        return self.inputs_and_outputs

class Equation(ParsedComputation):
    type_str = "Equation"

    def __init__(self, name, equation_string, auto_dep=None):
        equation_tree = eqparser.parse(equation_string)
        ParsedComputation.__init__(self, name, "EqProg",
                                   equation_tree, equation_string,
                                   auto_dep=auto_dep)

class Operator(ParsedComputation):
    type_str = "Operator"

    def __init__(self, name, operator_string, mat_opts=[],
                 auto_dep=None):
        operator_tree = opparser.parse(operator_string)
        ParsedComputation.__init__(self, name, "OpProg",
                                   operator_tree, operator_string,
                                   auto_dep=auto_dep)
        self.mat_opts = mat_opts

class CCode(Computation):
    type_str = "CCode"

class KSP(Computation):
    type_str = "KSP"

    def __init__(self, name, operator, precond_name=None,
                 ksp_type=None, pc_type=None, initial_guess_nonzero=False,
                 rtol=None, atol=None, dtol=None, maxits=None,
                 nullspace_subfields=None, nullspace_has_constant=False,
                 auto_dep=None):

        Computation.__init__(self, name, auto_dep=auto_dep)
        self.precond_name = precond_name
        self.operator = operator
        self.ksp_type = ksp_type
        self.pc_type = pc_type
        self.initial_guess_nonzero = initial_guess_nonzero
        self.rtol = rtol
        self.atol = atol
        self.dtol = dtol
        self.maxits = maxits
        self.nullspace_subfields = nullspace_subfields
        self.nullspace_has_constant = nullspace_has_constant

class BEM(Computation):
    type_str = "BEM"

    def __init__(self, name, mwe_name, dof_name,
                 hlib_params=None, boundary_spec="outer and material",
                 lattice_info=[], matoptions=[], auto_dep=None):
        Computation.__init__(self, name, auto_dep=auto_dep)
        self.mwe_name = mwe_name
        self.dof_name = [dof_name] if type(dof_name) == str else dof_name
        self.is_hlib = (hlib_params != None)
        self.hlib_params = hlib_params
        self.boundary_spec = boundary_spec
        self.lattice_info = lattice_info
        self.matoptions = matoptions
        self.auto_dep = auto_dep

class Computations(Group):
    pass
