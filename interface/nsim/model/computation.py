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

import re

import ocaml

from nsim import linalg_machine as nlam

import eqparser
import opparser
from eqtree import EqSimplifyContext
from optree import OpSimplifyContext
from group import Group
from obj import ModelObj
import quantity


class Computation(ModelObj):
    """A computation can be regarded as a black box which takes some
    quantities as input and produces some quantities as output."""

    type_str = "Computation"

    def __init__(self, name, inputs=None, outputs=None, auto_dep=None):
        ModelObj.__init__(self, name)
        self.inputs = inputs
        self.outputs = outputs
        self.auto_dep = auto_dep if auto_dep != None else True

    def get_inputs_and_outputs(self, context=None):
        """Get the input and output quantities involved in the computation."""
        i = self.inputs
        o = self.outputs
        return (i if i != None else [], o if o != None else [])

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

    def __init__(self, name, commands=None,
                 inputs=None, outputs=None, auto_dep=None):
        Computation.__init__(self, name, inputs=inputs, outputs=outputs,
                             auto_dep=auto_dep)
        self.commands = _expand_commands(commands) if commands else []

    def add_commands(self, commands):
        if type(commands[0]) == str:
            commands = [commands]
        self.commands += _expand_commands(commands)

    def get_prog_name(self):
        return self.get_full_name()

    def execute(self, fields=[], cofields=[]):
        """Execute the LAMProgram."""
        ocaml.lam_execute(self.get_lam(), self.get_prog_name(),
                          fields, cofields)


class ParsedComputation(LAMProgram):
    def __init__(self, computation_name, computation_prog_name,
                 computation_tree, computation_string,
                 inputs=None, outputs=None, auto_dep=None):
        LAMProgram.__init__(self, computation_name,
                            inputs=inputs, outputs=outputs, auto_dep=auto_dep)
        self.prog_name = computation_prog_name
        self.text = computation_string
        self.tree = computation_tree
        self.simplified_tree = None
        self.final_text = None
        self.final_ccode = None

    def get_prog_name(self):
        return "%s_%s" % (self.prog_name, self.name)

    def get_text(self, context=None):
        """Get the final (processed) text of the equation."""
        if self.simplified_tree == None:
            self.simplified_tree = self.tree.simplify(context=context)
        if self.final_text == None:
            self.final_text = str(self.simplified_tree)
        return self.final_text

    def get_ccode(self, context=None):
        """Get the final (processed) C code for the equation."""
        if self.simplified_tree == None:
            self.simplified_tree = self.tree.simplify(context=context)
        if self.final_ccode == None:
            self.final_ccode = self.simplified_tree.get_ccode()
        return self.final_ccode

    def get_inputs_and_outputs(self, context=None):
        """Get the input and output quantities involved in the computation.
        If the optional argument is not specified, then return the last
        computed value, if possible."""
        if self.simplified_tree == None:
            self.simplified_tree = self.tree.simplify(context=context)
            self.outputs = None

        if self.outputs == None:
            self.inputs, self.outputs = \
              self.simplified_tree.get_inputs_and_outputs()

        return Computation.get_inputs_and_outputs(self, context=context)


class Equation(ParsedComputation):
    type_str = "Equation"

    def __init__(self, name, equation_string,
                 inputs=None, outputs=None, auto_dep=None,
                 ocaml_to_parse=False):
        equation_tree = eqparser.parse(equation_string)
        ParsedComputation.__init__(self, name, "EqProg",
                                   equation_tree, equation_string,
                                   inputs=inputs, outputs=outputs,
                                   auto_dep=auto_dep)
        self.ocaml_to_parse = ocaml_to_parse

    def _build_lam_object(self, model, context=None):

        intensive_params = [] # For now...

        if self.ocaml_to_parse:
            eq_text = self.get_text(context=context)
            mwes_for_eq = self.get_inouts()
            print eq_text
            raw_input()
            return \
              nlam.lam_local(self.get_full_name(),
                             aux_args=intensive_params,
                             field_mwes=mwes_for_eq,
                             equation=eq_text)
        else:
            ccode = self.get_ccode(context=context)
            mwes_for_eq = self.get_inouts()
            print ccode
            raw_input()

            return \
              nlam.lam_local(self.get_full_name(),
                             aux_args=intensive_params,
                             field_mwes=mwes_for_eq,
                             c_code=ccode)


class Operator(ParsedComputation):
    type_str = "Operator"

    def __init__(self, name, operator_string, mat_opts=[],
                 inputs=None, outputs=None, cofield_to_field=False,
                 auto_dep=None):
        operator_tree = opparser.parse(operator_string)
        ParsedComputation.__init__(self, name, "OpProg",
                                   operator_tree, operator_string,
                                   inputs=inputs, outputs=outputs,
                                   auto_dep=auto_dep)
        self.mat_opts = mat_opts
        self.cofield_to_field = cofield_to_field


_variable_re = re.compile("[$][^$]*[$]")

def ccode_iter(src, fn):
    def substitutor(state):
        orig = state.group(0)
        ret = fn(orig[1:-1])
        if ret == True:
            return orig[1:-1]
        elif ret != None:
            return ret
        else:
            return orig
    return re.sub(_variable_re, substitutor, src)

def parse_idx(s):
    try:
        return int(s)
    except:
        return s.strip()

def ccode_subst(src, fn):
    def substitutor(s):
        if '(' in s:
            name_part, idx_part = s.split('(', 1)
            name_part = name_part.strip()
            idx_part = idx_part.strip()
            assert idx_part.endswith(')')
            indices = map(parse_idx, idx_part[:-1].split(','))
            idx_part = "(" + idx_part

        else:
            name_part = s
            idx_part = ""
            indices = None

        out = fn(name_part, indices)
        if not isinstance(out, tuple):
            return out

        else:
            name, indices = out
            if name == None:
                name = name_part
            if indices == None:
                indices = idx_part
            return str(name) + str(indices)

    return ccode_iter(src, substitutor)

def ccode_material_subst(model, ccode_name, ccode, mat_name,
                         safety_checks=True):
    required_qs = {}

    def subst(name, indices):
        q = model.quantities._by_name.get(name, None)
        assert q != None, ("Quantity %s is used by CCode %s but has not "
                           "been added to the model." % (name, ccode_name))
        if isinstance(q, quantity.Constant):
            value = q.as_float(where=mat_name)
            if hasattr(value, "__iter__"):
              value = value[indices[0]]
            return "(%s)" % value
        else:
            # Remeber the quantity and remove the dollars signs in ccode
            required_qs[name] = q

            # Substitute field with subfield
            if q.subfields:
                return ("%s_%s" % (name, mat_name), None)
            else:
                return True

    subst_ccode = ccode_subst(ccode, subst)

    if safety_checks:
        qs = [("%s_%s" % (q_name, mat_name) if q.subfields else q_name)
              for q_name, q in required_qs.iteritems()]

        if len(qs) > 0:
            safety_ccode = " && ".join(map(lambda s: "have_" + s, qs))
            subst_ccode = \
              "if (%s) {\n%s\n}\n" % (safety_ccode, subst_ccode)

    return (subst_ccode, required_qs)


class CCode(LAMProgram):
    type_str = "CCode"

    def __init__(self, name, inputs=None, outputs=None, auto_dep=False):
        LAMProgram.__init__(self, name,
                            inputs=inputs, outputs=outputs, auto_dep=auto_dep)

        self.ccodes = []
        self.intensive_params = []
        self.ccode = None
        self.required_quantities = None

    def get_prog_name(self):
        return "CCodeProg_%s" % self.name

    def append(self, ccode, materials=None):
        self.ccodes.append((ccode, materials))

    def _build_ccode(self, model):
        Computation.vivify(self, model)

        # We now substitute the constant quantities inside the ccode and
        # determine which field quantities are used by the ccode.

        ccode = ""
        all_required_qs = {}
        for orig_ccode, mats in self.ccodes:
            if mats == None:
                mats = model.all_material_names

            else:
                if not hasattr(mats, "__iter__"):
                    mats = [mats.name]
                else:
                    mats = map(lambda mat: mat.name, mats)

            for mat in mats:
                subst_ccode, required_qs = \
                  ccode_material_subst(model, self.name, orig_ccode, mat)
                ccode += subst_ccode
                all_required_qs.update(required_qs)

        self.ccode = ccode

        # Remove required quantities that the user specified explicitly
        for oq in self.outputs + self.inputs:
            if oq in all_required_qs:
                all_required_qs.pop(oq)

        # All the required quantities which were automatically detected and
        # the user did't mention are added as input quantities
        self.inputs.extend(all_required_qs.keys())

    def _build_lam_object(self, model):
        if self.ccode == None:
            self._build_ccode(model)

        required_quantities = self.get_inouts()
        return \
          nlam.lam_local(self.get_full_name(),
                         aux_args=self.intensive_params,
                         field_mwes=required_quantities,
                         c_code=self.ccode)


class KSP(Computation):
    type_str = "KSP"

    def __init__(self, name, operator, precond_name=None,
                 ksp_type=None, pc_type=None, initial_guess_nonzero=False,
                 rtol=None, atol=None, dtol=None, maxits=None,
                 nullspace_subfields=None, nullspace_has_constant=False,
                 inputs=None, outputs=None, auto_dep=None):

        Computation.__init__(self, name, inputs=inputs, outputs=outputs,
                             auto_dep=auto_dep)
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
                 lattice_info=[], matoptions=[],
                 inputs=None, outputs=None, auto_dep=None):
        Computation.__init__(self, name, inputs=inputs, outputs=outputs,
                             auto_dep=auto_dep)
        self.mwe_name = mwe_name
        self.dof_name = [dof_name] if type(dof_name) == str else dof_name
        self.is_hlib = (hlib_params != None)
        self.hlib_params = hlib_params
        self.boundary_spec = boundary_spec
        self.lattice_info = lattice_info
        self.matoptions = matoptions


class Computations(Group):
    type_str = "Computation"
