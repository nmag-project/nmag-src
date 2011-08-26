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

    def __init__(self, name, inputs=None, outputs=None, internals=None,
                 auto_dep=None):
        ModelObj.__init__(self, name)
        self.inputs = inputs
        self.outputs = outputs
        self.internals = internals
        self.auto_dep = auto_dep if auto_dep != None else True

    def get_inputs_and_outputs(self, context=None):
        """Get the input and output quantities involved in the computation."""
        i = self.inputs
        o = self.outputs
        return (i if i != None else [], o if o != None else [])

    def get_required_computations(self):
        """Return the computations (directly) required by this program."""
        return []

    def get_internals(self, context=None):
        """Get fields required internally by the computation."""
        return self.internals or []

    def get_all_quantities(self, context=None):
        """Get all the quantities involved in the computation.
        If the optional argument is not specified, then return the last
        computed value, if possible."""
        ins, outs = self.get_inputs_and_outputs(context=context)
        internals = self.get_internals(context=context)
        return ins + outs + internals

    def get_desc(self):
        return "%s(%s):" % (self.type_str, self.name)

    def _build_lam_object(self, model):
        """Build the LAM object corresponding to this computation."""
        raise NotImplementedError("_build_lam_object not implemented.")


class LAMProgram(Computation):
    type_str = "LAMProgram"

    def __init__(self, name, commands=None,
                 inputs=None, outputs=None, internals=None,
                 auto_dep=None):
        """LAM program. commands is a list of commands in the form
        ["COMMANDNAME", arg1, arg2, ...]. Arguments can be provided as strings
        or as ModeObj objects. In the latter case, the ModelObj will be used to
        compute dependencies automatically (so that the user does not need to
        specify dependencies manually).
        """
        self.commands = commands or []
        internals = _find_internals(self.commands, internals or [])
        Computation.__init__(self, name, inputs=inputs, outputs=outputs,
                             internals=internals, auto_dep=auto_dep)

    def add_commands(self, commands):
        if type(commands[0]) == str:
            commands = [commands]
        self.commands += _expand_commands(commands)

    def get_prog_name(self):
        return self.get_full_name()

    def get_required_computations(self):
        required_computations = {}
        for cmd in self.commands:
            for arg in cmd[1:]:
                if isinstance(arg, Computation):
                    required_computations[arg.get_full_name()] = arg
        return required_computations.values()
    get_required_computations.__doc__ = \
      Computation.get_required_computations.__doc__

    def execute(self, fields=[], cofields=[]):
        """Execute the LAMProgram."""
        ocaml.lam_execute(self.get_lam(), self.get_prog_name(),
                          fields, cofields)

    def get_desc(self):
        program_desc = "\n  ".join(map(str, self.commands))
        return "%s\nLAM program:\n  %s" \
          % (Computation.get_desc(self), program_desc)

    def _build_lam_program(self):
        commands = _expand_commands(self.commands)
        return nlam.lam_program(self.get_prog_name(),
                                commands=commands)


# Commands that Python knows in form 
#   "NAME": (list_of_types, list_of_argument_mapping_functions)
# NOTE: commands not known are treated "generically" (arguments are mapped
#   to string using the method arg.get_lam_name()).
known_commands = {"GOSUB": ((LAMProgram,),
                            (lambda obj: obj.get_prog_name(),))}

def _expand_command(cmd):
    """Used to expand a LAM command to a form which can be taken by OCaml
    (a list of strings).
    """

    cmd_name = cmd[0]
    args = cmd[1:]

    expected_args_and_maps = known_commands.get(cmd_name, None)
    if expected_args_and_maps != None:
        expected_args, maps = expected_args_and_maps
        expanded_cmd = [cmd_name]
        for i, expected_arg in enumerate(expected_args):
            arg = args[i]
            if type(arg) == str:
                arg_str = arg
                
            elif isinstance(arg, expected_arg):
                arg_str = maps[i](arg)

            else:
                msg = ("Error in LAMProgram command expansion: argument "
                       "%d of '%s' expects '%s', but got '%s'"
                       % (i, cmd_name, expected_arg, type(arg)))
                raise ValueError(msg)
            expanded_cmd.append(arg_str)
            return expanded_cmd

    else:
        def _expand_arg(arg):
            return (arg.get_lam_name() if isinstance(arg, ModelObj) else arg)
        return [cmd_name] + map(_expand_arg, args)

def _expand_commands(cmds):
    return map(_expand_command, cmds)

def _find_internals(cmds, internals=[]):
    idict = dict((q, None) for q in internals)
    for cmd in cmds:
        for arg in cmd:
            if isinstance(arg, quantity.SpaceField):
                idict[arg.get_name()] = arg
    return idict.keys()


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

    def simplify(self, context=None):
        """Simplify the computation using the provided simplification context.
        """
        if self.simplified_tree == None:
            self.simplified_tree = self.tree.simplify(context=context)
        
    def get_text(self, context=None):
        """Get the final (processed) text of the equation."""
        self.simplify(context=context)
        if self.final_text == None:
            self.final_text = str(self.simplified_tree)
        return self.final_text

    def get_ccode(self, context=None):
        """Get the final (processed) C code for the equation."""
        self.simplify(context=context)
        if self.final_ccode == None:
            self.final_ccode = self.simplified_tree.get_ccode()
        return self.final_ccode

    def get_desc(self):
        """Mostly used for debugging."""
        return LAMProgram.get_desc(self) + "\n" + \
          ("C-Code: '%s'" % self.final_ccode if self.final_ccode != None else
           "Expanded: '%s'" % self.final_text if self.final_text != None
           else "Not evaluated")

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
            mwes_for_eq = self.get_all_quantities()
            return \
              nlam.lam_local(self.get_full_name(),
                             aux_args=intensive_params,
                             field_mwes=mwes_for_eq,
                             equation=eq_text)
        else:
            ccode = self.get_ccode(context=context)
            mwes_for_eq = self.get_all_quantities()
            return \
              nlam.lam_local(self.get_full_name(),
                             aux_args=intensive_params,
                             field_mwes=mwes_for_eq,
                             c_code=ccode)


class Operator(ParsedComputation):
    type_str = "Operator"

    def __init__(self, name, operator_string, mat_opts=[], is_periodic=False,
                 inputs=None, outputs=None, cofield_to_field=False,
                 auto_dep=None):
        """Create a new Operator Computation.

        :Parameters:

          `is_periodic` : bool
            Whether the operator is periodic. If this is set to False, then
            the amendments 'periodic' in the operator string are silently
            removed during the operator simplification.
        """

        self.is_periodic = is_periodic
        operator_tree = opparser.parse(operator_string)
        ParsedComputation.__init__(self, name, "OpProg",
                                   operator_tree, operator_string,
                                   inputs=inputs, outputs=outputs,
                                   auto_dep=auto_dep)
        self.mat_opts = mat_opts
        self.cofield_to_field = cofield_to_field

    def simplify(self, context=None):
        if context != None:
            context.is_periodic = self.is_periodic
        ParsedComputation.simplify(self, context=context)


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

    def own(self, model):
        # We now substitute the constant quantities inside the ccode and
        # determine which field quantities are used by the ccode.

        LAMProgram.own(self, model)

        ccode = ""
        all_required_qs = {}
        for orig_ccode, mats in self.ccodes:
            if mats == None:
                mats = model.regions.all_entity_names

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
        assert self.is_owned(), "Cannot build LAM object: CCode is not owned."
        assert self.ccode != None
        required_quantities = self.get_all_quantities()
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

    def get_required_computations(self):
        return [self.operator]
    get_required_computations.__doc__ = \
      Computation.get_required_computations.__doc__

    def _build_lam_object(self, model):
        return \
          nlam.lam_ksp(self.get_full_name(), self.operator.get_full_name(),
                       precond_name=self.precond_name,
                       ksp_type=self.ksp_type, pc_type=self.pc_type,
                       initial_guess_nonzero=self.initial_guess_nonzero,
                       rtol=self.rtol, atol=self.atol, dtol=self.dtol,
                       maxits=self.maxits,
                       nullspace_subfields=self.nullspace_subfields,
                       nullspace_has_constant=self.nullspace_has_constant)


class BEM(Computation):
    type_str = "BEM"

    def __init__(self, name, mwe_name, dof_name,
                 hlib_params=None, boundary_spec="outer and material",
                 lattice_info=None, matoptions=[],
                 inputs=None, outputs=None, auto_dep=None):
        Computation.__init__(self, name, inputs=inputs, outputs=outputs,
                             auto_dep=auto_dep)
        self.mwe_name = mwe_name
        self.dof_name = [dof_name] if type(dof_name) == str else dof_name
        self.is_hlib = (hlib_params != None)
        self.hlib_params = hlib_params
        self.boundary_spec = boundary_spec
        self.lattice_info = lattice_info or []
        self.matoptions = matoptions

    def _build_lam_object(self, model):
        return \
          nlam.lam_bem(self.get_full_name(), is_hlib=self.is_hlib,
                       mwe_name=self.mwe_name, dof_name=self.dof_name,
                       boundary_spec=self.boundary_spec,
                       lattice_info=self.lattice_info,
                       matoptions=self.matoptions,
                       hlib_params=self.hlib_params)


class Computations(Group):
    type_str = "Computation"
