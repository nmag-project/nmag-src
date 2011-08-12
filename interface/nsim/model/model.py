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

"""
Module which allows to define in a high-level way the physics to be simulated
by the Nsim package.

nsim.model and SI units
-----------------------

The nsim.model module is completely independent from the SI object and the
nsim.si_units module, but at the same time the two modules can be used
together effectively. In this document we explain how this can be possible.
let's consider a SpaceField Quantity, for example. The user may want to set
it using an SI object. For example,

  M_sat.set_value(Value(0.86e6, SI("A/m"))

Obviously, the field data is just an array of floats, therefore one needs to
map the SI object to a float. This is done dependently on the units given in
the Quantity definition. For example, if the field was defined as:

  M_sat = SpaceField("M_sat", unit=SI(1e6, "A/m"))

Then the array of float representing the data for M_sat will be set uniformly
to the value 0.86 which is obtained doing

  float(set_value_arg/unit) (in this case, set_value_arg=0.86e6*SI("A/m"))

This construct is very generic and works in the case set_value_arg and
unit are two SI objects, but - more importantly - also when they are just
two floating point numbers. In the case of SI objects, what happens is that
if the value provided as argument to set_value (which we call set_value_arg)
has the right units, SI("A/m"), then the division will return an object of
type SI(1) which can be converted safely to a float. If the user provides
an SI object with different units, say SI("m"), then the division will give
rise to a dimensional SI object (SI("m^2/A")) which will induce the float
funciton to throw an exception. That is the way we get totally independent
from the SI object and the related stuff.
For example, the previous statements could be safely written as:

  M_sat = SpaceField("M_sat")
  M_sat.set_value(0.86e6)

In this case, unit is assumed to be simply 1.0, a floating point number.

Now let's see what happens when getting values from a Quantity.
For example, let's compute the average of the Quantity.

  v = M_sat.compute_average()

averages are returned as a Value objects. A Value object is composed by the
data (always non-dimensional: just floats) plus a factor. If you defined
M_sat as SpaceField(..., unit=SI(1e6, "A/m")), then the factor will be
SI(1e6, "A/m"). If you used SpaceField("M_sat"), then the prefactor will be
just 1.0. Notice that also M_sat.get_value() will return a Value object.
This does not imply a significant loss of performance, as the Value object
will be just a pair of the raw data (without any scaling, just as it is
represented internally) plus a multiplicative factor.

Notice that the Value object provides methods to convert it to floats
with the desired unit (see Value.change_unit).
"""

import sys, types, logging

import ocaml
import nmag
from nsim import linalg_machine as nlam

from computation import Computation, Computations, EqSimplifyContext, \
                        OpSimplifyContext, LAMProgram, Operator, Equation, \
                        CCode
from quantity import Quantity, Quantities, Constant
from timestepper import Timestepper, Timesteppers
from nsim.snippets import contains_all
from obj import ModelObj

__all__ = ['Model', 'NsimModelError']

logger = logging.getLogger('nsim')

class NsimModelError(Exception):
    pass

#-----------------------------------------------------------------------------

def _set_model(objs, model):
    for obj in objs:
        obj.set_model(model)

class MeshRegions(object):
    def __init__(self, entities_in_regions, min_region=-1):
        self.min_region = min_region
        self.entities_by_region = entities_in_regions
        self.regions_by_entity = None
        self.properties_by_region = None
        self.all_entity_names = None

        self._build_all_entity_names()
        self._build_regions_by_entity()
        self._build_properties_by_region()

    def _build_all_entity_names(self):
        all_entity_names = set()
        for entity_names in self.entities_by_region:
            for entity_name in entity_names:
                all_entity_names.add(entity_name)
        self.all_entity_names = sorted(list(all_entity_names))

    def _build_regions_by_entity(self):
        # Fill a map associating subfield name to region indices where
        # the subfield is defined.
        regions_by_entity = {}
        for region_idx, entity_names in enumerate(self.entities_by_region):
            for entity_name in entity_names:
                region_idxs = regions_by_entity.setdefault(entity_name, [])
                region_idxs.append(region_idx)
        self.regions_by_entity = regions_by_entity

    def _build_properties_by_region(self):
        """Internal: Generate properties_by_region"""
        pbr = {}
        def add_prop(region, prop):
            region_properties = pbr.setdefault(region, {})
            region_properties[prop] = True

        # Ensure all the outer regions have the property "outer":
        for vac in range(self.min_region, 0):
            add_prop(vac, "outer")

        for nr_region, entity_names in enumerate(self.entities_by_region):
            add_prop(nr_region, str(nr_region))
            for entity_name in entity_names:
                add_prop(nr_region, entity_name)
                add_prop(nr_region, "magnetic")
                add_prop(nr_region, "material")

        # Now that we initialized these hashes, map them back to lists:
        result = [(k, sorted(pbr[k].keys()))
                  for k in sorted(pbr.keys())]
        logger.info("properties_by_region: %s" % repr(result))
        self.properties_by_region = result


#-----------------------------------------------------------------------------

class Model(object):
    def __init__(self, name, mesh, mesh_unit, region_materials,
                 min_region=-1):
        """
        :Parameters:
          `region_materials` : list of list of string

            region_materials[region_nr] is a list of names (strings) of field
            components (like "mat1", "mat2") available in the mesh region
            with number 'region_nr'. For example, for a mesh with three
            regions and region_materials=[["one", "two"], ["one", "three"],
            ["one"]], a field 'f' defined 'per-material' will have three
            sub-fields 'f_one', 'f_two' and 'f_three' with: 'f_one' defined
            everywhere, 'f_two' defined in region 0 and 'f_three' defined in
            region 1.
        """
        # Just save the relevant stuff
        self.name = name
        self.mesh = mesh
        self.mesh_unit = mesh_unit
        self.dim = mesh.dim
        self.region_materials = region_materials
        self.min_region = min_region

        self.regions = \
          MeshRegions(region_materials, min_region=min_region)

        # Things that get constructed when the object is "used"
        self.computations = Computations()
        self.quantities = Quantities()
        self.timesteppers = Timesteppers()

        # Initialise some members
        self.quantities.primary = None
        self.quantities.derived = None
        self.quantities.dependencies = None
        self.targets = []

        self.intensive_params = []

        self.elems_by_field = {}     # For each field: list of elems ordered
                                     # per region
        self.prototype_elems = []    # List of the prototype elements
        self.sibling_elems = {}
        self.mwes = {}               # All the MWEs
        self.lam = None
        self._built = {}

    def _was_built(self, name):
        return (name in self._built)

    def add_quantity(self, *quant):
        """Add the given quantity 'quant' to the current physical model.
        If 'quant' is a list, then add all the elements of the list, assuming
        they all are Quantity objects."""
        self.quantities.add(*quant)
        _set_model(quant, self)

    def add_computation(self, *c):
        """Add the computation (Computation object) to the model."""
        self.computations.add(*c)
        _set_model(c, self)

    def add_timestepper(self, *ts):
        """Add the timestepper (Timestepper object) to the model."""
        self.timesteppers.add(*ts)
        _set_model(ts, self)

    def _build_elems_on_material(self, name, shape):
        # Build the 'elements' dictionary which maps an entity name to
        # a corresponding element
        elements = {}
        for entity_name in self.regions.all_entity_names:
            logger.info("Processing material '%s'" % entity_name)
            elem_name = "%s_%s" % (name, entity_name)
            elem = ocaml.make_element(elem_name, shape, self.dim, 1)
            elements[entity_name] = elem

        # Obtain a list fused_elem_by_region such that
        # fused_elem_by_region[idx] is the element obtained by fusing all the
        # elements corresponding to the materials defined in region idx.
        # fused_elem_by_region[idx] is then the right element for that region
        fused_elem_by_region = []
        for entities_in_region in self.regions.entities_by_region:
            # Build a list of the elements in the region
            elems_in_region = map(elements.__getitem__, entities_in_region)
            # Fuse all these elements
            fused_elem = reduce(ocaml.fuse_elements, elems_in_region,
                                ocaml.empty_element)
            fused_elem_by_region.append(fused_elem)

        return fused_elem_by_region

    def _build_elems_everywhere(self, name, shape):
        elem = ocaml.make_element(name, shape, self.dim, 1)
        return map(lambda region: elem, self.region_materials)

    def _build_elems(self, name, shape, on_material=False):
        shape = list(shape)

        # Find whether we have built a similar field before, we may then use
        # it to build the new one
        for p_name, p_shape, p_elems, p_on_material in self.prototype_elems:
            if p_shape == shape and p_on_material == on_material:
                logger.info("Using element %s for field %s (sibling)"
                            % (p_name, name))
                self.sibling_elems[name] = p_name
                return p_elems

        where = ("on material" if on_material else "everywhere")
        logger.info("Building element %s %s" % (name, where))

        if on_material:
            elems = self._build_elems_on_material(name, shape)
        else:
            elems = self._build_elems_everywhere(name, shape)

        self.prototype_elems.append((name, shape, elems, on_material))
        self.elems_by_field[name] = elems
        return elems

    def _build_mwe(self, name):
        if name in self.sibling_elems:
            p_name = self.sibling_elems[name] # prototype name
            assert not p_name in self.sibling_elems, \
                   "Cannot have sibling field of a sibling field."

            logger.info("Building MWE %s as sibling of %s" % (name, p_name))

            if p_name in self.mwes:
                p_mwe = self.mwes[p_name]
            else:
                p_mwe = self._build_mwe(p_name)

            rename_prefix = "%s/%s" % (name, p_name)
            q = self.quantities._by_name[name]
            if q.def_on_mat:
                all_entity_names = self.regions.all_entity_names
                relabelling = \
                  map(lambda mn: ("%s_%s" % (p_name, mn),
                                  "%s_%s" % (name, mn)),
                      all_entity_names)
            else:
                relabelling = [(p_name, name)]

            mwe = ocaml.mwe_sibling(p_mwe, name, rename_prefix, relabelling)
            self.mwes[name] = mwe
            return mwe

        logger.info("Building MWE %s" % name)
        elems = self.elems_by_field[name]
        mwe = ocaml.make_mwe(name, self.mesh.raw_mesh,
                             list(enumerate(elems)), [],
                             self.regions.properties_by_region)
        self.mwes[name] = mwe
        return mwe

    def _simplify_operators(self):
        """Simplify the operators before building them."""
        ops = self.computations._by_type.get('Operator', [])
        op_names = ", ".join(map(Operator.get_name, ops))
        logger.info("Simplifying operators: %s." % op_names)
        
        simplify_context = \
          OpSimplifyContext(quantities=self.quantities,
                            material=self.regions.all_entity_names)
        for op in ops:
            logger.debug("Simplifying operator %s" % op.name)
            op.simplify(context=simplify_context)
      
    def _build_operators(self):
        ops = self.computations._by_type.get('Operator', [])
        op_names = ", ".join(map(Operator.get_name, ops))
        logger.info("Building operators: %s." % op_names)

        operator_dict = {}
        for op in ops:
            logger.debug("Building operator %s" % op.name)
            op_text = op.get_text()
            mwe_in, mwe_out = op.get_inputs_and_outputs()
            op_full_name = op.get_full_name()
            assert len(mwe_in) == 1 and len(mwe_out) == 1, \
              ("Operators should only involve exactly one quantity as input "
               "and one quantity as output. However, for operator '%s' "
               "input=%s and output=%s." % (op.name, mwe_in, mwe_out))

            operator_dict[op_full_name] = \
              nlam.lam_operator(op_full_name, mwe_out[0], mwe_in[0], op_text)

            # We should now register a VM call to compute the equation
            logger.debug("Creating operator program for %s" % op.name)
            v_in, v_out = ["v_%s" % name for name in mwe_in + mwe_out]
            op.add_commands(["SM*V", op_full_name, v_in, v_out])
            if op.cofield_to_field:
                op.add_commands(["CFBOX", mwe_out[0], v_out])

        self._built["Operator"] = True
        return operator_dict

    def _build_ksps(self):
        ksps = self.computations._by_type.get('KSP', [])
        ksp_dict = {}
        for ksp in ksps:
            ksp_full_name = ksp.get_full_name()
            ksp_dict[ksp_full_name] = ksp._build_lam_object(self)
        self._built["KSP"] = True
        return ksp_dict

    def _build_bems(self):
        bems = self.computations._by_type.get('BEM', [])
        bem_dict = {}
        for bem in bems:
            bem_full_name = bem.get_full_name()
            bem_dict[bem_full_name] = bem._build_lam_object(self)
        return bem_dict

    def _simplify_equations(self):
        """Simplify the equations before building them."""
        eqs = self.computations._by_type.get('Equation', [])
        eq_names = ", ".join(map(Equation.get_name, eqs))
        logger.info("Simplifying equations: %s." % eq_names)

        simplify_context = \
          EqSimplifyContext(quantities=self.quantities,
                            material=self.regions.all_entity_names)
        for eq in eqs:
            logger.debug("Simplifying equation %s" % eq.name)
            eq.simplify(context=simplify_context)

    def _own_ccodes(self):
        ccodes = self.computations._by_type.get('CCode', [])
        ccodes_names = ", ".join(map(CCode.get_name, ccodes))
        logger.info("Get ownership of C-codes: %s." % ccodes_names)

        for ccode in ccodes:
            logger.debug("Getting ownership of ccode '%s'" % ccode.name)
            ccode.own(self)        

    def _build_equations(self):
        eqs = self.computations._by_type.get('Equation', [])
        eq_names = ", ".join(map(Equation.get_name, eqs))
        logger.info("Building equations: %s." % eq_names)

        equation_dict = {}
        for eq in eqs:
            logger.debug("Building equation %s" % eq.name)
            eq_full_name = eq.get_full_name()
            equation_dict[eq_full_name] = eq._build_lam_object(self)

            # We should now register a VM call to compute the equation
            logger.debug("Creating equation program for %s" % eq.name)
            fields = ["v_%s" % name for name in eq.get_all_quantities()]
            eq.add_commands(["SITE-WISE-IPARAMS", eq_full_name, fields, []])

        self._built["Equations"] = True
        return equation_dict

    def _build_ccodes(self):
        ccodes = self.computations._by_type.get('CCode', [])
        ccode_dict = {}
        for ccode in ccodes:
            logger.debug("Building ccode %s" % ccode.name)
            ccode_full_name = ccode.get_full_name()
            ccode_dict[ccode_full_name] = ccode._build_lam_object(self)

            # We should now register a VM call to launch the CCode
            logger.info("Creating ccode program for %s" % ccode.name)
            fields = ["v_%s" % name for name in ccode.get_all_quantities()]
            ccode.add_commands(["SITE-WISE-IPARAMS", ccode_full_name, fields, []])

        self._built["CCode"] = True
        return ccode_dict

    def _build_programs(self):
        progs = (self.computations._by_type.get('LAMProgram', [])
                 + self.computations._by_type.get('CCode', [])
                 + self.computations._by_type.get('Equation', [])
                 + self.computations._by_type.get('Operator', []))
        prog_names = ", ".join(map(LAMProgram.get_prog_name, progs))
        logger.info("Building programs: %s." % prog_names)

        prog_dict = {}
        for prog in progs:
            prog_name = prog.get_prog_name()
            logger.debug("Building program %s" % prog_name)
            prog_dict[prog_name] = prog._build_lam_program()

        self._built["LAMPrograms"] = True
        return prog_dict

    def _depend(self, q1_name, q2_name):
        """Return whether the quantity q1_name depends on the quantity
        q2_name."""
        if q1_name in self.quantities.dependencies:
            op, input_qs = self.quantities.dependencies[q1_name]
            for input_q in input_qs:
                if input_q == q2_name:
                    return True
                elif self._depend(input_q, q2_name):
                    return True
        return False

    def _depend_path(self, q1_name, q2_name):
        """Return whether the quantity q1_name depends on the quantity
        q2_name."""
        if q1_name in self.quantities.dependencies:
            op, input_qs = self.quantities.dependencies[q1_name]
            for input_q in input_qs:
                if input_q == q2_name:
                    return "%s ==(%s)==> %s" % (q2_name, op.name, q1_name)
                else:
                    s = self._depend(input_q, q2_name)
                    if s:
                        return "%s ==(%s)==> %s" % (s, op.name, q1_name)
        return ""

    def _build_dependency_tree(self):
        # Determine dependencies and involved quantities
        q_dependencies = {}
        involved_qs = {}
        for computation in self.computations._all:
            if computation.auto_dep:
                # Compute automatically the dependencies
                inputs, outputs = computation.get_inputs_and_outputs()
                for o in outputs:
                    q_dependencies[o] = (computation, inputs)
                for q_name in inputs + outputs:
                    if not q_name in involved_qs:
                        involved_qs[q_name] = None

        # Determine primary quantities
        derived_qs = []
        primary_qs = []
        for q_name in involved_qs:
            if q_name in q_dependencies:
                derived_qs.append(q_name)
            else:
                primary_qs.append(q_name)

        self.quantities.primary = primary_qs
        self.quantities.derived = derived_qs
        self.quantities.dependencies = q_dependencies

        # Cecking for circular dependencies
        for derived_q in derived_qs:
            if self._depend(derived_q, derived_q):
                # does the quantity derived_q depend on itself
                dep_path = self._depend_path(derived_q, derived_q)
                raise NsimModelError("Circular dependency detected for %s: %s"
                                     % (derived_q, dep_path))

        self._built["DepTree"] = True

    def _check_model_obj(self, obj):
        if not isinstance(obj, ModelObj):
            raise ValueError("The object you provided is not a ModelObj")

        if obj.model != self:
            raise ValueError("The object you provided has does not belong "
                             "to this Model.")

    def declare_target(self, *targets):
        """Declare the targets of the model, or - in other words - what
        objects the user wants to use. This will help the system to find out
        which parts of the model are unnecessary and can be removed.
        The system will examine the provided targets and find out which
        quantities and computations are needed in order to compute them.
        It will then remove the remanining objects in the model.
        If this method is not used, then all the model object are kept."""
        for target in targets:
            self._check_model_obj(target)
        self.targets.extend(targets)

    def collect_required_objects(self, targets, bag={}, req_by="?"):
        """Collect the Model objects required by the given model objects
        (provided in targets)."""
        for target in targets:
            tfn = target.get_full_name()
            if tfn in bag:
                continue

            logger.debug("collect_required_objects: %s required by %s"
                         % (tfn, req_by))
            bag[tfn] = target
            if isinstance(target, Computation):
                all_q_names = target.get_all_quantities()
                all_qs = map(self.quantities.__getitem__, all_q_names)
                self.collect_required_objects(all_qs, bag=bag, req_by=tfn)
                sub_comps = target.get_required_computations()
                self.collect_required_objects(sub_comps, bag=bag, req_by=tfn)

            elif isinstance(target, Quantity):
                computation_and_inputs = \
                  self.quantities.dependencies.get(target.name, None)
                if computation_and_inputs != None:
                    computation = computation_and_inputs[0]
                    self.collect_required_objects([computation], bag=bag,
                                                  req_by=tfn)

            elif isinstance(target, Timestepper):
                qs = map(self.quantities.__getitem__, target.x + target.dxdt)
                self.collect_required_objects(qs, bag=bag, req_by=tfn)

            else:
                raise NsimModelError("Unknown model object (%s)"
                                     % type(target))
        return bag
      
    def _remove_unused(self, targets):
        required_objects = self.collect_required_objects(targets)

        # Remove unused computations
        all_computations = list(self.computations._all)
        removed_computations = []
        for computation in all_computations:
            fn = computation.get_full_name()
            if fn not in required_objects:
                removed_computations.append(computation)
                self.computations.pop(computation)

        if removed_computations:
            c_names = \
              ", ".join(map(Computation.get_name, removed_computations))
            logger.info("Removed unused computations: %s." % c_names)

        # Remove quantities (except Constant quantities)
        all_quantities = list(self.quantities._all)
        removed_quantities = []
        for quantity in all_quantities:
            if not isinstance(quantity, Constant):
                fn = quantity.get_full_name()
                if fn not in required_objects:
                    removed_quantities.append(quantity)
                    self.quantities.pop(quantity)

        if removed_quantities:
            q_names = ", ".join(map(Quantity.get_name, removed_quantities))
            logger.info("Removed unused quantities: %s." % q_names)

    def write_dependency_tree(self, out=sys.stdout):
        """Write to the provided stream (sys.stdout, if not provided)
        information about the inferred dependency tree between quantities
        and operators."""
        out.write("Primary quantities: %s\n" % self.quantities.primary)
        out.write("Derived quantities: %s\n" % self.quantities.derived)
        out.write("Dependencies (as INPUTS==[COMPUTATION]==>OUTPUT):\n")
        ds = self.quantities.dependencies
        for o in ds:
            c, i = ds[o]
            out.write("%s ==(%s)==> %s\n" % (i, c.name, o))

    def write_debug_info(self, out=sys.stdout):
        def write_section(s):
            out.write("\n%s\n%s\n\n" % (s, len(s)*"-"))
        write_section("Dependency tree:")
        self.write_dependency_tree(out)
        write_section("Equations:")
        for computation in self.computations._all:
            out.write("%s\n\n" % computation.get_desc())

    def _build_target_maker(self, target, primaries={}, targets_to_make=[]):
        assert self._was_built("DepTree"), \
          "The target-maker builder can be used only after building the " \
          "dependency tree!"
        if not target in self.quantities.dependencies:
            primaries[target] = primaries.get(target, 0) + 1
        else:
            computation, inputs = self.quantities.dependencies[target]
            for i in inputs:
                self._build_target_maker(i, primaries, targets_to_make)
            targets_to_make.append(computation)

    def _build_timesteppers(self):
        nlam_tss = {}
        simplify_context = \
          EqSimplifyContext(quantities=self.quantities,
                            material=self.regions.all_entity_names)
        for ts in self.timesteppers._all:
            nr_primary_fields = len(ts.x)
            assert nr_primary_fields == len(ts.dxdt)

            inputs, outputs = \
              ts.eq_for_jacobian.get_inputs_and_outputs(context=simplify_context)

            all_names = inputs + outputs
            x_and_dxdt = ts.x + ts.dxdt
            if contains_all(outputs, ts.dxdt):
                other_names = [name
                               for name in all_names
                               if name not in x_and_dxdt]

            else:
                print ts.dxdt, outputs
                eq = ts.eq_for_jacobian
                msg = ("Jacobian for timestepper %s (x=%s and dxdt=%s) does "
                       "not determine the value of dxdt. The Jacobian is "
                       "%s, which was simplified from: %s."
                       % (ts.name, ts.x, ts.dxdt,
                          eq.simplified_tree, eq.text))
                raise ValueError(msg)

            # List of all quantities involved in jacobi equation
            all_names = x_and_dxdt + other_names

            # Build a dictionary containing info about how to derive each
            # quantity
            how_to_derive = {}
            if ts.derivatives != None:
                for quant, way in ts.derivatives:
                    if isinstance(way, Operator):
                        op_full_name = way.get_full_name()
                        how_to_derive[quant.name] = ("OPERATOR", op_full_name)
                    else:
                        raise ValueError("Timestepper was build specifying "
                          "that %s should be used to compute the derivative "
                          "of %s, but only operators can be used at the "
                          "moment." % (quant.name, way.name))

            all_v_names = ["v_%s" % name for name in all_names]
            derivs = [("PRIMARY", "")]*(2*nr_primary_fields)
            for name in other_names:
                if name in how_to_derive:
                    derivs.append(how_to_derive[name])
                else:
                    derivs.append(("IGNORE", ""))

            # Build dxdt update program
            primaries = {}
            targets_to_make = []
            for dxdt in ts.dxdt:
                self._build_target_maker(dxdt, primaries, targets_to_make)
            if False:
                print "In order to update the RHS I have to:"
                for p in primaries:
                    print "distribute quantity %s" % p
                for t in targets_to_make:
                    print "run computation %s" % t.name
                raw_input()

            dxdt_updater = LAMProgram("TsUp_%s" % ts.name)
            for t in targets_to_make:
                dxdt_updater.add_commands(["GOSUB", t.get_prog_name()])

            self.computations.add(dxdt_updater)
            assert not self._was_built("LAMPrograms"), \
              "Timesteppers should be built before LAM programs!"

            full_name = ts.get_full_name()
            nlam_ts = \
              nlam.lam_timestepper(full_name,
                                   all_names,
                                   all_v_names,
                                   dxdt_updater.get_full_name(),
                                   nr_primary_fields=nr_primary_fields,
                                   name_jacobian=None,
                                   pc_rtol=ts.pc_rtol,
                                   pc_atol=ts.pc_atol,
                                   max_order=ts.max_order,
                                   krylov_max=ts.krylov_max,
                                   jacobi_eom=ts.eq_for_jacobian.get_text(),
                                   phys_field_derivs=derivs,
                                   jacobi_prealloc_diagonal=
                                     ts.jacobi_prealloc_diagonal,
                                   jacobi_prealloc_off_diagonal=
                                     ts.jacobi_prealloc_off_diagonal)

            nlam_tss[full_name] = nlam_ts
        self._built["TSs"] = True
        return nlam_tss

    def _build_lam(self, remove_unused=True):
        # Carry out equation and operator simplifications
        self._simplify_operators()
        self._simplify_equations()
        self._own_ccodes()

        # Build the dependency tree
        self._build_dependency_tree()

        # Remove unused objects, if required
        if remove_unused:
            self._remove_unused(self.targets)

        # First build all the elements
        field_quants = self.quantities._by_type.get('SpaceField', [])
        field_quants += self.quantities._by_type.get('SpaceTimeField', [])
        param_quants = self.quantities._by_type.get('TimeField', [])
        for field_quant in field_quants:
            self._build_elems(field_quant.name,
                              field_quant.shape,
                              on_material=field_quant.def_on_mat)

        # Now build all the MWEs
        for field_quant in field_quants:
            self._build_mwe(field_quant.name)

        # Build the simplified operators
        operators = self._build_operators()
        equations = self._build_equations()

        bems = self._build_bems()
        ksps = self._build_ksps()        
        ccodes = self._build_ccodes()
        jacobi = {} # Not used (should clean this)
        timesteppers = self._build_timesteppers()
        programs = self._build_programs()
        debugfile = None

        # Build LAM vector dictionary
        vectors = {}
        for mwe_name in self.mwes.copy():
            vec_name = "v_%s" % mwe_name
            assert not vec_name in vectors, \
                   "Duplicate definition of vector '%s'" % vec_name
            quantity = self.quantities[mwe_name]
            vectors[vec_name] = \
              nlam.lam_vector(name=vec_name, mwe_name=mwe_name,
                              restriction=quantity.restrictions)

        lam = nlam.make_lam(self.name,
                            intensive_params=self.intensive_params,
                            mwes=self.mwes.values(),
                            vectors=vectors.values(),
                            operators=operators.values(),
                            bem_matrices=bems.values(),
                            ksps=ksps.values(),
                            local_operations=(equations.values()
                                              + ccodes.values()),
                            jacobi_plans=jacobi.values(),
                            programs=programs.values(),
                            timesteppers=timesteppers.values(),
                            lam_debugfile=debugfile)

        self.lam = lam

    def _vivify_objs(self, lam):
        logger.info("Vivifiying fields...")
        field_quants = (self.quantities._by_type.get('SpaceField', [])
                        + self.quantities._by_type.get('SpaceTimeField', []))
        for q in field_quants:
            q.vivify(self)

        logger.info("Vivifiying timesteppers...")
        for ts in self.timesteppers._all:
            ts.vivify(self)

        logger.info("Vivifiying computations...")
        for eq in self.computations._all:
            eq.vivify(self)

    def build(self):
        self._build_lam()
        self._vivify_objs(self.lam)
        self._built["LAM"] = True
