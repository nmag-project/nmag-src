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
"""

import types, logging

import ocaml
import nmag
from nsim import linalg_machine as nlam

from computation import Computations, SimplifyContext
from quantity import Quantities
from timestepper import Timesteppers

__all__ = ['Model']

logger = logging.getLogger('nsim')

#-----------------------------------------------------------------------------

def enumerated_list(z):
    return zip(range(len(z)),z)

def _extended_properties_by_region(region_materials, min_region=-1,
                                   extra_pbr=[]):
    """Internal: Generate properties_by_region"""
    pbr = {}

    def add_prop(region,prop):
        if pbr.has_key(region):
            pbr[region][prop] = True
        else:
            pbr[region] = {prop: True}

    # Ensure all the outer regions have the property "outer":
    for vac in range(min_region,0):
        add_prop(vac, "outer")

    for nr_region in range(len(region_materials)):
        add_prop(nr_region,str(nr_region))
        materials = region_materials[nr_region]
        for m in materials:
            add_prop(nr_region,m.name)
            for p in m.properties:
                add_prop(nr_region,p)

    # Now that we initialized these hashes, map them back to lists:

    def sorted_keys(h):
        k=h.keys()
        k.sort()
        return k

    srk = sorted_keys(pbr)

    result = [(k, sorted_keys(pbr[k])) for k in srk]
    logger.info("properties_by_region: %s" % repr(result))
    return result

#-----------------------------------------------------------------------------

class Model:
    def __init__(self, name, mesh, region_materials, min_region=-1,
                 properties_by_region=[]):
        # Just save the relevant stuff
        self.name = name
        self.mesh = mesh
        self.dim = ocaml.mesh_dim(mesh.raw_mesh)
        self.region_materials = region_materials
        self.min_region = min_region

        # Things that get constructed when the object is "used"
        self.computations = Computations()
        self.quantities = Quantities()
        self.timesteppers = Timesteppers()

        self.intensive_params = []

        self.all_material_names = [] # All different names of the materials

        self.elems_by_field = {}     # For each field: list of elems ordered
                                     # per region
        self.prototype_elems = []    # List of the prototype elements
        self.sibling_elems = {}
        self.properties_by_region = properties_by_region
        self.mwes = {}               # All the MWEs
        self.lam = {}
        self.built = False

    def add_quantity(self, quant):
        """Add the given quantity 'quant' to the current physical model.
        If 'quant' is a list, then add all the elements of the list, assuming
        they all are Quantity objects."""
        return self.quantities.add(quant)

    def add_computation(self, c):
        """Add the computation (Computation object) to the model."""
        return self.computations.add(c)

    def add_timestepper(self, ts):
        """Add the timestepper (Timestepper object) to the model."""
        return self.timesteppers.add(ts)

    def _build_elems_on_material(self, name, shape):
        # Build the 'all_materials' dictionary which maps a material name to
        # a corresponding element
        all_materials = {}
        for region in self.region_materials:
            for mat in region:
                logger.info("Processing material '%s'" % mat.name)

                if not all_materials.has_key(mat.name):
                    elem_name = "%s_%s" % (name, mat.name)
                    elem = ocaml.make_element(elem_name, shape, self.dim, 1)
                    all_materials[mat.name] = (mat, elem)

        # Build a list of all the different names of the involved materials
        self.all_material_names = [all_materials[mn][0].name
                                   for mn in all_materials]

        # Obtain a list fused_elem_by_region such that
        # fused_elem_by_region[idx] is the element obtained by fusing all the
        # elements corresponding to the materials defined in region idx.
        # fused_elem_by_region[idx] is then the right element for that region
        fused_elem_by_region = []
        for region in self.region_materials:
            # Build a list of the elements in the region
            elems_in_region = \
              map(lambda mat: all_materials[mat.name][1], region)
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

        if on_material:
            where = "on material"
        else:
            where = "everywhere"
        logger.info("Building element %s %s" % (name, where))

        if on_material:
            elems = self._build_elems_on_material(name, shape)

        else:
            elems = self._build_elems_everywhere(name, shape)

        self.prototype_elems.append((name, shape, elems, on_material))
        self.elems_by_field[name] = elems
        return elems

    def _build_mwe(self, name):
        if self.sibling_elems.has_key(name):
            p_name = self.sibling_elems[name] # prototype name
            assert not self.sibling_elems.has_key(p_name), \
                   "Cannot have sibling field of a sibling field."

            logger.info("Building MWE %s as sibling of %s" % (name, p_name))

            if self.mwes.has_key(p_name):
                p_mwe = self.mwes[p_name]
            else:
                p_mwe = self._build_mwe(p_name)

            rename_prefix = "%s/%s" % (name, p_name)
            all_material_names = self.all_material_names
            relabelling = \
              map(lambda mn: ("%s_%s" % (p_name, mn), "%s_%s" % (name, mn)),
                  all_material_names)

            mwe = ocaml.mwe_sibling(p_mwe, name, rename_prefix, relabelling)
            self.mwes[name] = mwe
            return mwe

        logger.info("Building MWE %s" % name)
        elems = self.elems_by_field[name]
        mwe = ocaml.make_mwe(name, self.mesh.raw_mesh,
                             enumerated_list(elems), [], self.ext_pbr)
        self.mwes[name] = mwe
        return mwe

    def _build_dependency_tree(self):
        # Determine dependencies and involved quantities
        q_dependencies = {}
        involved_qs = {}
        for computation in self.computations._all:
            inputs, outputs = computation.inputs_and_outputs
            print inputs, outputs
            raw_input()
            for o in outputs:
                q_dependencies[o] = inputs
            for q_name in inputs + outputs:
                if not involved_qs.has_key(q_name):
                    involved_qs[q_name] = None

        # Determine primary quantities
        intermediate_qs = []
        primary_qs = []
        for q_name in involved_qs:
            if q_dependencies.has_key(q_name):
                intermediate_qs.append(q_name)
            else:
                primary_qs.append(q_name)

        for pq_name in primary_qs:
            print "PRIMARY: %s" % pq_name

        for iq_name in intermediate_qs:
            print "DERIVED: %s; DEPEND ON: %s" \
             % (iq_name, ", ".join(q_dependencies[iq_name]))

    def _build_operators(self):
        return {}

    def _build_equations(self):
        eqs = self.computations._by_type.get('Equation', [])
        simplify_context = \
          SimplifyContext(quantities=self.quantities,
                          material=self.all_material_names)
        equation_dict = {}
        for eq in eqs:
            logger.info("Building equation %s" % eq.name)
            eq_text = eq.get_text(context=simplify_context)
            mwes_for_eq = eq.get_inouts()
            equation_dict[eq.name] = \
              nlam.lam_local(eq.name,
                             aux_args=self.intensive_params,
                             field_mwes=mwes_for_eq,
                             equation=eq_text)

        return equation_dict

    def _build_lam(self):
        # Build LAM vector dictionary
        vectors = {}
        for mwe_name in self.mwes:
            vec_name = "v_%s" % mwe_name
            assert not vectors.has_key(vec_name), \
                   "Duplicate definition of vector '%s'" % vec_name
            vectors[vec_name] = nlam.lam_vector(name=vec_name,
                                                mwe_name=mwe_name)

        operators = self._build_operators()
        bems = {}
        ksps = {}
        equations = self._build_equations()
        jacobi = {}
        programs = {}
        timesteppers = {}
        debugfile = None
        self._build_dependency_tree()

        lam = nlam.make_lam(self.name,
                            intensive_params=self.intensive_params,
                            mwes=self.mwes.values(),
                            vectors=vectors.values(),
                            operators=operators.values(),
                            bem_matrices=bems.values(),
                            ksps=ksps.values(),
                            local_operations=equations.values(),
                            jacobi_plans=jacobi.values(),
                            programs=programs.values(),
                            timesteppers=timesteppers.values(),
                            lam_debugfile=debugfile)

        self.lam = lam

    def build(self):
        # I'm keeping the same order of execution that we were using in the
        # old nmag_lam.py source
        field_quants = self.quantities._by_type.get('SpaceField', [])
        field_quants += self.quantities._by_type.get('SpaceTimeField', [])
        param_quants = self.quantities._by_type.get('TimeField', [])

        # Extended properties by region
        self.ext_pbr = \
          _extended_properties_by_region(self.region_materials,
                                         self.min_region,
                                         self.properties_by_region)

        # First build all the elements
        for field_quant in field_quants:
            self._build_elems(field_quant.name,
                              field_quant.shape,
                              on_material=field_quant.def_on_mat)

        # Now build all the MWEs
        for field_quant in field_quants:
            self._build_mwe(field_quant.name)

        self._build_lam()


        for mwe in self.mwes:
            print mwe, self.mwes[mwe]

        self.built = True


