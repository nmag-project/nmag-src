"""
Module which allows to define in a high-level way the physics to be simulated
by the Nsim package.
"""

import types, logging

import ocaml
import nmag
from nsim import linalg_machine as nlam

__all__ = ['Constant', 'SpaceField', 'TimeField', 'SpaceTimeField',
           'OperatorComp',
           'Model']

logger = logging.getLogger('nsim')

#-----------------------------------------------------------------------------

"""
There are four types of Quantity:
 - Constant: just a number which does not change in space nor in time
 - SpaceField: a vector field, which changes in space but not continuously
     in time. It can, however, change abruptly in time (it is piecewise
     constant in time;
 - TimeField: a number which changes continuously in time;
 - SpaceTimeField: a vector field which changes continuously in space and
     time.
"""

class Quantity:
    """ddd"""

    def __init__(self, name, shape=[], value=None, is_primary=True,
                 def_on_material=False):
        self.type = None
        self.type_str = "Quantity"
        self.name = name
        self.shape = shape
        self.value = None
        self.is_primary = is_primary
        self.def_on_mat = def_on_material
        self._specialised_init()

    def _specialised_init(self):
        raise NotImplementedError("This class is not meant to be used like "
                                  "this. You should use one of the derived "
                                  "classes.")

    def set_value(self, value):
        if self.value != None:
            raise ValueError("The initial value of the %s has been already "
                             "set." % self.type_str)
        self.value = value

    def is_always_zero(self):
        """Return whether the Quantity is a constantly and uniformly zero."""
        return False

    def depend_on(self, quants_list):
        pass

class Constant(Quantity):
    def _specialised_init(self):
        self.type_str = "Constant"

    def is_always_zero(self):
        return float(self.value) == 0.0
    is_always_zero.__doc__ = Quantity.is_always_zero.__doc__

class SpaceField(Quantity):
    def _specialised_init(self):
        self.type_str = "SpaceField"

class TimeField(Quantity):
    def _specialised_init(self):
        self.type_str = "TimeField"

class SpaceTimeField(Quantity):
    def _specialised_init(self):
        self.type_str = "SpaceTimeField"

#-----------------------------------------------------------------------------

class Computation:
    """A black box which takes some quantities as input and produces
    some quantities as output."""

    def __init__(self, input=None, output=None):
        self.type_str = "Computation"
        self.input = input
        self.output = output
        self._specialised_init()

    def _specialised_init(self):
        raise NotImplementedError("The class %s is not meant to be used "
                                  "directly. You should rather use one of "
                                  "the derived classes." % self.type_str)

class EquationComp(Computation):
    def _specialised_init(self):
        self.type_str = "EquationComp"

class OperatorComp(Computation):
    def __init__(self, operator_tree, running_indices=None,
                 input=None, output=None):
        Computation.__init__(self, input=input, output=output)
        self.running_indices = running_indices
        self.operator_tree = operator_tree

    def _specialised_init(self):
        self.type_str = "OperatorComp"
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

class CCodeComp(Computation):
    def _specialised_init(self):
        self.type_str = "CCodeComp"





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

        self.all_quantities = []     # list of all added Quantity-es
        self.quants_by_type = {}     # and here they are classified by type
        self.all_computations = []   # list of all added Computation-s
        self.comps_by_type = {}      # and here they are classified by type

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
        if isinstance(quant, types.ListType):
            quants = quant
        else:
            quants = [quant]

        for quant in quants:
            self.all_quantities.append(quant)
            try: 
                self.quants_by_type[quant.type_str].append(quant)
            except KeyError:
                self.quants_by_type[quant.type_str] = [quant]

    def add_operator(self, op):
        pass

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

    def _build_lam(self):
        intensive_params = []

        # Build LAM vector dictionary
        vectors = {}
        for mwe_name in self.mwes:
            vec_name = "v_%s" % mwe_name
            assert not vectors.has_key(vec_name), \
                   "Duplicate definition of vector '%s'" % vec_name
            vectors[vec_name] = nlam.lam_vector(name=vec_name,
                                                mwe_name=mwe_name)

        operators = {}
        bems = {}
        ksps = {}
        operations = {}
        jacobi = {}
        programs = {}
        timesteppers = {}
        debugfile = None

        lam = nlam.make_lam(self.name,
                            intensive_params=intensive_params,
                            mwes=self.mwes.values(),
                            vectors=vectors.values(),
                            operators=operators.values(),
                            bem_matrices=bems.values(),
                            ksps=ksps.values(),
                            local_operations=operations.values(),
                            jacobi_plans=jacobi.values(),
                            programs=programs.values(),
                            timesteppers=timesteppers.values(),
                            lam_debugfile=debugfile)

        self.lam = lam

    def build(self):
        # I'm keeping the same order of execution that we were using in the
        # old nmag_lam.py source
        field_quants = self.quants_by_type.get('SpaceField', [])
        field_quants += self.quants_by_type.get('SpaceTimeField', [])
        param_quants = self.quants_by_type.get('TimeField', [])

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


