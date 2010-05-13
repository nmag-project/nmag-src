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
There are four types of Quantity:
 - Constant: just a number which does not change in space nor in time
 - SpaceField: a vector field, which changes in space but not continuously
     in time. It can, however, change abruptly in time (it is piecewise
     constant in time;
 - TimeField: a number which changes continuously in time;
 - SpaceTimeField: a vector field which changes continuously in space and
     time.
Initially, a quantity is created as a prototype, meaning that it does not
correspond to a field allocated in memory. When the quantity is added to
a Model, then it is uniquely associated with a field in that model and can
hence be set, etc.
"""

__all__ = ['Constant', 'SpaceField', 'TimeField', 'SpaceTimeField']

import ocaml
import collections, types
from group import Group
from obj import ModelObj
from value import Value
from nsim.snippets import rec_scale
from setfield import flexible_set_fielddata

import nfem

class Quantity(ModelObj):
    """Generic Quantity object."""

    type_str = "Quantity"

    def __init__(self, name, shape=[], value=None, unit=1.0,
                 subfields=False):
        """Define a new quantity:
        name:  name of the quantity (should be different for each quantity)
        shape: shape. Examples: [] for scalars, [3] 3-component vectors, etc
        value: initial value of the quantity. An initial value should be
               provided only if the quantity is primary.
        unit:  the unit used for the quantity. Values will be stored in terms
               of units. For example, if unit=100 and you set the Quantity
               to 200, then - internally - 2 will be used.
        subfields:
               For each region specifies which subfield of the Quantity are
               present in it. Example: [["s1", "s2"], ["s1"], []] means
               that the field has two subfields "s1" and "s2" in the region
               with index 0, only "s1" in region with index 2, and is not
               defined in region 3. If subfield is set to False, then the
               field doesn't have any subfields. If it is set to True than
               it gets all the default subfields of the model.
        """
        ModelObj.__init__(self, name)
        self.shape = shape    # Shape of the field
        self.value = None     # Value (instance of Value class)
        self.unit = unit      # Unit
        self.subfields = subfields
        if subfields not in [True, False]:
            raise NotImplementedError("User defined subfield allocations are "
                                      "not permitted, yet. Use only "
                                      "subfields=False or subfields=True "
                                      "when instantiating a new Quantity.")
        self.def_on_mat = subfields

        self.regions = None   # self.regions[sf_name] is the list of regions
                              # indices where the subfield sf_name is defined
        self.volumes = None   # self.volumes[sf_name] is the sum of volumes
                              # of all the regions self.regions[sf_name].

        self.is_primary = True # unused at the moment

        self.set_value(value)

    def vivify(self, model):
        ModelObj.vivify(self, model)

        vols = model.mesh.regionvolumes
        if self.subfields == False:
            self.volumes = {self.name:sum(vols[1:])}

        else:
            if self.subfields == True:
                self.subfields = model.all_material_names

            # Calculate the volume per subfield as the sum of the volumes
            # where the subfield is defined
            self.volumes = volumes = {}
            for subfield in self.subfields:
                volumes["%s_%s" % (self.name, subfield)] = \
                  reduce(lambda v, region_idx: v + vols[region_idx],
                         model.regions_of_subfield[subfield], 0.0)

    def is_defined_on_material(self, material):
        """Return True if the quantity is defined on the specified material,
        False if that is not the case and None if the quantity is not defined
        on a per-material basis."""
        if self.def_on_mat:
            return True
        else:
            return None

    def set_value(self, value, material=None):
        """Sets the quantity to the given value"""
        if self.value != None:
            raise ValueError("The initial value of the %s has been already "
                             "set." % self.type_str)
        self.value = value

    def is_constant(self):
        """Whether the field is a constant."""
        return False

    def as_constant(self, material=None):
        raise NotImplementedError("Method as_constant is not implemented for "
                                  "Quantity of type %s." % self.type_str)

    def is_always_zero(self):
        """Return whether the Quantity is constantly and uniformly zero."""
        return False

    def is_always_one(self):
        """Return whether the Quantity is constantly and uniformly equal
        to one."""
        return False

    def compute_integral(self, where=None, use_su=False):
        """Integrate the field. If 'where' is not given, then integrate it on
        every material region where the field is defined.
        If 'where' is a string, then it is interpreted as the name of the
        region where the integral should be carried out.
        If 'where' is a list of strings, then the integral is carried out
        for the regions with the corresponding names.
        What is returned is a list [("mat1", value1), ("mat2", value2), ...].
        """
        raise NotImplementedError("Method integrate is not implemented for "
                                  "Quantity of type %s." % self.type_str)

    def compute_average(self, where=None):
        """Similar to 'compute_integral', but - for each material region -
        divide the result of the integral by the volume of the material region
        thus obtaining the spatial average."""
        integrals = self.compute_integral(where=where, use_su=True)
        result = []
        for subfield_name, value in integrals:
            avg = rec_scale(1.0/self.volumes[subfield_name], value)
            result.append((subfield_name, avg))

        return result

class Constant(Quantity):
    type_str = "Constant"

    def is_constant(self):
        return True

    def as_constant(self, material=None, in_unit=True):
        if self.value == None:
            raise AttributeError("The Quantity initial value has not been "
                                 "set, yet!")

        if material == None:
            assert self.def_on_mat == False, \
              ("This field is defined per material! as_constant then "
               "requires you to specify the material.")
            v = self.value

        else:
            v = self.value.as_constant(material)

        if in_unit:
            return float(v/self.unit)

        else:
            return v

class SpaceField(Quantity):
    type_str = "SpaceField"

    def __init__(self, name, shape=[], value=None, unit=1.0,
                 subfields=False):

        Quantity.__init__(self, name, shape, value, unit, subfields)

        self.mwe = None            # MWE associated to the field
        self.master = None         # Master copy of the field
        self.material_names = None # Name of materials where field is defined

    def vivify(self, model):
        Quantity.vivify(self, model)
        self.mwe = mwe = model.mwes[self.name]
        self.master = ocaml.raw_make_field(mwe, [], "", "")
        self.material_names = model.all_material_names
        self.set_value(self.value)

    def set_value(self, value):
        if not self.vivified:
            return Quantity.set_value(self, value)

        if value == None:
            return

        if not isinstance(value, Value):
            raise ValueError("The argument of set_value has type '%s', but "
                             "it should rather be an instance of the Value "
                             "class." % type(value))

        if self.def_on_mat:
            set_plan = value.get_set_plan(self.material_names)
            for v, m, u in set_plan:
                print self.master, v, m, u
                fn = "%s_%s" % (self.name, v)
                flexible_set_fielddata(self.master, fn, m, 1e9, scale_factor=1.0,
                                       normalise=False)

        else:
            set_plan = value.get_set_plan(self.material_names)
            assert len(set_plan) == 1
            v, m, u = set_plan[0]
            flexible_set_fielddata(self.master, self.name, m, 1e9, scale_factor=1.0,
                                   normalise=False)

        ocaml.lam_set_field(self.lam, self.master, "v_" + self.name)

    def compute_integral(self, where=None, use_su=False):
        ocaml.lam_get_field(self.lam, self.master, "v_" + self.name)

        dof_stem = ""
        if where != None:
            if type(where) == str:
                where = [where]
            dof_stem = ["%s_%s" % (self.name, mat_name)
                        for mat_name in where]
        return nfem.integrate_field(self.master, dof_stem)


class TimeField(Quantity):
    type_str = "TimeField"

class SpaceTimeField(SpaceField):
    type_str = "SpaceTimeField"

class Quantities(Group):
    pass
