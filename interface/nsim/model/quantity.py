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
from setfield import flexible_set_fielddata

import nfem

class Quantity(ModelObj):
    """ddd"""

    type_str = "Quantity"

    def __init__(self, name, shape=[], value=None, units=1.0,
                 is_primary=True, def_on_material=False):
        ModelObj.__init__(self, name)
        self.shape = shape

        self.value = None
        self.units = units
        self.is_primary = is_primary
        self.def_on_mat = def_on_material

        self.set_value(value)

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

    def integrate(self):
        raise NotImplementedError("Method integrate is not implemented for "
                                  "Quantity of type %s." % self.type_str)

class Constant(Quantity):
    type_str = "Constant"

    def is_constant(self):
        return True

    def as_constant(self, material=None, in_units=True):
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

        if in_units:
            return float(v/self.units)

        else:
            return v

class SpaceField(Quantity):
    type_str = "SpaceField"

    def __init__(self, name, shape=[], value=None, units=1.0,
                 is_primary=True, def_on_material=False):

        Quantity.__init__(self, name, shape, value, units, is_primary,
                          def_on_material)

        self.mwe = None            # MWE associated to the field
        self.master = None         # Master copy of the field
        self.material_names = None # Name of materials where field is defined

    def vivify(self, lam, mwe, material_names):
        Quantity.vivify(self, lam)
        self.mwe = mwe
        self.master = ocaml.raw_make_field(mwe, [], "", "")
        self.material_names = material_names
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

    def integrate(self):
        ocaml.lam_get_field(self.lam, self.master, "v_" + self.name)
        return nfem.integrate_field(self.master, "m_Py")


class TimeField(Quantity):
    type_str = "TimeField"

class SpaceTimeField(SpaceField):
    type_str = "SpaceTimeField"

class Quantities(Group):
    pass
