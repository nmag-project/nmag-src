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

import collections

class Quantity:
    """ddd"""

    type_str = "Quantity"

    def __init__(self, name, shape=[], value=None, units=1.0,
                 is_primary=True, def_on_material=False):
        self.name = name
        self.shape = shape
        self.value = value
        self.units = units
        self.is_primary = is_primary
        self.def_on_mat = def_on_material

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

class Constant(Quantity):
    type_str = "Constant"

    def set_value(self, value, material=None):
        if self.def_on_mat:
            if self.value == None:
                self.value = {}
            if material == None:
                self.value = {None: value}
            else:
                assert type(material) == str, \
                  ("Optional argument of Constant.set_value should be either "
                   "a string or None (the latter to set the constant on "
                   "every material).")
                if self.value.has_key(None):
                    self.value = {material: value}
                else:
                    self.value[material] = value

        else:
            assert material == None, \
              ("The Constant is not defined per material and hence the "
               "optional argument material of Constant.set_value should "
               "not be set to any value other than None.")
            self.value = value

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
            if self.def_on_mat:
                v = self.value[material]
            else:
                v = self.value

        if in_units:
            return float(v/self.units)

        else:
            return v

class SpaceField(Quantity):
    type_str = "SpaceField"

class TimeField(Quantity):
    type_str = "TimeField"

class SpaceTimeField(Quantity):
    type_str = "SpaceTimeField"

class Quantities:
    def __init__(self, quants):
        self.all_quants = []
        self.quant_by_type = {}
        self.quant_by_name = {}
        self.add_quantity(quants)

    def add_quantity(self, quant):
        """Add the given quantity 'quant' to the current Quantities instance.
        If 'quant' is a list, then add all the elements of the list, assuming
        they all are Quantity objects."""
        if isinstance(quant, collections.Sequence):
            quants = quant
        else:
            quants = [quant]

        for quant in quants:
            self.all_quants.append(quant)

            try:
                self.quant_by_type[quant.type_str].append(quant)
            except KeyError:
                self.quant_by_type[quant.type_str] = [quant]

            if self.quant_by_name.has_key(quant.name):
                raise ValueError("Quantities.add_quantity: found duplicate "
                                 "entry with name '%s'." % quant.name)
            self.quant_by_name[quant.name] = quant

    def get(self, name):
        """Return the quantity with the given name."""
        return self.quant_by_name[name]
