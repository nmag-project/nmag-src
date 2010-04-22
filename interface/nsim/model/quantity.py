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

    def set_value(self, value, where=None):
        """Sets the quantity to the given value"""
        if self.value != None:
            raise ValueError("The initial value of the %s has been already "
                             "set." % self.type_str)
        self.value = value

    def is_always_zero(self):
        """Return whether the Quantity is constantly and uniformly zero."""
        return False

    def is_always_one(self):
        """Return whether the Quantity is constantly and uniformly equal
        to one."""
        return False

class Constant(Quantity):
    type_str = "Constant"

    def is_always_zero(self):
        return float(self.value) == 0.0

    def is_always_one(self):
        return float(self.value) == 1.0

    is_always_zero.__doc__ = Quantity.is_always_zero.__doc__
    is_always_one.__doc__ = Quantity.is_always_one.__doc__

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
