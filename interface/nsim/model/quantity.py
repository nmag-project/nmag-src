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

__all__ = ['Value', 'Constant', 'SpaceField', 'TimeField', 'SpaceTimeField']

import collections, types
from group import Group
from obj import ModelObj

class Value:
    """A Value is an object which can be used to set a Quantity.
    It requires three things: a value, the units and a specification of where
    the field should be set to that value."""

    def __init__(self, value=None, where=None, unit=None):
        self.values = []
        if value != None:
            self.set(value, where, unit)

    def set(self, value, where=None, unit=None):
        """An example:

        FieldSetter(1.0).set(0.5, 'Dy').set(0.02, ['Py', 'Fe'])

        sets to 0.5 in the region with name 'Dy', sets to 0.02 in the region
        'Py' and in the region 'Fe' and sets to 1 in all the remaining
        regions."""
        if type(where) == str:
            where = [where]
        self.values.append((value, where, unit))
        return self

    def __repr__(self):
        fmt = "Value"
        if len(self.values) == 0:
            return "%s()" % fmt

        s = ""
        for value, where, unit in self.values:
            args = [str(value)]
            if where != None:
                if len(where) == 1:
                    args.append("where='%s'" % where[0])
                else:
                    args.append("where=%s" % str(where))
            if unit != None: args.append("unit=%s" % unit)
            s += "%s(%s)" % (fmt, ", ".join(args))
            fmt = ".set"
        return s

    def as_constant(self, where=None):
        """Get the value assuming it is constant over the region 'where'.
        If 'where' is not provided, then assumes the value is constant
        everywhere in space."""
        v = None
        v_default = None
        if where == None:
            for value, _, unit in self.values:
                new_v = (value, unit)
                if v == None:
                    v = new_v
                elif v != new_v:
                    raise ValueError("as_constant was called on a value "
                                     "which is not uniform in space: %s."
                                     % str(self))

        else:
            if type(where) == str:
                where = [where]

            for value, my_where, unit in self.values:
                if my_where == where:
                    new_v = (value, unit)
                    if v == None:
                        v = new_v
                    elif v != new_v:
                        raise ValueError("as_constant was called on a value "
                                         "(%s) which is not uniform in %s."
                                         % (str(where), self))
                elif my_where == None:
                    v_default = (value, unit)

        if v == None:
            if v_default != None:
                v = v_default
            else:
                raise ValueError("as_constant: value (%s) is not set in %s."
                                 % (self, str(where)))

        value, unit = v
        if unit == None:
            return value
        else:
            return value*unit


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

        #self.mwe = None          # MWE associated to the field
        #self.master = None       # Copy of the field stored on the master node

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

class TimeField(Quantity):
    type_str = "TimeField"

class SpaceTimeField(SpaceField):
    type_str = "SpaceTimeField"

class Quantities(Group):
    pass
