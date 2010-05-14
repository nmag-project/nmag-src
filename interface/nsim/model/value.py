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

'''
This file contains the implementation of the Value object.
'''

import collections

__all__ = ['Value']

def _is_where_specification(x):
    if type(x) == str:
        return True
    elif isinstance(x, collections.Sequence):
        if len(x) > 0:
            return type(x[0]) == str
        else:
            return True

class Value:
    """A Value is an object which can be used to set a Quantity.
    It requires three things: a value, the units and a specification of where
    the field should be set to that value. The value can be:

      - a floating number or a list of list of ... of floating point numbers
        (used to set a Quantity uniformly in space),

      - a function specifying, for every position, what the value is
        (used to set a Quantity dependently on the position in space),

      - a numpy array specifying a value for every site of the mesh
        (used when the number of nodes and topology of the mesh is known).

    A note should be made for the latter case. A Value object is not
    associated to a mesh. Therefore, the check that the size of the numpy
    array matches the number of mesh nodes is done when actually setting
    the field, i.e. by the function or code that is using the Value."""

    def __init__(self, arg1=None, arg2=None, arg3=None):
        """Specifies a value, the associated unit and the name of the material
        region where the value should be considered. There are two possible
        ways of creating an instance of the Value class:

          EXAMPLE 1: Value(["mat1", "mat2", ...], [1, 2, 3], SI(1e-9, "m"))
          EXAMPLE 2: Value([1, 2, 3], SI(1e-9, "m"))

        The first example specifies a value for material regions with name
        "mat1", "mat2", etc. The second specifies a value for all the
        available material regions.
        """
        self.values = []
        if arg1 != None:
            self.set(arg1, arg2, arg3)

    def set(self, arg1, arg2=None, arg3=None):
        """An example:

        Value(1.0).set('Dy', 0.5).set(['Py', 'Fe'], 0.02)

        sets to 0.5 in the region with name 'Dy', sets to 0.02 in the region
        'Py' and in the region 'Fe' and sets to 1 in all the remaining
        regions.
        The method receives three arguments and behaves similarly to the
        instantiator:

          EXAMPLE 1: v.set([1, 2, 3], SI(1e-9, "m"))
          EXAMPLE 2: v.set(["mat1", "mat2", ...], [1, 2, 3], SI(1e-9, "m"))

        The first sets everywhere, the second only on the listed materials.
        """
        if _is_where_specification(arg1):
            where = arg1
            value = arg2
            unit = arg3
        else:
            value = arg1
            unit = arg2
            where = arg3
            assert where == None, "You provided three arguments, but the " \
              "first one was not recognised as a subfield specification."

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
            if where != None:
                if len(where) == 1:
                    args = ["'%s'" % where[0]]
                else:
                    args = [str(where)]
            else:
                args = []
            args.append(str(value))
            if unit != None: args.append(str(unit))
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

    def get_set_plan(self, all_materials=[], allow_overwrt=True):
        """Goes trough the Value and return a list of triples:
        (material_string, value, units)."""
        d = {}
        for v, w, u in self.values:
            if w == None:
                w = [None]

            for m in w:
                if d.has_key(m) and not allow_overwrt:
                    raise ValueError("Value (%s) contains double "
                                     "specification for material '%s'."
                                     % (self, m))
                d[m] = (v, u)

        # Now write the plan
        plan = []
        used = {}
        for m in all_materials:
            if d.has_key(m):
                v, u = d[m]
            elif d.has_key(None):
                v, u = d[None]
            else:
                raise ValueError("Value (%s) does not specify how to set "
                                 "material %s." % (self, m))
            plan.append((m, v, u))
            used[m] = True

        # Check that all materials were used
        for m in d:
            if m != None and not used.has_key(m):
                raise ValueError("Material '%s' of value (%s) was not used."
                                 % (m, self))
        return plan
