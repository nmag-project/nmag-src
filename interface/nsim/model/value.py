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

__all__ = ['Value']

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

        Value(1.0).set(0.5, 'Dy').set(0.02, ['Py', 'Fe'])

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
