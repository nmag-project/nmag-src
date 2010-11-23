'''
This file contains the definition of the PredefinedAnisotropy class
and some useful functions for defining the magnetic anisotropy
of a magnetic material.
'''

import types

from nmag_exceptions import *
from  nsim.vectools  import *

def want_anisotropy(x, want_function=True):
    """Return True if x is a PredefinedAnisotropy object.
    If want_function is not given, or set to True, then x will also be
    required to have an associated energy function (this is needed,
    for example, when summing two PredefinedAnisotropy objects)."""

    if not isinstance(x, PredefinedAnisotropy):
        raise NmagUserError("%s <-- this is not a "
                            "PredefinedAnisotropy object." % x)
    if not x.has_function():
        raise NmagUserError("%s <-- you cannot operate on an anisotropy "
                            "object if this has not an associated energy "
                            "function!" % x)

class PredefinedAnisotropy:
    """
      PredefinedAnisotropy objects contain an anisotropy function plus the
      appropriate order of approximation or alternatively just the equation
      for the energy.
    """
    def __init__(self, function=None, order=None,
                 anis_type="functional",
                 axis1=None, axis2=None, axis3=None,
                 K1=None, K2=None, K3=None,
                 stringifier=None):
        if function == None and order == None:
            raise NmagUserError, "Please specify a sampling order."
        self.anis_type = anis_type
        self.function = function
        self.order = order
        self.K1 = K1
        self.K2 = K2
        self.K3 = K3
        self.axis1 = axis1
        self.axis2 = axis2
        self.axis3 = axis3
        self._str_extra = stringifier

    def has_function(self):
        """Returns True if the anisotropy has a corresponding energy function.
        """
        if self.function != None:
            return True

    def __str__(self):
        s = self.anis_type
        if self._str_extra:
            s += ", %s" % self._str_extra(self)
        return "<PredefinedAnisotropy:%s>" % s

    def __repr__(self):
        if self._str_extra:
            s = self._str_extra(self)
        else:
            s = "?"
        return 'PredefinedAnisotropy(anis_type="%s", %s)' % (self.anis_type, s)

    def __neg__(self):
        """Unary operator -"""
        neg_function = lambda m: -self.function(m)
        return PredefinedAnisotropy(neg_function, self.order)

    def __pos__(self):
        """Unary operator +"""
        return self.copy()

    def __add__(self, y):
        """Addition operator"""
        want_anisotropy(y)
        f = lambda m: self.function(m) + y.function(m)
        o = max(self.order, y.order)
        return PredefinedAnisotropy(f, o)

    def __sub__(self, y):
        """Subtraction operator"""
        want_anisotropy(y)
        f = lambda m: self.function(m) - y.function(m)
        o = max(self.order, y.order)
        return PredefinedAnisotropy(f, o)

def uniaxial_anisotropy(axis, K1, K2=0):
  """
  Returns a predefined anisotropy modelling an uniaxial anisotropy energy density term::

    E_anis = - K1 * <axis, m>^2 - K2 * <axis, m>^4

  (where `m` is the (normalised) magnetization.)

  :Parameters:

    `axis` : vector (=list)
      Easy axis (or hard axis, if K1 < 0; will be normalised).

    `K1` : SI Object
      Second-order phenomenological anisotropy constant (as used in the equation above).

    `K2` : SI Object
      Fourth-order phenomenological anisotropy constant (as used in the equation above).

      Default value is ``0``.
  """
  # normalise easy axis
  axis = normalised_vector(axis)

  # build anisotropy function
  def f(m):
    a = vector3_dot(axis, m)
    return -K1*a**2 - K2*a**4

  if K2:
    order = 4
  else:
    order = 2

  def stringifier(a):
      s = "axis=[%s, %s, %s], K1=%s" % tuple(a.axis1 + [a.K1])
      if a.K2 != 0.0:
          s += ", %s" % a.K2
      return s

  return PredefinedAnisotropy(anis_type="uniaxial",
                              function=f, order=order,
                              axis1=axis, K1=K1, K2=K2,
                              stringifier=stringifier)


def cubic_anisotropy(axis1, axis2, K1, K2=0, K3=0):
  """
  Returns a predefined anisotropy modelling a cubic anisotropy energy density term::

    E_anis = K1 * (<axis1,m>^2 <axis2,m>^2 + <axis1,m>^2 <axis3,m>^2 + <axis2,m>^2 <axis3,m>^2)
           + K2 * (<axis1,m>^2 <axis2,m>^2 <axis3,m>^2)
           + K3 * (<axis1,m>^4 <axis2,m>^4 + <axis1,m>^4 <axis3,m>^4 + <axis2,m>^4 <axis3,m>^4)

  (where `m` is the (normalised) magnetisation.)

  :Parameters:

    `axis1` : vector (=list)
      First cubic anisotropy axis (will be normalised).

    `axis2` : vector (=list)
      Second cubic anisotropy axis (will be orthonormalised with regards to `axis1`).

    `K1` : SI Object
      Fourth-order phenomenological anisotropy constant (as used in the equation above).

    `K2` : SI Object
      Sixth-order phenomenological anisotropy constant (as used in the equation above).

      Default value is ``0``.

    `K3` : SI Object
      Eigth-order phenomenological anisotropy constant (as used in the equation above).

      Default value is ``0``.
  """
  # build ortho-normal system
  axis3 = vector3_cross(axis1, axis2)
  axis2 = vector3_cross(axis3, axis1)
  axis1, axis2, axis3 = map(normalised_vector, [axis1, axis2, axis3])

  # build anisotropy function
  def f(m):
    a1 = vector3_dot(axis1, m)
    a2 = vector3_dot(axis2, m)
    a3 = vector3_dot(axis3, m)

    return K1*((a1*a2)**2 + (a1*a3)**2 + (a2*a3)**2) \
         + K2*((a1*a2*a3)**2) \
         + K3*((a1*a2)**4 + (a1*a3)**4 + (a2*a3)**4)

  def stringifier(a):
    s = ("axis1=[%s, %s, %s], axis2=[%s, %s, %s], "
         "K1=%s" % tuple((a.axis1 + a.axis2).append(a.K1)))
    if a.K2 != 0.0:
      s += ", %s" % a.K2
    if a.K3 != 0.0:
      s += ", %s" % a.K3
    return s

  if K3:
    order = 8
  elif K2:
    order = 6
  else:
    order = 4

  return PredefinedAnisotropy(anis_type="cubic",
                              function=f, order=order,
                              axis1=axis1, axis2=axis2, axis3=axis3,
                              K1=K1, K2=K2, K3=K3,
                              stringifier=stringifier)


















from nsim.si_units.si import SI
from nsim.model import Constant, SpaceField, TimeField, SpaceTimeField, Value
from nsim.model.quantity import Quantities


from nsim.model.computation import ccode_subst

def ccode_add_prefix(ccode, quantity_names, prefix):
    def substitutor(q_name, indices):
        if q_name in quantity_names:
            new_q_name = prefix + q_name
        else:
            new_q_name = q_name

        if indices == None:
            return "$%s$" % new_q_name
        else:
            indices_str = ", ".join(map(str, indices))
            return "$%s(%s)$" % (new_q_name, indices_str)
    return ccode_subst(ccode, substitutor)


class QuantityContainer(object):
    def __init__(self, allowed_quantities=None):
        self.quantities_types = \
          allowed_quantities if allowed_quantities != None else {}

    def _create_quantity(self, q_type, q_name, q_absname,
                         *args, **named_args):
        proposed_q_type = self.quantities_types.get(q_name, None)
        if proposed_q_type != None:
            q_type = proposed_q_type
        return q_type(q_absname, *args, **named_args)

    def declare(self, attrs, objs):
        """Provide some attributes for the given objects. 'attrs' is a list
        of the attributes (a list of strings) to associate to the objects
        'objs' (a list of strings). Example:

          sim.declare("space field", "llg_damping")

        Can be used to declare that the damping should be a field which can
        change in space, rather than just a constant.
        """

        # Available Quantity TypeS
        aqts = {"constant": Constant, "spacefield": SpaceField,
                "timefield": TimeField, "spacetimefield": SpaceTimeField}
        dos = self.quantities_types # Declarable ObjectS
        recognized_attrs = {}

        declare_usage = ("USAGE: x.declare(attrs, objs) where attrs is a "
                         "list of attributes (strings) to associate to the "
                         "objects whose names are in objs (a list of "
                         "strings).")

        if isinstance(objs, types.StringTypes):
            objs = [objs]

        if isinstance(attrs, types.StringTypes):
            attrs = [attrs]

        qt = []
        for obj_name in objs:
            if obj_name in dos:
                assert isinstance(obj_name, types.StringTypes), declare_usage
                for attr in attrs:
                    assert isinstance(attr, types.StringTypes), declare_usage
                    fmt_attr = attr.lower().replace(" ", "")
                    if fmt_attr in aqts:
                        qt.append(fmt_attr)
                        self.quantities_types[obj_name] = aqts[fmt_attr]

                    else:
                        msg = "Unrecognized attribute '%s'" % attr
                        raise NmagUserError(msg)

        if len(qt) > 1:
            msg = "Conflicting attributes %s." % (", ".join(qt))
            raise NmagUserError(msg)


class Anisotropy(QuantityContainer):
    """The anisotropy base class."""

    anisotropy_id = 0

    def __init__(self, allowed_quantities=None):
        # Every anisotropy needs to have an unique name, so that we can assign
        # a unique name to its parameters within the same model.
        # Example: if we define two uniaxial anisotropies, then we have two
        # sets of constants (K1, K2). We then name them as ("u1_K1", "u1_K2")
        # for the first anisotropy and ("u2_K1", "u2_K2") for the second one.
        self.name = "a%d" % Anisotropy.anisotropy_id
        Anisotropy.anisotropy_id += 1

        QuantityContainer.__init__(self, allowed_quantities)
        self.quantities = Quantities()
        self.quantities_defined = False

    def _create_quantity(self, q_type, q_name, *args, **named_args):
        q_absname = "%s_%s" % (self.name, q_name)
        q = QuantityContainer._create_quantity(self, q_type, q_name,
                                               q_absname, *args, **named_args)
        # We add the object to the group without the prefix which is
        # identifying the anisotropy
        self.quantities.add(q, name=q_absname)
        return q

    def _define_quantities(self):
        self.quantities_defined = True

    def get_E_equation(self):
        raise NotImplementedError("get_E_equation not implemented, yet.")

    def get_H_equation(self):
        raise NotImplementedError("get_H_equation not implemented, yet.")


class UniaxialAnisotropy(Anisotropy):
    def __init__(self, axis, K1, K2=0):
        allowed_quantities = {"K1": None, "K2": None, "axis": None}
        Anisotropy.__init__(self, allowed_quantities=allowed_quantities)

        self.axis = axis
        self.K1 = K1
        self.K2 = K2

    def _define_quantities(self):
        Anisotropy._define_quantities(self)
        K_unit = SI(1e6, "J/m^3")
        self._create_quantity(Constant, "K1", subfields=False,
                              value=Value(self.K1), unit=K_unit)
        self._create_quantity(Constant, "K2", subfields=False,
                              value=Value(self.K2), unit=K_unit)
        self._create_quantity(Constant, "axis", [3], subfields=False,
                              value=Value(self.axis), unit=SI(1))

    def get_E_equation(self, add_only=False):
        ccode = \
          ("double cs = $m(0)$*$axis(0)$ + $m(1)$*$axis(1)$ + $m(2)$*$axis(2)$,\n"
           "       cs2 = cs*cs, cs4 = cs2*cs2;\n"
           "$E_anis$ $OP$ -$K1$*cs2 - $K2$*cs4;\n")
        op = "+=" if add_only else "="
        ccode = ccode.replace("$OP$", op)
        return ccode_add_prefix(ccode, ["axis", "K1", "K2"], self.name + "_")

    def get_H_equation(self, add_only=False):
        ccode = \
          ("double a0 = $axis(0)$, a1 = $axis(1)$, a2 = $axis(2)$,\n"
           "       cs = $m(0)$*a0 + $m(1)$*a1 + $m(2)$*a2,\n"
           "       cs3 = cs*cs*cs,\n"
           "       factor = (2*$K1$*cs + 4*$K2$*cs3)/($mu0$*$M_sat$);\n"
           "$H_anis(0)$ $OP$ factor*a0;\n"
           "$H_anis(1)$ $OP$ factor*a1;\n"
           "$H_anis(2)$ $OP$ factor*a2;\n")
        op = "+=" if add_only else "="
        ccode = ccode.replace("$OP$", op)
        # Translate generic names into specific names: "K1" --> "a1_K1"
        # ("a1" is the anisotropy name)
        return ccode_add_prefix(ccode, ["axis", "K1", "K2"], self.name + "_")


class CubicAnisotropy(Anisotropy):
    def __init__(self, axis1, axis2, K1, K2=0, K3=0):
        Anisotropy.__init__(self)
        self.axis1 = axis1
        self.axis2 = axis2
        self.K1 = K1
        self.K2 = K2
        self.K2 = K3

        K_unit = SI(1e6, "J/m^3")
        self._create_quantity(Constant, "K1", subfields=False,
                              value=Value(K1), unit=K_unit)
        self._create_quantity(Constant, "K2", subfields=False,
                              value=Value(K2), unit=K_unit)
        self._create_quantity(Constant, "K3", subfields=False,
                              value=Value(K3), unit=K_unit)
        self._create_quantity(Constant, "axis1", [3], subfields=False,
                              value=Value(axis1), unit=SI(1))
        self._create_quantity(Constant, "axis2", [3], subfields=False,
                              value=Value(axis2), unit=SI(1))

    def get_E_equation(self, add_only=False):
        raise NotImplementedError("get_E_equation not implemented "
                                  "for cubic anisotropy")

    def get_H_equation(self, add_only=False):
        ccode = \
          ("double a10 = $axis(0)$, a11 = $axis(1)$, a12 = $axis(2)$,\n"
           "       cs = $m(0)$*a0 + $m(1)$*a1 + $m(2)$*a2,\n"
           "       cs3 = cs*cs*cs,\n"
           "       factor = (2*$K1$*cs + 4*$K2$*cs3)/($mu0$*$M_sat$);\n"
           "$H_anis(0)$ $OP$ factor*a0;\n"
           "$H_anis(1)$ $OP$ factor*a1;\n"
           "$H_anis(2)$ $OP$ factor*a2;\n")
        op = "+=" if add_only else "="
        ccode = ccode.replace("$OP$", op)
        # Translate generic names into specific names: "K1" --> "a1_K1"
        # ("a1" is the anisotropy name)
        return ccode_add_prefix(ccode, ["axis", "K1", "K2"], self.name + "_")

