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

    def set_value(self, value, where=None):
        """Sets the quantity to the given value"""
        if self.value != None:
            raise ValueError("The initial value of the %s has been already "
                             "set." % self.type_str)
        self.value = value

    def is_always_zero(self):
        """Return whether the Quantity is constantly and uniformly zero."""
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

