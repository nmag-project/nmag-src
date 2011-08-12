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
The basic object of a nsim.model.
The main characteristic of a ModelObj is that it has a type (object.type_str),
a name (object.name) and a full name which identifies the object univocally
in the model (call object.get_full_name() to retrieve it).

ModelObj are typically created by the user and are initially detached from
a model. They are then added to the Model (using 'Model.add_quantity',
'Model.add_computation' and similar). Once the Model is complete (i.e. all
objects have been added), a full specification of the mathematical problem
to be solved is obtained. The 'Model' object does indeed represent such
a complete description of the problem, a sort of abstract syntax tree of the
simulation.

The model is then built. When this happens, the Model goes over all its
objects and obtains their ownership by calling 'ModelObj.own' method.
The ModelObj can then perform basic operations, those which are only possible
when the Model is fully specified (simplifying equations and operators,
generating C code, etc).

Once this phase has finished, the Model goes through the simulation tree and
computes the dependency tree between all the objects. It identifies the
primary quantities, how they enter in the computations to give secondary
quantities. It also understands what computations and quantities are not
necessary and removes them.

Once this has been done, the Model is finally compiled. This will,
for example, involve calls to GCC to compile some C code snippets, etc.

Finally, when the model is built, all the objects are 'vivified'.
For example, the Quantity objects acquire the memory to store their values.
At this points the objects are ready to be used: the Model is realised.
'''

class ModelObj(object):
    type_str = None

    def __init__(self, name):
        assert self.type_str != None, \
          ("ModelObj is not meant to be used directly. Use rather one of "
           "its derived classes.")
        self.name = name
        self.lam = None
        self.model = None

    def _is_vivified(self):
        return self.lam != None

    vivified = property(_is_vivified)

    def get_name(self):
        return self.name

    def get_full_name(self):
        return "%s_%s" % (self.type_str, self.name)

    def get_lam(self):
        assert self.lam != None, \
          ("The %s object (name=%s) has not been vivified, yet. You need to "
           "add it to a model and to build the model."
           % (self.type_str, self.name))
        return self.lam

    def own(self, model):
        """Called by the Model to obtain ownership of this object."""
        #assert self.model == None, "Object was already owned by a Model!"
        self.model = model

    def set_model(self, model):
        """To be removed."""
        self.model = model

    def is_owned(self):
        """Whether the object is owned by another object."""
        return (self.model != None)

    def vivify(self, model):
        """This function is not meant to be called directly by the user.
        Called by the model to vivify the object. A ModelObj object can be
        in one of two states: 1) an object prototypes, 2) a living object.
        A Quantity, for example, is first created as a Quantity prototype.
        At that stage it cannot be written and has no storage associated
        with it. After the quantity has been added to a model and after the
        model has been built, the QUantity becomes is vivified, i.e. storage
        is associated to it and can be set/read normally."""
        self.lam = model.lam
