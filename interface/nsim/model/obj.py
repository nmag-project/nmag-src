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
'''

class ModelObj(object):
    type_str = None

    def __init__(self, name):
        assert self.type_str != None, \
          "ModelObj is not meant to be used directly. Use rather one of " \
          "its derived classes."
        self.name = name
        self.lam = None

    def _is_vivified(self):
        return self.lam != None

    vivified = property(_is_vivified)

    def get_full_name(self):
        return "%s_%s" % (self.type_str, self.name)

    def get_lam(self):
        assert self.lam != None, \
          "The %s object (name=%s) has not been vivified, yet. You need to " \
          "add it to a model and to build the model." \
          % (self.type_str, self.name)
        return self.lam

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
