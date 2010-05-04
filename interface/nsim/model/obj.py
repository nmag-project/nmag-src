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

class ModelObj:
    type_str = None

    def __init__(self, name):
        assert self.type_str != None, \
          "ModelObj is not meant to be used directly. Use rather one of " \
          "its derived classes."
        self.name = name

    def get_full_name(self):
        return "%s_%s" % (self.type_str, self.name)
