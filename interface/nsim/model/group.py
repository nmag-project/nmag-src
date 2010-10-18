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

import collections

class Group(object):
    type_str = "GroupItem"

    def __init__(self, objs=[]):
        self._all = []
        self._by_type = {}
        self._by_name = {}
        self.add(objs)

    def __getitem__(self, name):
        return self._by_name[name]

    def add(self, obj, name=None):
        """Add the given object 'obj' to the current instance.
        If 'obj' is a list, then add all the elements of the list."""
        if isinstance(obj, collections.Sequence):
            assert name == None, \
              ("The optional argument name is supposed to be used only "
               "when adding one quantity at a time.")
            objs = obj
        else:
            objs = [obj]

        for obj in objs:
            self._all.append(obj)

            try:
                self._by_type[obj.type_str].append(obj)
            except KeyError:
                self._by_type[obj.type_str] = [obj]

            n = name if name != None else obj.name
            if self._by_name.has_key(n):
                msg = ("Collection.add: found duplicate %s with name '%s' "
                       "(a %s). Make sure that all the objects you define "
                       "have a different name!"
                       % (self.type_str, n, obj.type_str))
                raise ValueError(msg)
            self._by_name[n] = obj

    def get(self, name):
        """Return the quantity with the given name."""
        try:
            return self._by_name[name]
        except KeyError:
            raise KeyError("Cannot find %s object with name %s."
                           % (self.type_str, name))
