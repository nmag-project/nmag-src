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

class Group:
    def __init__(self, objs=[]):
        self._all = []
        self._by_type = {}
        self._by_name = {}
        self.add(objs)

    def add(self, obj):
        """Add the given object 'obj' to the current instance.
        If 'obj' is a list, then add all the elements of the list."""
        if isinstance(obj, collections.Sequence):
            objs = obj
        else:
            objs = [obj]

        for obj in objs:
            self._all.append(obj)

            try:
                self._by_type[obj.type_str].append(obj)
            except KeyError:
                self._by_type[obj.type_str] = [obj]

            if self._by_name.has_key(obj.name):
                msg = ("Collection.add: found duplicate entry with name '%s' "
                       "(a %s). Make sure that all the objects you define "
                       "have a different name!" % (obj.name, obj.type_str))
                raise ValueError(msg)
            self._by_name[obj.name] = obj

    def get(self, name):
        """Return the quantity with the given name."""
        return self._by_name[name]
