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

from obj import ModelObj


class Group(object):
    type_str = "GroupItem"

    def __init__(self, objs=[]):
        self._all = []
        self._by_type = {}
        self._by_name = {}
        self.add(*objs)

    def __getitem__(self, name):
        return self._by_name[name]

    def __contains__(self, item):
        return self._by_name.__contains__(item)
        # ^^^ or should we rather define Group(dict)?

    def add(self, *objs, **named_args):
        """Add the given object 'obj' to the current instance.
        If 'obj' is a list, then add all the elements of the list."""

        args = named_args.keys()
        assert args in ([], ["name"]), \
          ('Group.add accepts only one optional argument: name. '
           'Example: group.add(q1, name="q1")')
        name = named_args.get("name", None)

        assert name == None or len(objs) == 1, \
          ("The optional argument name is supposed to be used only "
           "when adding one quantity at a time.")

        for obj in objs:
            n = name or obj.name
            if n in self._by_name:
                msg = ("Collection.add: found duplicate %s with name '%s' "
                       "(a %s). Make sure that all the objects you define "
                       "have a different name!"
                       % (self.type_str, n, obj.type_str))
                raise ValueError(msg)

            self._by_name[n] = obj
            self._all.append(obj)
            self._by_type.setdefault(obj.type_str, []).append(obj)

    def get(self, name):
        """Return the quantity with the given name."""
        try:
            return self._by_name[name]
        except KeyError:
            raise KeyError("Cannot find %s object with name %s."
                           % (self.type_str, name))

    def pop(self, item):
        """Remove an item from the group."""
        name = (item.name if isinstance(item, ModelObj) else item)
        item = self._by_name.pop(name)
        full_name = item.get_full_name()
        
        full_names = map(ModelObj.get_full_name, self._all)
        self._all.pop(full_names.index(full_name))
        
        by_type = self._by_type[item.type_str]
        full_names = map(ModelObj.get_full_name, by_type)
        by_type.pop(full_names.index(full_name))
        return item
