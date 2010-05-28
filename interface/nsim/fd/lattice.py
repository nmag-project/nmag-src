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
This module provides the Lattice class to describe multi dimensional
rectangular grids.
'''

__all__ = ["first_difference", "parse_lattice_spec", "Lattice"]

import logging

log = logging.getLogger('nsim')
logmsg = log.info

def first_difference(la, lb):
    """Given two lists 'la' and 'lb', returns the index at which the two lists
    differ or len(la) if the first entries in 'lb' match with all the entries
    of 'la'."""
    for i, a in enumerate(la):
        if a != lb[i]:
            return i
    return len(a)

def parse_lattice_spec(s):
    """The lattice specification should be a string such as "-5,10,5/0.5,2,2",
    which defines a two dimensional lattice where the x coordinate goes
    from -5 to 10 in 5 steps, while the y coordinate goes from 0.5 to 2 in 2
    steps. Another example is "-5,10,5/1.23,1.23,1", which defines a one
    dimensional lattice (since the y component is fixed to 1.23).
    The same can be expressed also as "-5,10,5/1.23".
    """
    def parse_dim_spec(dim_spec):
        try:
            nums = dim_spec.split(',')
            if len(nums) == 1:
                x_min = x_max = nums[0]
                num_steps = 1
            else:
                x_min, x_max, num_steps = nums
        except:
            raise NmagUserError('Error in lattice specification: '
                                + parse_lattice_specs.__doc__)
        return [float(x_min), float(x_max), int(num_steps)]
    return [parse_dim_spec(spec) for spec in s.split('/')]

class Lattice:
    """This class allows to define a n-dimensional square lattice and perform
    various operations on it. In particular it allows to iterate over the
    points of the lattice. No storage is needed for this (a Lattice of one
    millions of points doesn't require more memory that a Lattice of 1 point).
    The points of the lattice can be referred univocally by index."""

    def __init__(self, min_max_num_list, reduction=0.0):
        """Creates a lattice given a list containing, for each dimension,
        the corresponding minimum and maximum coordinate and the number of
        points in which it is discretised. Something like:
            
          [(x_min, x_max, x_num), (y_min, y_max, y_num), ...]
          
        Alternatively, a string is accepted following the same specification
        accepted by the function 'parse_lattice_spec'.
        """
        if type(min_max_num_list) == str:
            min_max_num_list = parse_lattice_spec(min_max_num_list)
        self.min_max_num_list = list(min_max_num_list)
        self.dim = len(min_max_num_list)
        self.reduction = reduction

    def __repr__(self):
        return "Lattice(%s)" % self.min_max_num_list

    def __add__(self, right):
        reduction = max(self.reduction, right.reduction)
        return Lattice(self.min_max_num_list + right.min_max_num_list,
                       reduction=reduction)

    def get_shape(self):
        """Returns the shape of the lattice, i.e. a list containing the number
        of points for each dimension of the lattice. Example: [10, 5, 20] for
        a 3D lattice made of 10 by 5 by 20 points."""
        return [i for _, _, i in self.min_max_num_list]

    def get_num_points(self):
        """Returns the total number of points in the lattice."""
        return reduce(lambda x, y: x*y, self.get_shape())

    def get_closest(self, position):
        """Given a position in space, returns the point in the lattice which
        is closer to it. What is returned is actually the index of the point
        in the lattice."""
        def get_closest_i(x, i, min_max_num_list):
            x_min, x_max, x_num = min_max_num_list
            if x_min < x_max:
                return int(round((x_num - 1) * (x - x_min)/(x_max - x_min)))
            else:
                return 0
        return [get_closest_i(position[i], i, min_max_num_list_i)
                for i, min_max_num_list_i in enumerate(self.min_max_num_list)]

    def get_pos_from_idx(self, idx):
        """Return the position of the point in the lattice which has the given
        index."""
        pos = []
        for nr, i in enumerate(idx):
            x_min, x_max, x_num = self.min_max_num_list[nr]
            if x_num > 1:
                delta_x = (x_max - x_min)/(x_num - 1)
                pos.append(x_min + delta_x*i)
            else:
                pos.append(x_min)
        return pos

    def _foreach(self, nr_idx, idx, pos, fn):
        if nr_idx == self.dim:
            fn(idx, pos)

        else:
            x_min, x_max, num_steps = self.min_max_num_list[nr_idx]
            x_min += self.reduction
            x_max -= self.reduction
            xi = x_min
            assert num_steps > 0, ("Number of steps is less than 1 for "
                                   "dimension %d of the lattice!" % nr_idx)
            if num_steps == 1:
                delta_xi = 0.0
            else:
                delta_xi = (x_max - x_min)/(num_steps - 1)

            for i in range(num_steps):
                pos[nr_idx] = xi
                idx[nr_idx] = i
                xi += delta_xi
                self._foreach(nr_idx+1, idx, pos, fn)

    def foreach(self, fn):
        """Iterates over all the points in the lattice and, for each of those,
        call 'fn(idx, pos)' where 'idx' is the index of the current point,
        while 'pos' is its position as given by the method 'get_pos_from_idx'.
        """
        idx = [0]*self.dim
        pos = [0.0]*self.dim
        self._foreach(0, idx, pos, fn)
