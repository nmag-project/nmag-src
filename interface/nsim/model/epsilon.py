# Nmag micromagnetic simulator
# Copyright (C) 2011 University of Southampton
# Hans Fangohr, Thomas Fischbacher, Matteo Franchin and others
#
# WEB:     http://nmag.soton.ac.uk
# CONTACT: nmag@soton.ac.uk
#
# AUTHOR(S) OF THIS FILE: Matteo Franchin
# LICENSE: GNU General Public License 2.0
#          (see <http://www.gnu.org/licenses/>)

def exchange(l, i1, i2):
  tmp = l[i1]; l[i1] = l[i2]; l[i2] = tmp

def _perm(indices, wanted, position, sign):
  """We start from 'indices=(0, 1, ..., d)' and 'position=0' and permute this
  so that we finally get 'wanted', taking note of the sign. If we cannot do
  this (wanted has repeated indices) return 0."""
  if position == len(indices):
    return sign

  elif indices[position] == wanted[position]:
    return _perm(indices, wanted, position + 1, sign)

  else:
    try:
        pos_wanted = indices.index(wanted[position], position + 1)
        exchange(indices, position, pos_wanted)
        return _perm(indices, wanted, position + 1, -sign)

    except ValueError:
        return 0

def epsilon(*indices):
  """Return the Levi-Civita symbol."""
  return _perm(range(len(indices)), indices, 0, 1)
