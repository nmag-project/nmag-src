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

__all__ = ['Computation']

class Computation:
    def __init__(self, inputs=[], outputs=[]):
        self.inputs = inputs
        self.outputs = outputs
        inouts = inputs + outputs
        self.inouts = inouts
        inouts_dict = {}
        for q in inouts:
            inouts_dict[q.name] = q
        self.inouts_dict = inouts_dict

