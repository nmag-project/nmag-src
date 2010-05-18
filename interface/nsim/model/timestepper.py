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

__all__ = ['Timestepper', 'Timesteppers']

import ocaml
from obj import ModelObj
from group import Group
from computation import Equation
from nsim.snippets import remove_unit

def optarg_to_ocaml(a):
    if a == None:
        return []
    else:
        return [a]

class Timestepper(ModelObj):
    type_str = "SundialsCVode"

    def __init__(self, name, x, dxdt,
                 eq_for_jacobian=None, time_unit=None,
                 rel_tol=1e-5, abs_tol=1e-5, initial_time=0.0,
                 max_order=2, krylov_max=300,
                 jacobi_prealloc_diagonal=75,
                 jacobi_prealloc_off_diagonal=45):
        ModelObj.__init__(self, name)

        if (eq_for_jacobian != None
            and not isinstance(eq_for_jacobian, Equation)):
            raise ValueError("The optional argument eq_for_jacobian should be"
                             "set to an instance of the Equation class.")

        if isinstance(x, str):
            x = [x]

        if isinstance(dxdt, str):
            dxdt = [dxdt]

        if len(x) != len(dxdt):
            raise ValueError("x and dxdt should be lists with the same number "
                             "of entries.")

        self.time_unit = time_unit
        self.x = x
        self.dxdt = dxdt
        self.eq_for_jacobian = eq_for_jacobian
        self.rel_tol = rel_tol
        self.abs_tol = abs_tol
        self.initial_time = initial_time
        self.max_order = max_order
        self.krylov_max = krylov_max
        self.jacobi_prealloc_diagonal = jacobi_prealloc_diagonal
        self.jacobi_prealloc_off_diagonal = jacobi_prealloc_off_diagonal
        self.initialised = False

    def initialise(self, initial_time=None,
                   rel_tol=None, abs_tol=None):
        if rel_tol == None:
            rel_tol = self.rel_tol
        if abs_tol == None:
            abs_tol = self.abs_tol
        if initial_time == None:
            initial_time = self.initial_time

        self.rel_tol = rel_tol
        self.abs_tol = abs_tol
        self.initial_time = initial_time

        initial_time = remove_unit(initial_time, self.time_unit)
        ocaml.lam_ts_init(self.get_lam(), self.get_full_name(),
                          initial_time, rel_tol, abs_tol)
        self.initialised = True

    def advance_time(self, target_time, max_it=-1, exact_tstop=False):
        target_time = remove_unit(target_time, self.time_unit)

        if not self.initialised:
            self.initialise()

        final_t_su = \
          ocaml.lam_ts_advance(self.get_lam(), self.get_full_name(),
                               exact_tstop, target_time, max_it)

        if self.time_unit != None:
            return final_t_su*self.time_unit
        else:
            return final_t_su

class Timesteppers(Group):
    pass
