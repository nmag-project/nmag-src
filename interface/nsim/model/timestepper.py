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
                 derivatives=None,
                 pc_rtol=1e-2, pc_atol=1e-7,
                 rtol=1e-5, atol=1e-5, initial_time=0.0,
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

        self.derivatives = derivatives
        self.time_unit = time_unit
        self.x = x
        self.dxdt = dxdt
        self.eq_for_jacobian = eq_for_jacobian
        self.rtol = rtol
        self.atol = atol
        self.pc_rtol = pc_rtol
        self.pc_atol = pc_atol
        self.initial_time = initial_time
        self.max_order = max_order
        self.krylov_max = krylov_max
        self.jacobi_prealloc_diagonal = jacobi_prealloc_diagonal
        self.jacobi_prealloc_off_diagonal = jacobi_prealloc_off_diagonal
        self.initialised = False

    def initialise(self, initial_time=None,
                   rtol=None, atol=None,
                   pc_rtol=None, pc_atol=None):
        self.rtol = self.rtol if rtol == None else rtol
        self.atol = self.atol if atol == None else atol
        self.pc_rtol = self.pc_rtol if pc_rtol == None else pc_rtol
        self.pc_atol = self.pc_atol if pc_atol == None else pc_atol
        self.initial_time = (self.initial_time if initial_time == None
                             else initial_time)

        if self._is_vivified():
            initial_time = remove_unit(self.initial_time, self.time_unit)
            ocaml.lam_ts_init(self.get_lam(), self.get_full_name(),
                              self.initial_time, self.rtol, self.atol)

            # XXX NOTE: HERE WE SHOULD ALSO SET THE PC TOLERANCES
            self.initialised = True

    def _ret_time(self, t):
        if self.time_unit is not None:
            return t*self.time_unit
        else:
            return t

    def advance_time(self, target_time, max_it=-1, exact_tstop=False):
        target_time = remove_unit(target_time, self.time_unit)

        if not self.initialised:
            self.initialise()

        final_t_su = \
          ocaml.lam_ts_advance(self.get_lam(), self.get_full_name(),
                               exact_tstop, target_time, max_it)

        return self._ret_time(final_t_su)

    def get_cvode(self):
        """Get the cvode object associated with the timestepper."""
        return ocaml.lam_get_ts_cvode(self.get_lam(), self.get_full_name())

    def get_num_steps(self):
        """Get the number of steps computed so far."""
        return int(ocaml.cvode_get_num_steps(self.get_cvode()))

    def get_last_dt(self):
        """Return the last step length."""
        return self._ret_time(ocaml.cvode_get_step_info(self.get_cvode())[0])



class Timesteppers(Group):
    pass
