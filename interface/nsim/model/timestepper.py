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

class Timestepper(ModelObj):
    type_str = "SundialsCVode"

    def __init__(self, name, x, dxdt,
                 eq_for_jacobian=None, time_unit=None):
        """
        Timestepper parameters:
            pc_rtol=None, pc_atol=None,
            max_order=2,
            krylov_max=None
        RHS:
            RHS='update_dmdt'
        Jacobi parameters:
            name_jacobian=None, # XXX TO BE OBSOLETED!
            jacobi_prealloc_diagonal=75,
            jacobi_prealloc_off_diagonal=45
        Parameters for computation of Jacobian:
            jacobi_eom=eq_rhs_jacobi_ts1,
            phys_field_derivs=[("PRIMARY",""),
                            ("PRIMARY",""),
                            ("OPERATOR","op_H_exch"),
                            ("IGNORE",""),
                            ("IGNORE","")],
            ['m','dmdt','H_total','H_anis','pin'], #check auxiliary fields for Jacobian are right
            ['v_m','v_dmdt','v_H_total','v_H_anis','v_pin'],
            nr_primary_fields=1,
        """
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

    def advance_time(self, target_time, max_it=-1, exact_tstop=False):
        if self.time_unit != None:
            try:
                target_time = float(target_time/self.time_unit)
            except:
                raise ValueError("advance_time got wrong units for the time. "
                                 "Expected %s, but got %s."
                                 % (self.time_units, target_time))

        print "advance_time: %s" % target_time
        lam = self.get_lam()
        ts_full_name = self.get_full_name()
        ocaml.lam_ts_init(lam, ts_full_name, 0.0, 1e-6, 1e-6)
        final_t_su = \
          ocaml.lam_ts_advance(lam, ts_full_name,
                               exact_tstop, target_time, max_it)

        if self.time_unit != None:
            return final_t_su*self.time_unit
        else:
            return final_t_su

class Timesteppers(Group):
    pass
