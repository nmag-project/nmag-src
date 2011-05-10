'''
Nmag wrapper for M3S. This module defines a FDSimulation class which provides
the Nmag interfaces (SimulationCore) to the M3S micromagnetic simulation
package developed by Massoud Najafi at the University of Hamburg.

Copyright |copy| 2009

:Author: M Franchin, M Najafi

:License: GNU Public License (GPL)
'''

# system imports
import os, logging
from numpy import array
from types import FunctionType

# m3s imports
from m3s.mesh import Mesh
from m3s.constants import MU_ZERO
from m3s.basic import average, norm_to
from m3s.zeeman_field import Zeemanfield
from m3s.demag.demagfield import Demagfield
from m3s.exchangefield import Exchangefield
from m3s.basic_micromagnetic_model import BasicMicromagneticModel
from m3s.integrator import Integrator,RKIntegrator
from m3s.field_array import FieldArray

# nsim imports (modules nsim.*)
import nsim
import nsim.map_terminals
from nsim.colwriter import ColDescriptor, ColWriter
import nsim.timings

# nmag imports (modules nmag.*)
import nmag
from nmag import SI
from material import MagMaterial # finally nmag will use this one anyway!
from simulation_core import SimulationCore
from constants import degrees_per_ns
from nmag_exceptions import *
import omf

timer1 = nsim.timings.Timer('nmag-fd')

log = logging.getLogger('nmag')

class FDSimulation(SimulationCore):
    def __init__(self, name=None, do_demag=True,model=BasicMicromagneticModel):
        SimulationCore.__init__(self, name=name, do_demag=do_demag,
                                id="FD Simulation class")
        self.initialised = False
        self.field_array = None

        if self._restarting:
            raise NotImplementedError('Restarting capabilities are not '
                                      'implemented by the FD code, yet!')

        # These are our default simulation units.
        self.units = nsim.su_units.SimulationUnits({'A': 1.0, 'kg': 1.0,
                                                    'm': 1.0, 's': 1.0,
                                                    'cd': 1.0, 'K': 1.0,
                                                    'mol': 1.0})

        # The FD mesh
        self.fd_mesh = None

        self.zeeman = None
        self.ts_rel_tol = 1e-0
        self.ts_abs_tol = 1e-0
        self.ts_tstep0 = 1e-15
        self.integrator = None
        self.dof = None
        self.model_cls = model

        self.stopping_dm_dt = self.units.of(1.0*degrees_per_ns)

    def _fd_na(self, reason):
        """Called internally to raise error messages which are raised
        frequently"""
        msgs = {
          "multi-material": "The finite difference version of Nmag does "
                            "support multiple materials, yet. You should "
                            "define and use just one MagMaterial!"
        }
        try:
            msg = msgs[reason]
        except:
            msg = "Unknown error!"
        raise NotImplementedError(msg)

    def _setup(self, doing=None):
        assert not self.initialised, \
               "FDSimulation._setup has already been called"

        if self.fd_mesh == None:
            if doing != None:
                doing = " " + doing
            raise NmagUserError("You should define a mesh before%s!" % doing)

        if len(self.materials) < 1:
            raise NmagUserError("You should specify at least one material!")
        elif len(self.materials) > 1:
            self._fd_na("multi-material")

        mat = self.materials[0]
        Ms = self.units.of(mat.Ms, compatible_with=SI('A/m'))
        A = self.units.of(mat.exchange_coupling, compatible_with=SI('J/m'))
        gamma = self.units.of(mat.llg_gamma_G, compatible_with=SI('m/s A'))
        alpha = self.units.of(mat.llg_damping, compatible_with=SI(1))

        Hext = array([0.0, 0.0, 0.0])
        Hext = Hext/MU_ZERO

        self.zeeman = Zeemanfield(self.fd_mesh, Hext)
        timer1.start('exchange init')
        exchange = Exchangefield(self.fd_mesh, Ms, A)
        timer1.stop('exchange init')

        field_dict = {'H_ext':self.zeeman, 'H_exch':exchange}
        timer1.start('demag init')
        if self.do_demag:
            demag = Demagfield(self.fd_mesh, init_random=False,
                               expansion="opt")
            field_dict['H_demag'] = demag
        self.field_array = FieldArray(field_dict)
        timer1.stop('demag init')

        self.fd_model = \
          self.model_cls(self.fd_mesh, self.field_array,
                                  Ms, gamma, alpha,
                                  precession_enabled=mat.do_precession)

        self.integrator = Integrator(self.fd_model, self.ts_rel_tol,
                                     self.ts_abs_tol, self.ts_tstep0,
                                     method="rk", order=2)

        self.initialised = True

    def _setup_if_needed(self, doing=None):
        if self.initialised:
            return
        self._setup(doing=doing)

    def set_params(self, stopping_dm_dt=None,
                   ts_rel_tol=None, ts_abs_tol=None):
        if stopping_dm_dt != None:
            self.stopping_dm_dt = self.units.of(stopping_dm_dt,
                                                compatible_with=SI('1/s'))
        reset_tols = False
        if ts_rel_tol != None:
            self.ts_rel_tol = ts_rel_tol
            reset_tols = True

        if ts_abs_tol != None:
            self.ts_abs_tol = ts_abs_tol
            reset_tols = True

        if reset_tols and self.integrator != None:
            raise NotImplementedError("Tolerances cannot be set after the "
                                      "integrator has been created. Try to "
                                      "call set_params before create_mesh.")

    def reinitialise(self, initial_time=None):
        pass

    def set_m(self, values, subfieldname=None):
        print "FIXME: set_m, set_H_ext etc are not fully implemented, yet!"

        if subfieldname != None:
            self._fd_na("multi-material")
        self._setup_if_needed("setting the magnetisation")
        factor = self.fd_model.Ms
        if not type(values) is FunctionType:
            try:
                values, u = nsim.map_terminals.SI_vector(values)
                factor *= self.units.of(u, compatible_with=SI(1))
            except:
                pass

        dof = self.fd_mesh.create_vectorfield(values,'magnetic')
        dof.shape = self.fd_mesh.get_vectorfield_flat_shape()
        norm_to(self.fd_mesh, dof, factor)
        dof = dof.reshape(-1)
        self.dof = dof
        self.integrator.set_initial_values(dof, 0.0)

    def set_H_ext(self, values, unit=None):
        v, u = nsim.map_terminals.SI_vector(values)
        factor = self.units.of(u, compatible_with=SI('A/m'))
        v_su = [vi*factor for vi in v]
        self._setup_if_needed('setting the external field')
        self.zeeman.set_field(v_su)

    def set_pinning(self, values):
        print "set_pinning not fully implemented, yet!"

    def create_mesh(self, cell_nums, cell_sizes, materials,
                    regions=None, origin=(0, 0, 0)):
        print "FIXME: create_mesh: boundary not included and not conforming " \
              "to the interface specification: see SimulationCore class!"
        cell_nums_su = [int(self.units.of(ni, compatible_with=SI(1)))
                        for ni in cell_nums]
        cell_sizes_su = [self.units.of(si, compatible_with=SI('m'))
                         for si in cell_sizes]
        origin_su = [self.units.of(si, compatible_with=SI('m'))
                     for si in origin]
        self.fd_mesh = Mesh(cells=cell_nums_su, cell_sizes=cell_sizes_su,regions=regions,origin=origin_su)
        if type(materials) == list:
            self.materials = materials
        else:
            self.materials = [materials]

        self._setup_if_needed()

    create_mesh.__doc__ = SimulationCore.create_mesh.__doc__

    def advance_time(self, target_time, max_it=-1):
        if not self.initialised:
            self._setup(doing="calling advance_time")

        time_su = self.units.of(target_time, compatible_with=SI('s'))
        num_steps_0 = self.integrator.used_steps
        time_0 = self.integrator.t
        self.integrator.integrate(time_su, max_it,
                                  end_dmdt=self.stopping_dm_dt)
        delta_steps = self.integrator.used_steps - num_steps_0
        delta_time = self.integrator.t - time_0

        time_su2si = self.units.conversion_factor_of(SI("s"))
        delta_time_si = time_su2si*delta_time

        self.clock['time'] += delta_time_si
        self.clock['stage_time'] += delta_time_si
        self.clock['step'] += delta_steps
        self.clock['stage_step'] += delta_steps
        self.clock['convergence'] = self.integrator.converged
        return self.clock['time']

    def get_subfield_average(self, field_name, subfield_name=None):
        material = self.materials[0]
        if subfield_name != None and subfield_name != material.name:
            self._fd_na("multi-material")

        if field_name == 'm':
            Ms = self.units.of(material.Ms)
            return list(average(self.get_M_values())/Ms)

        elif field_name == 'M':
            return list(average((self.get_M_values()))*SI('A/m'))

        elif field_name in ['H_ext', 'H_exch', 'H_demag']:
            try:
                H, E, H_avg = \
                  self.field_array.calculate_field(self.dof, 0.0, field_name,
                                                   True, True)
                return list(H_avg*SI('A/m'))
            except:
                return None

        else:
            return None

    def load_m_from_file(self, filename, format=None):
        print "FIXME: debug load_m_from_file"

        if format == None:
            extension = filename.split(os.extsep)[-1].lower()
            format = extension

        if not format in ['omf', 'ovf']:
            # report a warning, not an error: we do not want the user to find
            # that the simulation stopped just because the format was not the
            # right one (remember one may save at the end of the simulation)
            print ("FIXME: MAKE ME A LOG: WARNING: format not supported. "
                   "Using OMF format to save the field.")

        data, X = omf.load_omf(filename)
        self.set_m(data)

    def save_m_to_file(self, filename, format=None):
#        print "FIXME: save_m_to_file: Add a LOG!!!"
        if format == None:
            extension = filename.split(os.extsep)[-1].lower()
            format = extension

        if not format in ['omf', 'ovf']:
            # report a warning, not an error: we do not want the user to find
            # that the simulation stopped just because the format was not the
            # right one (remember one may save at the end of the simulation)
            print ("FIXME: MAKE ME A LOG: WARNING: format not supported. "
                   "Using OMF format to save the field.")

        header_dict = omf.new_header()
        nx, ny, nz = self.fd_mesh.cells
        sx, sy, sz = self.fd_mesh.cellsizes
        ox, oy, oz = self.fd_mesh.origin
        data_to_set = \
         [('xnodes', nx), ('ynodes', ny), ('znodes', nz),
          ('xmin', ox), ('ymin', oy), ('zmin', oz),
          ('xmax', ox+nx*sx), ('ymax', oy+ny*sy), ('zmax', oz+nz*sz),
          ('xstepsize', sx), ('ystepsize', sy), ('zstepsize', sz),
          ('xbase', 0.5*sx), ('ybase', 0.5*sy), ('zbase', 0.5*sz)]
        omf.add_header_data(header_dict,
                            [('Segment', 0), ('Header', 0)],
                            data_to_set)
        omf.write_omf(filename, self.get_M_values(), header_dict=header_dict)

    save_m_to_file.__doc__ = SimulationCore.save_m_to_file.__doc__

    def save_data(self, fields=None, avoid_same_step=False):
        SimulationCore.save_data(self, fields, avoid_same_step)
        if fields != None:
            if 'm' in fields or 'all' in fields:
                self.clock['id'] += 1
                filename = 'm-%012d.omf' % self.id
                self.save_m_to_file(filename)

    save_data.__doc__ = SimulationCore.save_data.__doc__

    def get_M_values(self):
        return self.integrator.M

