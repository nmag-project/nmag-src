# Nmag micromagnetic simulator
# Copyright (C) 2010, 2011 University of Southampton
# Hans Fangohr, Thomas Fischbacher, Matteo Franchin and others
#
# WEB:     http://nmag.soton.ac.uk
# CONTACT: nmag@soton.ac.uk
#
# AUTHOR(S) OF THIS FILE: Matteo Franchin
# LICENSE: GNU General Public License 2.0
#          (see <http://www.gnu.org/licenses/>)

"""
The fifth version of the nmag interface, completely based on nsim.model.
"""

# System imports
import os
import math
import logging

# Other imports
import numpy

# Nmag imports (modules nmag.*)
from nmag_exceptions import *
import nmesh
from simulation_core import SimulationCore
from nsim.model import *
from nsim.su_units import SimulationUnits
from nsim.si_units.si \
  import SI, bohr_magneton, positron_charge, degrees_per_ns, mu0
import nsim.si_units.si as si
import nfem
import nfem.hdf5_v01 as hdf5
import convergence
import anisotropy5
from anisotropy5 import Anisotropy

import ocaml

# Get some object into this namespace
from material import MagMaterial

# debug variables to study timing
from nsim.timings import Timer

timer1 = Timer("save-data")

# Get the logger
lg = logging.getLogger('nmag')

# XXX NOTE, NOTE, NOTE: Temporary solution
uniaxial_anisotropy = anisotropy5.UniaxialAnisotropy

# These are our default simulation units -- however, they can be
# modified by the user (by setting nmag.simulation_units manually
# before creating a (or in any case the first) Simulation object).
simulation_units = \
  SimulationUnits({'A': 1e-3, 'kg': 1e-27, 'm': 1e-9, 's': 1e-12,
                   'cd': 1.0, 'K': 1.0, 'mol': 1.0})

H_unit = SI(1e6, "A/m")   # simulation_units.conversion_factor_of(SI("A/m"))
E_unit = SI(1e6, "J/m^3") # simulation_units.conversion_factor_of(SI("J/m^3"))


def _su_of(u):
  """Internal: return the units for u.
  Example: _su_of(SI("m")) typically returns SI(1e-9, "m")
    (but the answer depends on the value given for the unit_length parameter
    in Simulation.load_mesh)
  """
  return simulation_units.conversion_factor_of(u)

def _default_qc(q_type, q_name, *args, **named_args):
    return q_type(q_name, *args, **named_args)

def canonical_subfieldname(fieldname, subfieldname):
    prefix = fieldname + "_"
    return (subfieldname[len(prefix):]
            if subfieldname.startswith(prefix)
            else subfieldname)


# Move this somewhere else, and implement it
class MemoryReport(object):
    def __init__(self, identifier, start=True):
        self.identifier = identifier
        self.start()

    def start(self):
        pass

    def finish(self):
        pass


class Simulation(SimulationCore):
    def __init__(self, name=None, do_demag=True, do_sl_stt=False):
        SimulationCore.__init__(self, name=name, do_demag=do_demag,
                                id="FE Simulation class")

        # General settings
        self.do_sl_stt = do_sl_stt      # Compute Slonczewski STT

        # Mesh stuff...
        self.mesh = None                # The mesh object
        self.region_name_list = None    # List of region names (corresponding
                                        #   to region 1, 2, ...)
        self.region_name_of_id = None   # Mapping ID -> region_name[ID]
        self.region_id_of_name = None   # region_name -> ID[region_name]
        self.mats_of_region_name = None # region_name -> material[region_name]
        self.mat_of_mat_name = None     # mat_name -> material[mat_name]
        self.materials = None

        # The physics...
        self._model = None              # The nsim.model.Model object

        # XXX NOTE, NOTE, NOTE:
        # The following two things are used to compute the norm of dm/dt
        # we should do this better: not on the master node!
        self._previous_m_field = None
        self._norm_dist_fun_m = None
        # Object used to check the convergence criterion and communicate to
        # the user how convergence is going.
        self.convergence = convergence.Convergence()
        self.stopping_dm_dt = 1.0*degrees_per_ns
        self.max_dm_dt = None

        # How the micromagnetic quantities will be constructed, i.e. a
        # dictionary mapping quantity name -> quantity_type, where
        # quantity_type is the class to use for construction.
        # This dictionary is provided for the user to influence how the
        # quantities are built. Quantities missing from here cannot be
        # influenced by the user in their construction.
        self.quantities_types = \
          {"H_ext": SpaceField,
           "P": Constant,
           "xi": Constant,
           "alpha": Constant,
           "Ms": Constant,
           "gamma_G": Constant,
           "exchange_factor": Constant}

        # Used by _set_qs_from_materials to set the Constant quantities to
        # their initial value, as set on a per-material basis in the given
        # MagMaterial objects.
        self._qs_from_material_setters = {}

    def get_model(self):
        if self._model != None:
            return self._model
        else:
            raise NmagUserError("Cannot get the Model object. You first "
                                "should load a mesh using the "
                                "Simulation.load_mesh method.")

    model = property(get_model)

    def declare(self, attrs, *objs):
        """Provide some attributes for the given objects. 'attrs' is a list
        of the attributes (a list of strings) to associate to the objects
        'objs' (a list of strings). Example:

          sim.declare("space field", "llg_damping")

        Can be used to declare that the damping should be a field which can
        change in space, rather than just a constant.
        """

        # Available Quantity TypeS
        aqts = {"constant": Constant, "spacefield": SpaceField,
                "timefield": TimeField, "spacetimefield": SpaceTimeField}
        dos = self.quantities_types # Declarable ObjectS
        recognized_attrs = {}

        declare_usage = ("USAGE: simulation.declare(attrs, objs) where attrs "
                         "is a list of attributes (strings) to associate to "
                         "the objects whose names are in objs (a list of "
                         "strings).")

        # NOTE: str has not iter attribute: if user gives a string then we
        #   interpret it as [string].
        attrs = (attrs if hasattr(attrs, "__iter__") else [attrs])

        for obj_name in objs:
            if obj_name in dos:
                assert type(obj_name) == str, declare_usage
                qt = []
                for attr in attrs:
                    assert type(attr) == str, declare_usage
                    fmt_attr = attr.lower().replace(" ", "")
                    if fmt_attr in aqts:
                        qt.append(fmt_attr)
                        self.quantities_types[obj_name] = aqts[fmt_attr]

                    else:
                        msg = "Unrecognized attribute '%s'" % attr
                        raise NmagUserError(msg)

            else:
                msg = "Cannot declare '%s'" % obj_name
                raise NmagUserError(msg)

        if len(qt) > 1:
            msg = "Conflicting attributes %s." % (", ".join(qt))
            raise NmagUserError(msg)

    def load_mesh(self, filename, region_names_and_mag_mats, unit_length,
                  do_reorder=False, manual_distribution=None):
        lg.info("Reading mesh from %s, unit_length is %s. "
                % (filename, unit_length))

        assert unit_length == SI(1e-9, "m"), \
          ("Error in Simulation.load_mesh: Nmag5 only supports unit_length="
          "SI(1e-9, \"m\") for now.")

        if self.mesh != None:
            raise NmagUserError("Mesh is already present!")

        # Remember the mesh unit length for writing data files
        self.mesh_unit_length = unit_length

        do_distribute = not manual_distribution
        if manual_distribution != None:
            lg.info("load_mesh: will use manual distribution of nodes (%s)"
                    % manual_distribution)

        # Need to convert the mesh positions into simulation units. The mesh
        # positions are SI when the unit_length is multiplied with the actual
        # coordinates. We thus only have to convert the unit_length SI object
        # into simulation units:
        factor = simulation_units.of(unit_length, compatible_with=SI("m"))

        lg.log(15, "User sets mesh_unit_length to be %s (in load_mesh)"
                   % self.mesh_unit_length)

        mr = MemoryReport("Simulation.load_mesh: nmesh.load")
        self.mesh = nmesh.load(filename, do_reorder, do_distribute)
        mr.finish()

        if manual_distribution:
            ocaml.mesh_set_vertex_distribution(self.mesh.raw_mesh,
                                               manual_distribution)

        if factor != 1.0:
            lg.debug("Scaling mesh coordinates with factor %g" % factor)
            self.mesh.scale_node_positions(factor)
        else:
            lg.debug("No need to scale mesh coordinates")

        self.region_name_list = map(lambda x: x[0], region_names_and_mag_mats)
        if len(self.region_name_list) != self.mesh.numregions - 1:
            msg = ("Found inconsistency between the mesh you loaded and the "
                   "provided list of (region_name, materials). The mesh has "
                   "%d regions but the provided list has %d pairs."
                   % (self.mesh.numregions - 1, len(self.region_name_list)))
            raise NmagUserError(msg)

        region_name_of_id = {}
        i = 1
        for name, mat in region_names_and_mag_mats:
            region_name_of_id[i] = name
            i += 1
        self.region_name_of_id = region_name_of_id

        # Complementary data structure to self.region_name_list. I.e. if I
        # know the region's name but want to know the id, then I can use
        # this dictionary.
        region_id_of_name = {}
        for id, name in enumerate(['vacuum'] + self.region_name_list):
            region_id_of_name[name] = id
        self.region_id_of_name = region_id_of_name

        lg.debug("self.region_id_of_name: = %s",  region_id_of_name)

        mats_of_region_name = {}
        mat_of_mat_name = {}
        for name, mag_mat in region_names_and_mag_mats:
            lg.info("Adding region '%s' to '%s' simulation object"
                    % (name, self.name))
            if type(mag_mat) == list:
                mats = mag_mat
            else:
                mats = [mag_mat]
            if name in mats_of_region_name:
                msg = ("Bad usage of the 'load_mesh' method! Your list of "
                       "(region_name, materials) contains two regions with "
                       "the same name '%s'. Different regions must have "
                       "different names!" % name)
                raise NmagUserError(msg)
            mats_of_region_name[name] = mats

            # ensure we have all materials registered in the simulation context...
            for m in mats:
                mat_of_mat_name[m.name] = m

        self.mats_of_region_name = mats_of_region_name
        self.mat_of_mat_name = mat_of_mat_name
        self.materials = self.mat_of_mat_name.values()

        self._create_model()

    def _quantity_creator(self, q_type, q_name, *args, **named_args):
        q_type = self.quantities_types.get(q_name, q_type)
        return _default_qc(q_type, q_name, *args, **named_args)

    def _create_model(self):
        # Create the model object
        model_name = "mumag_%s" % self.name
        region_materials = [[mat.name for mat in self.mats_of_region_name[name]]
                            for name in self.region_name_list]
        mu = 1e-9 # NOTE, NOTE, NOTE we should look into this
        region_materials = [[]] + region_materials
        self._model = model = Model(model_name, self.mesh, mu, region_materials)

        # Now add the different parts of the physics
        contexts = []
        _add_micromagnetics(model, contexts, self._quantity_creator)
        _add_exchange(model, contexts, self._quantity_creator)
        _add_demag(model, contexts, self._quantity_creator, self.do_demag)
        _add_stt_zl(model, contexts, self._quantity_creator)
        _add_stt_sl(model, contexts, self._quantity_creator,
                    do_sl_stt=self.do_sl_stt)
        _add_llg(model, contexts, self._quantity_creator, sim=self)
        _add_energies(model, contexts, self._quantity_creator)

        # Set the values of the constants in the micromagnetic model
        self._set_qs_from_materials()

        self._add_anis()

        # Now build the model
        model.build()

    def _set_qs_from_materials(self):
        # We now go through all the materials and build the initial values for
        # the quantities corresponding to the various parameters, gamma_G,
        # alpha, Ms, etc.
        qs = self.model.quantities._by_name

        def set_quantity_value(name, name_in_mat=None):
            name_in_mat = name_in_mat or name
            if name in qs:
                v = Value()
                v_was_set = False
                for mat_name, mat in self.mat_of_mat_name.iteritems():
                    if hasattr(mat, name_in_mat):
                        v_for_mat = getattr(mat, name_in_mat)

                    elif name_in_mat in self._qs_from_material_setters:
                        setter = self._qs_from_material_setters[name_in_mat]
                        v_for_mat = setter(mat)

                    else:
                        continue

                    v.set(mat_name, v_for_mat)
                    v_was_set = True

                if v_was_set:
                    qs[name].set_value(v)

        set_quantity_value("Ms")
        set_quantity_value("gamma_G", "llg_gamma_G")
        set_quantity_value("alpha", "llg_damping")
        set_quantity_value("norm_coeff", "llg_normalisationfactor")
        set_quantity_value("exchange_factor")
        set_quantity_value("P", "llg_polarisation")
        set_quantity_value("xi", "llg_xi")
        set_quantity_value("sl_P")
        set_quantity_value("sl_d")
        set_quantity_value("inv_alpha2")

    def _add_anis(self):
        # Now we run over the materials and create the anisotropy CCode
        has_anisotropy = False
        set_H_anis = CCode("set_H_anis", inputs=["m"], outputs=["H_anis"],
                           auto_dep=True)
        set_E_anis = CCode("set_E_anis", inputs=["m"], outputs=["E_anis"],
                           auto_dep=True)
        for mat_name, mat in self.mat_of_mat_name.iteritems():
            # The first thing we have to do is to clear H_anis_mat
            a = mat.anisotropy
            if isinstance(a, Anisotropy):
                # We enable anisotropy stuff only if necessary
                has_anisotropy = True

                # First we add the anisotropy quantities to the model
                if not a.quantities_defined:
                    a._define_quantities()
                    assert a.quantities_defined, \
                      ("Anistropy.quantity_defined is False. Did you call "
                       "Anisotropy._define_quantities from your custom "
                       "_define_quantities method?")

                self.model.add_quantity(*a.quantities._all)

                # Now we add the anisotropy C code to the CCode object
                set_H_anis.append(a.get_H_equation(), materials=mat)
                set_E_anis.append(a.get_E_equation(), materials=mat)

            elif a != None:
                raise NmagUserError("The material anisotropy for '%s' is not "
                                    "an Anisotropy object." % mat)
        if has_anisotropy:
            H_anis = SpaceField("H_anis", [3], subfields=True, unit=H_unit)
            E_anis = SpaceField("E_anis", [], subfields=True, unit=E_unit)
            self.model.add_quantity(H_anis, E_anis)
            self.model.add_computation(set_H_anis, set_E_anis)

        else:
            # We add anyway the H_anis field, since the computation of H_total
            # requires it. XXX NOTE, NOTE, NOTE: should make this a Constant.
            qc = self._quantity_creator
            H_anis = qc(SpaceField, "H_anis", [3], subfields=True,
                        value=Value([0, 0, 0]), unit=H_unit)
            E_anis = qc(SpaceField, "E_anis", [], subfields=True,
                        value=Value(0), unit=E_unit)
            self.model.add_quantity(H_anis, E_anis)

    def get_subfield_average(self, field_name, mat_name):
        f = self.model.quantities._by_name.get(field_name, None)
        if f == None:
            return None
        else:
            # XXX NOTE: the following should be cleaned up!!!
            full_mat_name = "%s_%s" % (field_name, mat_name)
            return f.compute_average().as_constant(where=full_mat_name)

    def set_params(self, stopping_dm_dt=None,
                   ts_rel_tol=None, ts_abs_tol=None):
        if stopping_dm_dt != None:
            if not SI("1/s").is_compatible_with(stopping_dm_dt):
                raise ValueError("The value given for stopping_dm_dt must "
                                 "have units of SI(\"1/s\")!")
            self.stopping_dm_dt = stopping_dm_dt

        ts = self.model.timesteppers["ts_llg"]
        ts.initialise(rtol=ts_rel_tol, atol=ts_abs_tol)

    def set_H_ext(self, values, unit=None):
        v = Value(values) if unit == None else Value(values, unit)
        self.model.quantities["H_ext"].set_value(v)

    def set_m(self, values, subfieldname=None):
        if subfieldname != None:
             sfn = canonical_subfieldname("m", subfieldname)
             v = Value(sfn, values)
        else:
             v = Value(values)
        self.model.quantities["m"].set_value(v)

    def set_pinning(self, values):
        #v = Value(values, subfieldname)
        v = Value(values) # <-- we should make this material dependent
        self.model.quantities["pin"].set_value(v)

    def set_current_density(self, values, unit=None):
        v = Value(values) if unit == None else Value(values, unit)
        self.model.quantities["current_density"].set_value(v)

    def reinitialise(self,
                     rel_tolerance=None,
                     abs_tolerance=None,
                     initial_time=None):
        ts = self.model.timesteppers["ts_llg"]
        ts.initialise(rtol=rel_tolerance, atol=abs_tolerance,
                      initial_time=initial_time)

    def advance_time(self, target_time, max_it=-1, exact_tstop=True):
        ts = self.model.timesteppers["ts_llg"]

        # XXX NOTE, NOTE, NOTE: we should improve the following
        m = self.model.quantities["m"]
        previous_m_field = self._previous_m_field
        if previous_m_field == None:
            self._previous_m_field = previous_m_field = \
              ocaml.raw_make_field(m.mwe, [], "", m.name + "_previous")
        ocaml.lam_get_field(self.model.lam, previous_m_field, "v_m")

        t0 = ts.get_time()
        step0 = ts.get_num_steps()
        t = ts.advance_time(target_time, max_it=max_it,
                            exact_tstop=exact_tstop)
        step = ts.get_num_steps()
        delta_step = step - step0
        delta_time = t - t0

        # The following code should also be improved!
        ocaml.lam_get_field(self.model.lam, m.master, "v_m")
        compute_norm = self._norm_dist_fun_m
        if compute_norm == None:
            self._norm_dist_fun_m = compute_norm = \
              ocaml.mwe_norm_dist_fun(m.mwe)
        delta_norms = compute_norm(m.master, previous_m_field)
        max_dm = max([max_dm for _, max_dm, _ in delta_norms])
        if delta_time > 0.0:
            self.max_dm_dt = max_dm/delta_time

        self.clock.step += delta_step
        self.clock.time += delta_time
        self.clock.stage_step = step
        self.clock.stage_time = t
        self.clock.last_step_dt_si = ts.get_last_dt()
        return t

    def is_converged(self):
        """
        Returns True when convergence has been reached.
        """
        lg.debug("Entering is_converged()")
        self.clock.convergence = False
        if self.max_dm_dt != None:
            converged, new_tol_factor = \
              self.convergence.check(self.step,
                                     self.max_dm_dt, self.stopping_dm_dt,
                                     0.0) #self.ts_in_lam.tol_factor)
            #if new_tol_factor != None and self._adjust_tolerances == True:
            #    print "Scaling the tolerances: factor = %f!" % new_tol_factor
            #    self.ts_in_lam.tol_factor = new_tol_factor
            #    raw_input()
            #    self.ts_in_lam.is_initialised = False
            self.clock.convergence = converged
            return converged

        else:
            lg.debug("is_converged(): self.max_dm_dt == None -> False")
            return False

    def _save_averages(self, *args, **named_args):
        # XXX NOTE NOTE NOTE:
        # Temporary fix for the dependency deficiencies of Nmag5
        # (to be solved in a better way)
        for en in ["en_demag", "en_exch", "en_ext", "set_E_anis", "en_total",
                   "eq_M"]:
            if en in self.model.computations:
                self.model.computations[en].execute()
        return SimulationCore._save_averages(self, *args, **named_args)

    def save_data(self, fields=None, avoid_same_step=False):
        """
        Save the *averages* of all defined (subfields) into a ascii
        data file. The filename iscomposed of the simulation name
        and the extension ``_dat.ndt``. The
        extension ``ndt`` stands for Nmag Data Table (analog to OOMMFs
        ``.odt`` extension for this kind of data file.

        If ``fields`` is provided, then it will also save the spatially
        resolved fields to a file with extensions ``_dat.h5``.

        :Parameters:
          `fields` : None, 'all' or list of fieldnames

            If None, then only spatially averaged data is saved into ``*ndt``
            and ``*h5`` files.

            If ``all`` (i.e. the string containing 'all'), then all fields are
            saved.

            If a list of fieldnames is given, then only the selected
            fieldnames will be saved (i.e. ['m','H_demag']).

          `avoid_same_step` : bool

            If ``True``, then the data will only be saved if the
            current ``clock.step`` counter is different from the
            step counter of the last saved configuration. If
            ``False``, then the data will be saved in any
            case. Default is ```False```. This is internally used by
            the hysteresis command (which uses ``avoid_same_step ==
            True``) to avoid saving the same data twice.

            The only situation where the step counter may not have
            changed from the last saved configuration is if the user
            is modifying the magnetisation or external field manually
            (otherwise the call of the time integrator to advance or
            relax the system will automatically increase the step
            counter).
        """

        lg.debug("Entering save_data, fields=%s, avoid_same_step=%s"
                  % (fields, avoid_same_step))
        timer1.start('save_data')

        # Get filenames
        h5filename = self._h5filename()

        # Check whether we have saved this step already:
        has_step = hdf5.average_data_has_step(h5filename, self.clock.step)

        if has_step and avoid_same_step:
            # In this case we have written the data already.
            lg.debug("save_data: No need to save step %d to %s "
                     "(saved already)" % (self.clock.step, h5filename))

        else:
            # Increase unique identifier:
            self.clock.id += 1
            lg.info("save_data(): id->id+1=%d, fields=%s"
                    % (self.clock.id, str(fields)))

            # first compute averages
            timer1.start('_save_averages')
            self._save_averages()
            timer1.stop('_save_averages')

        # If any fields are given, save them
        if fields:
            timer1.start('_save_fields')
            if type(fields) == str:
                if fields.lower() == 'all':
                    fields = []
                else:
                    # Likely been given one field in a string (but not
                    # wrapped up in list)
                    fields = [fields]
            self._save_fields(filename=h5filename, fieldnames=fields)
            timer1.stop('_save_fields')

        timer1.stop('save_data')
        lg.debug("Leaving save_data")

    def _create_h5_file(self, filename):
        lg.debug("_create_h5_file: initial creation of '%s'" % filename)
        hdf5.create_file(filename)
        hdf5.tagfile(filename,'nsimdata','0.1')

        # Add simulation source code and config files (to be done)
        hdf5.save_simulation_files(filename, [])

        # Add mesh
        hdf5.add_mesh(filename, self.mesh, self.mesh_unit_length)

    def _save_fields(self, filename=None, fieldnames=[]):
        """
        Save fields for current time step into hdf5 file.

        :parameters:

          `filename` : string
            The filename of the hdf5 file. Recommended extension is ".h5"

          `fieldnames` : list of fieldname strings

            A list of field names can be provided. All fields whose
            names are included in this list will be saved. An empty list
            suggests that all fields should be saved. (This is the default.)
        """

        timer1.start('save_fields')

        all_fieldnames =  self.get_all_field_names()

        if fieldnames == []:
            save_field_blacklist = ["grad_m"]
            fieldnames = [fn for fn in all_fieldnames
                          if not (fn in save_field_blacklist)]

        if filename == None:
            filename = self._h5filename()

        lg.log(15, "save_fields: About to save the following fields "
               "%s to %s" % (str(fieldnames), filename))

        if not os.path.exists(filename):
            lg.debug("save_fields: %s doesn't exist; will create it"
                     % filename)
            self._create_h5_file(filename)

        lg.log(15, "Saving field(s) %s into '%s'"
               % (str(fieldnames), filename))

        lg.debug("save_fields (%s): time is %s"
                 % (filename, self.clock.time.dens_str()))

        # Get medata for all fields, and for the ones we are meant to save.
        # We need the 'all fields' data in case the data file will be
        # created: we need to store all the dofsite metadata for all fields,
        # in case other fields will be saved later. (fangohr 28/05/2008)
        fields_to_save = {}
        all_fields_to_save = {}
        model_quantities = self.model.quantities._by_name
        for field_name in all_fieldnames:
            field_units = self.known_quantities_by_name[field_name].units
            q = model_quantities.get(field_name, None)
            if q != None:
                field_item = (q.master, field_units)
                if field_name in fieldnames:
                    fields_to_save[field_name] = field_item
                all_fields_to_save[field_name] = field_item

        # This is where the actual work (i.e. saving the data) is done. Also,
        # if the file is new, all the required meta data will be added.
        timer1.start('append_fields')
        hdf5.append_fields(filename, fields_to_save, all_fields_to_save,
                           self.clock.time, self.clock.step,
                           self.clock.stage, self.clock.id,
                           simulation_units)
        timer1.stop('append_fields')

        lg.info("Written fields %s data to %s" % (str(fieldnames), filename))
        timer1.stop('save_fields')

    def load_m_from_h5file(self, file_name):
        m = self.model.quantities["m"]
        names_and_shapes = nfem.data_doftypes(m.master)
        for subfield_name, shape in names_and_shapes:
            arr = hdf5.get_subfield_from_h5file(file_name, subfield_name)
            self.set_m(arr, subfieldname=subfield_name)

    def save_m_to_file(self, file_name):
        self._save_fields(file_name, fieldnames=["m"])

    def _probe_subfield(self, fieldname, pos, subfieldname, pos_units,
                        si_units):
        """XXX NOTE: temp hack, made to deliver a semi-working code to
        collaborators."""
        q = self.model.quantities.get(fieldname)
        if q == None:
            raise ValueError("Unknown field name '%s'" % fieldname)

        field = q.get_updated_master()
        factor = simulation_units.of(pos_units, compatible_with=SI("m"))
        su_pos = map(factor.__mul__, pos)
        su_data = ocaml.probe_field(field, subfieldname, su_pos)

        #if we get su_data == [], then there are two possible reasons
        # 1. the subfield is not defined at this position
        # 2. the subfield does generally not exist. We need to catch
        #    this point somewhere else and let the user know.
        if len(su_data) > 1:
            raise NmagInternalError("Didn't expect this: su_data=%s has more "
                                    "than one entry (tensor of rank2 or "
                                    "higher?)" % str(su_data))
        elif len(su_data) == 0:
            return None

        name, su_data = su_data[0]
        factor = 1.0/simulation_units.of(si_units)

        # If data is vector, multiply all components with factor
        if type(su_data) == list:
            return map(factor.__mul__, su_data)

        elif type(su_data) == float:
            return factor*su_data

        else:
            raise NmagInternalError("Didn't expect this type: %s"
                                    % type(su_data))

    def split_subfieldname(self, subfieldname):
        """XXX NOTE: temp hack, made to deliver a semi-working code to
        collaborators."""
        if subfieldname in self.known_quantities_by_name:
            return (subfieldname, None)

        else:
            fieldname, materialname = subfieldname.rsplit("_", 1)
            # ^^^ XXX NOTE: this is a hack at the moment
            if fieldname in self.known_quantities_by_name:
                return (fieldname, materialname)

            else:
                raise NmagUserError("Cannot find '%s'" % subfieldname)

    def probe_subfield_siv(self, subfieldname, pos, unit=None):
        """XXX NOTE: temp hack, made to deliver a semi-working code to
        collaborators."""
        fieldname, _ = self.split_subfieldname(subfieldname)
        q = self.known_quantities_by_name.get(fieldname, None)
        if q != None:
            return self._probe_subfield(fieldname, pos, subfieldname,
                                        SI("m"), q.units)

        else:  
            msg = "Couldn't find field for subfield '%s'" % subfieldname
            raise NmagUserError(msg)

    def _get_subfield(self, fieldname, subfieldname, si_units):
        """XXX NOTE: temp hack, made to deliver a semi-working code to
        collaborators."""
        field = self.model.quantities[fieldname].get_updated_master()
        factor = 1.0/simulation_units.of(si_units)
        return factor*numpy.array(ocaml.mwe_subfield_data(field, subfieldname))

    def get_subfield(self, subfieldname, units=None):
        """XXX NOTE: temp hack, made to deliver a semi-working code to
        collaborators."""
        fieldname, _ = self.split_subfieldname(subfieldname)
        q = self.known_quantities_by_name.get(fieldname, None)
        if units == None:
            units = q.units
        return self._get_subfield(fieldname, subfieldname, units)

    def fetch(self, fieldname):
        """XXX NOTE: temp hack, made to deliver a semi-working code to
        collaborators."""
        if fieldname == "H_demag":
            self.model.computations["set_H_demag"].execute()
        elif fieldname == "H_exch":
            if "H_exch" in self.model.computations:
                self.model.computations["H_exch"].execute()
            else:
                self.model.computations["exch"].execute()
        elif fieldname == "H_anis":
            if "H_anis" in self.model.computations:
                self.model.computations["H_anis"].execute()
        elif fieldname == "H_total":
            self.fetch("H_demag")
            self.fetch("H_exch")
            self.fetch("H_anis")
            self.model.computations["H_total"].execute()
        else:
            avail_fields = ["H_exch", "H_demag", "H_total"]
            avail_fields = ", ".join(map(lambda s: "'%s'" % s, avail_fields))
            raise ValueError("Don't know how to fetch '%s'. Available fields "
                             "are: %s." % (fieldname, avail_fields))



def _add_micromagnetics(model, contexts, quantity_creator=None):
    if "mumag" in contexts:
        return
    contexts.append("mumag")

    qc = quantity_creator or _default_qc
    m = qc(SpaceField, "m", [3], subfields=True, unit=SI(1))
    M = qc(SpaceField, "M", [3], subfields=True, unit=H_unit)
    Ms = qc(Constant, "Ms", subfields=True, unit=SI(1e6, "A/m"))
    mu0_const = Constant("mu0", value=Value(mu0), unit=SI(1e-6, "N/A^2"))
    model.add_quantity(m, M, Ms, mu0_const)

    eq = Equation("eq_M", "(M(i) <- Ms*m(i))_(i:3);")
    model.add_computation(eq)

def _add_exchange(model, contexts, quantity_creator=None):
    if "exch" in contexts:
        return
    contexts.append("exch")

    qc = quantity_creator or _default_qc

    # Constants and fields
    exchange_factor = qc(Constant, "exchange_factor", subfields=True,
                         unit=SI(1e-12, "m A"))
    H_exch = qc(SpaceField, "H_exch", [3], subfields=True, unit=H_unit)
    model.add_quantity(exchange_factor, H_exch)

    # The exchange operator
    if isinstance(exchange_factor, Constant):
        # If exchange_factor is a constant, we can include it in the operator
        # directly.
        op_str = "exchange_factor*<d/dxj H_exch(k)||d/dxj m(k)>, j:3,  k:3"
        op_exch = Operator("exch", op_str, cofield_to_field=True)
        model.add_computation(op_exch)

    else:
        # If not we have to create an intermediate equation...
        # NOTE: this is again something we would like nsim.model to do
        #   automatically (cover OCaml's deficiencies) but for now it is
        #   rather easy to do it this way.
        #raise NotImplementedError("Exchange factor can only be a constant "
        #                          "for now...")
        H_exch_tmp = qc(SpaceField, "H_exch_tmp", [3], subfields=True,
                        unit=H_unit)
        op_str = "<d/dxj H_exch_tmp(k)||d/dxj m(k)>, j:3,  k:3"
        op_exch = Operator("exch", op_str, cofield_to_field=True)
        eq_exch_tmp = \
          Equation("H_exch",
                   "(H_exch(i) <- exchange_factor*H_exch_tmp(i))_(i:3);")
        model.add_quantity(H_exch_tmp)
        model.add_computation(op_exch, eq_exch_tmp)


def _add_demag(model, contexts, quantity_creator=None, do_demag=True):
    qc = quantity_creator or _default_qc

    if do_demag == False or "demag" in contexts:
        # We add anyway the H_demag field, since the computation of H_total
        # requires it. XXX NOTE, NOTE, NOTE: should make this a Constant.
        H_demag = qc(SpaceField, "H_demag", [3], value=Value([0, 0, 0]),
                     unit=H_unit)
        model.add_quantity(H_demag)
        return

    contexts.append("demag")

    # Create the required quantities and add them to the model
    H_demag = qc(SpaceField, "H_demag", [3], unit=H_unit)
    phi1b = qc(SpaceField, "phi1b", [], restrictions="phi1b[outer]")
    phi2b = qc(SpaceField, "phi2b", [], restrictions="phi2b[outer]")
    phis = [qc(SpaceField, n, [], unit=SI("A"))
            for n in ["phi", "phi1", "phi2"]]
    rhos = [qc(SpaceField, n, [], unit=SI("A/m^2")) for n in ["rho", "rho_s"]]
    model.add_quantity(H_demag, phi1b, phi2b, *(phis + rhos))

    # Operators for the demag
    # NOTE: this is again a temporary hack to deliver the exchange extension
    #   in time to the collaborators.
    Ms = model.quantities["Ms"]
    if isinstance(Ms, Constant):
        op_div_m = Operator("div_m", "  Ms*<rho||d/dxj m(j)>"
                                     "+ Ms*<rho||D/Dxj m(j)>, j:3")
        command_adjust_rho = ["SCALE", "v_rho", -1.0]
        op_div_m_dest = "v_rho"

    else:
        #raise NotImplementedError("Ms can only be a constant for now...")
        assert isinstance(Ms, SpaceField), \
          "Ms can be either a Constant or a SpaceField for now!"
        rho_tmp = qc(SpaceField, "rho_tmp", [], subfields=True,
                     unit=SI("A/m^2"))
        op_div_m = Operator("div_m", "  <rho_tmp||d/dxj m(j)>"
                                     "+ <rho_tmp||D/Dxj m(j)>, j:3")
        eq_rho = Equation("eq_rho", "rho <- -Ms*rho_tmp;")
        # ^^^ NOTE XXX: the line above works only with one material!!!
        #     FIXME! (need explicit material sums)
        model.add_quantity(rho_tmp)
        model.add_computation(eq_rho)
        command_adjust_rho = ["GOSUB", eq_rho.get_prog_name()]
        op_div_m_dest = "v_rho_tmp"

    op_neg_laplace_phi = \
      Operator("neg_laplace_phi", "<d/dxj rho || d/dxj phi>, j:3",
               mat_opts=["MAT_SYMMETRIC", "MAT_SYMMETRY_ETERNAL"])
    op_grad_phi = Operator("grad_phi", "-<H_demag(j) || d/dxj phi>, j:3")
    op_laplace_DBC = \
      Operator("laplace_DBC",
               ("-<d/dxj phi[not outer] || d/dxj phi[not outer]>;"
                "phi[outer]=phi[outer], j:3"),
               mat_opts=["MAT_SYMMETRIC", "MAT_SYMMETRY_ETERNAL"],
               auto_dep=False)
    op_load_DBC = \
      Operator("load_DBC",
               ("<d/dxj phi[not outer] || d/dxj phi[outer]>;"
                "(L||R)=(*||phi[outer]), j:3"),
               auto_dep=False)

    # The two linear solver for the FEM/BEM
    ksp_solve_neg_laplace_phi = KSP("solve_neg_laplace_phi", op_neg_laplace_phi,
                                    ksp_type="gmres", pc_type="ilu",
                                    initial_guess_nonzero=True,
                                    rtol=1e-5, atol=1e-5, maxits=1000000,
                                    nullspace_has_constant=False,
                                    nullspace_subfields=["phi"])
    ksp_solve_laplace_DBC = KSP("solve_laplace_DBC", op_laplace_DBC,
                                ksp_type="gmres", pc_type="ilu",
                                initial_guess_nonzero=True,
                                rtol=1e-5, atol=1e-5, maxits=1000000)

    # The BEM matrix
    bem = BEM("BEM", mwe_name="phi", dof_name="phi")

    # The LAM program for the demag
    commands=[["SM*V", op_div_m, "v_m", op_div_m_dest],
              command_adjust_rho,
              ["SOLVE", ksp_solve_neg_laplace_phi, "v_rho", "v_phi1"],
              ["PULL-FEM", "phi", "phi[outer]", "v_phi1", "v_phi1b"],
              ["DM*V", bem, "v_phi1b", "v_phi2b"],
              ["SM*V", op_load_DBC, "v_phi2b", "v_rho_s"],
              ["SOLVE", ksp_solve_laplace_DBC, "v_rho_s", "v_phi2"],
              ["PUSH-FEM", "phi", "phi[outer]", "v_phi2b", "v_phi2"],
              ["AXPBY", 1.0, "v_phi1", 0.0, "v_phi"],
              ["AXPBY", 1.0, "v_phi2", 1.0, "v_phi"],
              ["SM*V", op_grad_phi, "v_phi", "v_H_demag"],
              ["CFBOX", "H_demag", "v_H_demag"]]
    prog_set_H_demag = \
      LAMProgram("set_H_demag", commands,
                 inputs=["m"], outputs=["rho", "phi", "H_demag"])

    model.add_computation(op_div_m, op_neg_laplace_phi, op_grad_phi,
                          op_laplace_DBC, op_load_DBC, ksp_solve_laplace_DBC,
                          ksp_solve_neg_laplace_phi, bem, prog_set_H_demag)

def _add_stt_zl(model, contexts, quantity_creator=None):
    """Spin transfer torque (Zhang-Li)
    See: S. Zhang and ...
    """
    if "stt" in contexts:
        return
    contexts.append("stt")

    qc = quantity_creator or _default_qc

    # Define STT-related quantities
    # Constants
    mu_B = qc(Constant, "mu_B", value=Value(bohr_magneton),
              unit=SI(1e-21, "m^2 A"))
    e = qc(Constant, "e", value=Value(positron_charge), unit=SI(1e-15, "A s"))
    model.add_quantity(mu_B, e)

    # Fields
    P = qc(Constant, "P", subfields=True, unit=SI(1))
    xi = qc(Constant, "xi", subfields=True, unit=SI(1))
    current_density = qc(SpaceField, "current_density", [3],
                         unit=SI(1e15, "A/m^2"))
    grad_m = qc(SpaceField, "grad_m", [3, 3], subfields=True,
                unit=SI(1e9, "1/m"))
    dm_dcurrent = qc(SpaceField, "dm_dcurrent", [3], subfields=True,
                     unit=SI(1e12, "1/s"))
    model.add_quantity(P, xi, current_density, grad_m, dm_dcurrent)

    # Define new computations
    # Gradient of m
    op_str = "<grad_m(i, j)||d/dxj m(i)>, i:3, j:%d" % model.dim
    op_grad_m = Operator("grad_m", op_str, cofield_to_field=True)

    # Derivative of m with respect to direction of the current (mul by factor)
    eq = ("(dm_dcurrent(i) <-"
          "   (-mu_B/(e*Ms*(1.0 + xi*xi)*(1.0 + alpha*alpha)))"
          "   * (grad_m(i, j)*current_density(j))_(j:3))_(i:3);")
    eq_dm_dcurrent = Equation("dm_dcurrent", eq)

    model.add_computation(op_grad_m, eq_dm_dcurrent)

def _add_stt_sl(model, contexts, quantity_creator=None, do_sl_stt=False):
    """Spin transfer torque (Slonczewski-Berger).
    See: J.C. Slonczewski, "Current-driven excitation of magnetic
         multilayers", JMMM 159 (1996) L1-L7
    See also:
       - http://wpage.unina.it/mdaquino/PhD_thesis/main/node47.html
       - L. Berger, "Emission of spin-waves by a magnetic multilayer
         traversed by a current", Phys. Rev. B 54 (1996), 9353-9358
    """
    if "sl" in contexts:
        return
    contexts.append("sl")

    qc = quantity_creator or _default_qc

    if not do_sl_stt:
        sl_coeff = qc(Constant, "sl_coeff", subfields=True, value=Value(0))
        model.add_quantity(sl_coeff)
        return

    # Constants
    hbar = qc(Constant, "hbar", value=Value(si.reduced_plank_constant),
              unit=_su_of(si.Joule*si.second))
    model.add_quantity(hbar)

    # Required quantities
    su_of_m = _su_of(SI("m"))
    sl_d = qc(Constant, "sl_d", subfields=True, unit=su_of_m)
    sl_current_density = qc(SpaceField, "sl_current_density",
                            unit=_su_of(SI("A/m^2")))
    sl_fix = qc(SpaceField, "sl_fix", [3], unit=SI(1))
    sl_P = qc(SpaceField, "sl_P", subfields=True, unit=SI(1))
    model.add_quantity(sl_P, sl_d, sl_current_density, sl_fix)

    # Auxiliary quantities
    sl_coeff = qc(SpaceField, "sl_coeff", subfields=True)
    model.add_quantity(sl_coeff)

    #eq = """
    #sl_coeff <- gamma_G*Ms/(1.0 + alpha*alpha)
    #            * sl_current_density/(mu0*Ms*Ms*e*sl_d/hbar)
    #            * 4.0/(sl_Pf*sl_Pf*sl_Pf
    #                   * (3.0 + m(0)*sl_fix(0)
    #                          + m(1)*sl_fix(1)
    #                          + m(2)*sl_fix(2))
    #                   - 16.0);"""
    #eq_sl_coeff = Equation("sl_coeff", eq)

    ccode = \
      ("double P_factor1 = sqrt($sl_P$)/(1.0 + $sl_P$),\n"
       "       P_factor3 = 4.0*P_factor1*P_factor1*P_factor1,\n"
       "       alpha = $alpha$, alpha2 = alpha*alpha;\n"
       "$sl_coeff$ = \n"
       "  $gamma_G$/(1.0 + alpha2)\n"
       "  * $sl_current_density$/($mu0$*$Ms$*$e$*$sl_d$/$hbar$)\n"
       "  * P_factor3/((3.0 + $m(0)$*$sl_fix(0)$\n"
       "                    + $m(1)$*$sl_fix(1)$\n"
       "                    + $m(2)$*$sl_fix(2)$)\n"
       "               - 4.0*P_factor3);\n")
    calc_sl_coeff = \
      CCode("calc_sl_coeff", inputs=["m"], outputs=["sl_coeff"], auto_dep=True)
    calc_sl_coeff.append(ccode)

    model.add_computation(calc_sl_coeff)

def _add_llg(model, contexts, quantity_creator=None, sim=None):
    if "llg" in contexts:
        return
    contexts.append("llg")

    qc = quantity_creator or _default_qc

    t_unit = SI(1e-12, "s")
    one = SI(1)
    invt_unit = 1/t_unit

    # Create the LLG constants
    gamma_G = qc(Constant, "gamma_G", subfields=True, unit=SI(1e6, "m/A s"))
    alpha = qc(Constant, "alpha", subfields=True, unit=one)
    norm_coeff = qc(Constant, "norm_coeff", subfields=True, unit=invt_unit)
    model.add_quantity(gamma_G, alpha, norm_coeff)

    # Create some fields required for the dynamics
    dmdt = qc(SpaceField, "dmdt", [3], subfields=True, unit=invt_unit)
    H_total = qc(SpaceField, "H_total", [3], subfields=True, unit=H_unit)
    H_ext = qc(SpaceField, "H_ext", [3], unit=H_unit)
    pin = qc(SpaceField, "pin", [], value=Value(1), unit=one)
    model.add_quantity(dmdt, H_total, H_ext, pin)

    # We get this out from the LLG as the OCaml parser cannot deal with
    # divisions (which becomes a problem when alpha is declared as a
    # SpaceField). For now this is the easiest thing to do...
    inv_alpha2 = qc(type(alpha), "inv_alpha2", subfields=True, unit=one)
    model.add_quantity(inv_alpha2)

    # If alpha is a constant, then we have to specify a function to set
    # inv_alpha2 (which will also be a constant). If alpha is a SpaceField
    # then we have to provide an equation to compute it.
    # NOTE: In the end, we would like nsim.model to find out which quantities
    #  the user did not define and whether they can be implicitly defined from
    #  the equations he provided. These quantities should then be defined
    #  automatically, choosing their type coherently with the RHS of the
    #  defining equations. For now we do all this manually (this is not a lot
    #  of work here, actually).
    if isinstance(alpha, Constant):
        def setter(mat):
            return float(1.0/(1.0 + mat.llg_damping**2))
        sim._qs_from_material_setters["inv_alpha2"] = setter

    else:
        assert isinstance(alpha, SpaceField), \
          "alpha can either be a Constant or a SpaceField"
        eq_inv_alpha2 = \
          Equation("eq_inv_alpha2", "inv_alpha2 <- 1/(1 + alpha*alpha);")
        model.add_computation(eq_inv_alpha2)

    # Equation for the effective field H_total
    split_exch = not isinstance(model.quantities["exchange_factor"], Constant)
    if split_exch:
        # Again some dirt...
        H_rest = qc(SpaceField, "H_rest", [3], subfields=True, unit=H_unit)
        eq = "(H_rest(i) <- H_ext(i) + H_demag(i) + H_anis(i))_(i:3);"
        eq_H_rest = Equation("H_rest", eq)
        eq = "(H_total(i) <- H_rest(i) + H_exch(i))_(i:3);"
        eq_H_total = Equation("H_total", eq)
        model.add_computation(eq_H_total, eq_H_rest)
        model.add_quantity(H_rest)

    else:
        eq = ("(H_total(i) <- "
              " H_ext(i) + H_exch(i) + H_demag(i) + H_anis(i))_(i:3);")
        eq_H_total = Equation("H_total", eq)
        model.add_computation(eq_H_total)

    # Equation of motion
    eq = """
    (dmdt(i) <-
    ((-gamma_G*inv_alpha2)
     *((eps(i,j,k)*m(j)*H_total(k))_(j:3, k:3)
        +alpha*(eps(i,j,k)*m(j)*eps(k,p,q)*m(p)*H_total(q))_(j:3,k:3,p:3,q:3))
     + norm_coeff*(1.0 - (m(j)*m(j))_(j:3))*m(i)
     + P*(1.0 + alpha*xi)
       *(eps(i,j,k)*m(j)*eps(k,p,q)*m(p)*dm_dcurrent(q))_(j:3,k:3,p:3,q:3)
     + P*(xi - alpha)*(eps(i,j,k)*m(j)*dm_dcurrent(k))_(j:3,k:3)
     + sl_coeff
       *(  alpha*(eps(i,j,k)*m(j)*sl_fix(k))_(j:3,k:3)
         + (eps(i,j,k)*m(j)*eps(k,p,q)*m(p)*sl_fix(q))_(j:3,k:3,p:3,q:3))
    )*pin)_(i:3);"""
    llg = Equation("llg", eq)
    model.add_computation(llg)


    # Timestepper

    # Define the equation for the Jacobian. This is still done using the old
    # OCaml parser. XXX NOTE: We have to do some acrobacies and bend around
    # the limitations of the OCaml framework. We'll later compute the Jacobian
    # from Python, and will be able to remove some portions of ugly code
    # NOTE: we omit the third term on the RHS, the STT and others. Should get
    #   back and see whether including the other is possible/advisable.
    op_exch = model.computations._by_name.get("exch", None)
    if split_exch:
        eq = """%range i:3, j:3, k:3, p:3, q:3;
          dmdt(i) <-
            (-gamma_G*inv_alpha2)
            *(  eps(i,j,k)*m(j)*(H_rest(k) + exchange_factor*H_exch_tmp(k))
              + alpha*eps(i,j,k)*m(j)*eps(k,p,q)*m(p)
                *(H_rest(q) + exchange_factor*H_exch_tmp(q)))*pin;"""
        llg_jacobi = Equation("llg-jacobi", eq, ocaml_to_parse=True)
        H_exch_tmp = model.quantities["H_exch_tmp"]
        derivatives = [(H_exch_tmp, op_exch)] if op_exch != None else None

    else:
        eq = """%range i:3, j:3, k:3, p:3, q:3;
          dmdt(i) <-
            (-gamma_G*inv_alpha2)
            *(  eps(i,j,k)*m(j)*H_total(k)
              + alpha*eps(i,j,k)*m(j)*eps(k,p,q)*m(p)*H_total(q))*pin;"""
        llg_jacobi = Equation("llg-jacobi", eq, ocaml_to_parse=True)
        derivatives = [(H_total, op_exch)] if op_exch != None else None

    # ^^^ llg_jacobi doesn't need to be added as it is only used by the
    # timestepper to compute the jacobian

    ts = Timestepper("ts_llg", x="m", dxdt="dmdt",
                     eq_for_jacobian=llg_jacobi,
                     derivatives=derivatives,
                     time_unit=t_unit)
    model.add_timestepper(ts)

def _add_energies(model, contexts, quantity_creator=None):
    if "mumag_energies" in contexts:
        return
    contexts.append("mumag_energies")

    qc = quantity_creator or _default_qc
    E_demag = qc(SpaceField, "E_demag", subfields=True, unit=E_unit)
    E_exch = qc(SpaceField, "E_exch", subfields=True, unit=E_unit)
    E_ext = qc(SpaceField, "E_ext", subfields=True, unit=E_unit)
    # E_anis defined in _add_anis
    E_total = qc(SpaceField, "E_total", subfields=True, unit=E_unit)
    model.add_quantity(E_demag, E_exch, E_ext, E_total)

    eq = "E_demag <- -0.5*mu0*Ms*(m(i)*H_demag(i))_(i:3);"
    en_demag = Equation("en_demag", eq)

    eq = "E_exch <- -0.5*mu0*Ms*(m(i)*H_exch(i))_(i:3);"
    en_exch = Equation("en_exch", eq)

    eq = "E_ext <- -mu0*Ms*(m(i)*H_ext(i))_(i:3);"
    en_ext = Equation("en_ext", eq)

    eq = "E_total <- E_demag + E_exch + E_ext + E_anis;"
    en_total = Equation("en_total", eq)

    model.add_computation(en_demag, en_exch, en_ext, en_total)
