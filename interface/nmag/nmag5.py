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

"""
The fifth version of the nmag interface, completely based on nsim.model.
"""

# System imports
import os
import math
import logging

# Nmag imports (modules nmag.*)
from nmag_exceptions import *
import nmesh
from simulation_core import SimulationCore
from nsim.model import *
from nsim.su_units import SimulationUnits
from nsim.si_units.si import SI
import nfem.hdf5_v01 as hdf5

# Get some object into this namespace
from material import MagMaterial

# debug variables to study timing
from nsim.timings import Timer

timer1 = Timer("save-data")

# Get the logger
lg = logging.getLogger('nmag')

# These are our default simulation units -- however, they can be
# modified by the user (by setting nmag.simulation_units manually
# before creating a (or in any case the first) Simulation object).
simulation_units = \
  SimulationUnits({'A': 1e-3, 'kg': 1e-27, 'm': 1e-9, 's': 1e-12,
                   'cd': 1.0, 'K': 1.0, 'mol': 1.0})


def _default_qc(q_type, q_name, *args, **named_args):
    return q_type(q_name, *args, **named_args)


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
    def __init__(self, name=None, do_demag=True):
        SimulationCore.__init__(self, name=name, do_demag=do_demag,
                                id="FE Simulation class")

        # Mesh stuff...
        self.mesh = None                # The mesh object
        self.region_name_list = None    # List of region names (corresponding
                                        #   to region 1, 2, ...)
        self.region_name_of_id = None   # Mapping ID -> region_name[ID]
        self.region_id_of_name = None   # region_name -> ID[region_name]
        self.mats_of_region_name = None # region_name -> material[region_name]
        self.mat_of_mat_name = None     # mat_name -> material[mat_name]
        self.materials = []

        # The physics...
        self._model = None              # The nsim.model.Model object

        # How the micromagnetic quantities will be constructed, i.e. a
        # dictionary mapping quantity name -> quantity_type, where
        # quantity_type is the class to use for construction.
        # This dictionary is provided for the user to influence how the
        # quantities are built. Quantities missing from here cannot be
        # influenced by the user in their construction.
        self.quantities_types = \
          {"H_ext": SpaceField}

    def get_model(self):
        if self._model != None:
            return self._model
        else:
            raise NmagUserError("Cannot get the Model object. You first should"
                                "load a mesh using the Simulation.load_mesh.")

    model = property(get_model)

    def declare(self, attrs, objs):
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
        qt = []
        for obj_name in objs:
            if obj_name in dos:
                assert type(obj_name) == str, declare_usage
                for attr in attrs:
                    assert type(attr) == str, declare_usage
                    fmt_attr = attr.lower().replace(" ", "")
                    if fmt_attr in qts:
                        qt.append(fmt_attr)
                        self.quantities_types[obj_name] = qt[fmt_attr]

                    else:
                        msg = "Unrecognized attribute '%s'" % attr
                        raise NmagUserError(msg)

        if len(qt) > 1:
            msg = "Conflicting attributes %s." % (", ".join(qt))
            raise NmagUserError(msg)

    def load_mesh(self, filename, region_names_and_mag_mats, unit_length,
                  do_reorder=False, manual_distribution=None):
        lg.info("Reading mesh from %s, unit_length is %s. "
                % (filename, unit_length))

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
            lg.info("Adding region '%s' to '%s' simulation object" % (name, self.name))
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
        _add_micromagnetics(model, self._quantity_creator)
        _add_exchange(model, self._quantity_creator)
        _add_demag(model, self._quantity_creator, self.do_demag)
        _add_llg(model, self._quantity_creator)

        # Set the values of the constants in the micromagnetic model
        self._set_qs_from_materials()

        # Now build the model
        model.build()

    def _set_qs_from_materials(self):
        # We now go through all the materials and build the initial values for
        # the quantities corresponding to the various parameters, gamma_GG,
        # alpha, M_sat, etc.
        qs = self.model.quantities._by_name

        def set_quantity_value(name, name_in_mat):
            v = Value()
            for mat_name, mat in self.mat_of_mat_name.iteritems():
                v.set(mat_name, getattr(mat, name_in_mat))
            qs[name].set_value(v)

        set_quantity_value("M_sat", "Ms")
        set_quantity_value("gamma_GG", "llg_gamma_G")
        set_quantity_value("alpha", "llg_damping")
        set_quantity_value("norm_coeff", "llg_normalisationfactor")
        set_quantity_value("exchange_factor", "exchange_factor")

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
        print "set_params NOT IMPLEMENTED, YET"

    def set_H_ext(self, values, unit=None):
        assert False

    def set_m(self, values, subfieldname=None):
        v = Value(values, subfieldname)
        self.model.quantities._by_name["m"].set_value(v)

    def set_pinning(self, values):
        assert False

    def advance_time(self, target_time, max_it=-1, exact_tstop=True):
        ts = self.model.timesteppers._by_name["ts_llg"]
        t = ts.advance_time(target_time, max_it=max_it)
        step = ts.get_num_steps()
        delta_step = step - self.clock.step
        delta_time = t - self.clock.time
        self.clock.step = step
        self.clock.time = t
        self.clock.stage_step += delta_step
        self.clock.stage_time += delta_time
        return t

    def save_data(self, fields=None, avoid_same_step=False):
        """
        Save the *averages* of all defined (subfields) into a ascii
        data file. The filename is composed of the simulation name
        and the extension ``_dat.ndt``. The
        extension ``ndt`` stands for Nmag Data Table (analog to OOMMFs
        ``.odt`` extension for this kind of data file.

        If ``fields`` is provided, then it will also save the spatially resolved fields
        to a file with extensions ``_dat.h5``.

        :Parameters:
          `fields` : None, 'all' or list of fieldnames

            If None, then only spatially averaged data is saved into ``*ndt`` and ``*h5`` files.

            If ``all`` (i.e. the string containing 'all'), then all fields are saved.

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




    def _create_h5_file(self,filename):
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

        lg.log(15, "Saving field(s) %s into '%s'" % (str(fieldnames), filename))

        lg.debug("save_fields (%s): time_reached_si is %s"
                 % (filename, self.clock.time_reached_si.dens_str()))

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
                           self.clock.time_reached_si, self.clock.step,
                           self.clock.stage, self.clock.id,
                           simulation_units)
        timer1.stop('append_fields')

        lg.info("Written fields %s data to %s" % (str(fieldnames), filename))
        timer1.stop('save_fields')






def _add_micromagnetics(model, quantity_creator=None):
    qc = quantity_creator or _default_qc
    m = qc(SpaceField, "m", [3], subfields=True, unit=SI(1))
    M_sat = qc(Constant, "M_sat", subfields=True, unit=SI(1e6, "A/m"))
    model.add_quantity([m, M_sat])

def _add_exchange(model, quantity_creator=None):
    qc = quantity_creator or _default_qc

    H_unit = SI(1e6, "A/m")

    # Constants and fields
    exchange_factor = qc(Constant, "exchange_factor", subfields=True,
                         unit=SI(1e6, "m A"))
    H_exch = qc(SpaceField, "H_exch", [3], subfields=True, unit=H_unit)

    # The exchange operator
    op_str = "exchange_factor*<d/dxj H_exch(k)||d/dxj m(k)>, j:3,  k:3"
    op_exch = Operator("exch", op_str)

    model.add_quantity([exchange_factor, H_exch])
    model.add_computation(op_exch)

def _add_demag(model, quantity_creator=None, do_demag=True):
    qc = quantity_creator or _default_qc

    H_unit = SI(1e6, "A/m")

    # Create the required quantities and add them to the model
    H_demag = qc(SpaceField, "H_demag", [3], unit=H_unit)
    phi1b = qc(SpaceField, "phi1b", [], restrictions="phi1b[outer]")
    phi2b = qc(SpaceField, "phi2b", [], restrictions="phi2b[outer]")
    phis = [qc(SpaceField, n, [], unit=SI("A"))
            for n in ["phi", "phi1", "phi2"]]
    rhos = [qc(SpaceField, n, [], unit=SI("A/m^2")) for n in ["rho", "rho_s"]]
    model.add_quantity([H_demag, phi1b, phi2b] + phis + rhos)

    # Operators for the demag
    op_div_m = Operator("div_m", "  M_sat*<rho||d/dxj m(j)>"
                                 "+ M_sat*<rho||D/Dxj m(j)>, j:3")
    op_neg_laplace_phi = \
      Operator("neg_laplace_phi", "<d/dxj rho || d/dxj phi>, j:3",
               mat_opts=["MAT_SYMMETRIC", "MAT_SYMMETRY_ETERNAL"])
    op_grad_phi = Operator("grad_phi", "-<H_demag(j) || d/dxj phi>, j:3")
    op_laplace_DBC = \
      Operator("laplace_DBC",
               ("-<d/dxj phi[not outer] || d/dxj phi[not outer]>;"
                "phi[outer]=phi[outer], j:3"""),
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
    commands=[["SM*V", op_div_m, "v_m", "v_rho"],
              ["SCALE", "v_rho", -1.0],
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

    model.add_computation([op_div_m, op_neg_laplace_phi, op_grad_phi,
                           op_laplace_DBC, op_load_DBC, ksp_solve_laplace_DBC,
                           ksp_solve_neg_laplace_phi, bem, prog_set_H_demag])

def _add_llg(model, quantity_creator=None):
    qc = quantity_creator or _default_qc

    H_unit = SI(1e6, "A/m")
    t_unit = SI(1e-12, "s")
    invt_unit = 1/t_unit

    # Create the LLG constants
    gamma_GG = qc(Constant, "gamma_GG", subfields=True, unit=SI(1e6, "m/A s"))
    alpha = qc(Constant, "alpha", subfields=True, unit=SI(1))
    norm_coeff = qc(Constant, "norm_coeff", subfields=True, unit=invt_unit)
    model.add_quantity([gamma_GG, alpha, norm_coeff])

    # Create some fields required for the dynamics
    dmdt = qc(SpaceField, "dmdt", [3], subfields=True, unit=invt_unit)
    H_total = qc(SpaceField, "H_total", [3], subfields=True, unit=H_unit)
    H_ext = qc(SpaceField, "H_ext", [3], unit=H_unit)
    model.add_quantity([dmdt, H_total, H_ext])

    # Equation for the effective field H_total
    # XXX NOTE NOTE NOTE: clean up the factor 1e18. It should be computed
    # automatically from the parse tree of the operator!
    eq = "%range i:3; H_total(i) <- H_ext(i) + H_exch(i) + H_demag(i);"
    eq_H_total = Equation("H_total", eq)

    # Equation of motion
    eq = ("%range i:3, j:3, k:3, p:3, q:3;"
           "dmdt(i) <- "
           "    (-gamma_GG/(1 + alpha*alpha))*(eps(i,j,k)*m(j)*H_total(k)"
           "  + alpha*eps(i,j,k)*m(j)*eps(k,p,q)*m(p)*H_total(q))"
           "  + norm_coeff*(1.0 - m(j)*m(j))*m(i);")
    llg = Equation("llg", eq)

    # Equation for the Jacobian: we omit the third term on the RHS
    eq = ("%range i:3, j:3, k:3, p:3, q:3; "
          "dmdt(i) <- "
          "    (-gamma_GG/(1 + alpha*alpha))*(eps(i,j,k)*m(j)*H_total(k)"
          "  + alpha*eps(i,j,k)*m(j)*eps(k,p,q)*m(p)*H_total(q));")
    llg_jacobi = Equation("llg-jacobi", eq)


    # llg_jacobi doesn't need to be added as it is only used by the
    # timestepper to compute the jacobian
    model.add_computation([llg, eq_H_total])

    # Timestepper
    op_exch = model.computations._by_name.get("exch", None)
    derivatives = [(H_total, op_exch)] if op_exch != None else None
    ts = Timestepper("ts_llg", x="m", dxdt="dmdt",
                     eq_for_jacobian=llg_jacobi,
                     derivatives=derivatives,
                     time_unit=t_unit)
    model.add_timestepper(ts)
