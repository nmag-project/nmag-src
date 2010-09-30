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
import math
import logging

# Nmag imports (modules nmag.*)
from nmag_exceptions import *
import nmesh
from simulation_core import SimulationCore
from nsim.model import *
from nsim.su_units import SimulationUnits
from nsim.si_units.si import SI

# Get some object into this namespace
from material import MagMaterial

#from constants import degrees_per_ns

# Get the logger
lg = logging.getLogger('nmag')

# These are our default simulation units -- however, they can be
# modified by the user (by setting nmag.simulation_units manually
# before creating a (or in any case the first) Simulation object).
simulation_units = \
  SimulationUnits({'A': 1e-3, 'kg': 1e-27, 'm': 1e-9, 's': 1e-12,
                   'cd': 1.0, 'K': 1.0, 'mol': 1.0})


def _default_qc(q_type, q_name, shape=[], subfields=False, unit=1):
    return q_type(q_name, shape=shape, subfields=subfields, unit=unit)


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
        self.model = None            # The nsim.model.Model object

        # How the micromagnetic quantities will be constructed, i.e. a
        # dictionary mapping quantity name -> quantity_type, where
        # quantity_type is the class to use for construction.
        # This dictionary is provided for the user to influence how the
        # quantities are built. Quantities missing from here cannot be
        # influenced by the user in their construction.
        self.quantities_types = \
          {"H_ext": SpaceField}

    def get_model(self):
        if self.model != None:
            return self.model
        else:
            raise NmagUserError("Cannot get the Model object. You first should"
                                "load a mesh using the Simulation.load_mesh.")

    model = property(get_model)

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

    def _quantity_creator(self, q_type, q_name, shape=[], subfields=False,
                          unit=1):
        q_type = self.quantities_types.get(q_name, q_type)
        return _default_qc(q_type, q_name, shape=shape,
                           subfields=subfields, unit=unit)

    def _create_model(self):
        # Create the model object
        model_name = "mumag_%s" % self.name
        region_materials = [[mat.name for mat in self.mats_of_region_name[name]]
                            for name in self.region_name_list]
        mu = 1e-9 # NOTE, NOTE, NOTE we should look into this
        region_materials = [[]] + region_materials
        self.model = model = Model(model_name, self.mesh, mu, region_materials)

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

    def advance_time(self, target_time, max_it=-1):
        ts = self.model.timesteppers._by_name["ts_llg"]
        return ts.advance_time(target_time)

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
    H_tot = qc(SpaceField, "H_tot", [3], subfields=True, unit=H_unit)
    H_ext = qc(SpaceField, "H_ext", [3], unit=H_unit)
    model.add_quantity([dmdt, H_tot, H_ext])

    # Equation for the effective field H_tot
    # XXX NOTE NOTE NOTE: clean up the factor 1e18. It should be computed
    # automatically from the parse tree of the operator!
    eq = "%range i:3; H_tot(i) <- H_ext(i) + H_exch(i);"
    eq_H_tot = Equation("H_tot", eq)

    # Equation of motion
    eq = ("%range i:3, j:3, k:3, p:3, q:3;"
           "dmdt(i) <- (-gamma_GG/(1 + alpha*alpha))*(eps(i,j,k)*m(j)*H_tot(k) +"
           "alpha*eps(i,j,k)*m(j)*eps(k,p,q)*m(p)*H_tot(q)) +"
           "norm_coeff*(1.0 - m(j)*m(j))*m(i);")
    llg = Equation("llg", eq)

    # Equation for the Jacobian: we omit the third term on the RHS
    eq = ("%range i:3, j:3, k:3, p:3, q:3; "
          "dmdt(i) <- (-gamma_GG/(1 + alpha*alpha))*(eps(i,j,k)*m(j)*H_tot(k) + "
          "alpha*eps(i,j,k)*m(j)*eps(k,p,q)*m(p)*H_tot(q));")
    llg_jacobi = Equation("llg-jacobi", eq)


    # llg_jacobi doesn't need to be added as it is only used by the
    # timestepper to compute the jacobian
    model.add_computation([llg, eq_H_tot])

    # Timestepper
    op_exch = model.computations._by_name.get("exch", None)
    derivatives = [(H_tot, op_exch)] if op_exch != None else None
    ts = Timestepper("ts_llg", x="m", dxdt="dmdt",
                     eq_for_jacobian=llg_jacobi,
                     derivatives=derivatives,
                     time_unit=t_unit)
    model.add_timestepper(ts)
