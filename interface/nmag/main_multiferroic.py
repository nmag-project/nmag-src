__docformat__ = "restructuredtext"

# TODO:
#  - Add Electric_ext for the electric field
#  - Add H_multiferroic [X]
#  - Note that we do not yet have E_multiferroic, and hence this is also not included in E_total!

# Standard modules
import sys, types, math, logging, time, os

# Python extensions (not nsim specific)
import tables

# We now proceed with the setup: parsing command line, removing old log files,
# setting up the loggers, etc.
import nsim.setup
nsim.setup.setup(sys.argv)

# Nsim specific modules
from   nmag_special_imports import ocaml, nmesh, nfem, nsim, infer, numpy
from   nmag_exceptions import *

import nfem.hdf5_v01 as hdf5

import nsim.logtools
import nsim.su_units
from   nsim.si_units   import SI, si
from   nsim.when       import every, at, never
from   nsim.sets       import vector_set, float_set
from   nsim.colwriter  import ColDescriptor, ColWriter
from   nsim.vectools   import *
from   nsim.shell      import ipython
from   anisotropy      import *
from   simulation_core import SimulationCore
from   hlib            import default_hmatrix_setup, HMatrixSetup

import hysteresis as hysteresis_implementation
import convergence
import fefields

import nsim
import nsim.versions
import hlib

log = logging.getLogger('nmag')
def memory_report(tag):
    t,vmem,rss = ocaml.time_vmem_rss()
    log.log(15,"Memory report: T=  %f VMEM=   %d KB RSS=   %d KB %s" % (t,int(vmem),int(rss),tag))


memory_report("during importing of nmag/main.py")


def _field_dependency_engine(to_mbuf_copier,
                             from_mbuf_copier,
                             lam_executor,
                             have_demag=True,
                             have_exch=True,
                             have_stt=False,
                             have_multiferroic=False,
                             ):
    # given a list 'l', returns a list where None entries have been removed
    flt = lambda l: [li for li in l if li != None]

    # Return thing only if we 'have_exch', otherwise return None
    def if_have_exch(thing):
        if have_exch: return thing
        return None

    # The same for 'have_demag'
    def if_have_demag(thing):
        if have_demag: return thing
        return None

    # The same for 'have_stt'
    def if_have_stt(thing_if, thing_else):
        if have_stt: return thing_if
        return thing_else

    # The same for 'have_multiferroic'
    def if_have_multiferroic(thing_if, thing_else=None):
        if have_multiferroic: return thing_if
        return thing_else


    deps = [
        {"name":"mbuf_m"},
        {"name":"mbuf_H_ext"},
        {"name":"mbuf_pin"},
        {"name":"mbuf_H_therm"},
        {"name":"mbuf_Temperature"},
        {"name":"mbuf_current_density"},
        {"name":"mbuf_Electric_ext"}, # EF = Electric field; Note that E already denotes the energy density!
        {"name":"mbuf_H_multiferroic"},
        # {"name":"mbuf_E_multiferroic"}, # XXX Not yet!
        {"name":"mbuf_dm_dcurrent","depends_on":["lam_m"],"how_to_make":[to_mbuf_copier("dm_dcurrent")]},
        # XXX Note: this is tricky, as we may have situations where the temperature
        # is user-controlled just like H_ext, and others where this is dynamical!
        #
        # The latter case will need major re-structurings anyway, however, as
        # then, we would have to introduce a combined field Y = (m,T).
        # --
        {"name":"mbuf_dmdt","depends_on":["lam_dmdt"],"how_to_make":[to_mbuf_copier("dmdt")]},
        {"name":"mbuf_rho","depends_on":["lam_rho"],"how_to_make":[to_mbuf_copier("rho")]},
        {"name":"mbuf_phi","depends_on":["lam_phi"],"how_to_make":[to_mbuf_copier("phi")]},
        {"name":"mbuf_M","depends_on":["lam_M"],"how_to_make":[to_mbuf_copier("M")]},
        {"name":"mbuf_grad_m","depends_on":["lam_grad_m"],"how_to_make":[]}, # grad_m is internal!
        if_have_exch( {"name":"mbuf_H_exch",
                       "depends_on":["lam_H_exch"],
                       "how_to_make":[to_mbuf_copier("H_exch")]} ),
        if_have_demag({"name":"mbuf_H_demag",
                       "depends_on":["lam_H_demag"],
                       "how_to_make":[to_mbuf_copier("H_demag")]}),
        {"name":"mbuf_H_anis","depends_on":["lam_H_anis"],"how_to_make":[to_mbuf_copier("H_anis")]},
        {"name":"mbuf_H_lc","depends_on":["lam_H_lc"],"how_to_make":[to_mbuf_copier("H_lc")]},
        {"name":"mbuf_H_total","depends_on":["lam_H_total"],"how_to_make":[to_mbuf_copier("H_total")]},

        {"name":"mbuf_E_ext","depends_on":["lam_E_ext"],"how_to_make":[to_mbuf_copier("E_ext")]},
        if_have_exch( {"name":"mbuf_E_exch",
                       "depends_on":["lam_E_exch"],
                       "how_to_make":[to_mbuf_copier("E_exch")]} ),
        if_have_demag({"name":"mbuf_E_demag",
                       "depends_on":["lam_E_demag"],
                       "how_to_make":[to_mbuf_copier("E_demag")]}),
        {"name":"mbuf_E_total","depends_on":["lam_E_total"],"how_to_make":[to_mbuf_copier("E_total")]},
        {"name":"mbuf_E_anis","depends_on":["lam_E_anis"],"how_to_make":[to_mbuf_copier("E_anis")]},
        {"name":"mbuf_E_lc","depends_on":["lam_E_lc"],"how_to_make":[to_mbuf_copier("E_lc")]},
        # ---
        if_have_demag({"name":"lam_rho",
                       "depends_on":["lam_m"],
                       "how_to_make":[lam_executor("set_H_demag")],
                       "also_updates":["lam_H_demag","lam_rho","lam_phi"]}),
        if_have_demag({"name":"lam_phi",
                       "depends_on":["lam_m"],
                       "how_to_make":[lam_executor("set_H_demag")],
                       "also_updates":["lam_H_demag","lam_rho","lam_phi"]}),

        {"name":"lam_m","depends_on":["mbuf_m"],"how_to_make":[from_mbuf_copier("m")]},
        {"name":"lam_H_ext","depends_on":["mbuf_H_ext"],"how_to_make":[from_mbuf_copier("H_ext")]},
        {"name":"lam_pin","depends_on":["mbuf_pin"],"how_to_make":[from_mbuf_copier("pin")]},
        {"name":"lam_Electric_ext","depends_on":["mbuf_Electric_ext"],"how_to_make":[from_mbuf_copier("Electric_ext")]},
        {"name":"lam_current_density",
         "depends_on":["mbuf_current_density"],
         "how_to_make":if_have_stt([from_mbuf_copier("current_density")], [])},
        {"name":"lam_dm_dcurrent",
         "depends_on":["lam_m"],
         "how_to_make":if_have_stt([lam_executor("set_dm_dcurrent")], [])},
        # ---
        {"name":"lam_M","depends_on":["lam_m"],"how_to_make":[lam_executor("set_M")]},
        {"name":"lam_grad_m","depends_on":["lam_m"],"how_to_make":[]},
        {"name":"lam_H_multiferroic","depends_on":["lam_Electric_ext","lam_grad_m","lam_m"],"how_to_make":[]}, # XXX this is internal, hence does not provide how_to_make yet!
        {"name":"lam_H_anis","depends_on":["lam_m"],"how_to_make":[lam_executor("set_H_anis")]},
        {"name":"lam_H_lc","depends_on":["lam_m"],"how_to_make":[lam_executor("set_H_lc")]},

        if_have_exch( {"name":"lam_H_exch",
                       "depends_on":["lam_m"],
                       "how_to_make":[lam_executor("set_H_exch")]} ),

        if_have_demag({"name":"lam_H_demag",
                       "depends_on":["lam_m"],
                       "how_to_make":[lam_executor("set_H_demag")],
                       "also_updates":["lam_H_demag","lam_rho","lam_phi"]}),

        {"name":"lam_E_anis","depends_on":["lam_m"],"how_to_make":[lam_executor("set_E_anis")]},

        {"name":"lam_E_lc","depends_on":["lam_m"],"how_to_make":[lam_executor("set_E_lc")]},

        {"name":"lam_H_total",
         "depends_on":flt([if_have_exch( "lam_H_exch" ),
                           if_have_demag("lam_H_demag"),
                           if_have_multiferroic("lam_H_multiferroic"),
                           "lam_H_ext",
                           "lam_H_anis"]),
         "how_to_make":[lam_executor("set_H_total")]},

        {"name":"lam_energies",
         "depends_on":["lam_m","lam_H_total"],
         "how_to_make":[lam_executor("update_E")],
         "also_updates":flt(["lam_E_ext",
                             if_have_exch( "lam_E_exch" ),
                             if_have_demag("lam_E_demag"),
                             "lam_E_total"])},

        {"name":"lam_E_ext","depends_on":["lam_energies"]},
        if_have_exch( {"name":"lam_E_exch","depends_on":["lam_energies"]} ),
        if_have_demag({"name":"lam_E_demag","depends_on":["lam_energies"]}),
        {"name":"lam_E_total","depends_on":["lam_energies"]},
        # ---
        {"name":"lam_dmdt","depends_on":["lam_H_total","lam_pin"],"how_to_make":[lam_executor("update_dmdt")]},
        # --- Not yet provided: ---
        ]

    return infer.InferenceEngine(flt(deps))



# These are our default simulation units -- however, they can be
# modified by the user (by setting nmag.simulation_units manually
# before creating a (or in any case the first) Simulation object).
simulation_units = nsim.su_units.SimulationUnits({'A': 1e-3,
                                                  'kg': 1e-27,
                                                  'm': 1e-9,
                                                  's': 1e-12,
                                                  'cd': 1.0,
                                                  'K': 1.0,
                                                  'mol': 1.0
                                                  }
                                                 )

# XXX TODO Note T.F.: Matteo, did you actually mix up these names?!?
# Temperature certainly is NOT per-material!
#
# Names of the fields defined on a per-material basis
fields_on_materials = ["H_demag", "H_ext", "Electric_ext", "phi", "rho", "pin",
                       "Temperature", "current_density"]
# Names of the fields defined just over the space
fields_on_space = ['H_anis', 'M', 'dmdt', 'm', 'grad_m',
                   'H_multiferroic',
                   'E_anis', 'E_exch', 'E_demag', 'E_total', 'E_ext',
                   'H_total', 'H_exch', 'H_therm', 'dm_dcurrent']

known_fields = fields_on_materials + fields_on_space

fieldunits_by_fieldname = {'phi':SI("A"),
                           'H_demag':SI("A/m"), 
                           'H_anis':SI("A/m"), 
                           'M':SI("A/m"),
                           'dmdt':SI("A/m s"),
                           'm':SI(""),
                           'grad_m':SI("A/m^2"),
                           'E_anis':SI("J/m^3"), 
                           'E_exch':SI("J/m^3"), 
                           'E_lc':SI("J/m^3"),
                           'rho':SI("A/m^2"), 
                           'E_demag':SI("J/m^3"),
                           'E_total':SI("J/m^3"),
                           'E_ext':SI("J/m^3"),
                           'H_total':SI("A/m"),
                           'H_exch':SI("A/m"),
                           'H_ext':SI("A/m"),
                           'H_therm':SI("A/m"),
                           'H_lc':SI("A/m"),
                           'Temperature':SI("K"),
                           'current_density':SI("A/m^2"),
                           'dm_dcurrent':SI("A/m^3"),
                           'pin':SI(""),
                           'H_multiferroic':SI("A/m"),
                           'Electric_ext':SI("V/m"),
                           }

otherunits_by_name = {'time':SI('s'),
                      'step':SI(1),
                      'last_step_dt':SI('s'),
                      'id':SI(1),
                      'stage_time':SI('s'),
                      'stage_step':SI(1),
                      'stage':SI(1)
                      }

import nmag_lam_multiferroic as nmag_lam

# debug variables to study timing
from nsim.timings import Timer

timer1 = Timer("save-data")

def recursive_map(f, l):
    def my_map(l):
        if type(l) == list:
            return map(my_map, l)
        else:
            return f(l)
    return my_map(l)

class MagMaterial:
    """

    :Parameters:
      `name` : string
        The name of the material. This will be used in the names of
        material dependent fields and subfields. Must be alphanumeric
        (i.e. contain only the characters 0-9\_a-zA-Z)
        Examples: ``'Py'``, ``'Fe_1'``,  ``'Fe_2'``

      `Ms` : SI Object
        The saturation magnetisation of the material (in Ampere per
        meter).

        Example (and default (PermAlloy) value): ``SI(0.86e6,"A/m")`` 

      `llg_gamma_G` : SI Object
        The constant in front of the precession term in the LLG equation:

          dM/dt = -llg_gamma_G * M x H + llg_damping * M x dM/dt

        It is often called gyromagnetic ratio, even if usually, in physics,
        the gyromagnetic ratio of a particle is the ratio between its magnetic
        dipole moment and its angular momentum (and has units A*s/kg).
        It is then an improper nomenclature, but it occurs frequently in the
        literature.

        Example (and default value): ``SI(2.210173e5, "m/A s")``.

      `llg_damping` : SI Object
        The damping parameter (often called alpha). Optimum damping
        for 1.0, realistic values are of the order of 0.01. The
        default value (as in OOMMF) is 0.5.

        Example (and default value): ``SI(0.5,"")``

      `exchange_coupling` : SI Object
        The coupling strength for the exchange interaction in Joule
        per meter.

        Example (and default value): ``SI(1.3e-11, "J/m")``


      `multiferroic_coupling` : SI Object
        The coupling strength for the multiferroic interaction in 1/Coulomb:

        Example (and default value): ``SI(0, "1/A s")``


      `anisotropy` : PredefinedAnisotropy Object  or function(vector) -> SI Object
        Either a predefined anisotropy (such as returned by
        uniaxial_anisotropy_ or cubic_anisotropy_), or a custom
        function (which must be polynomial in the components of ``m``)
        ``a(m)`` that computes anisotropy energy density
        as a function of the (normalised) magnetisation direction ``m``.

        If you specify a custom anisotropy function, you also
        need to pass the order of the polynomial in the ``anisotropy_order``
        parameter.

        Default value is ``None``, that is, no anisotropy term
        is used.

      `anisotropy_order` : int
        If a custom polynomial anisotropy function ``a(m)`` is specified, the order
        of the polynomial must be given in this parameter. This is not required
        for pre-defined uniaxial_anisotropy_ or cubic_anisotropy_ anisotropy
        functions.

        Default value is ``None``.

      `do_precession` : True or False
        Boolean that can switch off the precessional term in the LLG
        equation. This is useful to improve convergence speed
        when studying metastable configurations.

      `properties`: list of strings (default: ["magnetic","material"])
        A list of additional properties this material will be associated with.
        Normally, users do not have to change this, but it is used internally
        when setting up discretized operators.

        Example (and default value): ``True``
    
    """

    __argdoclong__ = """
    (self,
    name,
    Ms=SI(0.86e6, "A/m"),
    llg_damping=SI(0.5),
    llg_gamma_G=SI(2.210173e5, "m/A s"),
    exchange_coupling=SI(1.3e-11, "J/m"),
    multiferroic_coupling=SI(0,"1/A s"),
    anisotropy=None,
    anisotropy_order=None,
    do_precession=True)
    """

    def __init__(self,
                 name,
                 Ms=SI(0.86e6, "A/m"),
                 # The length of the magnetization
                 # vector M, may be negative (M antiparallel m)!
                 llg_damping=0.5,
                 # The damping coefficient in the LLG equation.
                 llg_gamma_G=SI(2.210173e5, "m/A s"),
                 # This is the constant in front of the LLG equation.
                 # It is often called gyromagnetic ratio, even if usually,
                 # in physics, the gyromagnetic ratio of a particle is
                 # the ratio between its magnetic dipole moment and its
                 # angular momentum (and has units A*s/kg).
                 # It is then an improper nomenclature, but it occours
                 # frequently in the literature. The default value of
                 # llg_gamma_G is 221017.3 m/(A*s) (for more details take
                 # a look at the OOMMF manual, and Werner Scholz's thesis,
                 # after (3.7)).
                 llg_normalisationfactor=SI(0.1e12, "1/s"),
                 # An extra term A m (1 - m*m), where m = M/Ms, is added
                 # to the RHS of the LLG equation to correct numerical errors
                 # in the norm of m. llg_normalisationfactor is
                 # the coefficient A of this term.
                 llg_xi=0.0,
                 # spin-transfer-torque: ratio between the exchange
                 # and the spin-flip relaxation times: xi = tau_ex / tau_sf.
                 llg_polarisation=0.0,
                 # spin-transfer-torque: the polarisation of the spin-current.
                 do_precession = True,
                 # if do_precession == False, then we switch off
                 # the precessional term in the LLG. This can be used
                 # to obtain faster convergence.
                 exchange_coupling=SI(1.3e-11, "J/m"),
                 # the exchange coupling constant.
                 multiferroic_coupling=SI(0,"1/A s"),
                 # The multiferroic coupling constant
                 anisotropy=None,
                 # PredefinedAnisotropy object, or function a(m) which returns an energy
                 # density for the given direction of the magnetisation m.
                 anisotropy_order=None,
                 # Order of approximation; only specify this if you specify an
                 # anisotropy function (as opposed to a PredefinedAnisotropy object)
                 properties=["magnetic","material"],
                 scale_volume_charges=1.0,
                 # Parameter to be set by developers for debugging.
                 # To be deleted soon.
                 ):

        memory_report("Constructor MagMaterial Class")

        self.name = name
        self.Ms=Ms
        self.llg_gamma_G = llg_gamma_G
        self.llg_damping = llg_damping
        self.llg_normalisationfactor = llg_normalisationfactor
        self.llg_xi = llg_xi
        self.llg_polarisation = llg_polarisation
        self.do_precession = do_precession
        self.properties = properties
        self.exchange_coupling=exchange_coupling
        self.multiferroic_coupling=multiferroic_coupling
        self.scale_volume_charges = \
          simulation_units.of(scale_volume_charges, compatible_with=SI(1))

        if isinstance(anisotropy, PredefinedAnisotropy):
            if anisotropy_order:
                raise NmagUserError("Cannot specify custom anisotropy_order "
                                    "when using a predefined anisotropy.")
            self.anisotropy = anisotropy.function
            self.anisotropy_order = anisotropy.order

        else:
            if anisotropy and not anisotropy_order:
                raise NmagUserError("You need to specify the "
                                    "anisotropy_order when using a custom "
                                    "anisotropy function.")
            self.anisotropy = anisotropy
            self.anisotropy_order = anisotropy_order

        #Compute simulation units
        self.su_Ms = simulation_units.of(self.Ms, compatible_with=SI("A/m"))
        self.su_llg_gamma_G = simulation_units.of(self.llg_gamma_G, compatible_with=SI("m/A s"))
        self.su_llg_damping = simulation_units.of(self.llg_damping, compatible_with=SI(""))
        self.su_llg_normalisationfactor = simulation_units.of(self.llg_normalisationfactor, compatible_with=SI("s^(-1)"))
        self.su_exchange_coupling = simulation_units.of(self.exchange_coupling, compatible_with=SI("J/m"))
        self.su_multiferroic_coupling = simulation_units.of(self.multiferroic_coupling, compatible_with=SI("1/A s"))

        # compute thermal factor (gets multiplied by T/(dV*dt) later)
        self.thermal_factor = (2.0*si.boltzmann_constant*self.llg_damping)/(-si.gamma0*si.mu0*self.Ms)
        self.su_thermal_factor = simulation_units.of(self.thermal_factor, SI(1, ["A",2,"m",1,"s",1,"K",-1]))

        # Here we calculate the parameters in simulation units
        # XXX NOTE: the user cannot modify self.llg_damping alone!!!
        #   we should provide properties to change these values, in such a way
        #   that the corresponding _su values will be immediately computed.
        gilbert_to_ll = 1.0/(1.0+self.su_llg_damping**2)
        self.su_llg_coeff1 = -self.su_llg_gamma_G*gilbert_to_ll
        self.su_llg_coeff2 = self.su_llg_coeff1*self.su_llg_damping
        su_llg_xi = simulation_units.of(self.llg_xi, compatible_with=SI(""))
        su_llg_polarisation = simulation_units.of(self.llg_polarisation,
                                                  compatible_with=SI(""))
        su_e = simulation_units.of(si.positron_charge)
        su_mub = simulation_units.of(si.bohr_magneton)
        su_f = -gilbert_to_ll*(su_llg_polarisation*su_mub /
                 (su_e*self.su_Ms*(1.0 + su_llg_xi*su_llg_xi)))
        self.su_llg_stt_prefactor = 1.0
        if su_f == 0.0: self.su_llg_stt_prefactor = 0.0
        self.su_llg_stt_nadiab = su_f*(su_llg_xi - self.su_llg_damping)
        self.su_llg_stt_adiab = su_f*(1.0 + self.su_llg_damping*su_llg_xi)

        if self.do_precession == False:
            log.info ("Setting su_llg_coeff1 to zero; thus no precession for material '%s'" % self.name)
            self.su_llg_coeff1 = 0.0

        if self.su_exchange_coupling < 0.0:
            raise NmagUserError("The exchange coupling constant " + \
              "must be positive. For material '%s', you specified: %s." \
              % (self.name, self.exchange_coupling))

        self.su_mu0 = simulation_units.of(si.mu0)
        self.su_exch_prefactor = \
          2.0*self.su_exchange_coupling / (self.su_mu0*self.su_Ms)

        self.su_anisotropy = None
        if self.anisotropy:
            def su_anisotropy(m):
                return simulation_units.of(self.anisotropy(m), compatible_with=SI("J/m^3"))
            self.su_anisotropy = su_anisotropy

        self.extended_print = False
        log.info( "Created new Material:\n %s " % str(self))

    def __str__(self):
        repr_str = "Material '%s'\n" % self.name
        attrs = filter(lambda a : a[0] != '_', dir(self))
        if not self.extended_print:
            attrs = ['name', 'Ms', 'exchange_coupling',
                     'multiferroic_coupling',
                     'anisotropy', 
                     'anisotropy_order', 'llg_gamma_G', 'llg_damping',
                     'llg_normalisationfactor', 'do_precession',
                     'llg_polarisation', 'llg_xi', 'thermal_factor',
                     'extended_print']

        for attr in attrs:
            repr_str += " %25s = %s\n" % (attr, eval('str(self.'+attr+')'))

        return repr_str

######################################################
#
# Our interface to the LAM (Linear Algebra Machine):
#               The Fields class
#
######################################################


class Fields:

    """

    Terminology:
    ============

    primary fields: M, H_ext (J, Electric_ext)

    dependent fields: H_demag, E_demag, H_exch, E_exch, ...

    The actual calculation is done in the Linear Algebra Machine (LAM). In
    general, as data will be (MPI) distributed over several machines, we
    have a 'master buffer' for every field at the master machine. What the
    user has direct access to, is this master buffer. (We migth as well
    call it user buffer but this would be misleading.)

    There is a non-trivial set of dependencies. To correctly re-compute
    and update all these fields as required, we use an inference engine,
    as specified above.

    The Fields class
    ================

    Note: The fields provided by the Fields-hash object are used
    internally only.  (They are ocaml pills). Potential problem: once
    a field is returned, we cannot know wether it is still up-to date
    (the user could store this object, change the magnetisation and
    then query the field-pill again. It will not have updated
    automatically.)
    
    Rethink this for next generation of code. A field should be
    a clever Python object with 'properties' etc. We are
    not sure yet, though, what a good userinterface would be,
    so we'll delay this design decision.

    """
    
    def __init__(self, master_mwes_and_fields_by_name,
                 lam, mesh_unit_length,
                 primary_fields=['m', 'H_ext', 'pin', 'current_density', "Electric_ext"]):
        #test this
        self.fieldnames = master_mwes_and_fields_by_name.keys()

        self.primary_fields = primary_fields

        self._shape_by_subfieldname = {} #convenience  #Hans update this

        self._master_mwes_and_fields_by_name = master_mwes_and_fields_by_name
        self._lam = lam
        self._mesh_unit_length = mesh_unit_length

        def to_mbuf_copier(fieldname):
            def fun():
                log.debug("DDD LAM -> MBUF field '%s'" % fieldname)
                (mwe,field)=master_mwes_and_fields_by_name[fieldname]
                ocaml.lam_get_field(self._lam,field,"v_"+fieldname)
            return fun

        def from_mbuf_copier(fieldname):
            def fun():
                log.debug("DDD LAM <- MBUF field '%s'" % fieldname)
                (mwe,field)=master_mwes_and_fields_by_name[fieldname]
                ocaml.lam_set_field(self._lam,field,"v_"+fieldname)
            return fun

        def lam_executor(name):
            def fun():
                log.debug("DDD LAM exec '%s'" % name)
                ocaml.lam_execute(lam,name,[],[])
            return fun

        have_stt = master_mwes_and_fields_by_name.has_key("current_density")
        self._dependencies=_field_dependency_engine(to_mbuf_copier,
                                                    from_mbuf_copier,
                                                    lam_executor,
                                                    have_stt=have_stt)

        self._fieldname_by_subfieldname_cache = {}

        self._to_mbuf_copier = to_mbuf_copier

        #Sometimes, we only need the _structure_ of a field, so it doesn't matter
        #wether the data is up-to-date.
        self.nutd = {} #nutd stands for Not-Up-To-Date fields 

        for fieldname in self.fieldnames:
            mwe,field = self._master_mwes_and_fields_by_name[fieldname]
            self.nutd[fieldname] = field

    def __setitem__(self,fieldname,value):
        raise NotImplementedError,"Use set_subfield instead"

    def __getitem__(self,fieldname):
        log.debug("Fields.__getitem__: fieldname = '%s'" % fieldname)

        self._dependencies.make("mbuf_"+fieldname)
        mwe,field = self._master_mwes_and_fields_by_name[fieldname]
        return field

    def __repr__(self):
	out = 'Fields are:\n'
        for fieldname in self._master_mwes_and_fields_by_name.keys():

            subfields_and_shape_list = nfem.data_doftypes(self.nutd[fieldname])
            for subfieldname,shape in subfields_and_shape_list:
                out += "\t%15s -> %15s (shape=%s)\n" % (fieldname,subfieldname,str(shape))
            
	return out

    def keys(self):
        """Need this to pretend this was a dictionary object."""
        return self.nutd.keys()

    def fieldname_by_subfieldname(self,subfieldname):
        """We need occasionally to find a field for a given subfield name.
        """

        fieldname = self._fieldname_by_subfieldname_cache.get(subfieldname)

        if fieldname == None: #fieldname == None, don't know this subfield,
              #need to update dictionary
              for fieldname in self.keys():
                  subfields_and_shape_list = nfem.data_doftypes(self.nutd[fieldname])
                  for subfieldname2,shape in subfields_and_shape_list:
                      self._fieldname_by_subfieldname_cache[subfieldname2]=fieldname

              fieldname = self._fieldname_by_subfieldname_cache.get(subfieldname)

              if fieldname == None: # this is an error
                  raise KeyError("Couldn't find field for subfieldname "
                                 "'%s' " % subfieldname
                                 + "Available fields and subfields  are \n"
                                 + self.__repr__())
        return fieldname
    

    def set_subfield(self, subfieldname, values, unit, auto_normalise=False, fieldname=None):
        """Can either provide [list of] subfield[s] to which to apply the operation, or
        can set subfieldname=None AND provide a fieldname, in which case this will be applied
        to all subfields in that field.
        """

        if unit.value == 0:
            raise NmagUserError,"The unit (SI) object must have a value different from 0."

        factor = simulation_units.of(unit)
        log.log(15,"set_subfield: Conversion factor is %s" %str(factor))

        if subfieldname:
            #get field for this subfieldname
            fieldname2 = self.fieldname_by_subfieldname(subfieldname)

            #consistency check: does user provided field agree with this?
            if fieldname:
                assert fieldname2 == fieldname,"Inconsistency: subfield '%s' " % subfieldname \
                       +"is not in field '%s' (as user suggests) but " % fieldname \
                       +"in field '%s'" % fieldname2

            fieldname = fieldname2

            #What is the type of subfieldname, and what does it
            if type(subfieldname)==types.StringType: #just one subfieldname
                subfieldnames = [subfieldname]
            elif type(subfieldname)==types.ListType: #probably a list of subfieldnames
                if type(subfieldname[0]) == types.StringType: #okay
                    subfieldnames = subfieldname
                else:
                    raise NmagUserError,"set_subfield: subfieldname='%s' is list of '%s' -- I don't understand." \
                          % (str(subfieldname),str(type(subfieldname)))
            else:
                raise NmagUserError,"set_subfield: subfieldname='%s' is type '%s' -- I don't understand." \
                      % (str(subfieldname),str(type(subfieldname)))


        else: #no subfieldname given
            if fieldname == None:
                raise NmagUserError,"Need to know subfieldname OR fieldname. Both are missing."
            else:
                #iterate over all subfields:
                list_of_subfield_and_shape_tuples = nfem.data_doftypes(self.nutd[fieldname])
                #Thomas, at this point, we don't actually want to update self._fields[fieldname]!
                subfieldnames = zip(*list_of_subfield_and_shape_tuples)[0]

        #Now we have a field name, and a list of corresponding subfieldnames that need to be modified.
        #Check that this can be modified
        if not fieldname in self.primary_fields:
            raise NmagUserError,"Can only set primary fields. You have tried to set '%s' (subfield %s)" \
                  % (fieldname,str(subfieldnames))

        #Let's make sure we update all the internal logic

        self._dependencies.invalidate("lam_"+fieldname)

        #Finally, update the fields
        for subfieldname in subfieldnames:
            unit_for_pos = simulation_units.conversion_factor_of(SI('m'))
            fefields.flexible_set_fielddata(self.nutd[fieldname],
                                            subfieldname,
                                            values,
                                            pos_unit_length=unit_for_pos,
                                            scale_factor=factor,
                                            normalise=auto_normalise)

    def shape_by_subfieldname(self,fieldname):
        
        if self._shape_by_subfieldname == {}:
            for field_name in self.nutd.keys():
                
                field=self.nutd[field_name]
                name_shape=ocaml.data_doftypes(field)
                for name,shape in name_shape:
                    self._shape_by_subfieldname[name]=shape
            log.debug("Fields: have populated self._shape_by_subfieldname: "
                      "'%s'" % self._shape_by_subfieldname)
                    
        return self._shape_by_subfieldname[fieldname]

class LAMTimestepper:
    def __init__(self,
                 name,
                 rel_tolerance=1e-6,
                 abs_tolerance=1e-6,
                 max_order=2,
                 krylov_max=300,
                 pc_rtol=1e-2,
                 pc_atol=1e-7,
                 initial_time=0.0):
        self.name = name
        self.rel_tolerance = rel_tolerance
        self.abs_tolerance = abs_tolerance
        self.tol_factor = 1.0
        self.max_order = max_order
        self.krylov_max = krylov_max
        self.initial_time = initial_time
        self.is_initialised = False
        self.pc_rtol = pc_rtol
        self.pc_atol = pc_atol

class SetLatticePoints:
    """Set the lattice points for the "virtual" copies of the system. XXX DOCUMENT-ME!"""
    def __init__(self,
                 vectorlist=[[0.0,0.0,0.0]],
                 scalefactor=SI(1,'m')
                 ):
        identity_matrix = [[1.0,0.0,0.0],[0.0,1.0,0.0],[0.0,0.0,1.0]]
        ortho_transf = 0
        greyfact = 1.0
        transf_ls=[]   # list of transformations to be done on the repeated system (orthogonal transf,
                       # displacement, weight)
        conversionfactor=simulation_units.of(scalefactor) # scale factor is expressed in su
        
        if [0.,0.,0.] not in vectorlist:
            vectorlist.append([0.0,0.0,0.0])

        for vector in vectorlist:
            su_vector = numpy.array(vector,'d')*conversionfactor
            transf_ls.append( (ortho_transf,greyfact,su_vector.tolist()) ) 
        self.structure = ([identity_matrix],transf_ls)

###        ortho_transf = 0
###        greyfact = 1
###        displacement = [1000,0,0]  
###        displacement2 = [0,0,0]  
###        self.structure = ([identity_matrix],
###                          [(ortho_transf,greyfact,displacement),
###                           (ortho_transf,greyfact,displacement2),
###                           ])





class Simulation(SimulationCore):
    """

      :Parameters:

        `name` : string
          Name of the simulation object; this is used e.g. for prefixing
          filenames created by nmag.

          Default value is the name of the current script (sans extension).

        `do_demag` : bool
          Pass ``False`` to disable the demagnetisation field.

        `do_anisotropy_jacobian` : bool
          Pass ``True`` to enable the inclusion of derivatives from the
          anisotropy into the Jacobian. (Complicated anisotropy terms may blow
          up memory requirements for the Jacobian.)

          Default value is ``True``.

        `temperature` : SI Object
          Simulated temperature (unless equal to None, stochastic thermal
          fluctuations will be enabled).

          Currently not supported (since July 2008)

        `thermal_delta_t` : SI Object
          Time step to use when stochastic thermal fluctuations are enabled.

          Currently not supported (since July 2008)

        `timestepper_max_order` : int
          Maximum order for the time integrator (we use the BDF method).

          Default value is 2.

        `timestepper_krylov_max` : int
          Maximum dimension of the Krylov subspace to be used in the time
          integrator.

          Default (recommended) value is 300.

        `ksp_tolerances`: dictionary
          Keys to this dictionary are:
          DBC.rtol DBC.atol DBC.dtol DBC.maxits          
          NBC.rtol NBC.atol NBC.dtol NBC.maxits

          Values are the petsc KSP-solver tolerances for the Dirichlet and
          von Neumann Laplace solvers used internally to compute the magnetic
          scalar potential from magnetic charge density.
    """

    def __init__(self,
                 name=None,
                 phi_BEM=None,
                 periodic_bc=None,
                 do_demag=True,
                 do_anisotropy_jacobian=False,
                 temperature=None,
                 thermal_delta_t=None,
                 user_seed_T=0,
                 timestepper_max_order=2,
                 timestepper_krylov_max=300,
                 ksp_tolerances={},
                 adjust_tolerances=False,
                 use_pvode=True,
                 lam_debugfile=None
                 ):

        memory_report("beginning Simulation Class Constructor")

        SimulationCore.__init__(self, name, do_demag=do_demag,
                                id="FE Simulation class")

        #first, display data about the version of this code.
        
        #We do this here (i.e. when a Simulation object is created
        #rather than when the nmag module is imported, because the
        #version information is somewhat irrelevant when playing with
        #nmag at the prompt but important in output files for
        #simulation (which always have a simulation object).

        import nsim.versions
        log.info(nsim.versions.get_nmag_release_dist_svn_string())
        log.info(nsim.versions.get_nmag_paths_string())

        # We have used two approaches to run simulations. Up to Jan
        # 2008 (around revision 5200) we have used a cvode time
        # integrator that was run on the master process, and was
        # controlling the linear algebra machine (lam). Then, we have
        # added an integrator to the lam, which can either be pvode
        # (for fully parallelised runs) or cvode. The conceptual
        # change is that the time integrator in the new approach is a
        # part of the lam. We expect (24 Jan 2008) that in the long
        # run only the new approach is used, and that the old one is
        # only helpful for debugging or performance comparisons that
        # are relevant to the nmag team.
        #
        # We do not support the CVODE ts anymore, mf 13 Aug 2009
        if not use_pvode:
            raise NmagUserError, "The cvode interface is no longer supported"

        # ts_in_lam should be renamed to ts after sequential ts code has been removed
        self.ts_in_lam = LAMTimestepper("timestepper",
                                        krylov_max=timestepper_krylov_max,
                                        max_order=timestepper_max_order)

        # Use by the convergence criterion computation
        self._previous_m_field = None

        # When we create the timestepper (for the first time), we need to have
        # m initialised. This flag indicates this, and is not used thereafter.
        self._m_has_been_set = False

        ######## Attributes to be used in _create_lam()
        self._lam = None
        self._do_demag=do_demag
        self._do_anisotropy_jacobian=do_anisotropy_jacobian
        self._lam_debugfile=lam_debugfile
        self._ksp_tolerances=ksp_tolerances
        self._master_mwes_and_fields_by_name = None
        self._norm_dist_fun_m = None
        self._temperature = temperature
        self._thermal_delta_t = thermal_delta_t
        self.user_seed_T=None
        self._su_temperature = None
        self._su_thermal_delta_t = None

        if temperature:
            if not thermal_delta_t:
                msg = ("Need to pass thermal_delta_t as well (time step for "
                       "Heun time integrator) when passing a temperature.")
                raise NmagUserError, msg
            self._su_temperature = simulation_units.of(temperature, compatible_with=SI("K"))
            self._su_thermal_delta_t = simulation_units.of(thermal_delta_t, compatible_with=SI("s"))
            self.user_seed_T=user_seed_T
        else:
          if thermal_delta_t:
              raise NmagUserError, ("Cannot pass thermal_delta_t "
                                    "without passing a temperature.")
        ########


        ######### Attributes to be set in load_mesh()
        self.mesh = None               #The mesh

        self.mesh_unit_length = None   #SI object that defines what the distance
                                       #1 in the mesh file means

        self.materials_by_regionname = None
                                       #dictionary: each key is region name,
                                       #each value is list of MagMaterial objects

        self.region_name_list = None   #list of names of mesh regions in order as
                                       #in mesh, starting with region 1 (i.e. no vacuum)
                                       
        self.regionnames_by_regionid = None
                                       #dictionary: same information as in self_name_list
                                       #but more consistent with our naming conventions.

        self.regionid_by_regionname={} #dictionary: each key is region name (as defined
                                       #when load_mesh() is called, the value is the
                                       #corresponding regionid [integer]. This is the
                                       #inverse to self.region_name_list

        self.mag_mat_by_name = {}      #dictionary: each key is a material name,
                                       #each value is a MagMaterial object
        self._fields = None
        #########

        self.save_field_blacklist = ["grad_m"] # The field that we don't want to save

        ######### Attributes computed when needed first

        self._material_volume_by_material_name ={}
                                       #Needed everytime we save data to the ndt file
                                       #as we integrate subfields, and then need to divide
                                       #through the volume they occupy to obtain the average.
                                       #We cache the volumes here, labelled by material
                                       #names (as this is how we save the data)

        ######### Attributes used in _create_lam():

        #energy is mu0*M*H, so we need mu0 here (could use J=mu*M to get around this )
        self.energy_factor = simulation_units.of(si.mu0)
        self._periodic_bc = periodic_bc

        if phi_BEM == None:
            self.use_hlib = False
            self.hlib_params = None

        elif phi_BEM.__module__=='nmag.hlib':
            log.info("Using HLib to compress the BEM matrix. "
                     "HMatrix setup parameters are: %s"
                     % str(phi_BEM))
            if not hlib.initialize_library(logmsg=log.debug):
                raise NmagUserError("Cannot initialise HLib!")
            self.hlib_params = phi_BEM.get_hlib_parameters_internal()
            self.use_hlib = True

        ######### Use euristics for tolerance adjustments

        self._adjust_tolerances = adjust_tolerances

        ######### Attributes to be set in set_local_magnetic_coupling
        self.local_couplings = {}

        ######### Used to save a log of time integrator statistics

        self._stat_writer = ColWriter(out=self._statfilename())
        # Specify the type for some columns (the default type is integer)
        float_cols = ['hinused', 'hlast', 'hcur', 'tcur', 't_J*V',
                      't_pc_setup', 't_pc_solve', 't_RHS', 't_Re_Jacobi']
        self._stat_writer.define_columns([ColDescriptor(name=nm, type='float')
                                          for nm in float_cols])

        #########

        # 1 degree per nanosecond corresponds to 1.74532925199e-05 in su.
        self.stopping_dm_dt = simulation_units.of(1.0*si.degrees_per_ns,
                                                  compatible_with=SI("1/s"))

        self.max_dm_dt = None # Used for convergence checks

        self.convergence = convergence.Convergence()

        # The value of exact_tstop which is used by advance_time when the
        # optional argument is not given. This is also the value used by the
        # relax and hysteresis methods.
        self._default_exact_tstop = True

        import nsim.logtools
        log.debug("Simulation loggers are: \n\%s" % nsim.logtools.logging_status_str())

        memory_report("end Simulation Class Constructor")

    # Provide timestepper property to user to be able to access
    # time stepper (and to change tolerances).
    def _get_timestepper(self):
        return ocaml.lam_get_ts_cvode(self._lam, self.ts_in_lam.name)

    timestepper = property(_get_timestepper)

    def _write_stats(self):
        """Save some statistics for timestepper performance into a log file."""
        if self.timestepper == None: return
        stats = self.timestepper.get_stats()
        self._stat_writer.write_row(stats)

    def _write_stats_pvode(self):
        #times_vals = self.fun_timings()
        #times_descs = ["t_J*V", "n_J*V", "t_pc_setup", "n_pc_setup",
                       #"t_pc_solve", "n_pc_solve", "t_RHS", "n_RHS",
                       #"t_Re_Jacobi", "n_Re_Jacobi"]
        #times = zip(times_descs, times_vals)
        cvode = ocaml.lam_get_ts_cvode(self._lam, self.ts_in_lam.name)
        stats = ocaml.cvode_get_stats(cvode)
        self._stat_writer.write_row(stats)

    def set_params(self, stopping_dm_dt=None, ts_rel_tol=None,
                   ts_abs_tol=None, exact_tstop=None):
        """
        Set the parameters which control the accuracy and performance
        of the simulation.

        :Parameters:

          `ts_rel_tol` : float
            the relative error tolerance (default is 1e-6) for the timestepper

          `ts_abs_tol` : float
            the absolute error tolerance (default is 1e-6) for the timestepper

          `stopping_dm_dt` : SI_ object
            the value used in the hysteresis_ and relax_ functions to decide
            whether convergence has been reached. If the largest value for dm/dt drops
            below ``stopping_dm_dt``, then convergence has been reached.

            The default value for ``stopping_dm_dt`` this is that the
            magnetisation changes less than one degree per nanosecond,
            i.e. ``stopping_dm_dt = SI(17453292.519943293,['s',-1])``.

          `exact_tstop` : bool
            the value of exact_tstop which is used by the advance_time method
            when the optional argument is not given. This is also the value
            used by the relax and hysteresis methods. See the documentation
            of advance_time for further details.

        Note that this command has to be issued *after* having created
        an m-field with the set_m_ command.
        """
        msg = "" # just for logging the params which have been set
        if stopping_dm_dt != None:
            self.stopping_dm_dt = \
              simulation_units.of(stopping_dm_dt, compatible_with=SI("1/s"))
            msg += ' stopping_dm_dt=%s' % stopping_dm_dt

        if ts_rel_tol != None:
            self.ts_in_lam.rel_tolerance = ts_rel_tol
            self.ts_in_lam.tol_factor = 1.0
            msg += ' ts_rel_tol=%s' % ts_rel_tol
 
        if ts_abs_tol != None:
            self.ts_in_lam.abs_tolerance = ts_abs_tol
            self.ts_in_lam.tol_factor = 1.0
            msg += ' ts_abs_tol=%s' % ts_abs_tol

        if exact_tstop != None:
            self._default_exact_tstop = exact_tstop


        self.ts_in_lam.is_initialised = False

        if msg == "": msg = "no change in parameters!"
        log.info('Simulation(name=%s).set_params():%s'
                 % (self.name, msg))

    def load_mesh(self, filename, region_names_and_mag_mats, unit_length, do_reorder=False,manual_distribution=None):
        """
        :Parameters:

          `filename` : string
            The file that contains the mesh in nmesh format (ascii or hdf5)

          `region_names_and_mag_mats` : list of 2-tuples
            A list of 2-tuples containing the region names and the
            magnetic materials associated to each region. For example,
            having two spheres (called ``region_A`` and ``region B``)
            with materials A and B in the mesh, the argument would be
            [("region_A", A),("region_B",B)] where
            A and B must have been defined previously as ``nmag.MagMaterial``.

            Having two Materials X and Y both defined in region A (as in a magnetic
            two-component alloy), we would use [("region_A",[X,Y])]. 

          `unit_length` : SI object
            The SI object defines what a length of 1.0 in the mesh file
            corresponds to in reality. If the length 1.0 in the mesh corresponds to
            a nanometer, then this SI object would be given as SI(1e-9,"m")

          `do_reorder` : bool
            If set to True, metis will be called to reorder the mesh
            (aiming to bring together node ids that correspond to node
            locations that are spatially close to each other). If this
            doesn't make sense to you, you should probably leave the
            default (which is ``False``).

            Generally, we recommend to order a mesh using ``nmeshpp
            --reordernodes mesh.nmesh orderedmesh.nmesh``, and *not to
            use* this reodering option here, if you think you need to
            order it.

            If you know nmag really well (you are probably a member of 
            the core team) then read on.

            The use of ``do_reorder`` *can* make sense if either your
            mesh is not ordered already, or you provide a
            ``manual_distribution`` of nodes.

            The use of ``do_reorder`` makes no sense, if you run on more
            than one CPU and leave the distribution of the nodes to
            nmag (i.e. you use the default
            ``manual_distribution==None``).
                        
          `manual_distribution` : list of integers
            This list (if provided) describes how many nodes are to be
            put onto which CPU under MPI-parallelized execution.  If
            this is ``None`` (i.e. the default), then the distribution
            is done automatically (through metis). This parameter
            should generally not be used (unless you really know what
            you are doing).

        :Returns:
           `mesh` : mesh object

        """

        log.info("Reading mesh from %s, unit_length is %s. " % (filename,unit_length))

        memory_report("Beginning of load_mesh()")

        if self.mesh:
            raise NmagUserError("Mesh is already present!")

	#Remember the mesh unit length for writing data files
        self.mesh_unit_length = unit_length

        if manual_distribution:
            do_distribute=False
            log.info("load_mesh: will use manual distribution of nodes (%s)" % manual_distribution)
        else:
            do_distribute=True

	#Need to convert the mesh positions into simulation units. The mesh positions are SI when 
	#the unit_length is multiplied with the actual coordinates. We thus only have to convert the 
	#unit_length SI object into simulation units:
	scalefactor=simulation_units.of(unit_length, compatible_with=SI(1,'m'))

        log.log(15,"User sets mesh_unit_length to be %s (in load_mesh)" % str(self.mesh_unit_length))

        memory_report("just before nmesh.load")
        self.mesh = nmesh.load(filename,do_reorder,do_distribute)
        memory_report("just after nmesh.load")
        
        if(manual_distribution):
            ocaml.mesh_set_vertex_distribution(self.mesh.raw_mesh,manual_distribution)

        if scalefactor != 1.0:
            log.debug("Scaling mesh coordinates with factor %g" % scalefactor)
            self.mesh.scale_node_positions(scalefactor)
        else:
            log.debug("No need to scale mesh coordinates")

        self.region_name_list = map((lambda x:x[0]), region_names_and_mag_mats)
        if len(self.region_name_list) != self.mesh.numregions-1:
            raise "Found inconsistency between the mesh you loaded and the " \
                  "provided list of (region_name, materials). The mesh has " \
                  "%d regions but the provided list has %d pairs." \
                  % (self.mesh.numregions-1, len(self.region_name_list))

        self.regionnames_by_regionid = {}
        i = 1
        for regionname,materials in region_names_and_mag_mats:
            self.regionnames_by_regionid[i] = regionname
            i += 1

        #Complementary  data structure to self.region_name_list. I.e. if I know the region's
        #name but want to know the id, then I can use this dictionary.
        for id,name in enumerate(['vacuum']+self.region_name_list):
            self.regionid_by_regionname[name]=id

        log.debug("self.regionid_by_regionname: = %s",  self.regionid_by_regionname)

        self.materials_by_regionname = {}
        for name, mag_mat in region_names_and_mag_mats:
            log.info("Adding region '%s' to '%s' simulation object" % (name, self.name))
            if type(mag_mat) == list:
                mats = mag_mat
            else:
                mats = [mag_mat]
            if self.materials_by_regionname.has_key(name):
                raise "Bad usage of the 'load_mesh' method! Your list of " \
                      "(region_name, materials) contains two regions with " \
                      "the same name '%s'. Different regions must have " \
                      "different names!" % name
            self.materials_by_regionname[name] = mats

            # ensure we have all materials registered in the simulation context...
            for m in mats:
                self.mag_mat_by_name[m.name] = m


        #We also need (in compute_averages) the regions in which a material exists as a
        #hast with the region id as the key. Let's construct this here.

        memory_report("just before creating lam")
        self._create_lam()
        memory_report("just after creating lam")

        self._fields = Fields(self._master_mwes_and_fields_by_name,
                              self._lam,
                              self.mesh_unit_length)

        memory_report("End of load_mesh")

    def save_mesh(self, filename, scontext=None):
        #Do we actually need this function? (Hans)
        if not(self.mesh):
            raise NmagUserError("No mesh!")
        self.mesh.save( filename )

    def set_local_magnetic_coupling(self, mat1, mat2, coupling):
        """Add a coupling between the magnetisations of two materials...
        """
        if self.ts_in_lam.is_initialised:
            raise NmagUserError, \
                   ("Too late to specify the local magnetic coupling: "
                    "the timestepper has already been created!")
        name1 = mat1.name
        name2 = mat2.name
        if not self.local_couplings.has_key(name1):
            self.local_couplings[name1] = {}
        if not self.local_couplings.has_key(name2):
            self.local_couplings[name2] = {}

        if (   self.local_couplings[name1].has_key(name2)
            or self.local_couplings[name2].has_key(name1)):
            raise NmagUserError, ("A coupling %s-%s has been "
                                  "already specified!" % (name1, name2))

        f = float(coupling/si.mu0) # This is a pure number, without units!
        en_dens = simulation_units.of(0.5*coupling*mat1.Ms*mat2.Ms,
                                      compatible_with=SI("J/m^3"))
        Ms1 = simulation_units.of(mat1.Ms, compatible_with=SI("A/m"))
        Ms2 = simulation_units.of(mat2.Ms, compatible_with=SI("A/m"))
        self.local_couplings[name1][name2] = (f*Ms2, en_dens)
        self.local_couplings[name2][name1] = (f*Ms1, en_dens)

    def _create_lam(self):
        region_materials = map(lambda name: self.materials_by_regionname[name], self.region_name_list)

        #add empty list at position 0 to represent vacuum
        region_materials = [[]]+region_materials

        # The lam (linear algebra machine)
        self._lam, self._master_mwes_and_fields_by_name = \
          nmag_lam.nmag_lam(timestepper=self.ts_in_lam,
                            name=self.name,
                            mesh=self.mesh.raw_mesh,
                            region_materials=region_materials,
                            energy_factor=self.energy_factor,
                            periodic_bc=self._periodic_bc,
                            su_temperature=self._su_temperature,
                            user_seed_T=self.user_seed_T,
                            use_hlib=self.use_hlib,
                            do_demag=self._do_demag,
                            do_anisotropy_jacobian=self._do_anisotropy_jacobian,
                            ksp_tolerances=self._ksp_tolerances,
                            local_couplings=self.local_couplings,
                            hlib_params=self.hlib_params,
                            lam_debugfile=self._lam_debugfile,
                            )

        mwe_m,field_m = self._master_mwes_and_fields_by_name['m']
        self._norm_dist_fun_m = ocaml.mwe_norm_dist_fun(mwe_m)

    def advance_time(self, target_time, max_it=-1, exact_tstop=None):
        """
        This method carries out the time integration of the
        Landau-Lifshitz and Gilbert equation.

        :Parameters:

          `target_time` : SI Object
            The simulation will run until this time is reached. If the target_time is
            zero, this will simply update all fields.

          `max_it` : integer
            The maximum number of iterations (steps) to be carried out
            in this time integration call. If set to ``-1``, then there is no limit.

          `exact_tstop` : boolean
            When exact_tstop is True, the time integration is advanced exactly
            up to the given target_time. When False, the time integration ends
            "close" to the target_time. The latter option can result in better
            performance, since the time integrator is free to choose time
            steps which are as wide as possible. When exact_tstop is not
            given, or is None, the default value for this option will be used.
            The default value can be set using the method set_params, which
            should hence be used to control the behaviour of the hysteresis
            and relax methods.
        """
        memory_report("Beginning of advance_time")

        if self._temperature:
            raise NotImplementError, ("Cannot do thermal integration with "
              "ts_in_lam time integration. If you need this feature, please "
              "contact the nmag team (nmag@soton.ac.uk), and report this "
              "error message (ticket:150)")

        # If max_it is negative or is None, then we advance only
        # to reach the target time, without any step number check
        if max_it < 0 or max_it == None:
            max_it = -1

        if exact_tstop == None:
            exact_tstop = self._default_exact_tstop

        target_time_su = simulation_units.of(target_time, compatible_with=SI("s"))

        self._fields._dependencies.make("lam_m") # make sure distributed m-vector is
                                                 # up-to-date as distributed timestepper
                                                 # initialises itself from the
                                                 # distributed physical primary vector(s)

        if not self.ts_in_lam.is_initialised:
          self.reinitialise()

        if self._previous_m_field == None:
            mwe, field = self._master_mwes_and_fields_by_name['m']
            self._previous_m_field = \
              ocaml.raw_make_field(mwe, [], "", self.name + "_previous_m")

        #self._write_stats_pvode() # Don't know if the place is good for this...

        #if target time is 0, then just call update_H_fields
        if target_time_su == 0:
            log.debug("advance_time: called with target_time = 0.0. "
                      "Not doing anything.")
            return target_time

        log.log(15, "advance_time: working towards t=%s (su:%g)" % (target_time, target_time_su))

        log.debug("advance_time: target_time=%s, max_it=%s" % (target_time, max_it))

        # Make sure primary fields are the same in LAM as in master buffer
        for f in self._fields.primary_fields:
            self._fields._dependencies.make("lam_"+f)

        delta_norms = None
        if self._temperature == None:
            m_mwe, m_field = self._master_mwes_and_fields_by_name['m']
            ocaml.field_copy_into(m_field, self._previous_m_field)

            time_reached_su = \
              ocaml.lam_ts_advance(self._lam, self.ts_in_lam.name,
                                   exact_tstop, target_time_su, max_it)

            self._fields._to_mbuf_copier("m")()
            self._fields._dependencies.invalidate("lam_m")

            # analyse integration
            delta_norms = \
              self._norm_dist_fun_m(m_field, self._previous_m_field)

            cvode = ocaml.lam_get_ts_cvode(self._lam, self.ts_in_lam.name)
            # save last time step length, steps performed
            last_step_dt_su = ocaml.cvode_get_step_info(cvode)[0]
            delta_step = \
              ocaml.cvode_get_num_steps(cvode) - self.clock['stage_step']

            # log sundials timing info
            #times=timestepper.fun_timings()
            #log.debug("Sundials\n J*V         : %10.6f sec %4f calls\n"
            #          " pc setup    : %10.6f sec %4f calls\n"
            #          " pc solve    : %10.6f sec %4f calls\n"
            #          " RHS         : %10.6f sec %4f calls\n"
            #          " Re-Jacobi : %10.6f sec %4f calls\n" 
            #          % (times[0], times[1], times[2], times[3], times[4],
            #             times[5], times[6], times[7], times[8], times[9]))

        else:
            # perform manual time integration (Heun's method)
            t0=self.clock['time_reached_su']
            self._fields._dependencies.make("lam_m")
            ocaml.lam_set_iparam(self._lam, "TIME", t0)
            ocaml.lam_set_iparam(self._lam, "THERMAL_DELTA_T", self._su_thermal_delta_t)

            # perform fixed steps
            delta_step = 0
            while t0+self._su_thermal_delta_t <= target_time_su:
                ocaml.lam_execute(self._lam, "advance_time", [], [])
                t0 += self._su_thermal_delta_t
                delta_step += 1
                if delta_step == max_it: break
            last_step_dt_su = self._su_thermal_delta_t

            # perform one last step if target time wasn't hit exactly
            max_time_miss_su = 1e-6  # XXX: is this a sensible epsilon?
            if delta_step != max_it and (target_time_su - t0) > max_time_miss_su:
                last_step_dt_su = target_time_su - t0
                ocaml.lam_set_iparam(self._lam, "THERMAL_DELTA_T", last_step_dt_su)
                ocaml.lam_execute(self._lam, "advance_time", [], [])
                delta_step += 1

            time_reached_su=ocaml.lam_get_iparam(self._lam, "TIME")

            # copy m to master buffer
            ocaml.lam_get_field(self._lam,self._fields.nutd['m'],"v_m")

        # invalidate everything
        self._fields._dependencies.invalidate("mbuf_m")

        # move field_m_final data into field_m_initial for next call of cvode
        #ocaml.field_copy_into(field_m_final,field_m_initial)

        # debug and timings
        #ocaml.lam_execute(self._lam,"report_timers",[],[]) # DDD
        delta_time = time_reached_su - self.clock['time_reached_su']
        time_to_si = simulation_units.conversion_factor_of(SI("s"))
        delta_time_si = time_to_si*delta_time
        time_reached_si = time_to_si*time_reached_su
        self.clock['time_reached_si'] = time_reached_si
        self.clock['time_reached_su'] = time_reached_su
        last_step_dt_si = time_to_si*last_step_dt_su
        self.clock['last_step_dt_si'] = last_step_dt_si
        self.clock['last_step_dt_su'] = last_step_dt_su

        if delta_norms != None:
            max_dm = max([max_dm for _, max_dm, _ in delta_norms])
            if delta_time > 0.0: self.max_dm_dt = max_dm/delta_time

        if self._adjust_tolerances and delta_step > 0 and False:
            required_tolerance = self.stopping_dm_dt*delta_time/delta_step

            if timestepper.abs_tolerance > required_tolerance:
                required_tolerance *= 0.5
                log.info("CVODE tol: improve: %s --> %s (step %d) "
                         % (timestepper.abs_tolerance, required_tolerance,self.clock['step']))
                self.set_params(ts_abs_tol=required_tolerance,
                                ts_rel_tol=required_tolerance)
            elif timestepper.abs_tolerance < 0.25*required_tolerance:
                required_tolerance *= 0.3
                log.info("CVODE tol: relax: %s --> %s (step %d)"
                         % (timestepper. abs_tolerance, required_tolerance,self.clock['step']))
                self.set_params(ts_abs_tol=required_tolerance,
                                ts_rel_tol=required_tolerance)

            open(self._tolfilename(),'a').write('%d %f %f\n'
                                                % (self.clock['step'],
                                                   required_tolerance,
                                                   timestepper.abs_tolerance)
                                                )


        # mf: iterations_reached is returned as a float (there is a reason
        # for that, ask to thomas): but max_it needs to be an int:
        # we have to solve this inconsistency. For now we do it in the lazy way:
        # just convert 'step' to int.
        self.clock['step'] += int(delta_step)
        self.clock['time'] += delta_time_si
        self.clock['stage_step'] += int(delta_step)
        self.clock['stage_time'] = time_reached_si

        log.debug("advance_time: have reached t=%s (su:%g)" %
                  (str(time_reached_si), time_reached_su))

        log.debug("advance_time: done %d steps, "
                  "have reached sundials step count=%d, "
                  "global count=%d " % (delta_step,
                  self.clock['stage_step'], self.clock['step']))

        memory_report("end of advance_time")

        return time_reached_si

    def _probe_subfield(self,fieldname,pos,subfieldname,pos_units,si_units):
        """Probe the subfield ``subfieldname`` of field ``fieldname``
        at position ``pos``*pos_unit and return value in si numbers.

        :Parameters:
          `fieldname` : string
            name of the field 

          `subfieldname` : string
            name of the subfield

          `pos` : list of floats
            The position at which we'd like to probe the subfield

            `pos_units` : SI object
          
            Multiplicator for ``pos`` to get SI object that describes
            the position (i.e. the units of 'pos' are given in 'pos_units')

          `si_units` : SI object

            We also need to know what the correct SI units for the
            entity are ('si_units') to convert from simulation units.

        This is a low level function to be called from high-level user interfaces.
        """

        log.debug("fieldname is '%s'" % fieldname)
        log.debug("subfieldname is '%s'" % subfieldname)
        log.debug("pos_units is '%s' " % pos_units)
        log.debug("pos is '%s' " % str(pos))

        #work out position in simulation units
        factor = simulation_units.of(pos_units, compatible_with=SI(1,"m"))
        log.debug("pos_units factor is '%s'" % factor)

        su_pos = map( lambda a : a*factor, pos )

        field = self._fields[fieldname]
        
        su_data = ocaml.probe_field(field,subfieldname,su_pos)

        #if we get su_data == [], then there are two possible reasons
        # 1. such a subfield is not defined at this position
        # 2. such a subfield does generally not exist. We need to catch
        #    this point somewhere else and let the user know.

        if len(su_data)>1:
            raise NmagInternalError,"Didn't expect this: su_data=%s has more than one entry (tensor of rank2 or higher?)" % str(su_data)
        elif len(su_data) == 0: #empty list, Dof not defined at this point in space
            return None

        name,su_data = su_data[0]

        #in what units is the actual entitity measured
        factor = 1.0/simulation_units.of(si_units)

        #if data is vector, multiply all components with factor
        if type(su_data) == types.ListType:
            si_data = map( lambda a : a*factor, su_data )
        elif type(su_data) == types.FloatType:
            si_data = factor*su_data
        else:
            NmagInternalError,"Didn't expect this type: %s" % str(type(su_data))
        return si_data


    def probe_subfield(self,subfieldname,pos,unit=None):
        """for a given subfield name and position (SI object), return data (as SI object).

        Note that ``get_subfield_siv`` has the same functionality but
        takes a list of floats for the position (instead of an SI
        object) and returns (a list of) float(s) which is just the
        `SI-value`_ of that physical entity.

        If the subfield is not defined at that part of space, ``None``
        is returned.

        If the subfield does generally not exist, then a ``KeyError`` exception
        is thrown.

        :Parameters:
          `subfieldname` : string
            The name of the subfield
            
          `pos` : SI object
            The position for which the data should be returned

          `unit` : SI object
            If you request the value for a subfield of a field that is
            part of |nmag| (i.e. fields M, m, H_demag, etc), then you do
            not need to provide this object.

            If you request data of any other (multi-physics) fields,
            then this function needs to know the SI dimensions of that
            field (for the correct conversion from simulation units to
            SI units).

            If incorrect dimensions are provided, the returned data is
            likely to be wrongly scaled.

        :Returns:
          `data` : [list [of list[ of ...]]] SI objects
            The returned object is an SI object for scalar subfields,
            a list of SI objects for vector fields, a list of list of
            SI objects for (rank 2) tensor fields, etc.
            
        """


        log.debug("Entering get_subfield with subfield='%s'" % subfieldname)
        fieldname = self._fields.fieldname_by_subfieldname(subfieldname)
        log.debug("Corresponding field='%s' " % fieldname)

        if unit == None:
            if fieldname in fieldunits_by_fieldname.keys():
                unit = fieldunits_by_fieldname[fieldname]
            else:
                msg = "Couldn't find field for subfield '%s'\n" % subfieldname
                msg +="If this is an additionl (multi-physics) field, you need to provide a 'unit' parameter"
                raise NmagUserError,msg

        field = self._fields[fieldname]

        try:
            pos_units = SI(1,pos[0].units)
        except TypeError,msg:
            #This could mean that pos is not a list
            log.debug("Caught error ('%s'); could be that pos is not a list of SI objects but just an SI object" % msg)
            pos_units = SI(1,pos.units)
            log.debug("Using %s as pos_units" % pos_units)
        purepos  = map( lambda a : a.in_units_of(pos_units), pos)

        si_units = unit
        data = self._probe_subfield(fieldname,purepos,subfieldname,pos_units,si_units)

        if data == None:
            return None

        try:
            result = map(lambda a : a*unit, data)
        except TypeError: #if data is not a list
            result = data*unit

        return result


    def probe_subfield_siv(self,subfieldname,pos,unit=None):
        """
        The same behaviour as ``get_subfield`` but the ``pos`` and return
        data are `SI-value`_\ s (not SI objects). 

        If the subfield is not defined at that part of space, ``None``
        is returned.

        If the subfield does generally not exist, then a ``KeyError``
        exception is thrown.

        The input (position) and returned data is expressed in SI
        units but of type float.

        :Parameters:
          `subfieldname` : string
            The name of the subfield
            
          `pos` : list of floats
            The position for which the data should be returned (in meters)

          `unit` : SI object
            If you request the value for a subfield of a field that is part of nmag (i.e.
            fields M, m, H_demag, etc), then you do not need to provide this object.

            If you request data of any other (multi-physics) fields,
            then this function needs to know the SI dimensions of that
            field (for the correct conversion from simulation units to
            SI units).

            If incorrect dimensions are provided, the returned data is
            likely to be wrongly scaled.

        :Returns:
          `data` : [list [of list[ of ...]]] float
            The returned object is a float for scalar subfields,
            a list of floats for vector fields, a list of list of
            floats for (rank 2) tensor fields, etc.
            
        """

        log.debug("Entering get_subfield with subfield='%s'" % subfieldname)
        fieldname = self._fields.fieldname_by_subfieldname(subfieldname)
        log.debug("Corresponding field='%s' " % fieldname)

        if unit == None:
            if fieldname in fieldunits_by_fieldname.keys():
                unit = fieldunits_by_fieldname[fieldname]
            else:
                msg = "Couldn't find field for subfield '%s'\n" % subfieldname
                msg +="If this is an additionl (multi-physics) field, you need to provide a 'unit' parameter"
                raise NmagUserError,msg

        field = self._fields[fieldname]

        purepos = pos
        pos_units = SI(1,'m')
        si_units = unit
        data = self._probe_subfield(fieldname,purepos,subfieldname,pos_units,si_units)
        return data

    def _set_subfield(self,subfieldname,values,unit,fieldname,auto_normalise):        
        self._fields.set_subfield(subfieldname,
                                  values,
                                  unit,
                                  fieldname=fieldname,
                                  auto_normalise=auto_normalise)

        self.ts_in_lam.is_initialised = False

    def set_H_ext(self, values, unit=None):

        """

        :Parameters:
        
          `values` : vector (=list), function or numpy array.
            See set_m_ for an explanation of possible ``values``.
            
          `unit` : SI Object
          
            An SI Object that is used as a multiplier for the
            ``values``. This unit has to be physically compatible with
            Ampere per meter.

            To set an applied field that is homogenous and points in
            +x-direction, one can use::

              sim.set_H_ext([1e6,0,0],SI(1,"A/m"))

              which is equivalent to::

              sim.set_H_ext([1,0,0],SI(1e6,"A/m"))

            However, we could also define the field in Oersted::

              from nmag.si import Oe
              sim.set_H_ext([100,0,0],Oe)

            or in Tesla/mu0::

              from nmag.si import Tesla, mu0
              sim.set_H_ext([1,0,0],T/mu0)

        """

        
        fieldname = 'H_ext'
        
        if unit:
            #This is the default way of setting values as document for functions and numpy arrays:
            #values = (values,unit)
            v=values
            u=unit

        #However, the values argument can also be provided as a 3-list
        #of SI objects.  This is used extensively by the hysteresis
        #loop command, and can be used by users. As far as I can see
        #at the moment (fangohr 03/10/2007) this only works for a
        #3-vector (i.e. [Hx,Hy,Hz], and not for numpy arrays or
        #functions.)

        else:
            import nsim.map_terminals
            v, u = nsim.map_terminals.SI_vector(values)

        log.debug("set_H_ext(%s,%s): v=%s, u=%s" % (values,unit,v,u))

        subfieldname = None #H_Ext has no subfields
        self._set_subfield(subfieldname, v, u, fieldname=fieldname,auto_normalise=False)

    def set_Electric_ext(self, values, unit=None):

        """

        :Parameters:
        
          `values` : vector (=list), function or numpy array.
            See set_m_ for an explanation of possible ``values``.
            
          `unit` : SI Object
          
            An SI Object that is used as a multiplier for the
            ``values``. This unit has to be physically compatible with
            Volts per meter.

            To set an applied field that is homogenous and points in
            +x-direction, one can use::

              sim.set_Electric_ext([1e3,0,0],SI(1,"V/m"))

              which is equivalent to::

              sim.set_Electric_ext([1,0,0],SI(1e3,"V/m"))

        """

        
        fieldname = 'Electric_ext'
        
        if unit:
            #This is the default way of setting values as document for functions and numpy arrays:
            #values = (values,unit)
            v=values
            u=unit

        #However, the values argument can also be provided as a 3-list
        #of SI objects.  This is used extensively by the hysteresis
        #loop command, and can be used by users. As far as I can see
        #at the moment (fangohr 03/10/2007) this only works for a
        #3-vector (i.e. [Hx,Hy,Hz], and not for numpy arrays or
        #functions.)

        else:
            import nsim.map_terminals
            v, u = nsim.map_terminals.SI_vector(values)

        log.debug("set_Electric_ext(%s,%s): v=%s, u=%s" % (values,unit,v,u))

        subfieldname = None # Electric_Ext has no subfields
        self._set_subfield(subfieldname, v, u, fieldname=fieldname,auto_normalise=False)

    def set_m(self,values,subfieldname=None):
	"""
        :Parameters:
        
          `values` : vector (=list), function or numpy array.
            The values to be set. See more detailed explanation below.

        This method sets the (normalised) magnetisation  (i.e. the ``m`` field)
        to a particular value (or pattern).
        
        It can be used in three different ways:

        1. Providing a constant vector

           If given a vector, this function sets the ``m`` field to uniformly point in
           the given direction, everywhere.
 
           For example, to have the magnetisation point
           in +x-direction, we could call the function like this::
    
             sim.set_m([1,0,0])
    
           To point in a 45 degree direction between the x- and y-axis,
           we could use::
    
             sim.set_m([1,1,0])
    
           (The magnetisation will automatically be normalised.)
 

        2. Providing a function

           If the magnetisation is meant to vary spatially, then a
           function can be given to the ``set_m`` method as in this
           example::
 
             def my_magnetisation((x,y,z)):
                 # get access to pi, cos and sin
                 import math  
             
                 # change angle of Mx and My by 10 degree when x varies by 1nm
                 angle = (x*1e9)*10./360*2*math.pi 
                 Mx = math.cos(angle)
                 My = math.sin(angle)
                 Mz = 0
 
                 #return magnetisation vector for position (x,y,z)
                 return (Mx,My,Mz)
                 
             sim.set_m(my_magnetisation)
 
           The function ``my_magnetisation`` returns the magnetisation vector
           corresponding to the given 3d position in space.
           
           This position ``(x,y,z)`` as given to the function is expressed in meters.
 
        3. Providing a numpy array.

           If a numpy array is provided to set the values of the
           subfield, then the shape of this array has to match the
           shape of the subfield data. For example, if the subfield is
           the magnetisation of material X, and this material is
           defined on n mesh sites, then the array needs to have n
           entries. Each of those has to be a 3-component array,
           as the magnetisation vector has three components.

           Note: the Simulation.get_subfield() function can be used to
           obtain exactly such a numpy array for the relevant
           subfield.  

           To read such a numpy array from a file, you can use the
           get_subfield_from_h5file_ function. However, you have to be
           sure that the node order in the mesh (that is stored in the
           _dat.h5 file) is the same as the mesh you are currently
           using in your simulation. This should certainly be the case
           if (i) both runs [i.e. the saved and the current] are based
           on the same mesh, and (ii) you only us one CPU [as using
           more than one results in repartitioning and reordering of
           the mesh]. We aim to not allow setting 'wrong' data here in
           the future, but currently such checking is not
           implemented. (fangohr 31/05/2008)

        """

        unit = SI(1)

        self._set_subfield(subfieldname,values,unit,fieldname='m',auto_normalise=True)
        self._m_has_been_set = True

    def load_m_from_h5file(self, file_name):
        """Use the magnetisation stored in ``file_name`` to set the magnetisation of 
        the simulation. (If more than one magnetisation configurations have been saved
        in the file, it will load the first one.)

        This can be used to retrieve the magnetisation saved in a
        restart file, and to set the current magnetisation of the
        simulation object to this magnetisation.

        This restart file could have been written explicitely (using
        the save_restart_file_ method), or implicitly by providing 
        a 'restart' action to the hysteresis_/relax_ commands.
        
        To simply continue a hysteresis/relax simulation using the
        ``--restart`` option, there is no need to use this
        function. It should only be used if lower-level manipulation
        is required (see for example `Current-driven motion of a
        vortex in a thin film`_).

        """
        # Temporary hack to mimic an ideal world, where one needs not to care
        # about field subnames and such stuff :-)
        mwe, field = self._master_mwes_and_fields_by_name['m']
        names_and_shapes = nfem.data_doftypes(field)
        for subfield_name, shape in names_and_shapes:
            m_subfield = get_subfield_from_h5file(file_name, subfield_name)
            self.set_m(m_subfield, subfieldname=subfield_name)

    def get_timers(self, timers=[]):
        """
        Return timing statistics for time spent on individual computations done
        in the internal lam (linear algebra machine).

        if timers==[], return all timings. Otherwise, return timings for the
        chosen selection.

        Return value is a list of (timer_name,accumulated_time_in_seconds).
        """
        return ocaml.lam_get_timers(self._lam,timers)


    def set_pinning(self, values):
      """
        :Parameters:
          `values` : vector (=list), function or numpy array.

        This method sets the scalar pinning field which defines a
        local scale factor for ``dm/dt``.

        Default value is ``1.0``, use ``0.0`` to force ``dm/dt`` to
        zero, that is, to "pin" (fix) magnetisation at a certain
        position.

        Semantics of the `values` parameter match set_m_.
      """
      self._set_subfield(None, values, SI(1), fieldname='pin', auto_normalise=False)

    def set_current_density(self, values, unit=None):
      """
        :Parameters:
          `values` : vector (=list), function or numpy array.

        This method sets the current density for the electric
        current which interacts with the local magnetisation.

        Semantics of the `values` parameter match set_m_.
      """
      if unit:
        v = values
        u = unit
      else:
        import nsim.map_terminals
        v, u = nsim.map_terminals.SI_vector(values)

      log.debug("set_current_density(%s, %s): v=%s, u=%s"
                % (values, unit, v, u))

      self._set_subfield(None, # no sub-fields
                         v,
                         u,
                         fieldname='current_density',
                         auto_normalise=False)

    def _get_subfield(self,fieldname,subfieldname,si_units):
        """Probe the subfield ``subfieldname`` of field ``fieldname``
        and return value in si numbers.

        :Parameters:
          `fieldname` : string
            name of the field 

          `subfieldname` : string
            name of the subfield

          `si_units` : SI object

            We also need to know what the correct SI units for the
            entity are ('si_units') to convert from simulation units.

        This is a low level function to be called from high-level user interfaces.
        """

        log.debug("_get_subfield_numpy:fieldname is '%s'" % fieldname)
        log.debug("_get_subfield_numpy:subfieldname is '%s'" % subfieldname)

        field = self._fields[fieldname]

        # in what units is the actual entitity measured
        factor = 1.0/simulation_units.of(si_units)
        return factor*numpy.array(ocaml.mwe_subfield_data(field,subfieldname))

    def _get_subfield_posvector(self,fieldname,subfieldname,pos_si_units=SI('m')):
        """Return numpy array with positions of subfield nodes.

        :Parameters:
          `fieldname` : string
            name of the field 

          `subfieldname` : string
            name of the subfield

          `pos_si_units` : SI object

            We also need to know the desired physical dimensions of the data in the result array.

            A typical value is SI('m') for positions (this is the default).
            
        This is a low level function to be called from high-level user interfaces.
        """

        log.debug("_get_subfield_posvector:fieldname is '%s'" % fieldname)
        log.debug("_get_subfield_posvector:subfieldname is '%s'" % subfieldname)

        field = self._fields[fieldname]

        #in what units is the actual entitity measured
        factor = 1.0/simulation_units.of(pos_si_units, compatible_with=SI("m"))

        log.debug("_get_subfield_posvector:scaling factor is '%f'" % factor)

        sites, pos, shape, site_vols = ocaml.mwe_subfield_metadata(field,subfieldname)

        return numpy.array(pos)*factor


    def get_subfield(self,subfieldname,units=None):
        """
        Given a subfieldname, this will return a numpy-array
        containing all the data (one element for each site).

        :Parameters:
          `subfieldname` : string
            The name of the subfield, for example ``m_Py`` or ``H_demag``.

          `units` : SI object
          
            Optional parameter. If it is provided, then the entity is
            expressed in these units. If it is not provided, then the
            correct SI dimensions for this subfield are looked up, and
            `SI-value`_\ s are returned.

            If you would like to see simulation units in the output,
            then use ``SI(1)``.

            In short: if you omit the second parameter, you will
            obtain SI values.

        :Returns:
          `data` : numpy-array

        """
        
        fieldname = self._fields.fieldname_by_subfieldname(subfieldname)
        if units == None:
            units = fieldunits_by_fieldname[fieldname]
        return self._get_subfield(fieldname,subfieldname,units)


    def get_subfield_sites(self,subfieldname):
        """
        This function provides the node indices of the sites for data
        obtained with get_subfield_.

        :Parameters:
          `subfieldname` : string
            The name of the subfield, for example ``m_Py`` or ``H_demag``.

        :Returns:
          `data` : numpy-array
            Array containing a list of integers for every site. The
            integers within each list are node indices of the
            mesh. There will be only one integer per site in first
            order basis function calculations (which is the usual case
            in micromagnetics)
        
        """
        
        fieldname = self._fields.fieldname_by_subfieldname(subfieldname)
        field = self._fields[fieldname]
        sites, pos, shape, site_vols = ocaml.mwe_subfield_metadata(field,subfieldname)
        return numpy.array(sites)

    def get_subfield_positions(self,subfieldname,pos_units=SI('m')):
        """
        This function provides the positions of the sites for data
        obtained with get_subfield_.

        :Parameters:
          `subfieldname` : string
            The name of the subfield, for example ``m_Py`` or ``H_demag``.

          `pos_units` : SI Object
            Specifies the physical dimension in which positions are to be expressed.
            Default is ``SI(1,'m')``, which means to return site positions in meters.

            To obtain site positions in nanometers, use ``SI(1e-9,'m')``.

        :Returns:
          `pos` : numpy-array
            Array containing a position (i.e. 3 floating point
            numbers) for every site.
        

        """

        fieldname = self._fields.fieldname_by_subfieldname(subfieldname)
        return self._get_subfield_posvector(fieldname,subfieldname,pos_units)

    def region_volume(self, material_name):
        print "Warning call to region_volume"
        raise NotImplementedError,"Use material_volume instead"
        return
        """Returns the volume of the region material_name or the total volume
           of the mesh is material_name==None"""
        material_volume = 0
        if material_name == None:
            for region_id in range(1, len(self.region_name_list)+1):
                material_volume += self.mesh.regionvolumes[region_id]
            return material_volume
        if not(self.mesh): raise NmagUserError("No mesh!")

        for region_id in self.region_ids_by_materialname[material_name]:
            material_volume += self.mesh.regionvolumes[region_id]
        return material_volume

    def probe_H_demag_siv(self, pos, pos_unit=SI("m"), epsilon=1e-7):
        """BUG, FIXME: this function returns a wrong value for points outside
        the mesh. For a sphere uniformly magnetised along +x, the x component
        of the demag field outside should be positive, while it is negative.

        Compute the demag field at given position. Works inside
        and outside of magnetic materials. Note that most fields can only
        be probed where they are defined. This function computes the demag
        field at the given position on the fly, based on the boundary element
        method.

        Note that for large distances away from the magnetic material, we expect
        this not to be very accurate. Furthermore, there is an awkward technical
        problem whenever the probe point lies in-plane with any of the surface
        triangles. These awkward limitations are strongly linked to the method
        used to compute the scalar potential internally and are intrinsically
        difficult to avoid. They will go away in the future when potential
        computations will be performed with Hlib.

        Also, this function should (at present) not be used to probe the
        demag field for periodic structures.

        :Parameters:
          `pos` : list of floats
            The SI numbers described the position in meters. A command like
            ``probe_H_demag_siv([0,0,1e-9])`` would thus probe the demag field one
            nanometer away (in z-direction) from the origin.

          `pos_unit` : SI object
            Optional argument that defaults to SI("m"). The full SI position is
            computed as pos*pos_unit. The above example could therefore be written as
            ``probe_H_demag_siv([0,0,1],pos_unit=SI(1e-9,"m"))``.

          `epsilon` : float
            This parameter is used internally to compute the demag
            field via central differences from the magnetic
            potential if the observer point is in the exterior ("vacuum")
            region. It is the distance between the two points at
            which each of the field components is being computed
            (because the field is the negative gradient of the
            potential). The default value of 1e-7 should be sensible
            if normal simulation units are used (i.e. the mesh was
            provided with coordinates in the range 1-1000).
            Typically, this parameter should be ignored. Note that this number is
            measured in simulation units.

        :Returns:
          A list of floats containing the demag field in SI units (i.e. A/m) at the
          specified position.

        """

        #convert position from SI to SU
        factor = simulation_units.of(pos_unit, compatible_with=SI("m"))
        pos_su = numpy.array(pos,dtype='d')*factor
        log.debug("get_H_demag: pos=%s %s -> %s simulation units" % (pos,pos_unit,pos_su))
        
        h_demag_su=ocaml.lam_bem_field_strength(\
            epsilon,self._lam,"H_demag","v_H_demag","H_demag","BEM","v_phi1b",pos_su.tolist())

        # Convert to SI
        factor = 1.0/simulation_units.of(fieldunits_by_fieldname['H_demag'])
        h_demag_si = numpy.array(h_demag_su,dtype='d')*factor
        return h_demag_si.tolist()

    def material_volume(self,material_name):
        """Returns the volume of the region material_name or the total volume
           of the mesh is material_name==None"""

        if material_name == None: #TODO: "Do we actually need this? Hans "
            return sum(self.mesh.regionvolumes[1:]) #return total volume of mesh
            #It seems this is actually being called, although I don't know why
            #at the moment. Hans 5/08/07


        if self._material_volume_by_material_name == {}:

            if not(self.mesh):
                raise NmagUserError("No mesh!")

            material_name_list = [] #Only for debug statement

            for regionname in self.region_name_list:
                materials = self.materials_by_regionname[regionname]
                for material in materials:
                    material_name = material.name
                    if not material_name in material_name_list:
                        material_name_list.append(material_name)
                    region_id = self.regionid_by_regionname[regionname]
                    material_volume_in_this_region = self.mesh.regionvolumes[region_id]

                    if self._material_volume_by_material_name.has_key(material_name):
                        self._material_volume_by_material_name[material_name]\
                            += material_volume_in_this_region
                    else:
                        self._material_volume_by_material_name[material_name]\
                            = material_volume_in_this_region


            msg = ""
            for material_name in material_name_list:
                msg += "\tMaterial %s has volume %s (simulation units)\n" % (material_name,self._material_volume_by_material_name[material_name])
            log.debug("compute_material_volume(): Populated"+
                      "self._material_volume_by_material_name:\n%s" % msg)

        return self._material_volume_by_material_name[material_name]

    def _average_of_material_integral(self, materialname, integrated_value):
        return integrated_value / self.material_volume(materialname)

    def get_subfield_average_siv(self, field_name, subfield_name=None):
        """Returns the average of the subfield ``subfield_name`` of
        the field ``fieldname`` as a single floating point number (or
        a list if it is a vector, or a list of list for matrices etc).
        
        The number is expressed in SI units (hence the suffix _siv
        which stands for si value).

        :Parameters:
          `field_name` : string
            name of the field

          `subfield_name` : string
            name of the subfield

        Example::

          ave_M = sim.get_subfield_average_siv("M","Py")

        will obtain the average magnetisation of the subfield M_Py of field M, for example
        ``ave_M = [100000.00,0.,0.]``
        
        """
        try:
            field_units_si = fieldunits_by_fieldname[field_name]
        except:
            raise NmagUserError("Cannot compute average of '%s':"
                                " unknown field name!" % field_name)

        field_units_su = simulation_units.conversion_factor_of(field_units_si)

        # Fields are defined per space or per material: need to discriminate!
        if field_name in fields_on_materials:
            dof_name = field_name
        else:
            if not self.mag_mat_by_name.has_key(subfield_name):
                raise NmagUserError, \
                  ("You want to compute the average of '%s', but the subfield"
                   " you specified(%s) is not one of its subfields!"
                   % (field_name, subfield_name))
            dof_name = "%s_%s" % (field_name, subfield_name)

        result = nfem.integrate_field(self._fields[field_name], dof_name)
        if len(result) == 0:
            raise NmagInternalError("No dof='%s' found" % dof_name)

        returned_dof_name, value = result[0]

        assert returned_dof_name == dof_name, \
               "InternalError: returned_dof_name=%s != dof_name=%s" \
               % (returned_dof_name, dof_name)

        factor = 1.0 / self.material_volume(subfield_name)
        factor /= simulation_units.of(field_units_si)
        return recursive_map(lambda x: x*factor, value)

    def get_subfield_average(self, field_name, subfield_name=None):
        """Returns the average of the subfield ``subfield_name`` of
        the field ``fieldname`` as an SI object (or a list of [list
        of [list of [...]]] SI objects in the field is a vector, 2nd rank tensor etc.

        :Parameters:
          `field_name` : string
            name of the field

          `subfield_name` : string
            name of the subfield

        See also get_subfield_average_siv_.
        """
        try:
            field_units_si = fieldunits_by_fieldname[field_name]

        except:
            raise NmagUserError("Cannot compute average of '%s':"
                                " unknown field name!" % field_name)
        siv = self.get_subfield_average_siv(field_name, subfield_name)
        return recursive_map(lambda x: x*field_units_si, siv)

    def _compute_averages( self ):
        """Helper function for save_ndt_table, and to save the same data in the h5 file.
         (fangohr 16/03/2007)

        Return values will be lists of:

        the name (dofname) of an entity (a string)
        the value          of this entity (could be float, or list, or list of list, ...)
        the units          of this entity (SI object)

        """

        log.debug("Entering _computing_averages (step %d)"
                  % int(self.clock['step']))

        selected_fields = [fn for fn in self._fields.fieldnames
                              if (not fn in self.save_field_blacklist)]
        timer1.start('compute_averages')

        res_field_name = []
        res_field_unit = []
        res_field_value= []

        #Extra parameters
        extranames = ['time', 'id', 'step', 'last_step_dt', 'stage_time', 'stage_step', 'stage']
        extraunits_si = map(lambda key: otherunits_by_name[key], extranames)
        extradata = [simulation_units.of(self.clock['time']),
                     self.clock['id'],
                     self.clock['step'],
                     simulation_units.of(self.clock['last_step_dt_si']),
                     simulation_units.of(self.clock['stage_time']),
                     self.clock['stage_step'],
                     self.clock['stage']]

        fieldunits_si = map(lambda key : fieldunits_by_fieldname[key], selected_fields)

        fieldunits_su = []
        for unit in fieldunits_si:
            fieldunits_su.append(simulation_units.conversion_factor_of(unit))

        extraunits_su = []
        for si_unit in extraunits_si:
            extraunits_su.append(simulation_units.conversion_factor_of(si_unit))

        units='si'
        if units.lower() == 'si':
            fieldunits = fieldunits_si
            extraunits = extraunits_si
        elif units.lower() == 'su':
            fieldunits = fieldunits_su
            extraunits = extraunits_su
        else:
            raise NotImplementedError,"Can only do SI units or Simulation Units (SU).."

        #and what materials?
        materialnames = self.mag_mat_by_name.keys()
        #write data for one material after the other, in alphabetical order
        materialnames.sort()

        #first we gather some metadata (can skip this for second call)
        dof_metadata = [] #example entry: [ ('M_Dy', 'M', 'Dy', [None,None,None], SI(1,"A/m")) ]

        #work out metadata
        for fieldname, units, units_si in map(None,
                                              selected_fields,
                                              fieldunits,
                                              fieldunits_si):
            # demag is a special case as it is not defined per material...
            if fieldname in ["H_demag", "H_ext", "Electric_ext", "phi", "rho",
                             "pin", "Temperature", "current_density"]:
                dofname = fieldname
                

                dof_stem = self._fields.shape_by_subfieldname(dofname)
                skeleton = nfem.fields.make_dof_list_structure(dof_stem)
                dof_metadata.append((dofname, fieldname, None,
                                     skeleton, units, units_si))
            else:
                for materialname in materialnames:
                    dofname = "%s_%s" % (fieldname, materialname)
                    dof_stem = self._fields.shape_by_subfieldname(dofname)
                    skeleton = nfem.fields.make_dof_list_structure(dof_stem)
                    dof_metadata.append((dofname, fieldname, materialname,
                                         skeleton, units, units_si))

        #start working on the data
        for name,data,unit in map(None,extranames,extradata,extraunits):
            factor = 1.0/simulation_units.of(unit)

            #stage and step should be integers:
            if name in ['step', 'stage','id']:
                data = int(data*factor)
            else:
                data = data*factor

            log.debug('working out number for %s %s (factor=%s)' % (str(data),str(unit),str(factor)))

            res_field_name.append(name)
            res_field_unit.append(unit)
            res_field_value.append(data)

        for dofname,fieldname,material,skeleton,unit,unit_si in dof_metadata:
            timer1.start("compute_averages:get_field")
            field = self._fields[fieldname]
            timer1.stop("compute_averages:get_field")
            timer1.start('compute_averages:integrate_field')
            result = nfem.integrate_field(field, dofname)
            timer1.stop('compute_averages:integrate_field')
            if len(result) == 0:
                raise NmagInternalError,"No dof='%s' found, Known dofs are: '%s'" %\
                                            (dofname,str(nfem.integrate_field(self.nutd[fieldname])))
            dofname2, data = result[0]

            assert dofname2==dofname, "InternalError: dofname=%s!=dofname2=%s" % (dofname,dofname2)

            #work out correct conversion. First we need to know what
            #the data is in SI units but we have only the number in
            #simulation units. However, we know what SI units the
            #number should have (given in unit_si). So:

            #Now we need to express this SI object in units of the
            #required output:
            factor = 1.0/simulation_units.of(unit)

            timer1.start('compute_averages_average_of_material_integral')
            if len(skeleton) == 0: #data is just a float

                average = self._average_of_material_integral(material, data)*factor
            else: #data is a vector
                average = map( lambda datafloat : \
                               self._average_of_material_integral( material, datafloat)\
                               *factor,data)
            timer1.stop('compute_averages_average_of_material_integral')

            res_field_name.append(dofname)
            res_field_unit.append(unit_si)
            res_field_value.append(average)

        #extra stuff at end:

        #first the maxangles
        timer1.start('compute_averages_maxangles')
        dofname_maxangle_list = ocaml.mumag_vector_field_max_angles(self._fields["m"])
        timer1.stop('compute_averages_maxangles')
        for dofname,maxangle in dofname_maxangle_list:
            maxangle_deg = maxangle*360.0/2.0/math.pi
            res_field_unit.append('<deg>')
            res_field_value.append(maxangle_deg)
            res_field_name.append('maxangle_'+dofname)

        #then the time and unixtime
        t = time.localtime()
        tstr="%04d/%02d/%02d-%02d:%02d:%02d" % (t[0],t[1],t[2],t[3],t[4],t[5])

        res_field_name.append('localtime')
        res_field_unit.append('<>')
        res_field_value.append(tstr)

        res_field_name.append('unixtime')
        res_field_unit.append(SI(1,'s'))
        res_field_value.append(time.time())

        timer1.stop('compute_averages')
        return res_field_name, res_field_value, res_field_unit 

    def save_data(self,fields=None,avoid_same_step=False):
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
            current ``clock['step']`` counter is different from the
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

        log.debug("Entering save_data, fields=%s, avoid_same_step=%s" % (fields,avoid_same_step))
        timer1.start('save_data')

        #get filenames
        ndtfilename = self._ndtfilename()
        h5filename = self._h5filename()

        #check whether we have saved this step already:
        timer1.start('save_data_hd5hasstep')
        hasstep = hdf5.average_data_has_step(h5filename, self.clock['step'])
        timer1.stop('save_data_hd5hasstep')

        if hasstep and avoid_same_step:
            log.debug("save_data: No need to save step %d to %s " \
                      "(saved already)" % (self.clock['step'], h5filename))
            #in this case we have written the data already.
            pass

        else:           
            # increase unique identifier:
            self.clock['id'] += 1
            log.info("save_data(): id->id+1=%d, fields=%s " % (self.clock['id'],str(fields)))

            # first compute averages
            timer1.start('_compute_averages')
            names,values,units = self._compute_averages()
            timer1.stop('_compute_averages')

            # and write to ndt file
            timer1.start('_save_data_table_to_ndt')
            self._save_data_table_to_ndt(names,values,units,ndtfilename)
            timer1.stop('_save_data_table_to_ndt')

            timer1.start('_save_data_table_to_h5')

            #and h5 file

            #remove this for extra speed fangohr 18/12/2007)
            #self._save_data_table_to_h5(names,values,units,h5filename)
            timer1.stop('_save_data_table_to_h5')

        #if any fields are given, save them
        if fields:
            timer1.start('_save_fields')
            if type(fields) == types.StringType:
                if fields.lower() == 'all':
                    fields = []
                else: #likely been given one field in a string (but not wrapped up in list)
                    fields = [fields]
            self._save_fields(filename=h5filename,fieldnames=fields)
            timer1.stop('_save_fields')

        timer1.stop('save_data')
        log.debug("Leaving save_data")

    save_data_table = save_data

    def _create_h5_file(self,filename):

        log.debug("_create_h5_file: initial creation of '%s'" % (filename))
        hdf5.create_file(filename)

        hdf5.tagfile(filename,'nsimdata','0.1')

        #add simulation source code and config files (to be done)
        hdf5.save_simulation_files(filename,[])

        #add mesh
        hdf5.add_mesh(filename,self.mesh,self.mesh_unit_length)

        #assert self.mesh_unit_length.value == simulation_units.conversion_factor_of(SI('m')).value,\
        #       "Internal error: mesh unit length (%s) and simulation unit length %s seem to disagree" % \
        #       (str(self.mesh_unit_length),str(simulation_units.conversion_factor_of(SI('m'))))

    def _save_data_table_to_h5(self,names,values,units,filename):

        if not os.path.exists(filename):
            self._create_h5_file(filename)

        log.log(15, "step %d: Saving field(s) averaged data to '%s'"
                % (self.clock['step'], filename))

        log.debug("_save_data_table_to_h5 (filename=%s): time_reached_si is %s" \
		  % (filename,self.clock['time_reached_si'].dens_str()))


        hdf5.append_averages(filename, names, values, units)
                             #self.clock['time_reached_si'],
                             #self.clock['step'],
                             #self.clock['stage'])


    def _save_data_table_to_ndt( self, names,values,units,filename, \
                              float_format_string = "% 25.13g ", \
                              int_format_string = "% 25d ", \
                              string_format_string = "% 25s ", \
                              format_string_titleline = "%-25s ",
                              headers_done={}):

        log.debug("Entering _save_data_table_to_ndt")

        # if file doesn't exist, create new one:
        if not os.path.exists(filename):
            f = open(filename,'w')
            log.debug("opening %s for writing" % filename)
        else:
            f = open(filename,'a')
            log.debug("opening %s for appending" % filename)

        if not filename in headers_done.keys():
            #add title line

            firstline = "#"
            secondline = "#"

            for name, tensorvalue, unit in map(None,names,values,units):

                #'c' stands for component
                cnames,cvalues,cunits = nfem.tensor2componentstrings(name,tensorvalue,unit)

                for name,value,unit in map(None,cnames,cvalues,cunits):
                    firstline += format_string_titleline % name
                    log.debug("about to add %s with unit %s to header for %s" % (name,unit,filename))
                    if type(unit) == types.StringType:
                        unitstr = unit
                    else: #assume SI type
                        unitstr = unit.dens_str()
                    secondline += format_string_titleline % unitstr


            f.write(firstline+'\n')
            f.write(secondline+'\n')

            headers_done[filename]=True

        #now write data
        for name, tensorvalue, unit in map(None,names,values,units):

                #'c' stands for component
                cnames,cvalues,cunits = nfem.tensor2componentstrings(name,tensorvalue,unit)

                for name,value,unit in map(None,cnames,cvalues,cunits):
                    #log.debug("About to write %s " % name)
                    if type(value) == types.FloatType:
                        f.write(float_format_string % (value))
                    elif type(value) == types.IntType:
                        f.write(int_format_string % (value))
                    elif type(value) == types.StringType:
                        f.write(string_format_string % (value))
                    else:
                        raise ValueError,"Don't know how to format type %s" % type(value)

        f.write('\n')
        f.close()

        log.debug("_save_data_table_to_ndt: finished writing data to %s" % (filename))

    def save_fields(self,filename=None,fieldnames='all'):
        raise "InterfaceChangedError","Use save_data(fields=fields) to save fields"

    def _save_fields(self,filename=None,fieldnames=[]):

        """Save fields for current time step into hdf5 file.

        :parameters:

          `filename` : string
            The filename of the hdf5 file. Recommended extension is ".h5"

          `fieldnames` : list of fieldname strings

            A list of field names can be provided. All fields whose
            names are included in this list will be saved. An empty list
            suggests that all fields should be saved. (This is the default.)

    """

        timer1.start('save_fields')

        all_fieldnames =  self._fields.keys()

        if fieldnames==[]:
            fieldnames = [fn for fn in all_fieldnames
                          if not (fn in self.save_field_blacklist)]

        if filename==None:
            filename = self._h5filename()

        log.log(15,"save_fields: About to save the following fields %s to %s" % (str(fieldnames),filename))

        if not os.path.exists(filename):
            log.debug("save_fields: %s doesn't exist; will create it" % filename)
            self._create_h5_file(filename)

        log.log(15,"Saving field(s) %s into '%s'" % (str(fieldnames),filename))

        log.debug("save_fields (%s): time_reached_si is %s" % (filename,self.clock['time_reached_si'].dens_str()))

        # get medata for all fields, and for the ones we are meant to save.
        # We need the 'all fields' data in case the data file will be
        # created: we need to store all the dofsite metadata for all fields,
        # in case other fields will be saved later. (fangohr 28/05/2008)
        fields_to_save = {}
        all_fields_to_save = {}
        for fieldname in all_fieldnames:
            fieldunits = fieldunits_by_fieldname[fieldname]
            if fieldname in fieldnames:
                fields_to_save[fieldname]=(self._fields[fieldname],fieldunits)
            all_fields_to_save[fieldname]=(self._fields[fieldname],fieldunits)

        #This is where the actual work (i.e. saving the data) is done. Also, if the file is new, all the required
        #meta data will be added.
        timer1.start('append_fields')
        hdf5.append_fields(filename, fields_to_save, all_fields_to_save,
                           self.clock['time_reached_si'], self.clock['step'],
                           self.clock['stage'], self.clock['id'],
                           simulation_units)
        timer1.stop('append_fields')

        log.info("Written fields %s data to %s" % (str(fieldnames),filename))
        timer1.stop('save_fields')

    def reinitialise(self,
                     rel_tolerance=None,
                     abs_tolerance=None,
                     initial_time=None):
        # make sure distributed m-vector is up-to-date as distributed
        # timestepper initialises itself from the distributed physical
        # primary vector(s)
        self._fields._dependencies.make("lam_m")

        if initial_time == None:
            t0 = self.clock['time_reached_su']
        else:
            t0 = simulation_units.of(initial_time,
                                     compatible_with=SI("s"))
            self.clock['time_reached_su'] = t0
            self.clock['time_reached_si'] = \
              t0 * simulation_units.conversion_factor_of(SI("s"))

        if rel_tolerance != None: self.ts_in_lam.rel_tolerance = rel_tolerance
        if abs_tolerance != None: self.ts_in_lam.abs_tolerance = abs_tolerance

        rt = self.ts_in_lam.rel_tolerance*self.ts_in_lam.tol_factor
        at = self.ts_in_lam.abs_tolerance*self.ts_in_lam.tol_factor
        ocaml.lam_ts_init(self._lam, self.ts_in_lam.name, t0, rt, at)
        self.ts_in_lam.is_initialised = True

    def do_next_stage(self, stage=None):
        self.max_dm_dt = None
        if stage == None:
            self.clock['stage'] += 1
        else:
            self.clock['stage'] = stage
        self.clock['stage_step'] = 0
        self.clock['stage_time'] = SI(0.0, "s")
        self.clock['convergence'] = False
        self.clock['zero_stage_step'] = self.clock['step']
        self.clock['zero_stage_time'] = self.clock['time']

    def is_converged(self):
        """
        Returns True when convergence has been reached.
        """
        log.debug("Entering is_converged()")
        self.clock['convergence'] = False
        if self.max_dm_dt != None:
            converged, new_tol_factor = \
              self.convergence.check(self.step,
                                     self.max_dm_dt, self.stopping_dm_dt,
                                     self.ts_in_lam.tol_factor)
            if new_tol_factor != None and self._adjust_tolerances == True:
                print "Scaling the tolerances: factor = %f!" % new_tol_factor
                self.ts_in_lam.tol_factor = new_tol_factor
                raw_input()
                self.ts_in_lam.is_initialised = False
            self.clock['convergence'] = converged
            return converged

        else:
            log.debug("is_converged(): self.max_dm_dt == None -> False")
            return False

    # Use same data structure as for _dat.h5 files, but save only one time step.
    # Currently, we override the file every time this function is called. It
    # would be better to just update the data (saves saving all the metadata,
    # and mesh if requested etc).
    # However, first the functionality, then make it fast.
    # (Hans 15 March 2007)
    def save_restart_file(self,
                          filename=None,
                          fieldnames=['m'],
                          all=False):
        """Save current magnetic configuration into file that can be used for
        restarting. 

        This function saves the current magnetisation, the time and
        all what is needed to restart the simulation exactly from
        the point it was invoked.

        :Parameters:

          `filename` : string

            The file into which the restart file is written. Defaults
            RUNID_restart.h5.

          `fieldnames`: list  
            The fieldnames to be saved. Defaults to ['m']

          `all`:bool
            If true, then all fields will be saved.

        This function is used by the hysteresis_ and relax_ commands
        to save a magnetic configuration from which a run can be
        continued (using --restart).

        Example:

           A common usecase for this function maybe to write the
           magnetic configuration that comes from a relaxation process
           to a file. And to load that configuration as the initial
           configuration for a subsequent (series of) simulation(s).

           In this case, one may want to provide the filename explicitely.
           For example::

             sim.save_restart_file(filename="relaxed_configuration.h5")
           
           One can then use the load_m_from_h5file_, to read this file
           ``relaxed_configuration.h5`` and to use it to set the
           magnetisation up in the subsequent simulation.

        """

        if all:
            fieldnames = self._fields.fieldnames

        if filename==None:
            filenamebase = self.get_restart_file_name()
            filename = nsim.snippets.output_file_location(filenamebase)

        log.log(15, "save_restart_file: About to save the following files "
                    "%s to %s" % (fieldnames, filename))

        # This is an adjustment by mf to avoid potential disasters using
        # the function save_restart_field: suppose the machine is turned off
        # while the restart file is being saved. In this case the user
        # loses the previous restart file and the new one would be broken.
        # We absolutely have to avoid this!
        tmp_file = filename + ".internal"

        #this will override existing file
        hdf5.create_file(tmp_file, mode='w')

        #add simulation source code and config files (to be done)
        hdf5.save_simulation_files(tmp_file, [])

        #add mesh
        hdf5.add_mesh(tmp_file, self.mesh, self.mesh_unit_length)

        log.log(15, "Saving field(s) %s into '%s'" % (fieldnames, tmp_file))

        log.debug("save_fields (%s): time_reached_si is %s"
                  % (tmp_file, self.clock['time_reached_si'].dens_str()))

        fields_to_save = {}
        for fieldname in fieldnames:
            fieldunits = fieldunits_by_fieldname[fieldname]
            fields_to_save[fieldname] = (self._fields[fieldname], fieldunits)
        hdf5.append_fields(tmp_file, fields_to_save, fields_to_save,
                           self.clock['time_reached_si'],
                           self.clock['step'], self.clock['stage'],
                           self.clock['id'], simulation_units)

        log.info("Written fields %s data to %s" % (str(fieldnames), tmp_file))

        #append clock data (cruel hack to get Matteo going)
        clockasstring = repr(self.clock)
        hdf5.add_attribute(tmp_file, '/data/fields/m', 'clock', clockasstring)

        os.rename(tmp_file, filename)
        log.info("File '%s' renamed to '%s'" % (tmp_file, filename))

    def load_restart_file(self, file_name=None):
        """Reload the system status (magnetisation configuration plus
          clock object) from a restart file.
        """
        if file_name == None:
            file_name = self.get_restart_file_name()

        log.info("Restarting from file '%s'" % file_name)
        self.load_m_from_h5file(file_name)

        # This command gives the clock object from the time
        # when the restart file was saved
        clock_repr = hdf5.get_attribute(file_name, '/data/fields/m', 'clock')
        self.clock = eval(clock_repr)
        log.info("Got clock object from restart file: %s" % self.clock)
        stage = self.clock['stage']
        if self.clock['convergence']:
            log.info("Restart-file was saved at convergence of stage %d "
                     "starting stage %d" % (stage, stage+1))
            self.clock['stage'] += 1
        else:
            log.info("Restart-file was saved in the middle of stage %d "
                     "continuing from this stage!" % stage)

def get_subfield_from_h5file(filename, subfieldname, id=None, row=None,
                             unit=None):
    """
    Retrieve data from h5 file. Data are returned as `SI-value`_\ s.

    Analog to get_subfield_ (which returns subfield data for a
    subfield of a simulation object), but will retrieve data from
    saved ``_dat.h5`` file.
    
    :Parameters:
      `filename` : string
         The full name of the ``_dat.h5`` data file.
         
      `subfieldname` : string
         The name of the subfield to be retrieved.

      `id` : integer
         The ``id`` of the configuration to return (defaults to 0)

      `row` : integer 
         If the ``id`` is not specified, the ``row`` can
         be used to address the data row with index ``row``. 

         For example, the magnetisation may have been saved at some
         point during the simulation into a file (for example
         using the `Restart example`_ functionality, or using the
         save_data_ method for the first time to save the m-field
         (i.e. ``sim.save_data(fields=['m']``) into a new file). 

         We can use ``row=0`` to read the first magnetisation
         configuration that has been written into this file (and
         ``row=1`` to access the second etc).

    :Returns:
      numpy array
    """

    if unit:
        raise NotImplementedError,"This feature is not implemented yet (unit=*)."

    fh = hdf5.open_pytables_file(filename,'r') #TODO move this to hdf5 
    field = hdf5.fieldname_by_subfieldname(fh, subfieldname)
    if id == None:
        if row == None:
            row = 0
    else:
        row, _, _, _ = \
	  hdf5.get_row_stage_step_time_for_field_and_id(fh, field, id)

    supos, sidata, site = hdf5.get_dof_row_data(fh, field, subfieldname, row)

    hdf5.close_pytables_file(fh)

    return numpy.array(sidata)

def get_subfield_positions_from_h5file(filename, subfieldname):
    """
    Analogous to get_subfield_positions_ (which returns the positions of
    nodes for a subfield of a simulation object), but will retrieve
    data from saved ``_dat.h5`` file.

    :Parameters:
      `filename` : string
         The full name of the ``_dat.h5`` data file.
         
      `subfieldname` : string
         The name of the subfield to be retrieved.

    :Returns:
      numpy array
        The positions are returned as `si-value`_\ s.  

      
    """

    fh = hdf5.open_pytables_file(filename,'r') #TODO move this to hdf5 
    field = hdf5.fieldname_by_subfieldname(fh,subfieldname)
    row=0
    supos,sidata,site = hdf5.get_dof_row_data(fh,field,subfieldname,row)
    mesh_scale_factor_si = hdf5.get_mesh_unit(fh)

    hdf5.close_pytables_file(fh)
    
    return numpy.array(supos)*mesh_scale_factor_si.value


def get_subfield_sites_from_h5file( filename, subfieldname):
    """
    Analogous to get_subfield_sites_ (which returns the site ids of
    nodes for a subfield of a simulation object), but will retrieve
    data from saved ``_dat.h5`` file.

    :Parameters:
      `filename` : string
         The full name of the ``_dat.h5`` data file.
         
      `subfieldname` : string
         The name of the subfield to be retrieved.

    :Returns:
      numpy array
        The ids are returned as `si-value`_\ s.  

    """

    fh = hdf5.open_pytables_file(filename, 'r') #TODO move this to hdf5

    field = hdf5.fieldname_by_subfieldname(fh,subfieldname)

    row=0
    supos,sidata,site = hdf5.get_dof_row_data(fh,field,subfieldname,row)

    hdf5.close_pytables_file(fh)

    return numpy.array(site)

def fit_oscillation(data,freq=None):
    """Given an array of vectors whose 0th component is time
    and j-th component measurement data, fit a set of
    functions of the form
    
                c_j+a_j*math.sin(2*pi*f*t+p_j)

    against each component, i.e. parameters determined
    in this fit are the offsets d_j, amplitudes c_j,
    phase shifts p_j for each vector component, plus
    an overall frequency. The initial frequency f used
    for optimization is either guessed, or user-provided.
    
    Returns (f,[(c0,a0,p0),(c1,a1,p1),...])
    """
    from math import sin
    from scipy.optimize import leastsq
    import numpy
    
    nr_steps=len(data)
    dim=len(data[0])-1
    
    if freq==None:
        # We have to guess the number of oscillations,
        # and that guess should better be somewhat
        # in the right ballpark.
        # The strategy which we use here is to
        # (1) smoothen the function and (2) count the
        # number of sign changes. We assume that the
        # first oscillation component is non-flat.

        def smooth(n):
            return data[max(0,n-2)][1]+4*data[max(0,n-1)][1]+6*data[max(0,n)][1]+4*data[min(nr_steps-1,n+1)][1]+data[min(nr_steps-1,n+2)][1]

        nr_sign_flips=0

        for i in range(nr_steps-1):
            if smooth(i)*smooth(i+1)<0:
                nr_sign_flips+=1

        freq = 0.5*nr_sign_flips/(data[nr_steps-1][0]-data[0][0])
    
    def deviation(params):
        w=params[0]
        result=numpy.zeros(nr_steps*dim)
        for i in range(dim*nr_steps):
            k=i%dim
            n=i/dim
            time=data[n][0]
            d_nk = data[n][1+k]
            a_nk= params[1+3*k]+params[2+3*k]*sin(2*math.pi*time*params[0]+params[3+3*k])
            result[i]=d_nk-a_nk
        return result
    
    start=numpy.zeros(3*dim+1)
    start[0]=freq

    fit=leastsq(deviation,start)
    if not(fit):
        return None

    fit=fit[0]

    return (fit[0],[(fit[3*k+1],fit[3*k+2],fit[3*k+3]) for k in range((len(fit)-1)/3)])
    if fit:
        return fit[0]
    else:
        return None

