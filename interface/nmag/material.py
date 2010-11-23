"""
Definition of the MagMaterial class, which allows to define a magnetic
material and all its parameters (saturation magnetisation, exchange coupling,
etc.)
"""

from nsim.si_units import SI, si
from anisotropy import PredefinedAnisotropy

import logging
log = logging.getLogger('nmag')

class MagMaterial(object):
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

      `anisotropy` : PredefinedAnisotropy Object or function(vector) -> SI Object
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
        If a custom polynomial anisotropy function ``a(m)`` is specified, the
        order of the polynomial must be given in this parameter. This is not
        required for pre-defined uniaxial_anisotropy_ or cubic_anisotropy_
        anisotropy functions.

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
                 anisotropy=None,
                 # PredefinedAnisotropy object, or function a(m) which returns
                 # an energy density for the given direction of the
                 # magnetisation m.
                 anisotropy_order=None,
                 # Order of approximation; only specify this if you specify an
                 # anisotropy function (as opposed to a PredefinedAnisotropy
                 # object)
                 properties=["magnetic", "material"],
                 scale_volume_charges=1.0,
                 # Parameter to be set by developers for debugging.
                 # To be deleted soon.
                 ):
        self.name = name
        self.Ms = Ms
        self.llg_gamma_G = llg_gamma_G
        self.llg_damping = llg_damping
        self.llg_normalisationfactor = llg_normalisationfactor
        self.llg_xi = llg_xi
        self.llg_polarisation = llg_polarisation
        self.do_precession = do_precession
        self.properties = properties
        self.exchange_coupling = exchange_coupling
        self.scale_volume_charges = scale_volume_charges

        # Let's whether we got the right units
        one = SI(1)
        units = (("Ms", SI("A/m")), ("llg_gamma_G", SI("m/A s")),
                 ("llg_damping", one), ("llg_normalisationfactor", SI("1/s")),
                 ("llg_xi", one), ("llg_polarisation", one),
                 ("exchange_coupling", SI("J/m")))
        for name, unit in units:
            value = getattr(self, name)
            if not unit.is_compatible_with(value):
                raise NmagUserError("The argument '%s' of MagMaterial "
                                    "requires values with unit of %s"
                                    % (name, unit))

        if isinstance(anisotropy, PredefinedAnisotropy):
            if anisotropy_order:
                raise NmagUserError("Cannot specify custom anisotropy_order "
                                    "when using a predefined anisotropy.")
                anisotropy_order = anisotropy.order
                anisotropy = anisotropy.function

        elif anisotropy != None or anisotropy_order != None:
            # At this point we must import anisotropy5 (and nsim.model)
            from anisotropy5 import Anisotropy
            if isinstance(anisotropy, Anisotropy):
                pass

            else:
                if anisotropy and not anisotropy_order:
                    raise \
                      NmagUserError("You need to specify the "
                                    "anisotropy_order when using a custom "
                                    "anisotropy function.")

        self.anisotropy = anisotropy
        self.anisotropy_order = anisotropy_order

        # compute thermal factor (gets multiplied by T/(dV*dt) later)
        self.thermal_factor = (2.0*si.boltzmann_constant*self.llg_damping)/(-si.gamma0*si.mu0*self.Ms)

        # Here we calculate the parameters in simulation units
        # XXX NOTE: the user cannot modify self.llg_damping alone!!!
        #   we should provide properties to change these values, in such a way
        #   that the corresponding _su values will be immediately computed.
        #gilbert_to_ll = 1.0/(1.0+self.su_llg_damping**2)
        #self.su_llg_coeff1 = -self.su_llg_gamma_G*gilbert_to_ll
        #self.su_llg_coeff2 = self.su_llg_coeff1*self.su_llg_damping

        if self.do_precession == False:
            log.info ("Setting su_llg_coeff1 to zero; thus no precession for material '%s'" % self.name)
            self.su_llg_coeff1 = 0.0

        if self.exchange_coupling < 0.0:
            raise NmagUserError("The exchange coupling constant " + \
              "must be positive. For material '%s', you specified: %s." \
              % (self.name, self.exchange_coupling))

        #self.su_anisotropy = None
        #if self.anisotropy:
            #    def su_anisotropy(m):
                #    return simulation_units.of(self.anisotropy(m), compatible_with=SI("J/m^3"))
                #self.su_anisotropy = su_anisotropy

        self.extended_print = False
        log.info("Created new Material:\n %s " % str(self))

    def get_exchange_factor(self):
        return -2.0*self.exchange_coupling/(si.mu0*self.Ms)

    exchange_factor = property(get_exchange_factor)

    def __str__(self):
        repr_str = "Material '%s'\n" % self.name
        attrs = filter(lambda a : a[0] != '_', dir(self))
        if not self.extended_print:
            attrs = ['name', 'Ms', 'exchange_coupling', 'anisotropy',
                     'anisotropy_order', 'llg_gamma_G', 'llg_damping',
                     'llg_normalisationfactor', 'do_precession',
                     'llg_polarisation', 'llg_xi', 'thermal_factor',
                     'extended_print']

        for attr in attrs:
            repr_str += " %25s = %s\n" % (attr, eval('str(self.'+attr+')'))

        return repr_str
