"""
Automatic conversion of SI objects into simulation units (su)

-----------------
Simulation Units
-----------------


Motivation
-----------------


We want to use simulation units in the following sense: a physical
quantity x, for example x=11e-9metre, should be represented in the
simulation as s_x where s_x = x / conv_L. The entity s_x is
dimensionless (because we can do only dimensionless calculations with
the usual numeric libraries) and conv_L is the conversion factor for
lengths (L).

We could simply strip off the units from x by using conv_L = metre, so
that for our example above s_x = 11e-9. We would effectively be using
SI 'units' for our calculations (but have formally removed the units
from the numbers). One could call these numbers 'dimensionless SI
numbers' or just SI numbers.

We could also choose conv_L = 1e-9metre in which case s_x = 11. This
probably makes more sense for the typical micromagnetic
calculation. One could also imagine conv_L = 1e-6metre (so that
s_x=11000).

In general, this choice should be left to the user. For our
particular micromagnetic problem, it makes sense for us to suggest a
set of conversion factors.

In any case, we need to provide functions to convert back from
simulation units to SI units. This removes all uncertainties about
what the numbers mean, and the result in SI units should always be the
same no matter what set of conversion factors we use (unless the
conversion factors are chosen so badly that we have overflow/underflow
issues or such).

With the 'Physical' class we have a good tool available to make this
process flexible and somewhat transparent. The remaining challenge is
to wrap this up in a nice user interface.

General idea
------------

Have a set of conversion factors that the user can set (and defaults
are provided). These are only necessery for the basic SI primitives,
all other conversion factors are derived from this, and the simulation
should be able to return this to the user (in case they don't know
what they are).

This SimulationUnits class below should address this issue.


Usage Example
-------------

::

  >>> from nsim.su_units import SimulationUnits
  
  >>> from nsim.si_units import SI
  
  >>> x=SI(2.45e-9,'m')
  
  >>> simulationUnits=SimulationUnits({'m':1e-9,'s':1e-12})
  
  >>> print simulationUnits
    A: one simulation unit -> 1A
   kg: one simulation unit -> 1kg
    K: one simulation unit -> 1K
    m: one simulation unit -> 1e-09m
   cd: one simulation unit -> 1cd
    s: one simulation unit -> 1e-12s
  mol: one simulation unit -> 1mol
  
  >>>  print simulationUnits.of(x)
  2.45
  
  >>> x_su= simulationUnits.of(x)
  
  >>> conv_L = simulationUnits.conversion_factor_of(SI(1,'m'))
  
  >>> print conv_L
  <SI: 1e-09  m >
  
  >>> print conv_L * x_su
  <SI: 2.45e-09  m >
  


"""

__docformat__="restructuredtext"

from si_units import Physical
from nsim_exceptions import *

class SimulationUnits:
    """Conversion of SI objects to numbers in simulation units"""

    _basicscales = {'m':Physical(1,["m",1]),
                    'A':Physical(1,["A",1]),
                    'kg':Physical(1,["kg",1]),
                    's':Physical(1,["s",1]),
                    'cd':Physical(1,["cd",1]),
                    'mol':Physical(1,["mol",1]),
                    'K':Physical(1,["K",1])}

    def __init__(self,scales=None):
        """Set up SimulationUnits.

        Note that there is only a class-object.

        The constructor takes a dictionary 'scales' with SI unit scaling
        factors. A typical choice for micromagnetic calculations looks
        like this::

          SimulationUnits({'A': 1e-3,'kg': 1e-27,'m': 1e-9,'s': 1e-12,\
                           'cd': 1.0,'K': 1.0,'mol': 1.0})

        If entries are not listed in the dictionary, then they default to 1.0

        The units listed above will result in this set up:

        A: one simulation unit -> 0.001A
        kg: one simulation unit -> 1e-27kg
        K: one simulation unit -> 1K
        m: one simulation unit -> 1e-09m
        cd: one simulation unit -> 1cd
        s: one simulation unit -> 1e-12s
        mol: one simulation unit -> 1mol

        See source code file for usage examples.
        
        """

        self.locked = False #lock simulation units: must not change 
                            #conversion factors halfway through
                            #calculation

        if scales:
            for key in scales.keys():
                self.setscale(key,scales[key])

    def lock(self):
        """Avoid further invocations of the method 'setscale'"""
        self.locked = True

    def unlock(self):
        """Allow the user to change the scales with 'setscale'"""
        self.locked = False

    def is_locked(self):
        """To check whether simulation_units are locked"""
        return self.locked


    def setscale(self, unit, scalefactor):
        """Set conversion factors for basic SI units"""
        # For now we use this locking-unlocking method to avoid a bad usage
        # of the class: setscale should be used at the beginning, just after
        # the initialization of the class (should we embed this function
        # inside the __init__ method? Is there any better idea?
        # Should the class SimulationUnits be a class?
        if self.locked:
            raise NsimUserError("Cannot change the scales: " +
             "the class has been locked!")
        SimulationUnits._basicscales[unit] = Physical(scalefactor,[unit, 1])

    def __str__(self):
	"""Nice represenation of current simulation units

	Example:

         >>> from nsim.su_units import SimulationUnits 
         >>> su=SimulationUnits({'m':1e-9,'s':1e-12})
         >>> print str(su)
           A: one simulation unit -> 1A
          kg: one simulation unit -> 1kg
           K: one simulation unit -> 1K
           m: one simulation unit -> 1e-09m
          cd: one simulation unit -> 1cd
           s: one simulation unit -> 1e-12s
         mol: one simulation unit -> 1mol
         >>>
        """

        s=SimulationUnits._basicscales
        repr = ""

        for key in s.keys():
           repr +="%3s: one simulation unit -> %g%s\n" % (key,s[key].value,key) 
        return repr

    def __repr__(self):
	"""Representation operator.

	Example:
         >>> from nsim.su_units import SimulationUnits 
         >>> su=SimulationUnits({'m':1e-9,'s':1e-12})
	 >>> repr(su)
         "SimulationUnits({'A': 1.0, 'kg': 1.0, 'K': 1.0, 'm': 1.0000000000000001e-09, 'cd': 1.0, 's': 9.9999999999999998e-13, 'mol': 1.0})"

        """

        tmpdic = dict([(unitname,unit_si.value) for unitname,unit_si in SimulationUnits._basicscales.items()])
        return 'SimulationUnits('+repr(tmpdic)+')'

    def of(self, physical_quantity, compatible_with=None):
        """Return the physical entity physical_quantity in simulation units

        :Parameters:
          `physical_quantity` : Physical object 
	    (eg Physical(4*math.pi*1e-7,['kg',1,'m',1,'A',-2,'s',-2]))

        :Returns:
	  ``float``
            Returns the factor that needs to be multiplied with ``physical_quantity`` to obtain the SI object. 

        Why the name 'of'?

	Assuming you have an object simulation_units, then a call to this method would read::

	  mu0 = Physical(4*math.pi*1e-7,['kg',1,'m',1,'A',-2,'s',-2]))
	  x = simulationunits.of(mu0)

	and x will provide the numerical value of mu0 in simulation units.

        """
        if type(physical_quantity) != Physical:
            # floats and ints are compatible with SI(1) values
            return self.of(Physical(float(physical_quantity)),compatible_with)

        if compatible_with != None:
            if not physical_quantity.is_compatible_with(compatible_with):
                raise NsimUserError(
                  "You gave me %s, but I expect %s or something with "
                  "compatible units." % (physical_quantity, compatible_with))

        if physical_quantity.value == 0.0:
            return 0.0

        else:
            scale = 1.0
            for i in range(0,len(physical_quantity.units),2):
                unit = physical_quantity.units[i]
                power = physical_quantity.units[i+1]
                scale = scale*SimulationUnits._basicscales[unit].value**(-power)

            return scale*physical_quantity.value

    def conversion_factor_of(self,normalised_physical_quantity):
        """Return a Physical object that can be multiplied with simulation number to obtain SI value

        :Parameters:

          `normalised_physical_quantity` : Physical object
            This is the SI object for which the conversion factor
            will be returned.


        Example: Suppose we have a number x in simulation units for
        the magnetisation (measured in SI in A/m).  This methods can
        be called to get the necessary conversion factor::

          su = SimulationUnits()
          [can set scales here]

          si_factor = su.conversion_factor_of(Physical(1,"A / m"))

        We can then (subsequently) convert x into SI units with::

          x*si_factor

        or, if only interested in the numerical value::

          x*si_factor.value

        """

        #if normalised_physical_quantity.value != 1.0:
        #    raise ValueError, "The conversion factor only makes sense for dimensional values for the value 1.0"
        return Physical(1.0/self.of(normalised_physical_quantity),normalised_physical_quantity.units)



#if __name__ == "__main__":
#
#    su=SimulationUnits()
#
#    su.setscale('m',1e-9)
#    su.setscale('A',1e-3)
#    su.setscale('s',1e-12)
#    su.setscale('kg',1e-27)
#    
#    
#    print su
#
#    M = Physical(1e6,['A',1,'m',-1])
#    M_sim = su.of(M)
#    print "M:",M,"->",M_sim , "in simulation units"
#
#    A = Physical(13e-12,['kg',1,'m',1,'s',-2])
#    print "A:",A,"->",su.of(A), "in simulation units"
#
#    gammallg = Physical(2.210173e5,['m',1,'A',-1,'s',-1])
#    print "gammallg: ",gammallg,"->",su.of(gammallg),"in simulation units"
#
#    import math
#
#    mu0 =Physical(4*math.pi*1e-7,['kg',1,'m',1,'A',-2,'s',-2])
#
#    print "mu0:",mu0,"->",su.of(mu0),"in simulation units"
#
#    factor_M = su.conversion_factor_of(Physical(1,["A",1,"m",-1]))
#    print factor_M
#    print "What is scaling factor for A/m to get from simulation to SI units?"
#
#
#    print "Reminder M_sim = %g (in simulation units)" % M_sim
#    print "Convert M_sim back into SI units: the SI value is M_sim*factor_M=",M_sim*factor_M.value
#
#    x=repr(su)
#
#    print 'x',x
#
#    from si_units import SI
#
#    y=eval(x)
#
#    print y
#
    
