"""Some useful SI constants"""
import math
from lib import SI
kilogram = SI(1.0,["kg",1])  #: The kilogram
meter = SI(1.0,["m",1])      #: The meter
metre = meter                #  alternative spelling
Ampere = SI(1.0,["A",1])     #: The Ampere
Kelvin = SI(1.0,["K",1])     #: The Kelvin
second = SI(1.0,["s",1])     #: The second
candela = SI(1.0,["cd",1])   #: The candela
mol = SI(1.0,["mol",1])      #: The mol

#specific units for magnetism

Newton = kilogram*meter/second**2          #: Newton
mu0 = SI(4.0e-7*math.pi, "N/A^2")          #: vacuum permeability mu0
Tesla = kilogram/Ampere/second**2          #: Tesla
Gauss = 1e-4*kilogram/Ampere/second**2     #: Gauss
Oersted=Gauss/mu0 		           #: Oersted
Oe=Oersted          		           #: Oersted
gamma0 = SI(-2.2137286285040001e5, "m/A s")#: gyromagnetic ratio gamma0

# units: degrees/nanosecond: Useful to specify the stopping_dm_dt
degrees_per_ns = SI(math.pi/180.0)/SI(1e-9, "s")

# other units and constants
Joule = SI("J")
bohr_magneton = 9.2740094980e-24*Joule/Tesla # Bohr magneton
positron_charge = SI(1.6021765314e-19, "C")
electron_charge =  -positron_charge
boltzmann_constant = SI(1.3806504e-23, "J/K")
