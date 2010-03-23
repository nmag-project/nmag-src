'''This module has the only purpose to easy the life of Nmag users.
Instead of having to import nmag, the SI, at, every objects, etc.
one simply needs to do

from nmag.common import *

to have the most commonly used objects in the current namespace.
'''

from nmag import SI, every, at, never, MagMaterial, Simulation, \
                 uniaxial_anisotropy, cubic_anisotropy

from nsim.si_units.si import degrees_per_ns

