# we have a rectangular film in the plane xy with size sx, sy rotated by
# angle. The center of the film lies at coordinate (0, 0, 0).

import math
from nmag import SI

nm = SI(1e-9, 'm')
degrees = math.pi/180.0

# Size of the film
sx = 220*nm
sy = 220*nm
sz = 2.5*nm

# Angle of rotation
angle = 0*degrees

# Discretisation
dx_dy = 2.5*nm # Discretisation for both FD and FE
nz = 1         # Number of cells in the z direction
dz = sz/nz

# Initial magnetisation
r0 = float(sx/SI('m'))*0.5
def m0(r):
    x, y, z = [ri/r0 for ri in r]
    mx, my, mz = m = [y, -x, 0.5]
    m_norm = math.sqrt(mx*mx + my*my + mz*mz)
    r = [mi/m_norm for mi in m]
    return r

