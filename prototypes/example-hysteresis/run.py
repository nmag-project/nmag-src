# Nmag example: briefly describe what it does

from nmag.tools import *
from nmag import vector_set

mat_Py = MagMaterial('Py')

s = Simulation()

s.load_mesh("mesh.nmesh.h5", [('Py', mat_Py)], unit_length=SI(1e-9, 'm'))

s.set_m([1, 1, 1])

Hs = vector_set(direction=[1., 0.01, 0],
                norm_list=[ 1.00,  0.95, [], -1.00,
                           -0.95, -0.90, [],  1.00],
                units=SI(0.4e6, 'A/m'))

s.hysteresis(Hs, save=[('restart', 'fields', at('convergence'))])

