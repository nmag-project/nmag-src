import nmag, nsim
from nmag import SI, at, every
from nmag import Simulation, MagMaterial
from nmag.constants import degrees_per_ns

mat_Py = MagMaterial(name='Py',
                     Ms=SI(0.86e6, 'A/m'),
                     exchange_coupling=SI(13.0e-12, 'J/m'))

s = Simulation(do_demag=True)
s.set_params(stopping_dm_dt=1*degrees_per_ns)

s.load_mesh("film.nmesh.h5", [('Py', mat_Py)], unit_length=SI(1e-9, 'm'))

s.set_m([1, 1, 1])

Hs = nmag.vector_set(direction=[1.0, 1.0, 1.0],
                     norm_list=[1.00, 0.95, [], 0.1, 0.09, [],
                                -0.1, -0.15, [], -1.00],
                     units=1e6*SI('A/m'))

s.hysteresis(Hs, save=[('averages', at('convergence'))])

