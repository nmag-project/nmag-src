import nmag, nsim
from nmag import SI, at, every
from nmag.fdsimulation import FDSimulation, MagMaterial
from nmag.constants import degrees_per_ns

mat_Py = MagMaterial(name='Py',
                     Ms=SI(0.86e6, 'A/m'),
                     exchange_coupling=SI(13.0e-12, 'J/m'))

s = FDSimulation(do_demag=True)
s.set_params(stopping_dm_dt=1*degrees_per_ns)

nm = SI(1e-9, "m")
def rectangle(pos):
    if (pos[0] >=10e-9 and pos[0] <= 20e-9 and 
       pos[1] >=10e-9 and pos[1] <= 20e-9):
        return 'magnetic' 
    else:
        return None

s.create_mesh([10, 10, 1], [2.0*nm, 2.0*nm, 3.0*nm], mat_Py,regions=rectangle)
    
s.set_m([1,1,1])

Hs = nmag.vector_set(direction=[1.0, 1.0, 1.0],
                     norm_list=[1.00, 0.95, [], 0.1, 0.09, [],
                                -0.1, -0.15, [], -1.00],
                     units=1e6*SI('A/m'))

s.hysteresis(Hs, save=[('field_m', at('time', SI(0, 's')) | at('convergence'))])

