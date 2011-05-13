import nmag, nsim
from nmag import SI, at, every
from nmag.fdsimulation import FDSimulation, MagMaterial
from nmag.constants import degrees_per_ns
import os

mat_Py = MagMaterial(name='Py',
                     Ms=SI(0.86e6, 'A/m'),
                     exchange_coupling=SI(13.0e-12, 'J/m'))

s = FDSimulation(do_demag=True)
s.set_params(stopping_dm_dt=20*degrees_per_ns)

nm = SI(1e-9, "m")
def rectangle(pos):
#    if (pos[0] >=10e-9 and pos[0] <= 20e-9 and 
#       pos[1] >=10e-9 and pos[1] <= 20e-9):
        return 'magnetic' 
#    else:
#        return None

s.create_mesh([5, 5, 1], [5.0*nm, 5.0*nm, 3.0*nm], mat_Py,regions=rectangle)
    
s.set_m([1, 1, 1])

Hs = nmag.vector_set(direction=[1.0, 1.0, 1.0],
                     norm_list=[1.00, 0.95, [], 0.1, 0.09, [],
                                -0.1, -0.15, [], -1.00],
                     units=1e6*SI('A/m'))

s.hysteresis(Hs, save=[('field_m', at('time', SI(0, 's')) | at('convergence'))])

# Process the results
NCOL=os.path.join(os.path.dirname(os.path.abspath(nmag.__file__)), "../../bin/ncol")
os.system(NCOL + " run_fdsimulation H_ext_0 H_ext_1 H_ext_2   M_Py_0 M_Py_1 M_Py_2 > fd.dat")
os.system("gnuplot plot.gnp")

