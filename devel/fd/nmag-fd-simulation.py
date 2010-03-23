import nmag
from nmag import SI, at

# For now we do not use the MagMaterial defined in nmag
# finally nmag will use this one anyway!
from nmag.material import MagMaterial

#### The following code should then go into the fd module
from nmag.simulation_core import SimulationCore
import math

class FDSimulation(SimulationCore):
    def __init__(self, name="noname"):
        SimulationCore.__init__(self)
        self.id = "FD Simulation class"
        self.name = name
        self.stupid = 0

    # override the advance_time method
    def advance_time(self, target_time, max_it=-1):
        # To get the time in SI units you could do as follows, for now
        time_su = float(target_time/SI("s"))
        # ^^^ dividing an SI("s") object by an SI("s") object you get
        # an SI(1) object, which can be safely cast to a float number.
        # if target_time is not a SI("s"), but - for example - an SI("m")
        # then the float will fail:
        #   float(SI("m")/SI("s")) == float(SI("m/s"))

        delta_t = SI(1e-12, "s")
        self.clock['time'] += delta_t
        self.clock['stage_time'] += delta_t
        self.clock['step'] += 1
        self.clock['stage_step'] += 1
        return self.clock['time']

    def is_converged(self):
        #return True
        self.stupid += 1
        # Just to simulate a convergence check
        return (self.stupid % 10) == 0

####

mat_Py = MagMaterial(name='Py')

s = FDSimulation()

def boundary_of_sphere(pos):
    # pos is a triple of floating point numbers (in meters)
    return math.sqrt(sum([xi*xi for xi in pos])) < 50e-9

# origin and shape should be optional arguments
nm = SI(1e-9, "m")
s.create_mesh([100, 100, 10], # number of cells for each dimension
              [2*nm]*3, # size of cells in each dimension
              origin=[-50.0*nm, -50.0*nm, -5.0*nm], # translate the mesh
              shape=boundary_of_sphere)

s.set_m([1, 1, 1])

Hs = nmag.vector_set(direction=[1.,0.01,0],
                     norm_list=[1.00, 0.95, [], -1.00, -0.95, -0.90, [], 1.00],
                     units=1e6*SI('A/m'))

s.hysteresis(Hs, save=[('restart', 'fields', at('convergence'))])

