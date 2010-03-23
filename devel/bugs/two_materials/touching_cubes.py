import nmag
from nmag import SI, every, at
sim = nmag.Simulation()

Pz = nmag.MagMaterial(name= "Pz", Ms=SI(1100e3,"A/m"))
Py = nmag.MagMaterial(name= "Py", Ms=SI(1714e3,"A/m"))

#load mesh
# This works
# sim.load_mesh("sep_cubes.nmesh.h5", [("box1", Pz), ("box2", Py)], unit_length=SI(1e-9,"m"))
# This doesn't
sim.load_mesh("touching_cubes.nmesh.h5",
              [("box1", Pz), ("box2", Py)],
	      unit_length=SI(1e-9,"m"))

# set initial magnetisation
sim.set_m([0.0, 0.0, 1])

sim.advance_time(SI(1e-12, "s"))

