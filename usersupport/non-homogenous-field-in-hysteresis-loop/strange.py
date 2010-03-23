import nmag
from nmag import SI, mesh, at, every

mat_Py = nmag.MagMaterial(name="Py",
                          Ms=SI(0.86e6, "A/m"),
                          exchange_coupling=SI(13.0e-12, "J/m"),
                          llg_damping=0.5)

sim = nmag.Simulation(name="strange")
sim.load_mesh("cube.nmesh.h5", [("cube", mat_Py)], unit_length=SI(1e-9,"m"))

# We imagine to partition the space into two regions:
# region1 is a sphere centered in p0 with radius r
# region2 is the remaining space
p0 = [0.0e-9, 0.0e-9, 0.0e-9]
r = 10.0e-9 # in meters, remember!

# This function creates a function to set H_ext, which returns H_ext
# inside region1 and returns -H_ext in region2
def new_function_to_set_H_ext(H_ext):
  r_square = r*r
  def function_to_set_H_ext(p):
    d_square = (p[0] - p0[0])**2 + (p[1] - p0[1])**2 + (p[2] - p0[2])**2
    if d_square <= r_square:
      return H_ext
    else:
      return [-H_ext[0], -H_ext[1], -H_ext[2]]

  return function_to_set_H_ext # we return the function above

sim.set_m([1, 1, 1]) # set the magnetisation

# Create the list of H_ext vectors
H_vectors = nmag.vector_set(direction=[1, 0, 0],
                            norm_list=[-1.0, -0.9, [], 1.0],
                            units=1.0e6) # units is a float, not a SI

# From such list, we create now a list of function which specify
# exactly how to set the external field in our bizzarre way
H_functions = [(new_function_to_set_H_ext(H), SI("A/m")) for H in H_vectors]

# Perform the hysteresis loop
sim.hysteresis(H_functions, save=[('fields', at('convergence'))])
