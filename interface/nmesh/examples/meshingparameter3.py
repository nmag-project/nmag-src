import nmesh

ellipsoid = nmesh.ellipsoid([1.25,0.75])

bbox = [[-1.25,-0.75],[1.25,0.75]]

#Define all parameters, for example in string:
myconf = """
[nmesh-2D]
a0                     : 1.0
shape_force_scale      : 0.1
volume_force_scale     : 0.0
neigh_force_scale      : 1.0
irrel_elem_force_scale : 0.0
thresh_add             : 0.6
thresh_del             : 1.6
initial_settling_steps : 200
sliver_correction      : 1.0
max_relaxation         : 3.0
topology_threshold     : 0.2
max_relaxation         : 3.0
topology_threshold     : 0.2
time_step_scale        : 0.1
tolerated_rel_move     : 0.002
max_steps              : 2000
"""

#Then create MeshingParameters object 
mp = nmesh.MeshingParameters(string=myconf)

#and use MeshingParameter object when computing the mesh:
mesh = nmesh.mesh(objects = [ellipsoid], bounding_box=bbox,a0=0.5,\
                  meshing_parameters=mp)
