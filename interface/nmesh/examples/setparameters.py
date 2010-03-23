import nmesh

# create sections of a square ring
bottom = nmesh.box( [2.0,1.0], [4.0,2.0])
right  = nmesh.box( [4.0,1.0], [5.0,3.0])
top    = nmesh.box( [3.0,3.0], [5.0,4.0])
left   = nmesh.box( [2.0,2.0], [3.0,4.0])

# create compound 
square_ring = nmesh.union([bottom,right,top,left])


rod= 0.4
bbox = [[-1.,-1.],[7.,6.]]
fixed_points = [[3.0,2.0],[4.0,2.0,],[3.0,3.0],[4.0,3.0]]

# create mesh 
mesh_ex = nmesh.mesh(objects = [square_ring], a0=rod, bounding_box=bbox, \
                     fixed_points=fixed_points,

                     topology_threshold=0.2, # change the threshold
                                             # for re-triangulation
                                             # during mesh relaxation

                     time_step_scale=0.1,    # change the factor which
                                             # controls the time step
                                             # used to relax the mesh

                     tolerated_rel_move=0.002,# change the threshold to
                                              # stop the mesh relaxation

                     max_relaxation=3.0,     # change the "freedom" of
                                             # points at the beginning
                                             # of mesh relaxation

                     neigh_force_scale=1.,   # change the contribution
                                             # of the force from neighbours
                                             # to the total force on each node
                     
                     shape_force_scale=0.,   # change the contribution
                                             # of the simplices shape force 
                                             # to the total force on each node
                                         
                     volume_force_scale=0.2,   # change the contribution
                                             # of the simplices volume force
                                             # to the total force on each node
                                          
                     initial_settling_steps=100,# change the number of steps
                                                # where a "settling" behaviour
                                                # of the mesh is likely

                     thresh_add = 0.6,       # change the threshold for the
                                             # insertion of a new point
                     
                     thresh_del = 1.4,       # change the threshold for the
                                             # deletion of a point.

                     max_steps=1000)         # change the max number of
                                             # steps for mesh relaxation


# plot mesh
nmesh.visual.plot2d_ps(mesh_ex,"setparameters.ps")

