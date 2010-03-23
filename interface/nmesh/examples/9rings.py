import nmesh, Numeric

#   |           -------     -------     -------           /
#   |          /       \   /       \   /       \          | 
#   |         /   ---   \ /   ---   \ /   ---   \         |
#   |        |   /   \       /   \       /   \   |        |
#   |        |   \   /       \   /       \   /   |        |
#   |         \   ---   / \   ---   / \   ---   /         |
#   |          \       /   \       /   \       /          |
#   |          /       \   /       \   /       \          |
#   |         /   ---   \ /   ---   \ /   ---   \         
#   |        |   /   \       /   \       /   \   |        3000
#   |        |   \   /       \   /       \   /   |        
#   |         \   ---   / \   ---   / \   ---   /         |
#   |          \       /   \       /   \       /          |
#   |          /       \   /       \   /       \          |
#   |         /   ---   \ /   ---   \ /   ---   \         |
#   |        |   /   \       /   \       /   \   |        |
#   |        |   \   /       \   /       \   /   |        |
#   |         \   ---   / \   ---   / \   ---   /         |
#   |          \       /   \       /   \       /          |
#   |           -------     -------     -------           /
#            /---------------- 3100 -------------/


rod = 0.50

outer_radius = 5.50 
inner_radius = 3.25 
centres_distance = 10 
thickness = 1                # thickness of the sample 
eps = 1e-6
half_bbox_dimx = 17
half_bbox_dimy = 15

half_voltage_probe_dimx = centres_distance+outer_radius
half_voltage_probe_dimy = 100

rings_box = nmesh.box([-17, -15],
                      [17,15])

bbox = [[-half_bbox_dimx,-half_bbox_dimy],[half_bbox_dimx,half_bbox_dimy]]

fix_pts = []
rings = []
voltage_probes = []

for x in range(-1,2):
    for y in range(-1,2):
        
        # external ring
        R = nmesh.ellipsoid(
            [outer_radius, outer_radius],
            transform=[("shift",[x*centres_distance, y*centres_distance])]
            )
        #internal ring
        r = nmesh.ellipsoid(
            [inner_radius, inner_radius],
            transform=[("shift",[x*centres_distance, y*centres_distance])]
            )
        # take the difference
        rings.append(nmesh.difference(R,[r]))

# cut the rings at top and bottom
union_rings = nmesh.union(rings)
rings_array = nmesh.intersect([union_rings,rings_box])


# semiaxis of a rhombus on the diagonals of the array 
d = (centres_distance/2.) - Numeric.sqrt(outer_radius**2 - (centres_distance/2.)**2)

# rhombi on the diagonals of the array
for x in range(-3,4,2):
    for y in range(-3,4,2):
        
        centre_x = 0.5*x*centres_distance
        centre_y = 0.5*y*centres_distance
        # centre of the rhombus
        c = Numeric.array([centre_x,centre_y])

        # 4 corners of the rhombus
        if centre_x > -15. and centre_x < 15. :
            fix_pts.append(c+Numeric.array([d,0]))
            fix_pts.append(c+Numeric.array([-d,0]))
            if centre_y > -15. :
                fix_pts.append(c+Numeric.array([0,-d]))
            if centre_y < 15. :
                fix_pts.append(c+Numeric.array([0,d]))

        elif centre_x > -20. and centre_x < 15. :
            fix_pts.append(c+Numeric.array([d,0]))
            
        elif centre_x < 20. and centre_x > -15. : 
            fix_pts.append(c+Numeric.array([-d,0]))

        else: continue
        


density = """density = 1.;"""

##import pylab
##for p in fix_pts :
##    px,py = p
##    pylab.plot([px],[py],"bo",markersize = 10)
##    print p
##pylab.show()
##raw_input()

N = 100

# create mesh of three objects and bounding box
mesh_ex = nmesh.mesh(objects = [rings_array],#[outer,voltage_probes[0], voltage_probes[1], rings_array],
                     a0=rod,
                     bounding_box=bbox,
                     fixed_points = fix_pts,
                    
                     neigh_force_scale = 1.,
                     shape_force_scale = 0.1,

                     density = density,
                     thresh_del = 1.4,
                     
                     initial_settling_steps = 100,
                     max_relaxation = 4,
                     
                     #callback=(my_function, N),
                     max_steps=1000
                     )
#mesh_loaded = nmesh.load("9rings_alone.nmesh")
mesh_ex.save("9rings.nmesh")
nmesh.visual.plot2d_ps(mesh_ex,"9rings.ps")


