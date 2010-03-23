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

import nmesh, pylab, Numeric
import mayavi, sys
sys.argv=["",""]     

# define a few globals, so that the interval function can use them
globals()['v'] = mayavi.mayavi()
globals()['intervals'] = Numeric.arange(0,1.05,0.05)
globals()['call_counter'] = 0


# define the interval function
def my_function( piece, timestep, mesh_info ):

    # to visualise without writing to disk, uncomment this section
    vtkData, points, simplices, simplexIndicies, icradii, ccradii = nmesh.visual.mesh2vtk(mesh_info, VTKonly=False)
    in2circ = nmesh.visual.findRatios(icradii, ccradii, factor=2) #2D mesh
    vtkData = nmesh.visual.append2vtk(vtkData, in2circ, "2*inradius/circumradius")
    globals()['v'].close_all()
    globals()['v'] = nmesh.visual.mesh2mayavi(vtkData, myv=globals()['v'], lut_range=(0,1))
    m2 = v.load_module('SurfaceMap',0)
    m2.actor.GetProperty().SetRepresentationToWireframe()
    m2.mapper.SetScalarVisibility(0)
    m2.renwin.Render()
    dvm = v.get_current_dvm()
    mm = dvm.get_current_module_mgr()
    luthandler = mm.get_scalar_lut_handler()
    luthandler.set_lut_red_blue()
    luthandler.sc_bar.SetVisibility(1)
    luthandler.sc_bar.GetTitleTextProperty().SetShadow(1)
    luthandler.sc_bar.GetLabelTextProperty().SetShadow(1)
    v.Render()
    globals()['v'].renwin.z_plus_view()  # this line will reduce fluidity
    raw_input('Hit enter to continue...')


    

rod = 0.30

outer_radius = 5.45 
inner_radius = 3.30 
centres_distance = 10 
thickness = 1                # thickness of the sample 
eps = 1e-6
half_bbox_dimx = 5.00
half_bbox_dimy = 5.00

half_voltage_probe_dimx = centres_distance+outer_radius
half_voltage_probe_dimy = 100

rings_box = nmesh.box([-5.00, -5.00],
                      [5.00,5.00])

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

f = open("run_9rings/load-data.nmesh")
data = f.readlines()
f.close()

mob_pts = []
nr_mob_pts = int(data[0])
for i in range(nr_mob_pts):
    x,y = data[1+i].split()
    fx, fy = float(x), float(y)
    mob_pts.append([fx, fy])


    

##import pylab
##for p in fix_pts :
##    px,py = p
##    pylab.plot([px],[py],"bo",markersize = 10)
##    print p
##pylab.show()
##raw_input()

N = 1

# create mesh of three objects and bounding box
mesh_ex = nmesh.mesh(objects = [rings_array],#[outer,voltage_probes[0], voltage_probes[1], rings_array],
                     a0=rod,
                     bounding_box=bbox,
                     fixed_points = fix_pts,
                     mobile_points = mob_pts,
                     irrel_elem_force_scale = 1.0,
                     sliver_correction = 1.0,
                     #callback=(my_function,N),
                     max_steps=1000
                     )
#mesh_loaded = nmesh.load("9rings_alone.nmesh")
mesh_ex.save("single-ring.nmesh")
nmesh.visual.plot2d_ps(mesh_ex,"single-ring.ps")


