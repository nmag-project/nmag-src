import math

import nmag, nsim
from nmag import SI, at, every
from nmag.fdsimulation import FDSimulation, MagMaterial
from nmag.constants import degrees_per_ns

import problem_def as pb

# 2D rotation function
def rotate(p, angle):
    ca = math.cos(angle)
    sa = math.sin(angle)
    x = p[0]
    y = p[1]
    r = list(p)
    r[0] = ca*x - sa*y
    r[1] = sa*x + ca*y
    return r

# Calculate the size of the container: we make it extra big, for now we don't
# care (there is also a bug now which make it preferable)
container_size = pb.sx*1.5 #(pb.sx*pb.sx + pb.sy*pb.sy)**0.5
container_discr = int(float(container_size/pb.dx_dy))

mat_Py = MagMaterial(name='Py',
                     Ms=SI(0.86e6, 'A/m'),
                     exchange_coupling=SI(13.0e-12, 'J/m'),
                     llg_damping=0.2,
                     do_precession=True)

s = FDSimulation()
s.set_params(stopping_dm_dt=1*degrees_per_ns,
             ts_rel_tol=1e-1, ts_abs_tol=1e-1)

m = SI('m')
half_sx_m = 0.5*float(pb.sx/m)
half_sy_m = 0.5*float(pb.sy/m)
def film(pos):
    x, y, _ = rotate(pos, -pb.angle)
    if (x >= -half_sx_m and x <= half_sx_m and
        y >= -half_sy_m and y <= half_sy_m):
        return 'magnetic'

    else:
        return None

s.create_mesh([container_discr, container_discr, pb.nz],
              [pb.dx_dy, pb.dx_dy, pb.dz],
              mat_Py, regions=film,
              origin=(pb.sx*0.5, pb.sy*0.5, pb.sz*0.5))

s.set_m(pb.m0)

Hs = nmag.vector_set(direction=rotate([0.0, 1.0, 0.0], pb.angle),
                     norm_list=[0.0, 0.0015, 0.003, 0.004,
                                0.0045, 0.0001, 0.0065],
                     units=1e6*SI('A/m'))

reference_mz = []
def check_switching_field(s):
    m = s.get_subfield_average('m')
    mz = abs(m[2])
    if len(reference_mz) == 0:
        reference_mz.append(mz)

    else:
        (mz0, ) = reference_mz
        if mz < 0.05*mz0:
            f = open("switching_fields.dat", "a")
            H = [float(Hi/SI('A/m')) for Hi in Hs[s.stage-1]]
            f.write("%g %g %g %g\n" % tuple([pb.angle] + H))
            s.hysteresis_exit()
            f.close()
    print "check_switching_field: done!"

s.hysteresis(Hs,
             save=[('field_m', at('convergence'))],
             do=[(check_switching_field, at('convergence'))])

