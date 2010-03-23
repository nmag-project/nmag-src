import math
import povmov

s = povmov.scene()
s.screen(width=320, height=200)
s.show_preview(False)

a = 10
aa = 10.1
r = 0.3
r = ((r,r,r), (0,0,0))
s.set_default("color", (1,0,0))
s.box("wall1", corner1=(-a,-a,-a), corner2=(a,a,-aa), reflection=r)
s.box("wall2", corner1=(-a,-a,-a), corner2=(a,-aa,a), reflection=r)
s.box("wall3", corner1=(-a,-a,-a), corner2=(-aa,a,a), reflection=r)
s.box("wall4", corner1=(a,a,a), corner2=(-a,-aa,aa), reflection=r)
s.box("wall5", corner1=(a,a,a), corner2=(-a,aa,-a), reflection=r)
s.box("wall6", corner1=(a,a,a), corner2=(aa,-a,-a), reflection=r)

s.set_default("color", (1,1,1))
s.box("layer", corner1=(-3,-2,-3), corner2=(3,2,3))
s.sphere("hole", center=(0,2,0), radius=1)
s.difference("diff", "layer", "hole", reflection=r)
s.camera(position=(5, 10, -5), look_at=(0,2,0))

num_frames = 80
dt = 6.0/num_frames

for i in range(num_frames):
    s.attributes[("hole", "radius")] = i*dt
    s.render()

s.create_movie("test4.avi", delay=dt)
