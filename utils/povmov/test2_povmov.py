import math
import povmov

s = povmov.scene()
s.screen(width=320, height=200)
s.torus("torus1", radius=1, width=0.2, color=(1,0,0))
s.torus("torus2", radius=2, width=0.2, color=(1,1,0))

num_steps = 25
for i in range(num_steps):
    angle = 360*i/num_steps
    s.rotate("torus1", a2=0.5*math.pi, a3=angle)
    s.rotate("torus2", a2=0.5*math.pi, a1=angle)
    s.render()

s.create_movie()
