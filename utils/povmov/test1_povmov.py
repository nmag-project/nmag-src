import math
import povmov

s = povmov.scene()
s.screen(width=320, height=200)
s.box("box1", corner1=(0.0, 0.0, 0.0), corner2=(3.0, 3.0, 3.0), color=(1, 0, 0), reflection=((1,1,1), (0,0,0)))
#s.box("box2", corner1=(2.0, 0.0, 0.0), corner2=(3.0, 1.0, 1.0), color=(1, 1, 0))
#s.cylinder("cylinder1", radius=0.5, translate=(0, -2, 0), color=(0, 1, 1), reflection=((1,1,1), (0,0,0)))
s.sphere("sphere1", radius=1.0, translate=(0, -2, 0), color=(0, 1, 1), reflection=((1,1,1), (0,0,0)))
s.cone("cone1", translate=(1, 2, -1), color=(0, 0, 1), reflection=((1,1,1), (0,0,0)))
s.torus("torus1", color=(1,1,1))

num_steps = 25
for i in range(num_steps):
    angle = 360*i/num_steps
    s.rotate("torus1", a2=0.5*math.pi, a3=angle)
    #s.translate("torus1", x=angle)
    s.render()

s.create_movie()
