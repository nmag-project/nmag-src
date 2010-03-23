import math
import povmov

s = povmov.scene()
s.screen(width=320, height=200)
s.set_default("color", (1,1,1))
s.box("layer", corner1=(-3,0,-3), corner2=(3,1,3))
s.sphere("hole", center=(0,0.5,0), radius=1)
s.difference("diff", "layer", "hole")
s.camera(position=(5, 10, -5), look_at=(0,2,0))

for i in range(40):
    s.attributes[("hole", "radius")] = i*0.1
    s.render()

s.create_movie()
