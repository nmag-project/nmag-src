import math
import povmov

s = povmov.scene()
f = 2
s.screen(width=160*f, height=120*f)

r = ((0.5, 0.5, 0.5), (0,0,0))
col_l1 = (1,0,0)
col_l2 = (1,1,0)
col_l3 = col_l1

a = 1000
b = 1
s.box("layer1", corner1=(-a,0,-a), corner2=(a,1,a))
s.box("layer2", corner1=(-a,1,-a), corner2=(a,3,a))
s.box("layer3", corner1=(-a,3,-a), corner2=(a,4,a))
s.box("hole", corner1=(-b,-1,-b), corner2=(b,5,b))
s.difference("l1", "layer1", "hole", color=col_l1)
s.difference("l2", "layer2", "hole", color=col_l2)
s.difference("l3", "layer3", "hole", color=col_l3)

s.box("piece1", corner1=(-b,0,-b), corner2=(b,1,b), color=col_l1, reflection=r)
s.box("piece2", corner1=(-b,1,-b), corner2=(b,3,b), color=col_l2, reflection=r)
s.box("piece3", corner1=(-b,3,-b), corner2=(b,4,b), color=col_l3, reflection=r)

s.camera(position=(5, 10, -10))
s.show_preview(False)

# We first extract a piece from the tri-layer
s.skip_pov_gen(False)
for i in range(35):
    y = 0.2*i
    s.translate("piece1", y=y)
    s.translate("piece2", y=y)
    s.translate("piece3", y=y)
    s.camera(look_at=(0,5+0.5*y,0))
    s.render()

# Then we zoom into it
for i in range(12):
    f = 1.0 - 0.03*i
    s.camera(position=(5*f, 10, -10*f))
    s.render()

# We hide the rest of the tri-layer
for i in range(10):
    f = 1.0 - i*0.1
    c1 = [ci*f for ci in col_l1]
    c2 = [ci*f for ci in col_l2]
    c3 = [ci*f for ci in col_l3]
    s.material("l1", color=c1)
    s.material("l2", color=c2)
    s.material("l3", color=c3)
    s.render()

s.skip_pov_gen(False)

#s.arrow("x", p1=(-5,6,0), p2=(-4,6,0), color=(1,0,0))
#s.arrow("y", p1=(-5,6,0), p2=(-5,7,0), color=(0,1,0))
#s.arrow("z", p1=(-5,6,0), p2=(-5,6,1), color=(0,0,1))

# We display the geometrical details about the system
x = 1.2
z = 0.8
sw = 0.08
s.arrow("length1", p1=(x,y,z), p2=(x,y+1,z), shaftwidth=sw, double=True)
s.arrow("length2", p1=(x,y+1,z), p2=(x,y+3,z), shaftwidth=sw, double=True)
s.arrow("length3", p1=(x,y+3,z), p2=(x,y+4,z), shaftwidth=sw, double=True)
r = 0.2
s.cylinder("disc1", p1=(x,y,z), p2=(x,y+0.01,z), radius=r)
s.cylinder("disc2", p1=(x,y+1,z), p2=(x,y+1.01,z), radius=r)
s.cylinder("disc3", p1=(x,y+3,z), p2=(x,y+3.01,z), radius=r)
s.cylinder("disc4", p1=(x,y+4,z), p2=(x,y+4.01,z), radius=r)
x += 0.5
s.text("label1", text="10 nm", translate=(x, y+0.3, z), scale=0.8)
s.text("label2", text="20 nm", translate=(x, y+1.7, z), scale=0.8)
s.text("label3", text="10 nm", translate=(x, y+3.1, z), scale=0.8)

# And the compositions
x = -3.2
z = -1.2
s.text("material1", text="DyFe", translate=(x, y+0.3, z), scale=0.8, color=col_l1)
s.text("material12", text="2", translate=(x+1.9, y+0.1, z), scale=0.5, color=col_l1)
s.text("material2", text="YFe", translate=(x, y+1.7, z), scale=0.8, color=col_l2)
s.text("material22", text="2", translate=(x+1.5, y+1.5, z), scale=0.5, color=col_l2)
s.text("material3", text="DyFe", translate=(x, y+3.1, z), scale=0.8, color=col_l3)
s.text("material32", text="2", translate=(x+1.9, y+2.9, z), scale=0.5, color=col_l3)

s.render()

s.create_movie(loop=1, delay=0.05, width=5, file="es_intro.tex")
s.keep_tmp_files(keep_pov=False)
