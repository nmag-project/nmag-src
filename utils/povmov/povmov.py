import math
import commands

global povers

def pov_scale(object, attributes):
    if not attributes.has_key((object, "scale")): return ""
    return "   scale %f\n" % attributes[(object, "scale")]

def pov_translation(object, attributes):
    if not attributes.has_key((object, "translation")): return ""
    tx, ty, tz = attributes[(object, "translation")]
    return "   translate <%f, %f, %f>\n" % (tx, ty, tz)

def pov_rotation(object, attributes):
    if not attributes.has_key((object, "rotation")): return ""
    rx, ry, rz = attributes[(object, "rotation")]
    return "   rotate <%f, %f, %f>\n" % (rx, ry, rz)

def pov_transformation(object, attributes):
    return (pov_scale(object, attributes) +
      pov_translation(object, attributes) +
      pov_rotation(object, attributes))

def pov_color(object, attributes):
    if not attributes.has_key((object, "color")): return ""
    if attributes.has_key((object, "transmit")):
        t = attributes[(object, "transmit")]
        r, g, b = attributes[(object, "color")]
        return "   pigment {color rgbt <%f, %f, %f, %f>}\n" % (r, g, b, t)
    else:
        r, g, b = attributes[(object, "color")]
        return "   pigment {color rgb <%f, %f, %f>}\n" % (r, g, b)

def pov_reflection(object, attributes):
    if not attributes.has_key((object, "reflection")): return ""
    rmin, rmax = attributes[(object, "reflection")]
    r, g, b = rmin
    rr, gg, bb = rmax
    l = "   finish {\n      reflection {\n"
    l += "         rgb <%f, %f, %f>, rgb <%f, %f, %f>\n" % \
            (r, g, b, rr, gg, bb)
    l += "      }\n   }\n\n"
    return l

def pov_material(object, attributes):
    return pov_reflection(object, attributes)+pov_color(object, attributes)

def pov_transformation_material(object, attributes):
    return pov_transformation(object, attributes) + \
           pov_material(object, attributes)

def pov_background(object, attributes):
    r, g, b = attributes[(object, "color")]
    l = "background {<%f, %f, %f>}\n" % (r, g, b)
    return l


def pov_cylinder(object, attributes):
    p1x, p1y, p1z = attributes[(object, "p1")]
    p2x, p2y, p2z = attributes[(object, "p2")]
    radius = attributes[(object, "radius")]
    l = "cylinder {\n"
    l += "   <%f, %f, %f>, <%f, %f, %f>, %f\n" % \
         (p1x, p1y, p1z, p2x, p2y, p2z, radius)
    l += pov_transformation_material(object, attributes)
    l += "}\n\n"
    return l

def pov_cone(object, attributes):
    p1x, p1y, p1z = attributes[(object, "p1")]
    p2x, p2y, p2z = attributes[(object, "p2")]
    r1 = attributes[(object, "r1")]
    r2 = attributes[(object, "r2")]
    l = "cone {\n"
    l += "   <%f, %f, %f>, %f,\n" % (p1x, p1y, p1z, r1)
    l += "   <%f, %f, %f>, %f\n"  % (p2x, p2y, p2z, r2)
    l += pov_transformation_material(object, attributes)
    l += "}\n\n"
    return l

def pov_sphere(object, attributes):
    cx, cy, cz = attributes[(object, "center")]
    radius = attributes[(object, "radius")]
    l = "sphere {\n"
    l += "   <%f, %f, %f>, %f\n" % (cx, cy, cz, radius)
    l += pov_transformation_material(object, attributes)
    l += "}\n\n"
    return l

def pov_box(object, attributes):
    c1x, c1y, c1z = attributes[(object, "corner1")]
    c2x, c2y, c2z = attributes[(object, "corner2")]
    l  = "box {\n   <%f, %f, %f>, <%f, %f, %f>\n" \
            % (c1x, c1y, c1z, c2x, c2y, c2z)
    l += pov_transformation_material(object, attributes)
    l += "}\n\n"
    return l

def pov_torus(object, attributes):
    radius = attributes[(object, "radius")]
    width = attributes[(object, "width")]
    l = "torus {\n"
    l += "   %f, %f\n" % (radius, width)
    l += pov_transformation_material(object, attributes)
    l += "}\n\n"
    return l

def pov_text(object, attributes):
    width = attributes[(object, "width")]
    text = attributes[(object, "text")]
    font_file = attributes[(object, "font_file")]
    l = "text {\n"
    l += '   ttf "%s"\n' % font_file
    l += '   "%s"\n' % text
    l += "   %f, <0, 0>\n" % width
    l += pov_transformation_material(object, attributes)
    l += "}\n\n"
    return l

def pov_arrow(object, attributes):
    p1 = attributes[(object, "p1")]
    p2 = attributes[(object, "p2")]
    shaftwidth = attributes[(object, "shaftwidth")]
    headwidth = attributes[(object, "headwidth")]
    headlength = attributes[(object, "headlength")]
    double = attributes[(object, "double")]
    l = "union {\n"
    if double:
        d = [p2i - p1i for p1i, p2i in zip(p1, p2)]
        n = math.sqrt(sum([x*x for x in d]))
        if shaftwidth:
            r = shaftwidth
        else:
            r = 0.05*n
        if headlength:
            hl = headlength
        else:
            hl = 3.0*r
        if headwidth:
            hw = headwidth
        else:
            hw = 2.0*r
        f = hl/n
        pp1 = [p1i + di*f for p1i, di in zip(p1, d)]
        f = (n - hl)/n
        pp2 = [p1i + di*f for p1i, di in zip(p1, d)]

        my_attr = {
          ("cy", "p1"):pp1, ("cy", "p2"):pp2, ("cy", "radius"):r,
          ("c1", "p1"):pp1, ("c1", "p2"):p1, ("c1", "r1"):hw, ("c1", "r2"):0,
          ("c2", "p1"):pp2, ("c2", "p2"):p2, ("c2", "r1"):hw, ("c2", "r2"):0
        }
        l += pov_cylinder("cy", my_attr)
        l += pov_cone("c1", my_attr)
        l += pov_cone("c2", my_attr)
    else:
        d = [p2i - p1i for p1i, p2i in zip(p1, p2)]
        n = math.sqrt(sum([x*x for x in d]))
        if shaftwidth:
            r = shaftwidth
        else:
            r = 0.05*n
        if headlength:
            hl = headlength
        else:
            hl = 3.0*r
        if headwidth:
            hw = headwidth
        else:
            hw = 2.0*r
        f = (n - hl)/n
        p12 = [p1i + di*f for p1i, di in zip(p1, d)]

        my_attr = {
          ("cy", "p1"):p1, ("cy", "p2"):p12, ("cy", "radius"):r,
          ("co", "p1"):p12, ("co", "p2"):p2, ("co", "r1"):hw, ("co", "r2"):0,
        }
        l += pov_cylinder("cy", my_attr)
        l += pov_cone("co", my_attr)
    l += pov_transformation_material(object, attributes)
    l += "}\n\n"
    return l

def pov_curve(object, attributes):
    c_min = attributes[(object, "c_min")]
    c_max = attributes[(object, "c_max")]
    pos = attributes[(object, "pos_fn")]
    width = attributes[(object, "width_fn")]
    n = attributes[(object, "n")]

    l = "union {\n"

    delta_c = (c_max - c_min)/n
    prev_r = pos(c_min)
    c = c_min + delta_c
    obj_list = []
    for i in range(n):
        r = pos(c)
        c += delta_c
        w = width(c)
        my_attr = {
          ("cy", "p1"):prev_r, ("cy", "p2"):r, ("cy", "radius"):w,
          ("sp", "center"):prev_r, ("sp", "radius"):w
        }
        l += pov_cylinder("cy", my_attr)
        l += pov_sphere("sp", my_attr)
        prev_r = r

    l += pov_transformation_material(object, attributes)
    l += "}\n\n"
    return l


def pov_global_settings(object, attributes):
    assumed_gamma = attributes[(object, "assumed_gamma")]
    noise_generator = attributes[(object, "noise_generator")]
    l = "global_settings {\n"
    l += "   assumed_gamma %f\n" % assumed_gamma
    l += "   noise_generator %f\n" % noise_generator
    l += "}\n\n"
    return l

def pov_light_source(object, attributes):
    position = attributes[(object, "position")]
    color = attributes[(object, "color")]
    px, py, pz = position
    r, g, b = color
    l = "light_source {<%f, %f, %f>, rgb <%f, %f, %f>}\n\n" % \
        (px, py, pz, r, g, b)
    return l

def pov_camera(object, attributes):
    px, py, pz = attributes[(object, "position")]
    sx, sy, sz = attributes[(object, "sky")]
    dx, dy, dz = attributes[(object, "direction")]
    rx, ry, rz = attributes[(object, "right")]
    ux, uy, uz = attributes[(object, "up")]
    lx, ly, lz = attributes[(object, "look_at")]
    l = "camera {\n   perspective\n"
    l += "   location <%f, %f, %f>\n" % (px, py, pz)
    l += "   sky <%f, %f, %f>\n" % (sx, sy, sz)
    l += "   direction <%f, %f, %f>\n" % (dx, dy, dz)
    l += "   right <%f, %f, %f>\n" % (rx, ry, rz)
    l += "   up <%f, %f, %f>\n" % (ux, uy, uz)
    l += "   look_at <%f, %f, %f>\n" % (lx, ly, lz)
    l += "}\n\n"
    return l

def pov_difference(object, attributes):
    left, left_type = attributes[(object, "left")]
    right, right_type = attributes[(object, "right")]
    left_pover = povers[left_type]
    right_pover = povers[right_type]
    l = "difference {\n"
    l += left_pover(left, attributes)
    l += right_pover(right, attributes)
    l += pov_transformation_material(object, attributes)
    l += "}\n\n"
    return l

def pov_constructive_geometry(t, object, attributes):
    l = "%s {\n" % t
    part_num = 0
    while True:
        part_num += 1
        part_name = "part%d" % part_num
        if attributes.has_key((object, part_name)):
            part, part_type = attributes[(object, part_name)]
            part_pover = povers[part_type]
            l += part_pover(part, attributes)
        else:
            break
    l += pov_transformation_material(object, attributes)
    l += "}\n\n"
    return l

def pov_union(object, attributes):
    return pov_constructive_geometry("union", object, attributes)

def pov_intersection(object, attributes):
    return pov_constructive_geometry("intersection", object, attributes)

# Associate each object type with a pover-writer
povers = {
    "background":pov_background,
    "box":pov_box,
    "cylinder":pov_cylinder,
    "cone":pov_cone,
    "sphere":pov_sphere,
    "torus":pov_torus,
    "text":pov_text,
    "arrow":pov_arrow,
    "curve":pov_curve,
    "global_settings":pov_global_settings,
    "light_source":pov_light_source,
    "camera":pov_camera,
    "union":pov_union,
    "difference":pov_difference,
    "intersection":pov_intersection
}

class scene:
    def __init__(self):
        self.world = {}
        self.attributes = {}
        self.default = {}
        self.global_settings()
        self.pov_file_pattern = "/tmp/povmov"
        self.antialiasing = True
        self.antialiasing_val = 0.1
        self.width = 640
        self.height = 480
        self.render_num = 0
        self.rm_pov = True
        self.rm_png = True
        self.verbose = 0
        self.preview = False
        self.skip = False
        self.camera(position=(5.0, 5.0, -5.0), sky=(0.0, 1.0, 0.0),
                    direction=(0.0, 0.0, 1.0), right=(1.333333, 0.0, 0.0),
                    up=(0.0, 1.0, 0.0), look_at=(0.0, 0.0, 0.0))
        self.initial_frame = 0
        self.initial_i = 0 # This code is becoming shit!

    def __del__(self):
        self.rm_tmp_files(pov=self.rm_pov, png=self.rm_png)

    def set_default(self, attribute, value):
        self.default[attribute] = value

    def get_default(self, attribute, value=None):
        if value != None:
            return value
        else:
            if self.default.has_key(attribute):
                return self.default[attribute]
            else:
                return None

    def keep_tmp_files(self, keep_png=False, keep_pov=False):
        self.rm_pov = not keep_pov
        self.rm_png = not keep_png

    def show_preview(self, show=True):
        self.preview = show

    def set_verbosity(self, level=1):
        self.verbose = level

    def skip_pov_gen(self, skip=True):
        self.skip = skip

    def screen(self, width=None, height=None):
        if width: self.width = width
        if height: self.height = height

    def translate(self, object, x=0.0, y=0.0, z=0.0, vector=None):
        if vector:
            self.attributes[(object, "translation")] = vector
        else:
            self.attributes[(object, "translation")] = (x, y, z)

    def rotate(self, object, a1=0.0, a2=0.0, a3=0.0, angles=None):
        if angles:
            self.attributes[(object, "rotation")] = angles
        else:
            self.attributes[(object, "rotation")] = (a1, a2, a3)

    def scale(self, object, factor=None):
        if factor:
            self.attributes[(object, "scale")] = factor

    def transform(self, object, vector=None, angles=None, factor=None):
        self.translate(object, vector=vector)
        self.rotate(object, angles=angles)
        self.scale(object, factor=factor)

    def color(self, object, r=1.0, g=1.0, b=1.0, rgb=None):
        if rgb:
            self.attributes[(object, "color")] = rgb
        else:
            self.attributes[(object, "color")] = (r, g, b)

    def reflection(self, object, rmin=(0.5,0.5,0.5), rmax=(0,0,0),
                   reflection=None):
        if reflection:
            self.attributes[(object, "reflection")] = reflection
        else:
            self.attributes[(object, "reflection")] = (rmin, rmax)

    def material(self, object, rgb=None, color=None,
                 transmit=None, reflection=None):
        if color: rgb=color
        if rgb: self.color(object, rgb=rgb)
        if reflection: self.reflection(object, reflection=reflection)
        if transmit: self.attributes[(object, "transmit")] = transmit

    def pov(self):
        if self.skip: return
        have_light = False
        for obj_name in self.world:
            obj_type = self.world[obj_name]
            if obj_type == "light_source":
                have_light = True
                break
        if not have_light:
            self.light_source("light")

        for obj_name in self.world:
            if not self.attributes.has_key((obj_name, "ignore")):
                obj_type = self.world[obj_name]
                pover = povers[obj_type]
                self.out( pover(obj_name, self.attributes) )

    def box(self, name, corner1=(0.0, 0.0, 0.0), corner2=(1.0, 1.0, 1.0),
            color=None, reflection=None,
            translate=None, rotate=None, scale=None):
        color = self.get_default("color", value=color)
        self.world[name] = "box"
        self.transform(name, vector=translate, angles=rotate, factor=scale)
        self.material(name, color=color, reflection=reflection)
        self.attributes[(name, "corner1")] = corner1
        self.attributes[(name, "corner2")] = corner2

    def cylinder(self, name,
                 p1=(0.0, 0.0, 0.0), p2=(1.0, 0.0, 0.0), radius=0.3,
                 color=None, reflection=(), translate=(), rotate=(),scale=()):
        color = self.get_default("color", value=color)
        self.world[name] = "cylinder"
        self.transform(name, vector=translate, angles=rotate, factor=scale)
        self.material(name, color=color, reflection=reflection)
        self.attributes[(name, "p1")] = p1
        self.attributes[(name, "p2")] = p2
        self.attributes[(name, "radius")] = radius

    def cone(self, name,
             p1=(0.0, 0.0, 0.0), r1=0.0, p2=(1.0, 0.0, 0.0), r2=0.3,
             color=None, reflection=(), translate=(), rotate=(), scale=()):
        color = self.get_default("color", value=color)
        self.world[name] = "cone"
        self.transform(name, vector=translate, angles=rotate, factor=scale)
        self.material(name, color=color, reflection=reflection)
        self.attributes[(name, "p1")] = p1
        self.attributes[(name, "p2")] = p2
        self.attributes[(name, "r1")] = r1
        self.attributes[(name, "r2")] = r2

    def sphere(self, name, center=(0.0, 0.0, 0.0), radius=0.3,
               color=None, reflection=(), translate=(), rotate=(), scale=()):
        color = self.get_default("color", value=color)
        self.world[name] = "sphere"
        self.transform(name, vector=translate, angles=rotate, factor=scale)
        self.material(name, color=color, reflection=reflection)
        self.attributes[(name, "center")] = center
        self.attributes[(name, "radius")] = radius

    def torus(self, name, radius=1.0, width=0.2,
              color=None, reflection=(), translate=(), rotate=(), scale=()):
        color = self.get_default("color", value=color)
        self.world[name] = "torus"
        self.transform(name, vector=translate, angles=rotate, factor=scale)
        self.material(name, color=color, reflection=reflection)
        self.attributes[(name, "radius")] = radius
        self.attributes[(name, "width")] = width

    def text(self, name, text="Text", width=0.1, font_file="./arial.ttf",
             color=None, reflection=(), translate=(), rotate=(), scale=()):
        color = self.get_default("color", value=color)
        self.world[name] = "text"
        self.transform(name, vector=translate, angles=rotate, factor=scale)
        self.material(name, color=color, reflection=reflection)
        self.attributes[(name, "text")] = text
        self.attributes[(name, "width")] = width
        self.attributes[(name, "font_file")] = font_file

    def arrow(self, name, p1, p2,
              shaftwidth=None, headwidth=None, headlength=None, double=False,
              color=None, reflection=None, translate=(), rotate=(), scale=()):
        color = self.get_default("color", value=color)
        self.world[name] = "arrow"
        self.transform(name, vector=translate, angles=rotate, factor=scale)
        self.material(name, color=color, reflection=reflection)
        self.attributes[(name, "p1")] = p1
        self.attributes[(name, "p2")] = p2
        self.attributes[(name, "shaftwidth")] = shaftwidth
        self.attributes[(name, "headwidth")] = headwidth
        self.attributes[(name, "headlength")] = headlength
        self.attributes[(name, "double")] = double

    def curve(self, name, pos_fn, width, c_min=0.0, c_max=1.0, n=50,
              shaftwidth=None, headwidth=None, headlength=None, double=False,
              color=None, reflection=None, translate=(), rotate=(), scale=()):
        color = self.get_default("color", value=color)
        self.world[name] = "curve"
        self.transform(name, vector=translate, angles=rotate, factor=scale)
        self.material(name, color=color, reflection=reflection)
        self.attributes[(name, "c_min")] = c_min
        self.attributes[(name, "c_max")] = c_max
        self.attributes[(name, "pos_fn")] = pos_fn
        if type(width) == float:
            width_fn = lambda c: width
        else:
            width_fn = width
        self.attributes[(name, "width_fn")] = width_fn
        self.attributes[(name, "n")] = n

    def ignore(self, name):
        '''Function used whenever an object appears in intersections,
           differences, etc. This function clean the material attributes
           of the object (which are not needed) and marks the object
           to be ignored by the system.'''
        self.attributes[(name, "ignore")] = True
        if self.attributes.has_key((name, "color")):
            del self.attributes[(name, "color")]
        if self.attributes.has_key((name, "reflection")):
            del self.attributes[(name, "reflection")]

    def union(self, name, object_list, color=None, reflection=None,
              translate=None, rotate=None, scale=None):
        color = self.get_default("color", value=color)
        self.world[name] = "union"
        self.transform(name, vector=translate, angles=rotate, factor=scale)
        self.material(name, color=color, reflection=reflection)
        part_num = 0
        for part in object_list:
            part_num += 1
            part_name = "part%d" % part_num
            self.attributes[(name, part_name)] = (part, self.world[part])
            self.ignore(part)

    def difference(self, name, left, right, color=None, reflection=None,
                   translate=None, rotate=None, scale=None):
        color = self.get_default("color", value=color)
        self.world[name] = "difference"
        self.transform(name, vector=translate, angles=rotate, factor=scale)
        self.material(name, color=color, reflection=reflection)
        self.ignore(left)
        self.ignore(right)
        self.attributes[(name, "left")] = (left, self.world[left])
        self.attributes[(name, "right")] = (right, self.world[right])

    def global_settings(self, assumed_gamma=1.5, noise_generator=2.0):
        name = "global_settings"
        self.world[name] = "global_settings"
        self.attributes[(name, "assumed_gamma")] = assumed_gamma
        self.attributes[(name, "noise_generator")] = noise_generator

    def light_source(self, name, position=(4.0, 10.0, -5.0), color=(1.0, 1.0, 1.0)):
        self.world[name] = "light_source"
        self.attributes[(name, "position")] = position
        self.attributes[(name, "color")] = color

    def set(self, name, attribute, value):
        if value != None:
            self.attributes[(name, attribute)] = value
        else:
            if self.attributes.has_key((name, attribute)): return
            raise "Error: '%s' does not have the attribute '%s'" % \
                  (name, attribute)

    def get(self, name, attribute):
        return self.attributes[(name, attribute)]

    def background(self, color=None):
        self.world["background"] = "background"
        if color != None:
            self.attributes[("background", "color")] = color

    def camera(self, position=None, sky=None,
               direction=None, right=None,
               up=None, look_at=None):
        name = "camera"
        self.world[name] = "camera"
        self.set(name, "position", position)
        self.set(name, "sky", sky)
        self.set(name, "direction", direction)
        self.set(name, "right", right)
        self.set(name, "up", up)
        self.set(name, "look_at", look_at)

    def file(self, num_file=None, ext=None):
        f = self.pov_file_pattern
        if num_file: f += "%05d" % num_file
        if ext: f += '.' + ext
        return f

    def pov_file(self, num_file=None):
        return self.file(num_file=num_file, ext="pov")

    def png_file(self, num_file):
        return self.file(num_file=num_file, ext="png")

    def render(self, png_file_name=None):
        if self.skip: return
        f_name = self.pov_file(num_file=self.render_num)
        if png_file_name != None:
            f_png_name = png_file_name
        else:
            f_png_name = self.png_file(num_file=self.render_num)
        self.render_num += 1
        f = open(f_name, "w")
        def out(string): f.write(string)
        self.out = out
        self.pov()
        f.close()
        options = "+W%d +H%d " % (self.width, self.height)
        if self.antialiasing:
            options += "+A%1.2f " % self.antialiasing_val
        if not self.preview:
            options += "-D"
        cmnd = "povray %s %s +O%s" % (f_name, options, f_png_name)
        if self.verbose > 0: print cmnd
        output = commands.getoutput(cmnd)
        if 'command not found' in output:
            print "Problem: ", output

    def create_movie(self, file="movie.gif", loop=0, delay=0.1,
                     width=None, height=None, keep=False, clear=False,
                     codec="mpeg4"):
        '''delay is the delay between frames expressed in seconds.
           codec is using when creating an AVI file and can be:
             cinepak: (require VFW, doesn't work on Mac?)
             mjpeg: Motion JPEG
             h263: codec H263
             h263p: H263 Plus
             mpeg4, divx5: DivX 4/5
             rv10: an old RealVideo codec
             mpeg1video: MPEG1 video
           experience suggest that: mpeg4 is good, but not perfectly supported
           (Powerpoint for Mac works badly, PowerPoint 2003 for Win
           behaves in a buggy way: first frame shown, but when playing
           screen is black). Conclusion: avoid mpeg4 in presentations.
           mpeg1video is not supported by Quicktime and is buggy under Win,
           mjpeg is an extension of jpeg, but is not supported by default.
           Cinepak is the right way to go! It is installed by default on Win
           and Mac. There is no need to install codecs, this is what one
           really wants when bringing his presentation to a conference
           (your laptop could break and you may need to use another
           unknown machine, Win machine 99 % of cases). Cinepak is slow
           when compressing and gives bad results in terms of size,
           but your movie will be portable!!!
        '''
        pngs_list = [self.png_file(i) for i in range(self.render_num)]
        pngs = " ".join(pngs_list)
        if ".gif" in file:
            d = int(delay*100.0)
            cmnd = "convert -loop %d -delay %d %s %s" % (loop, d, pngs, file)
            if self.verbose > 0: print cmnd
            print commands.getoutput(cmnd)
        elif ".avi" in file:
            #import tempfile
            #new_dir = tempfile.mkdtemp()
            #if self.verbose > 0:
                #print "Created the new directory '%s' where to put " \
                      #"the png files obtained from povray." % new_dir
            #os.chdir(new_dir)
            all_pngs = " ".join(["mf://"+f for f in pngs_list])
            all_pngs = "mf://%s" % self.pov_file_pattern + "*.png"
            fps = int(1.0/delay)
            # Common lavc encoding options
            lavc = "-ovc lavc -lavcopts vcodec=%s"
            # Common vfw (video for Windows) encoding options
            vfw = "-ovc vfw -xvfwopts codec=%s"
            known_codecs = {"mjpeg":(lavc % "mjpeg", ""),
                            "mpeg1video":(lavc % "mpeg1video", ""),
                            "mpeg1":(lavc % "mpeg1video", ""),
                            "mpeg2video":(lavc % "mpeg2video", ""),
                            "mpeg2":(lavc % "mpeg2video", "-ffourcc MPG2"),
                            "divx4":(lavc % "mpeg4", "-ffourcc DX50"),
                            "divx5":(lavc % "mpeg4", "-ffourcc DX50"),
                            "mpeg4":(lavc % "mpeg4", "-ffourcc DX50"),
                            "h263":(lavc % "h263", ""),
                            "h263p":(lavc % "h263p", ""),
                            "rv10":(lavc % "rv10", ""),
                            "cinepak":(vfw % "iccvid.dll", "")}
            codec_opts, extra_opts = known_codecs[codec]

            cmnd = "mencoder %s " % all_pngs + \
                   "-mf fps=%d " % fps + \
                   " %s %s " % (codec_opts, extra_opts) + \
                   "-of avi -o %s" % file
            if self.verbose > 0: print cmnd
            s = commands.getoutput(cmnd)
            if self.verbose > 0: print s
        elif ".tex" in file:
            sizes = []
            if width: sizes.append("width=%fcm" % width)
            if height: sizes.append("height=%fcm" % height)
            sizes_str = ", ".join(sizes)
            import os
            os.mkdir(file)
            os.chdir(file)
            self.rm_png = False
            f = open(file, "w")
            num_frame = self.initial_frame
            png_files = [self.png_file(i)
                         for i in range(self.initial_frame, self.render_num)]
            i = self.initial_i
            for png_file in png_files:
                num_frame += 1
                if num_frame == self.initial_frame+len(png_files):
                    i += 1
                    frame_file = "frame_last.png"
                    if keep:
                        si = "%d-" % i
                    else:
                        si = "%d" % i
                elif (num_frame-1) % 2 == 0:
                    i += 1
                    frame_file = "frame%d.png" % i
                    si = "%d" % i
                else:
                    continue
                commands.getoutput("mv %s ./%s" % (png_file, frame_file))
                f.write("\includegraphics<%s>[%s]{%s/%s}%%\n"
                        % (si, sizes_str, file, frame_file))
            f.write("\\transduration<%d-%d>{%f}%%\n"
                    % (self.initial_i+2, i-1, delay))
            f.write("\\pause[%d]%%\n" % i)
            f.close()
            os.chdir("../")
            self.initial_i = i
        else:
            raise "Cannot recognize the output movie format."
        self.initial_frame = self.render_num
        if clear:
            self.rm_tmp_files()
            self.render_num = 0

    def rm_tmp_files(self, pov=True, png=True):
        pngs = ""
        povs = ""
        if png:
            pngs = " ".join([self.png_file(i) for i in range(self.render_num)])
        if pov:
            povs = " ".join([self.pov_file(i) for i in range(self.render_num)])
        if not (pov or png): return
        cmnd = "rm -rf %s %s" % (povs, pngs)
        if self.verbose > 0: print cmnd
        commands.getoutput(cmnd)
