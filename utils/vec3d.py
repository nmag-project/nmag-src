#!/usr/bin/python
###########################################################################
#   Copyright (C) 2006 by Matteo Franchin                                 #
#   fnch@libero.it                                                        #
#                                                                         #
#   This program is free software; you can redistribute it and/or modify  #
#   it under the terms of the GNU General Public License as published by  #
#   the Free Software Foundation; either version 2 of the License, or     #
#   (at your option) any later version.                                   #
#                                                                         #
#   This program is distributed in the hope that it will be useful,       #
#   but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         #
#   GNU General Public License for more details.                          #
#                                                                         #
#   You should have received a copy of the GNU General Public License     #
#   along with this program; if not, write to the                         #
#   Free Software Foundation, Inc.,                                       #
#   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             #
###########################################################################

import sys

def show_usage_and_exit():
  print "vec3d: program to visualize animations of 3D fields"
  print
  print "BASIC USAGE: ./vec3d field1 field2 field3 ..."
  print " Where \"fieldn\" is a file made up of rows containing:"
  print "  column 1: the time"
  print "  column 2, 3, 4: the coordinates of the position of the field"
  print "  column 5, 6, 7: the field (x, y, z components)"
  print "ADVANCED USAGE: you can use also the following options:"
  print " -h         shows this help screen and exits"
  print " -n         the following(only the following) field will be normalized"
  print " -s factor  the following(only the following) field will be scaled"
  print " -z         allows VPython to auto-scale the scene"
  print " -w time    waits 'time' seconds before starting the animation"
  print " -tx length translate the mesh in the x-direction by 'length'"
  print " -ty length translate the mesh in the y-direction by 'length'"
  print " -tz length translate the mesh in the z-direction by 'length'"
  print " -f time    seconds per frame"
  print " -d factor  randomly decimate data (between 0 and 1)"
  print " -a         avoid displaying the reference system"
  print " -l         avoid to label the three reference axes"
  print " -c         assign colors looking also at the norm of vectors"
  print " -sw width  shaftwidth for the arrows"
  print
  print "VERSION ID: $Id$"
  sys.exit(0)

if len(sys.argv) < 2: show_usage_and_exit()

import math
import visual
import re
import colorsys
import time
import random

class settings:
  def __init__(self):
    self.auto_zoom = False
    self.wait_before_visualization = 10
    self.normalize = False
    self.scale = 1.0
    self.translation_x = 0.0
    self.translation_y = 0.0
    self.translation_z = 0.0
    self.sec_per_frame = 0.0
    self.decimate = 1.0
    self.axis = True
    self.label = True
    self.colorize_norm = True
    self.max_abs_coords = [5.0, 5.0, 5.0]
    self.min_arrow_width = 0.0
    self.shaftwidth = 0.0

  def reset(self):
    self.normalize = False
    self.scale = 1.0
    self.translation_x = 0.0
    self.translation_y = 0.0
    self.translation_z = 0.0
    self.decimate = 1.0
    self.colorize_norm = True

class mumag_data:
  def __init__(self, file_name="", file_object=sys.stdin):
    # Visualization settings
    self.dim = None
    self.trace_ray   = 0.02
    self.arrow_width = 0.03
    self.arrow_col = visual.color.red
    self.arrow_shaftwidth = 0.0
    self.pnt_col = visual.color.green
    self.pnt_radius = 0.05
    self.tx = 0.0
    self.ty = 0.0
    self.tz = 0.0
    self.decimate = 1.0
    self.colorize_norm = True

    # Field settings
    self.normalize = False
    self.scale = 0.5

    # Internal variables
    self.name = "unknown"
    self.last_line = ""
    self.frame_no = 0
    #  speed = 1
    self.field_mean = [0.0, 0.0, 0.0]
    self.first_scan = False

    # Temporary code
    self.maxvec_norm = 0.0

    self.step_num = 0
    if file_name != "":
      self.file = open(file_name, 'r')
      self.name = file_name
    else:
      self.file = file_object

  def read_line(self, line):
    col = line[:-1].split()
    dim = self.dim = len(col) - 4
    if self.dim == None:
      if dim < 1 or dim > 3:
        raise "Only 1D, 2D and 3D data can be shown."
      self.dim = dim
    else:
      if dim != self.dim:
        raise "Found inconsistency in the number of columns."
      x = ["0", "0", "0"]
      sys.stdout.flush()
      for i in range(dim):
        x[i] = col[i+1]
      return [col[0], x[0], x[1], x[2], col[-3], col[-2], col[-1]]

  def __destroy__(self):
    self.file.close()

  def set_normalize(self, n): self.normalize = n

  def set_decimate(self, d): self.decimate = d

  def set_scale(self, s): self.scale = s

  def set_arrow_shaftwidth(self, sw): self.arrow_shaftwidth = sw

  def set_translation(self, x, y, z):
    self.tx = x
    self.ty = y
    self.tz = z

  def set_colorize_norm(self, b): self.colorize_norm = b

  def get_name(self):
    return self.name

  def min_arrow_width(self, default=0.0):
    min_width = default
    for arrow_stuff in self.arrows:
      on, _, arrow, sph = arrow_stuff
      if on:
        if arrow.shaftwidth < min_width or min_width == 0.0:
          min_width = arrow.shaftwidth
    return min_width

  def adjust_pos(self, position):
    return [position[0]+self.tx, position[1]+self.ty, position[2]+self.tz]

  def adjust_vector(self, vector):
    vx = vector[0]
    vy = vector[1]
    vz = vector[2]

    # Temporary code
    v = math.sqrt(vx*vx + vy*vy + vz*vz)
    if (v > self.maxvec_norm): self.maxvec_norm = v

    if self.normalize:
      # Normalize the vector if required
      v = math.sqrt(vx*vx + vy*vy + vz*vz)
      vx /= v
      vy /= v
      vz /= v

    s = self.scale
    return [s*vx, s*vy, s*vz]

  def preliminary_scan(self):
    # During the first time-step we only collect all the positions:
    # we will create an arrow for every position encountered
    i = 0 # column number
    t_old = 0.0
    self.field_size = 0
    self.arrows = []
    field_sum = [0.0, 0.0, 0.0]
    max_x = 0.0
    max_y = 0.0
    max_z = 0.0

    while True:
      self.last_line = self.file.readline()
      col = self.read_line(self.last_line)
      t = float(col[0])
      if (i > 0) & (t != t_old): break;

      i = i + 1
      t_old = t

      # Read the postition where the field is located
      p = self.adjust_pos([float(col[1]), float(col[2]), float(col[3])])

      if abs(p[0]) > max_x: max_x = abs(p[0])
      if abs(p[1]) > max_y: max_y = abs(p[1])
      if abs(p[2]) > max_z: max_z = abs(p[2])

      # Read the field
      field = self.adjust_vector([float(col[4]), float(col[5]), float(col[6])])
      field_sum[0] += field[0]
      field_sum[1] += field[1]
      field_sum[2] += field[2]

      if ( random.random() <= self.decimate ):
        sph = visual.sphere(color=self.pnt_col, pos=p, radius=self.pnt_radius)
        arr = visual.arrow(color=self.arrow_col, pos=p, axis=field,
         shaftwidth=self.arrow_shaftwidth)
        on = True
      else:
        sph = None
        arr = None
        on = False

      self.arrows.append( [on, p, arr, sph] )
      self.field_size += 1

    self.max_abs_coords = [max_x, max_y, max_z]
    self.t_old = t

    # Now we calculate the mean magnetization
    self.field_mean[0] = field_sum[0] / self.field_size
    self.field_mean[1] = field_sum[1] / self.field_size
    self.field_mean[2] = field_sum[2] / self.field_size
    self.first_scan = True

  def get_max_abs_coords(self, default=[0.0, 0.0, 0.0]):
    mcx, mcy, mcz = self.max_abs_coords
    if default[0] > mcx: mcx = default[0]
    if default[1] > mcy: mcy = default[1]
    if default[2] > mcz: mcz = default[2]
    return [mcx, mcy, mcz]

  def get_time(self):
    if not self.first_scan: return -1.0
    if (self.last_line == ""): return -1.0
    col = self.read_line(self.last_line)
    return float(col[0])

  def read_again(self):
    if not self.first_scan: return True

    # Now we can start the animation
    d = [0.0, 0.0, 0.0]
    field_sum = [0.0, 0.0, 0.0]
    self.step_num += 1
    for i in range(self.field_size):
      if (self.last_line == ""): break
      col = self.read_line(self.last_line)
      self.last_line = self.file.readline()
      t = float(col[0])
      if (t != self.t_old):
        print "Error: At time step (%d, t=%f) I found a wrong number " \
          "of field entries." % (self.step_num, self.t_old)
        return False

      on, pos, arr, sph = self.arrows[i]

      # Read field and its position
      p = self.adjust_pos([float(col[1]), float(col[2]), float(col[3])])
      field = self.adjust_vector([float(col[4]), float(col[5]), float(col[6])])
      field_sum[0] += field[0]
      field_sum[1] += field[1]
      field_sum[2] += field[2]

      if (p[0] != pos[0]) | (p[1] != pos[1]) | (p[2] != pos[2]):
        print "Error in consistency check! It seems that my expectations are wrong!"
        print "Expected position: ", pos
        print "Read position: ", p
        return False

      if on:
        if self.colorize_norm:
          f1 = field
          f2 = self.field_mean
          nf1 = math.sqrt(f1[0]*f1[0]+f1[1]*f1[1]+f1[2]*f1[2])
          nf2 = math.sqrt(f2[0]*f2[0]+f2[1]*f2[1]+f2[2]*f2[2])
          d[0] = f1[0]/nf1 - f2[0]/nf2
          d[1] = f1[1]/nf1 - f2[1]/nf2
          d[2] = f1[2]/nf1 - f2[2]/nf2
        else:
          d[0] = field[0] - self.field_mean[0]
          d[1] = field[1] - self.field_mean[1]
          d[2] = field[2] - self.field_mean[2]

        norm_field_diff = math.sqrt(d[0]*d[0] + d[1]*d[1] + d[2]*d[2])
        col = colorsys.hsv_to_rgb(0.5*norm_field_diff, 1.0, 1.0)

        arr.axis = field
        arr.color = col

    # Now we calculate the mean magnetization (to be used in the next call)
    self.field_mean[0] = field_sum[0] / self.field_size
    self.field_mean[1] = field_sum[1] / self.field_size
    self.field_mean[2] = field_sum[2] / self.field_size
    if (self.last_line == ""): return False # No more data to read
    col = self.read_line(self.last_line)
    self.t_old = float(col[0])
    return True

  def skip_frame(self):
    if not self.first_scan: return True

    # Now we can start the animation
    self.step_num += 1
    for i in range(self.field_size):
      if (self.last_line == ""): break
      col = self.read_line(self.last_line)
      self.last_line = self.file.readline()
      t = float(col[0])
      if (t != self.t_old):
        print "Error: At time step (%d, t=%f) I found a wrong number \
          of field entries." % (self.step_num, self.t_old)

      on, pos, arr, sph = self.arrows[i]

      # Read field and its position
      p = self.adjust_pos([float(col[1]), float(col[2]), float(col[3])])
      field = self.adjust_vector[float(col[4]), float(col[5]), float(col[6])]
      field_sum[0] += field[0]
      field_sum[1] += field[1]
      field_sum[2] += field[2]

      if (p[0] != pos[0]) | (p[1] != pos[1]) | (p[2] != pos[2]):
        print "Error in consistency check! It seems that my expectations are wrong!"
        print "Expected position: ", pos
        print "Read position: ", p
        sys.exit(1)

    # Now we calculate the mean magnetization (to be used in the next call)
    self.field_mean[0] = field_sum[0] / self.field_size
    self.field_mean[1] = field_sum[1] / self.field_size
    self.field_mean[2] = field_sum[2] / self.field_size
    if (self.last_line == ""): return False # No more data to read
    col = self.read_line(self.last_line)
    self.t_old = float(col[0])
    return True


# Just a replacement for the buggy arrow (it seems so)
def my_arrow(pos, axis, proportion=True,
  shaftwidth=0.1, headwidth=2, headlength=3, color=[1.0, 1.0, 1.0]):
  px, py, pz = pos
  x, y, z = axis
  sw = shaftwidth
  length = math.sqrt(x*x + y*y + z*z)
  if proportion: sw *= length
  hl = sw*headlength/length
  l = 1.0 - hl
  lx = l*x; ly = l*y; lz = l*z
  cylinder_axis = [lx, ly, lz]
  cone_pos = [px+lx, py+ly, pz+lz]
  cone_axis = [hl*x, hl*y, hl*z]
  cone_radius = sw*headwidth
  visual.cylinder(pos=pos, axis=cylinder_axis, radius=sw, color=color)
  visual.cone(pos=cone_pos, axis=cone_axis, radius=cone_radius, color=color)


################################################################################
#                               MAIN PROGRAM                                   #
################################################################################

s = settings()

try:
  print "Parsing the command line"
  field_list = []
  index = 1
  num_args = len(sys.argv)
  while True:
    if index >= num_args: break
    arg = sys.argv[index]
    index += 1
    if arg[0] == '-' and len(arg) > 1:
      if arg[1] == '-':
        if len(arg) > 2:
          option = arg[2:]
        else:
          print "Error: invalid option entered. Exiting!"
          sys.exit(1)
      else:
        option = arg[1:]
      if option == "n" or option == "normalize":
        s.normalize = True
      elif option == "z" or option == "zoom":
        s.auto_zoom = True
      elif option == "a" or option == "noaxis":
        s.axis = not s.axis
      elif option == "l" or option == "nolabel":
        s.label = not s.label
      elif option == "c":
        s.colorize_norm = not s.colorize_norm
      elif option == "?" or option == "h" or option == "help":
        print
        show_usage_and_exit()
      else:
        if index < num_args:
          arg = sys.argv[index]
          index += 1
        else:
          print "Error: option not found."
          sys.exit(1)

        if option == "s" or option == "scale":
          s.scale = float(arg)
        elif option == "sw" or option == "shaftwidth":
          s.shaftwidth = float(arg)
        elif option == "w" or option == "wait":
          s.wait_before_visualization = float(arg)
        elif option == "tx":
          s.translation_x = float(arg)
        elif option == "ty":
          s.translation_y = float(arg)
        elif option == "tz":
          s.translation_z = float(arg)
        elif option == "f":
          s.sec_per_frame = float(arg)
        elif option == "d":
          s.decimate = float(arg)
        else:
          print "Error: option not found."
          sys.exit(1)

    else:
      print "Drawing initial configuration for '%s'" % arg
      m = mumag_data(arg)
      m.set_colorize_norm(s.colorize_norm)
      m.set_normalize(s.normalize)
      m.set_scale(s.scale)
      m.set_translation(s.translation_x, s.translation_y, s.translation_z)
      m.set_decimate(s.decimate)
      m.set_arrow_shaftwidth(s.shaftwidth)
      m.preliminary_scan()
      s.max_abs_coords = m.get_max_abs_coords(default=s.max_abs_coords)
      s.min_arrow_width = m.min_arrow_width(default=s.min_arrow_width)
      field_list.append(m)
      s.reset()

except SystemExit:
  sys.exit(0)

except ValueError:
  print "You gave a bad argument to the option!"
  sys.exit(1)

except IOError:
  print "IO problem (does the file exist?)"
  sys.exit(1)

if s.axis:
  print "Drawing the axis of the reference system"
  axis_width = s.min_arrow_width
  axis_colx = visual.color.red
  axis_coly = visual.color.green
  axis_colz = visual.color.blue
  lx, ly, lz = s.max_abs_coords
  x_versor = (lx, 0, 0)
  y_versor = (0, ly, 0)
  z_versor = (0, 0, lz)
  my_arrow(color=axis_colx, pos=(0,0,0), axis=x_versor, shaftwidth=axis_width, proportion = False)
  my_arrow(color=axis_coly, pos=(0,0,0), axis=y_versor, shaftwidth=axis_width, proportion = False)
  my_arrow(color=axis_colz, pos=(0,0,0), axis=z_versor, shaftwidth=axis_width, proportion = False)
  if s.label:
    visual.label(pos=x_versor, text="x axis", xoffset=5, yoffset=5)
    visual.label(pos=y_versor, text="y axis", xoffset=5, yoffset=5)
    visual.label(pos=z_versor, text="z axis", xoffset=5, yoffset=5)

visual.scene.autoscale = s.auto_zoom

print "Waiting %d seconds" % s.wait_before_visualization
time.sleep(s.wait_before_visualization)

print "Starting the visualization"
while True:
  if s.sec_per_frame > 0.0: time.sleep(s.sec_per_frame)

  # Find the nearest next time-step
  field_to_evolve = field_list[0]
  t_nearest = field_to_evolve.get_time()
  t_max = -1.0
  for field in field_list:
    t = field.get_time()
    if t < t_nearest and t != -1:
      t_nearest = t
      field_to_evolve = field
    if t > t_max:
      t_max = t

  if t_max == -1: break

  field_to_evolve.read_again()
  print "Time: %f, '%s' evolves. its norm is %f" % \
    (t_nearest, field_to_evolve.get_name(), field_to_evolve.maxvec_norm)

print "End of visualization"

