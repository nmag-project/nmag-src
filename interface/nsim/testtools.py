# Nmag micromagnetic simulator
# Copyright (C) 2011 University of Southampton
# Hans Fangohr, Thomas Fischbacher, Matteo Franchin and others
#
# WEB:     http://nmag.soton.ac.uk
# CONTACT: nmag@soton.ac.uk
#
# AUTHOR(S) OF THIS FILE: Matteo Franchin
# LICENSE: GNU General Public License 2.0
#          (see <http://www.gnu.org/licenses/>)

"""Provides tools needed in the test suite."""

import os
import sys

def find_make_exec(envvar="MAKE"):
  """Find the executable for the program 'make' by first inspecting
  the environment variable 'MAKE' and making guesses if this is not defined.
  """
  make_exec_env = os.getenv(envvar)
  return (make_exec_env if make_exec_env else "make")

def run_make(file_in_dir, make=None):
  """Run make inside the directory which contains the file 'file_in_dir'."""
  make_exec = make or find_make_exec()
  makefile_dir = os.path.split(file_in_dir)[0] or "."
  exit_status = run_in_dir(makefile_dir, make_exec)
  if exit_status != 0:
    raise ValueError("%s failed with exit status %d" % (make, exit_status))

def run_in_dir(directory, command):
  """Run (using os.system) the command given in 'command' inside the directory
  'directory'. Return the exit status of the command."""

  original_dir = os.getcwd()
  try:
    os.chdir(directory)
  except:
    raise ValueError("Cannot chdir into the directory '%s'" % directory)
  exit_status = os.system(command)
  os.chdir(original_dir)
  return exit_status

def get_test_mesh(size=None, dim=None, shape=None, regions=1, periodic=None):
  """Return the full path to an existing mesh with the required
  characteristics, or raise an exception if such a mesh doesn't exist.

  :Parameters:
    `size` : string
      One of the strings: "small": just few nodes (very small mesh),
      "medium": mesh big enough to make computation of things like the demag
      field meaningful, but which still will lead to fast simulations,
      "big": a mesh which will presumably lead to simulations whose setup
      phase may take one minute or more.

    `dim` : int
      The dimension of the mesh (1, 2 or 3).

    `shape` : string
      The shape of the mesh. Supported shapes are: "ellipsoid".

    `regions` : int
      Number of regions in the mesh.

    `periodic` : bool
      Whether the mesh should have periodic points (suitable for PBC
      simualtions).
  """
  test_path_env = "NSIM_TEST_PATH"
  test_path = os.getenv(test_path_env)
  if test_path == None:
    raise StandardError("The environment variable %s is not set."
                        % test_path_env)

  mesh_path = os.path.join(test_path, "mesh")
  if not os.path.exists(mesh_path):
    raise StandardError("The directory '%s' containing the test meshes does "
                        "not exist:" % mesh_path)
  size_str = {None: "s", "small": "s", "medium": "m", "large": "l"}[size]
  shape_str = {None: "ellipsoid", "ellipsoid": "ellipsoid"}[shape]
  dim_str = {None: "3", 1: "1", 2: "2", 3: "3"}[dim]
  pbc_str = {None: "n", False: "n", True: "p"}[periodic]
  nreg_str = {None: "1", 1: "1"}[regions]
  mesh_filename = (size_str + dim_str + pbc_str + nreg_str + shape_str
                   + ".nmesh.h5")
  full_path = os.path.join(mesh_path, mesh_filename)

  if os.path.exists(full_path):
    return full_path
  else:
    raise StandardError("Cannot find a test mesh with the desired features "
                        "('%s' does not exist)." % full_path)

