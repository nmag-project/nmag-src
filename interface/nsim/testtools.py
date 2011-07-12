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

