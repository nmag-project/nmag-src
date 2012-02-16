"""This file implements the logic to perform micromagnetic simulations
spanning some given parameters. We make this clear with an example:
suppose you want to simulate a process for different sizes of the system.
This script allows to automatically generate the mesh (with NetGen)
and to execute all the simulations independently on different
directories. The simulations can then be executed on a cluster,
without interferences.
"""

import os, sys, math, commands

def default_logger(msg):
  print msg

class ParametricRun:
    def __init__(self,
                 directory_from_state,
                 mesh_from_state,
                 mesh_name_from_state,
                 is_already_done,
                 states,
                 default_state={},
                 mesh_directory="mesh",
                 logger=default_logger,
                 write_host_file=True):
        self.directory_from_state = directory_from_state
        self.mesh_from_state = mesh_from_state
        self.mesh_name_from_state = mesh_name_from_state
        self.mesh_directory = mesh_directory
        self.logger = logger
        # First we check if all the required meshes have been generated
        if not os.path.isdir(mesh_directory):
            os.mkdir(mesh_directory)
            self.logger("Created mesh directory")

        required_meshes = {}
        for partial_state in states:
            state = default_state.copy()
            state.update(partial_state)
            mn = mesh_name_from_state(state)
            if not os.path.exists(os.path.join(mesh_directory, mn)):
                required_meshes[mn] = state
        self._required_meshes = required_meshes

        if len(required_meshes) != 0:
            self.logger("On this run only the meshes will be created!")
            os.chdir(mesh_directory)
            for mn in required_meshes:
                state = default_state.copy()
                state.update(required_meshes[mn])
                self.logger("Creating mesh file '%s'" % mn)
                mesh_from_state(state, mn)
            sys.exit(0)

        all_done = True
        for partial_state in states:
            state = default_state.copy()
            state.update(partial_state)
            dn = directory_from_state(state)
            cwd = os.getcwd()
            self._directory_name = dn
            if not os.path.isdir(dn):
                os.makedirs(dn)
                self.logger("Created directory '%s': " % dn)
            os.chdir(dn)
            if not is_already_done(self, state):
                all_done = False
                self._this_state = state
                break
            os.chdir(cwd)

        if all_done: sys.exit(1)

        if write_host_file:
            try:
                f = open("host", "w")
                f.write(commands.getoutput("hostname"))
                f.close()

            except:
                pass

    def get_directory_name(self):
        return self._directory_name

    def get_state(self):
        return self._this_state

    def get_mesh_file_name(self):
        mfn = self.mesh_name_from_state(self._this_state)
        mfn = os.path.join(self.mesh_directory, mfn)
        return os.path.join("..", mfn)
