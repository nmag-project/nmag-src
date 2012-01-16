'''
Module which defines the SimulationCore class, an abstract class from which
the real simulation objects are derived (by inheritance).
Such a design has the goal of separating the parts of the Simulation object
which depend on the specific discretisation (FD, FE) from the parts which
do not depend on it (such as the hysteresis logic).
Here is the structure we have in mind:

                  /----> FDSimulation ----\
SimulationCore ---|                       |---> Simulation
                  \----> FESimulation ----/

NOTE: Derived from the old Simulation class in nsim/interface/nmag/main.py
which was joint work of all the Nmag developers. See the file AUTHORS.

Copyright |copy| 2009

:Author: M Franchin, M Najafi

:License: GNU Public License (GPL)
'''

import os, sys, time, logging

from nmag_exceptions import *

import nsim
from nsim.si_units import SI
from nsim.colwriter import ColDescriptor, ColWriter
import hysteresis as hysteresis_m
import nsim.setup
from clock import SimulationClock

features, _ = nsim.setup.get_features()

log = logging.getLogger('nmag')


class Quantity(object):
    '''A quantity is a description of any thing that may be saved inside
    a ndt file. It may just represent an integer-floating point number
    or a vector field. The Quantity class is a descriptor for such a thing.
    It contains details about the corresponding type, SI unit, the id name,
    the signature (indices and per-material).'''

    def __init__(self, name, type, units, signature=None, context=None):
        '''name is the ID name for the quantity. type is the type of the
        quantity (int, float, date, field, pfield). unit is an SI object
        representing the unit of the data. signature is useful only for
        fields. It can be '_?' for fields depending on the materials,
        such as E_demag_Py (? represent Py, the material) or it can be
        '_?_*' for per-material fields which have also components (such
        as m_Py_0).'''

        self.name = name
        self.type = type
        self.units = units
        self.signature = signature
        if signature == None:
            self.signature = name
        self.parent = None
        self.context = context

    def sub_quantity(self, name):
        q = Quantity(name=name, type=self.type,
                     units=self.units, signature=self.signature)
        q.parent = self
        return q

# The following table contains all the quantities which may be saved to file.
known_quantities = [
  #                      name      type         unit signature context
  Quantity(             'id',    'int',       SI(1),   None),
  Quantity(           'step',    'int',       SI(1),   None),
  Quantity(     'stage_step',    'int',       SI(1),   None),
  Quantity(          'stage',    'int',       SI(1),   None),
  Quantity(   'last_time_dt',  'float',     SI('s'),   None),
  Quantity(           'time',  'float',     SI('s'),   None),
  Quantity(     'stage_time',  'float',     SI('s'),   None),
  Quantity(      'real_time',  'float',     SI('s'),   None),
  Quantity(       'unixtime',  'float',     SI('s'),   None),
  Quantity(       'maxangle',  'float',       SI(1),   None),
  Quantity(      'localtime',   'date',        None,   None),
  Quantity(        'H_total',  'field',   SI('A/m'), '_?_*'),
  Quantity(              'M',  'field',   SI('A/m'), '_?_*'),
  Quantity(              'm', 'pfield',       SI(1), '_?_*'),
  Quantity(            'pin', 'pfield',       SI(1),   None),
  Quantity('current_density', 'pfield', SI('A/m^2'),   '_*',  'stt'),
  Quantity(           'dmdt',  'field',   SI('1/s'), '_?_*'),
  Quantity(    'dm_dcurrent',  'field', SI('1/s'), '_?_*',  'stt'),
  Quantity(          'H_ext',  'field',   SI('A/m'),   '_*'),
  Quantity(         'H_anis',  'field',   SI('A/m'), '_?_*',  'anis'),
  Quantity(         'H_exch',  'field',   SI('A/m'), '_?_*',  'exch'),
  Quantity(        'H_demag',  'field',   SI('A/m'),   '_*',  'demag'),
  Quantity(        'E_total',  'field', SI('J/m^3'),   '_?'),
  Quantity(          'E_ext',  'field', SI('J/m^3'),   '_?'),
  Quantity(         'E_anis',  'field', SI('J/m^3'),   '_?',  'anis'),
  Quantity(         'E_exch',  'field', SI('J/m^3'),   '_?',  'exch'),
  Quantity(        'E_demag',  'field', SI('J/m^3'),   '_?',  'demag'),
  Quantity(            'phi',  'field',     SI('A'),   None,  'demag'),
  Quantity(            'rho',  'field', SI('A/m^2'),   None,  'demag')
]

known_quantities_by_name = dict([(q.name, q) for q in known_quantities])
known_field_quantities = [q for q in known_quantities
                            if q.type in ('field', 'pfield')]

def not_implemented(fn_name, cl_name):
    raise NotImplementedError("%s is not implemented by %s"
                              % (fn_name, cl_name))


class SimulationCore(object):
    def __init__(self, name=None, do_demag=True,
                 id="Generic Simulation class"):
        self.class_id = id       # String identifying the kind of Simulation
                                 # class
        self.units = None        # Simulation units used by this class
        self.do_demag = do_demag # Whether we should include the demag field

        # List of all the materials used by the Simulation object
        self.materials = None

        # Dictionary used by the hysteresis method to find abbreviations for
        # frequently used things to save or do.
        # Example: for ``sim.hysteresis(..., save=[('averages', at(...))])``
        # the string 'averages' needs to be a key in this dictionary.
        # The corresponding value is the function to call.
        self.action_abbreviations = {}

        # Every quantity the user may want to save needs to be listed here
        self.known_quantities = known_quantities
        self.known_quantities_by_name = known_quantities_by_name
        self.known_field_quantities = known_field_quantities

        ### Set the simulation name
        if name == None:
            self.name = features.get('etc', 'runid')
        else:
            self.name = name
        log.info("Simulation(name=%s) object created" % self.name)

        ### Check whether the files we are going to write do already exist.
        # if this is the case we should stop, unless the --clean option was
        # given: we do not want to overwrite data as a default!
        self._restarting = False
        data_filenames = [self._ndtfilename(), self._h5filename(),
                          self._tolfilename()]
        if features.get('nmag', 'clean', raw=True):
            # Existing data files should be deleted
            nsim.snippets.rename_old_files(data_filenames)

        elif features.get('nmag', 'restart', raw=True):
            log.info("Starting simulation in restart mode...")
            self._restarting = True

        else:
            # Check that no data files exist
            for filename in data_filenames:
                if os.path.exists(filename):
                    msg = ("Error: Found old file %s -- cannot proceed. "
                           "To start a simulation script with old data "
                           "files present you either need to use '--clean' "
                           "(and then the old files will be deleted), "
                           "or use '--restart' in which case the run "
                           "will be continued." % filename)
                    raise NmagUserError(msg)

        # See documentation for SimulationClock object
        self.clock = SimulationClock()

        # The advance_time method does not allow to carry on the simulation
        # up to t = infinite. Sometimes we want to simulate for n steps,
        # without any time limits. However we must give a time limit.
        # This is then how we approximate t = infinite.
        # For now, we do not provide any function to set or change it.
        # The user should just use:
        #   sim = Simulation()
        #   sim.max_time_reached = SI(1000, "s")
        self.max_time_reached = SI(1, "s")

        # Add abbreviations so that things can be saved just by giving
        # corresponding ID strings.
        # Example: hysteresis(..., save=[('averages', ...)])
        self.add_save_abbrev('save_averages',
                             lambda sim: sim.save_data(avoid_same_step=True))
        self.add_save_abbrev('save_fields',
                             lambda sim: sim.save_data(fields='all',
                                                       avoid_same_step=True))
        self.add_save_abbrev('save_field_m',
                             lambda sim: sim.save_data(fields=['m'],
                                                       avoid_same_step=True))
        self.add_save_abbrev('save_restart',
                             lambda sim: sim.save_restart_file())
        self.add_do_abbrev('do_next_stage',
                           SimulationCore.hysteresis_next_stage)
        self.add_do_abbrev('do_exit', SimulationCore.hysteresis_exit)

        # Used to write the ndt file
        self._ndt_writer = ColWriter(out=self._ndtfilename())
        self._ndt_writer.set_formats([( 'float', '% 25.13g'),
                                      (   'int', '% 25d'),
                                      (  'date', '% 25s'),
                                      ('pfield', '% 25.13g'),
                                      ( 'field', '% 25.13g')])

        # The following list contains a description of the physics components
        # which are included in the physical model For example,
        # ["exch", "demag"] indicates that exchange and demag are included.
        # In this case, spin transfer torque is not. This information
        # is used to understand which fields are relevant and which are not
        # (so that we do not save empty fields). Following the previous
        # example, dm_dcurrent, current_density won't be saved.
        self._components = None

    def _get_id(self): return self.clock.id
    def _get_stage(self): return self.clock.stage
    def _get_step(self): return self.clock.step
    def _get_time(self): return self.clock.time
    def _get_stage_step(self): return self.clock.stage_step
    def _get_stage_time(self): return self.clock.stage_time
    def _get_real_time(self): return self.clock.real_time

    id = property(_get_id, doc="ID.")
    stage = property(_get_stage, doc="Stage number.")
    step = property(_get_step, doc="Global step number (always increases).")
    time = property(_get_time, doc="Global time reached (always increases).")
    stage_step = property(_get_stage_step,
                           doc="Step number counted from the beginning "
                               "of the current stage.")
    stage_time = property(_get_stage_time,
                          doc="Time reached counted from the beginning "
                              "of the current stage.")
    real_time = property(_get_real_time,
                         doc="Time passed in the 'real' world.")

    def get_components(self):
        if self._components != None:
            return self._components

        else:
            components = ["exch"]
            if self.do_demag:
                components.append("demag")
            self._components = components
            return components

    components = property(get_components, doc="Get the physical components "
                                              "included in the model.")

    def get_all_field_names(self):
        return [q.name for q in self.known_field_quantities
                if q.context == None
                or q.context in self.components]

    def hysteresis_next_stage(sim):
        """Terminate the current stage of the hysteresis computation
        and start the next one. This function is intended to be called
        by one of the functions passed to ``hysteresis`` using the ``save``
        or ``do`` optional arguments).
        """
        sim.clock.stage_end = True

    def hysteresis_exit(sim):
        """Exit from the running hysteresis computation. This function can be
        used to exit from an hysteresis loop computation and is intended to be
        called by one of the functions passed to ``hysteresis`` using the
        ``save`` or ``do`` optional arguments).
        """
        sim.clock.exit_hysteresis = True
        sim.clock.stage_end = True

    # The methods 'hysteresis', 'relax', 'hysteresis_next_stage' and
    # 'hysteresis_exit' of the class 'Simulation' are defined inside a separate
    # module: this helps to keep the sources cleaner.
    relax = hysteresis_m.simulation_relax

    hysteresis = hysteresis_m.simulation_hysteresis

    hysteresis.__argdoclong__ = \
      hysteresis_m.simulation_hysteresis.__argdoclong__


    def add_action_abbrev(self, abbreviation, function, prefix=None):
        if prefix == None:
            self.action_abbreviations[abbreviation] = function
            return

        else:
            valid_prefixes = ["save", "do"]
            if not (prefix in valid_prefixes):
                raise NmagUserError("Valid prefixes for action abbreviations "
                                    "are %s, you gave '%s'!"
                                    % (valid_prefixes, prefix))

            if abbreviation.startswith(prefix):
                self.action_abbreviations[abbreviation] = function
            else:
                full_abbreviation = prefix + '_' + abbreviation
                self.action_abbreviations[full_abbreviation] = function

    def add_save_abbrev(self, abbreviation, function):
        '''Add an abbreviation to be used in the 'save' argument of the
        hysteresis method. For example, if you use the following:

            def funky_function(sim): print "Hello, I'm Funky!"
            sim.add_save_abbrev('funky', funky_function)

        Then you can call:

            sim.hysteresis(Hs, save=[('funky', at('convergence'))])

        and this will be equivalent to:

            sim.hysteresis(Hs, save=[(funky_function, at('convergence')])
        '''
        self.add_action_abbrev(abbreviation, function, prefix='save')

    def add_do_abbrev(self, abbreviation, function):
        '''Similar to add_save_abbrev, but affects the 'do' argument of the
        hysteresis method.'''
        self.add_action_abbrev(abbreviation, function, prefix='do')

    def do_next_stage(self, stage=None):
        self.clock.inc_stage(stage=stage)

    def _get_filename(self, ext):
        basename = self.name + ext
        return nsim.snippets.output_file_location(basename)

    def _ndtfilename(self):
        return self._get_filename("_dat.ndt")

    def _h5filename(self):
        return self._get_filename("_dat.h5")

    def _statfilename(self):
        return self._get_filename("_cvode.log")

    def _tolfilename(self):
        return self._get_filename("_tol.log")

    def get_restart_file_name(self):
        """Return the default name for the restart file."""
        return self.name + "_restart.h5"

    def is_converged(self):
        """
        Returns True when convergence has been reached.
        """
        return self.clock.convergence

    def get_materials_of_field(self, field_name):
        '''Returns all the materials for which the field with the given name
        is defined. Returns None if the field is not a per-material defined
        field.'''

        quantity = self.known_quantities_by_name[field_name]
        if '?' in quantity.signature:
            return self.materials
        else:
            return None

    def get_ndt_columns(self):
        '''This function returns the data that normally should go into the NDT
        file together with a corresponding description (a Quantity object).
        The function returns a pair (columns, quantities), where:

        - columns is a list of pairs (data_name, data_value), where data_name
          is the name of the data (such as 'time' or 'M_Py_0'), while
          data_value is the corresponding value including the units, as an SI
          object, if possible. such as SI(1e6, 'A/m').

        - quantities is a list having the same size of ``columns``.
          quantities[i] is a Quantity object describing the entry columns[i].
        '''
        # These quantities are not dependent on the discretisation
        lt = time.localtime()
        lt_str = ("%04d/%02d/%02d-%02d:%02d:%02d"
                  % (lt[0], lt[1], lt[2], lt[3], lt[4], lt[5]))

        columns = [('id', self.id),
                   ('step', self.step),
                   ('stage_step', self.stage_step),
                   ('stage', self.stage),
                   ('time', self.time),
                   ('stage_time', self.stage_time),
                   ('real_time', self.real_time),
                   ('unixtime', SI(time.time(), 's')),
                   ('localtime', lt_str)]

        quantities = [self.known_quantities_by_name[name]
                      for name, _ in columns]

        # Now we need to add the averages of all fields

        # This function appends to columns the averages for each component
        # of the specified subfield.
        def process_subfield(field_name, prefix, quantity, mat_name=None):
            if True:
                avg = self.get_subfield_average(field_name, mat_name)
                if avg == None:
                    return
            else: # except:
                return

            if type(avg) == list:
                for i, comp_value in enumerate(avg):
                    comp_name = "%s_%s" % (prefix, i)
                    columns.append((comp_name, comp_value))
                    quantities.append(quantity.sub_quantity(comp_name))

            else:
                columns.append((prefix, avg))
                quantities.append(quantity.sub_quantity(prefix))

        # Loop over all the fields and add averages for all their components
        for quantity in self.known_quantities:
            field_name = quantity.name
            if quantity.type in ['field', 'pfield']:
                if '?' in quantity.signature:
                    for material in self.get_materials_of_field(field_name):
                        prefix = "%s_%s" % (field_name, material.name)
                        process_subfield(field_name, prefix, quantity,
                                         mat_name=material.name)
                else:
                    process_subfield(field_name, field_name, quantity)

        return (columns, quantities)

    def _save_fields(self, filename=None, fieldnames=[]):
        self._raise_not_implemented("load_m_from_file")

    def _save_averages(self, fields=None, avoid_same_step=False):
        '''Save the averages of all available fields into the NDT file.
        If fields is a list of fields name, then these fields will be saved
        into the h5 file. If fields == 'all', then all the available fields
        will be saved in the h5 file.'''

        # Get the data
        columns, quantities = self.get_ndt_columns()

        # Define the columns available for output in the _ndt_writer object
        # (if this wasn't done before)
        if not self._ndt_writer.has_columns():
            # Check that all columns correspond to known quantities
            for quantity in quantities:
                if not (quantity in self.known_quantities
                        or quantity.parent in self.known_quantities):
                    raise NmagInternalError("The quantity '%s' is not listed "
                                            "as a known quantity and cannot "
                                            "be saved!" % col_name)

            # Define a column for every available data: note that the first
            # write determines which columns are available and which are not!
            # (this means that a column cannot become available later)
            for kq in self.known_quantities:
                selected = [q for q in quantities
                              if q == kq or q.parent == kq]
                for quantity in selected:
                    units = quantity.units
                    units_str = None
                    if units != None:
                        units_str = units.dens_str()
                    column_desc = ColDescriptor(name=quantity.name,
                                                type=quantity.type,
                                                units=units,
                                                units_str=units_str)
                    self._ndt_writer.define_column(column_desc)

        # Write finally the data to file
        self._ndt_writer.write_row(columns)

    def save_data(self, fields=None, avoid_same_step=False):
        """
        Save the *averages* of all defined (subfields) into a ascii
        data file. The filename is composed of the simulation name
        and the extension ``_dat.ndt``. The
        extension ``ndt`` stands for Nmag Data Table (analog to OOMMFs
        ``.odt`` extension for this kind of data file.

        If ``fields`` is provided, then it will also save the spatially
        resolved fields to a file with extensions ``_dat.h5``.

        :Parameters:
          `fields` : None, 'all' or list of fieldnames

            If None, then only spatially averaged data is saved into ``*ndt``
            and ``*h5`` files.

            If ``all`` (i.e. the string containing 'all'), then all fields are
            saved.

            If a list of fieldnames is given, then only the selected
            fieldnames will be saved (i.e. ['m', 'H_demag']).

          `avoid_same_step` : bool

            If ``True``, then the data will only be saved if the
            current ``clock.step`` counter is different from the
            step counter of the last saved configuration. If
            ``False``, then the data will be saved in any
            case. Default is ```False```. This is internally used by
            the hysteresis command (which uses ``avoid_same_step ==
            True``) to avoid saving the same data twice.

            The only situation where the step counter may not have
            changed from the last saved configuration is if the user
            is modifying the magnetisation or external field manually
            (otherwise the call of the time integrator to advance or
            relax the system will automatically increase the step
            counter).

        """
        self._save_averages(fields=fields, avoid_same_step=avoid_same_step)

    def _raise_not_implemented(self, fn_name):
        #raise NotImplementedError("%s is not implemented by %s"
        #                          % (fn_name, self.class_id))
        print "%s is not implemented by %s" % (fn_name, self.class_id)

    def save_mesh(self, filename):
        self._raise_not_implemented("save_mesh")

    def load_mesh(self, filename, region_names_and_mag_mats, unit_length,
                  do_reorder=False, manual_distribution=None):
        self._raise_not_implemented("load_mesh")

    def create_mesh(self, cell_nums, cell_sizes, materials,
                    regions=None, origin=(0.0, 0.0, 0.0),):
        """Specify the FD mesh to use.

        - ``cell_nums`` if a list containing the number of cells per each
          dimension.

        - ``cell_sizes`` is a list the sizes of the cells for each dimension
          (the FD mesh is made by cells having all the same size).

        - ``materials`` is the material (in case of single material
          simulation) or a list of tuples (region, material), in case of
          multi-material simulation.

        - ``regions`` is a function which takes the coordinates of a point as
          input and returns the region to which the point belongs to
          (a string). If ``regions`` is not given, then single-material
          simulation is assumed and the region in which the magnetisation
          is defined is assumed to be the whole mesh space (a cubic sample
          is simulated).

        - ``origin`` is the coordinate of the first cell, all the other cells
          occupy the cubic region of space whose corners are ``origin`` itself
          and ``origin[i] + cell_size[i]*cell_nums[i]`` for running index i.
        """
        self._raise_not_implemented("create_mesh")

    def set_params(self, stopping_dm_dt=None,
                   ts_rel_tol=None, ts_abs_tol=None):
        """
        Set the parameters which control the accuracy and performance
        of the simulation.

        :Parameters:

          `ts_rel_tol` : float
            the relative error tolerance (default is 1e-6) for the timestepper

          `ts_abs_tol` : float
            the absolute error tolerance (default is 1e-6) for the timestepper

          `stopping_dm_dt` : SI_ object
            the value used in the hysteresis_ and relax_ functions to decide
            whether convergence has been reached. If the largest value for dm/dt drops
            below ``stopping_dm_dt``, then convergence has been reached.

            The default value for ``stopping_dm_dt`` this is that the
            magnetisation changes less than one degree per nanosecond,
            i.e. ``stopping_dm_dt = SI(17453292.519943293,['s',-1])``.

          `exact_tstop` : bool
            the value of exact_tstop which is used by the advance_time method
            when the optional argument is not given. This is also the value
            used by the relax and hysteresis methods. See the documentation
            of advance_time for further details.

        Note that this command has to be issued *after* having created
        an m-field with the set_m_ command.
        """
        self._raise_not_implemented("set_params")

    def reinitialise(self, initial_time=None):
        self._raise_not_implemented("reinitialise")

    def set_local_magnetic_coupling(self, mat1, mat2, coupling):
        self._raise_not_implemented("set_local_magnetic_coupling")

    def set_H_ext(self, values, unit=None):
        self._raise_not_implemented("set_H_ext")

    def set_m(self, values, subfieldname=None):
        self._raise_not_implemented("set_m")

    def set_pinning(self, values):
        self._raise_not_implemented("set_pinning")

    def set_current_density(self, values, unit=None):
        self._raise_not_implemented("set_current_density")

    def advance_time(self, target_time, max_it=-1, exact_tstop=None):
        """
        This method carries out the time integration of the
        Landau-Lifshitz and Gilbert equation.

        :Parameters:

          `target_time` : SI Object
            The simulation will run until this time is reached. If the target_time is
            zero, this will simply update all fields.

          `max_it` : integer
            The maximum number of iterations (steps) to be carried out
            in this time integration call. If set to ``-1``, then there is no limit.

          `exact_tstop` : boolean
            When exact_tstop is True, the time integration is advanced exactly
            up to the given target_time. When False, the time integration ends
            "close" to the target_time. The latter option can result in better
            performance, since the time integrator is free to choose time
            steps which are as wide as possible. When exact_tstop is not
            given, or is None, the default value for this option will be used.
            The default value can be set using the method set_params, which
            should hence be used to control the behaviour of the hysteresis
            and relax methods.
        """
        self._raise_not_implemented("advance_time")

    def save_restart_file(self, filename=None, fieldnames=['m'], all=False):
        self._raise_not_implemented("save_restart_file")

    def save_m_to_file(self, filename, format=None):
        '''Save the magnetisation to file, so that it can be reloaded later
        with the method load_m_from_file. ``format`` is a string specifying
        the format to be used. For example, ``format='h5'`` will produce an h5
        file, while ``format='omf'`` will produce an OMF file (OOMMF).
        If format is not given, then the file format will be deduced
        from the extension or, if no extension is used, the predefined file
        format will be used.
        '''
        self._raise_not_implemented("save_m_to_file")

    def load_m_from_file(self, filename, format=None):
        '''Load a magnetisation configuration stored in the file with the
        given name. Read the documentation of the method ``save_m_to_file``
        for more information.'''
        self._raise_not_implemented("load_m_from_file")

    def probe_subfield(self,subfieldname,pos,unit=None):
        """for a given subfield name and position (SI object), return data (as SI object).

        Note that ``get_subfield_siv`` has the same functionality but
        takes a list of floats for the position (instead of an SI
        object) and returns (a list of) float(s) which is just the
        :ref:`SI-value <SI object>` of that physical entity.

        If the subfield is not defined at that part of space, ``None``
        is returned.

        If the subfield does generally not exist, then a ``KeyError`` exception
        is thrown.

        :Parameters:
          `subfieldname` : string
            The name of the subfield

          `pos` : SI object
            The position for which the data should be returned

          `unit` : SI object
            If you request the value for a subfield of a field that is
            part of |nmag| (i.e. fields M, m, H_demag, etc), then you do
            not need to provide this object.

            If you request data of any other (multi-physics) fields,
            then this function needs to know the SI dimensions of that
            field (for the correct conversion from simulation units to
            SI units).

            If incorrect dimensions are provided, the returned data is
            likely to be wrongly scaled.

        :Returns:
          `data` : [list [of list[ of ...]]] SI objects
            The returned object is an SI object for scalar subfields,
            a list of SI objects for vector fields, a list of list of
            SI objects for (rank 2) tensor fields, etc.

        """
        self._raise_not_implemented("probe_subfield")

    def probe_subfield_siv(self, subfieldname, pos, unit=None):
        """
        The same behaviour as ``get_subfield`` but the ``pos`` and return
        data are :ref:`SI-value <SI object>`\ s (not SI objects).

        If the subfield is not defined at that part of space, ``None``
        is returned.

        If the subfield does generally not exist, then a ``KeyError``
        exception is thrown.

        The input (position) and returned data is expressed in SI
        units but of type float.

        :Parameters:
          `subfieldname` : string
            The name of the subfield

          `pos` : list of floats
            The position for which the data should be returned (in meters)

          `unit` : SI object
            If you request the value for a subfield of a field that is part of nmag (i.e.
            fields M, m, H_demag, etc), then you do not need to provide this object.

            If you request data of any other (multi-physics) fields,
            then this function needs to know the SI dimensions of that
            field (for the correct conversion from simulation units to
            SI units).

            If incorrect dimensions are provided, the returned data is
            likely to be wrongly scaled.

        :Returns:
          `data` : [list [of list[ of ...]]] float
            The returned object is a float for scalar subfields,
            a list of floats for vector fields, a list of list of
            floats for (rank 2) tensor fields, etc.

        """
        self._raise_not_implemented("probe_subfield_siv")

    def get_subfield(self, subfieldname, units=None):
        """
        Given a subfieldname, this will return a numpy-array
        containing all the data (one element for each site).

        :Parameters:
          `subfieldname` : string
            The name of the subfield, for example ``m_Py`` or ``H_demag``.

          `units` : SI object

            Optional parameter. If it is provided, then the entity is
            expressed in these units. If it is not provided, then the
            correct SI dimensions for this subfield are looked up, and
            :ref:`SI-value <SI object>`\ s are returned.

            If you would like to see simulation units in the output,
            then use ``SI(1)``.

            In short: if you omit the second parameter, you will
            obtain SI values.

        :Returns:
          `data` : numpy-array

        """
        self._raise_not_implemented("get_subfield")
