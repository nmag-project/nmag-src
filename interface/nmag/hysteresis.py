# Copyright (C) 2009 Matteo Franchin
# School of Engineering Sciences
# University of Southampton
# 
# This file is part of Nmag.

'''
This file contains the implementation of the hysteresis method
of the Simulation class.
'''

import types, re, time, sys

import logging
log = logging.getLogger('nmag')

from nsim.when import every, at
from nsim.si_units import SI, str_of_SI_vector
from nsim.reporttools import true_at_most_every_n_seconds

from nmag_exceptions import *

_subsequent_spaces = re.compile('[ \t_]+')

def _repr_of_clock(clock):
    """Return a nice string representation of the clock object."""
    clockstr = ""
    for key, value in clock.items():
        clockstr += "\n\thyst-info: %25s : %s" % (key, value)
    return clockstr

def _update_progress_file(self, H_ext, progress_file_name,
                          progress_message_minimum_delay):
    if true_at_most_every_n_seconds('hysteresis_reporting',
                                    progress_message_minimum_delay):
        tmpf = open(progress_file_name, 'w')
        tmpf.write(time.asctime() + '\n')
        for k, v in self.clock.items():
            tmpf.write("%20s : %s\n" % (k, v))
        tmpf.write("\n")
        tmpf.write(self.convergence.get_log())
        tmpf.close()

        log.info("it %d, time %s; stage %d; H_ext=%s"
                 % (self.clock['step'],
                    self.clock['time_reached_si'].dens_str(),
                    self.clock['stage'],
                    str_of_SI_vector(H_ext)))

def _string_normalise(s, lower=True, spaces='_'):
    """Used by `_append_x_list` to obtain a standard string format.
       Examples:

       'save  fields' --> 'save_fields'
       'Save_Fields'  --> 'save_fields'
    """
    ns = s
    if lower: ns = s.lower()
    if spaces != None:
        ns = re.sub(_subsequent_spaces, spaces, ns)
    return ns

# The following lines extend the syntax of the 'save' list:
# (thing, when) is the usual syntax: now we allow also
# (thing1, thing2, ..., when)
def _append_x_list(target_list, input_list, have_already, prefix="",
                   predefined_actions={}):
    """Internally used by the function `_join_save_and_do_lists`.
       See that function for further info.
    """
    try:
        for tuple_item in input_list:
            list_item = list(tuple_item)
            things = list_item[0:-1]
            when = list_item[-1]
            for thing in things:
                if type(thing) == types.FunctionType:
                    target_list.append((thing, when))
                    continue

                elif type(thing) == types.StringType:
                    normalised_thing = _string_normalise(prefix + " " + thing)
                    if predefined_actions.has_key(normalised_thing):
                        action = predefined_actions[normalised_thing]
                        target_list.append((action, when))
                        continue

                    else:
                        normalised_thing = _string_normalise(thing)
                        if predefined_actions.has_key(normalised_thing):
                            action = predefined_actions[normalised_thing]
                            target_list.append((action, when))
                            continue

                msg = ("Error in optional argument '%s' of method "
                       "'hysteresis': you want to %s '%s' "
                       "but I don't know how to do it. Hint: when specifying "
                       "what to %s you can use a string or a function. "
                       "Available strings are: %s."
                       % (prefix, prefix, thing, prefix,
                          ", ".join(predefined_actions.keys())))
                raise NmagUserError, msg

    except TypeError:
        msg = ("Bad syntax for argument '%s' of the method 'hysteresis': "
               "remember that you should provide a list of tuples "
               "(things_to_%s, when). You can also provide tuples "
               "with many things to save, such as (thing1, thing2, "
               "..., when)." % (prefix, prefix))
        raise NmagUserError(msg)

def _join_save_and_do_lists(save_list, do_list, predefined_actions={}):
    """This function takes the `save` and the `do` parameters
       of the `hysteresis` method and joins them in an unique list,
       where tags (strings such as "save averages" or "save fields")
       have been replaced with the appropriate functions.
       This function also checks that the provided tags actually exist.
    """
    joint_list = []
    have_already = {}
    # Note: it is important to keep the following order for the invocation
    #  of _append_x_list! First the do commands, then the save commands.
    #  This is necessary because among the do commands there can be
    #  a _next_stage call. If one inverts the order of execution, then
    #  things are first saved and then the _next_stage happens.
    #  In such a way the save routines would not know that we are dealing
    #  with the last step of this stage!
    _append_x_list(joint_list, do_list, have_already, prefix="do",
                   predefined_actions=predefined_actions)
    _append_x_list(joint_list, save_list, have_already, prefix="save",
                   predefined_actions=predefined_actions)

    #XXX Matteo, can we at this point order the save_ entries such that
    #the save_restart is the last? See my explanation in ticket:169.
    #Hans, 11/ll/2008

    
    return joint_list

def _next_deltas(event, clock, suggest=None, tols=None):
    """Return the next occurrence of the given event. In particular
    it returns a triple of:
    - number of steps to the next event;
    - amount of simulation time to the next event;
    - amount of real time to the next event
    """
    def delta(name):
        n = event.next_time(name, clock, tols=tols)
        if type(n) == bool: return None
        return n - clock[name]
    def minimum(ls):
        m = None
        for l in ls:
            if l != None:
                if m == None or l < m: m = l
        return m
    if suggest != None:
        delta_step, delta_time, delta_real_time = suggest
    else:
        delta_step, delta_time, delta_real_time = None, None, None
    delta_step = minimum([delta('step'), delta('stage_step'), delta_step])
    #print "minimum([%s, %s, %s])" % \
          #(str(delta('time')), str(delta('stage_time')), str(delta_time)),
    delta_time = minimum([delta('time'), delta('stage_time'), delta_time])
    #print " = %s" % str(delta_time)
    delta_real_time = minimum([delta('real_time'), delta_real_time])
    #x = (delta_step, delta_time, delta_real_time)
    #print "_next_deltas(%s, suggest=%s): return %s" % (event, str(suggest), str(x))
    return (delta_step, delta_time, delta_real_time)

def _next_time(event, clock, tols=None):
    """Similar to _next_deltas. Returns a triple made of three values:
    the 'step', 'time' and 'real_time' counters when the event
    is expected to occour.
    """
    # NOTE: first I was simply calling _next_deltas to implement
    # this function, but I had to change it because of unexpected
    # failures when comparing floating point numbers, see the note
    # inside the hysteresis method implementation.
    # (to solve the problem I introduced 'zero_stage_step'
    # and 'zero_stage_time' in the dictionary 'self.clock')
    def delta(name, starting_time=None):
        n = event.next_time(name, clock, tols=tols)
        if type(n) == bool: return None
        if starting_time != None:
            return n - clock[starting_time]
        else:
            return n
    def minimum(ls):
        m = None
        for l in ls:
            if l != None:
                if m == None or l < m: m = l
        return m
    next_time = minimum([delta('time', 'zero_stage_time'),
                         delta('stage_time')])
    #if delta_time != None:
        #next_time = clock['zero_stage_time'] + delta_time
    #else:
        #next_time = None

    next_step = minimum([delta('step', 'zero_stage_step'),
                         delta('stage_step')])
    #if delta_step != None:
        #next_step = clock['zero_stage_step'] + delta_step
    #else:
        #next_step = None

    next_real_time = delta('real_time')
    return (next_step, next_time, next_real_time)
    # NOTE: maybe it would be easier to express the concept
    # of "no next suggested value" with a zero (this would
    # help to reduce the checks for None)

def simulation_relax(self,
                     H_applied = None,
                     save=[('averages', 'fields', at('stage_end'))],
                     do = [],
                     convergence_check=every(5, 'step')):
    """
    This method carries out the time integration of the LLG until
    the system reaches a (metastable) equilibrium.
    Internally, this uses the hysteresis_ loop command.

    :Parameters:
      `H_applied` : list of SI objects
        For a 3-d simulation, the SI-objects Hx, Hy and Hz would be
        specified as ``[Hx,Hy,Hz]``.

        Default value is ``None``, resulting in the currently applied external
        field ``H_ext`` being used.

      `save` : Schedule object
        Allows to define what data to save at what events.
        See documentation on the hysteresis_ method and
        on the ``Schedule`` object.

      `convergence_check` : every object The default value
      (``every(5,'step')`` specifies that we ask the time
      integrator to carry out 5 steps before we check for
      convergence. If in doubt, ignore this feature.

    """
    log.debug("Entering 'relax'")
    fields = [H_applied]
    log.debug("Calling hysteresis(%s)" % str(fields))
    return self.hysteresis(fields,
                           save=save,
                           do=do,
                           convergence_check=convergence_check)

def simulation_hysteresis(self, H_ext_list,
                          save=[('averages', 'fields', at('stage_end'))],
                          do=[],
                          convergence_check=every(5, 'step'),
                          progress_message_minimum_delay=60):

    #Note we are using __argdoclong__ here (below this function).

    """
    This method executes a simulation where the applied field
    is set in sequence to the values specified in ``H_ext_list``.
    The time integration proceeds with the same applied field
    until convergence is reached. At this point the field is changed
    to the next one in ``H_ext_list`` and the method ``reinitialise()``
    is called to proceed with the simulation.
    The user can specify when to save data using the optional
    argument ``save``.

    This allows to carry out hysteresis loop computations
    and write the results to disk.

    Technically we say that this function performs a multi-stage
    simulation. In our terminology, a stage is a part of the simulation
    where the field does not change. Therefore, every value
    for the applied field specified in ``H_ext_list`` corresponds
    to a different stage. Stages are numbered starting from 1,
    which corresponds to ``H_ext_list[0]``. In general during
    stage number ``i`` the applied field is ``H_ext_list[i-1]``.

    :Parameters:
      `H_ext_list` : list of values for the applied field
        It is something like ``[H1, H2, H3, ...]``, where
        ``Hi`` is the triple of components of the applied field,
        i.e. SI objects having units of "A/m";

      `save` : list of pairs ``(thing_to_save, when)``
        ``thing_to_save`` is either a string or a function provided
        by the user and ``when`` is an instance of the class ``When``,
        i.e. an object which contains the specification of when
        "the thing" has to be saved.

        Possible string values for ``thing_to_save`` are:

          - ``"averages"``: to save the averages of all the fields
            together with other information (such as the stage number,
            the time reached, etc.). This is done calling the method
            ``save_data()``. Refer to its documentation
            for further details;
          - ``"fields"``: to save all the fields. The method
            ``save_data(fields='all')`` is called for this purpose;
          - ``"restart"``: to save the current magnetisation configuration
            and all the information needed to restart the simulation.

      `do` : list of pairs ``(thing_to_do, when)``
        is very similar to the ``save`` argument, but is usually used
        for other purposes.
        ``thing_to_do`` is either a string or a function provided
        by the user and ``when`` is an instance of the class ``When``.

        Possible string values for ``thing_to_do`` are:

          - ``"next_stage"``: induces the hysteresis method to advance
            to the next stage;
          - ``"exit"``: induces the hysteresis method to exit,
            even if the hysteresis computation has not still reached
            its end.

        The user can provide his own function to save data.
        For example, the following three lines::

          def my_fun(sim): 
            sim.save_data()
          sim.hysteresis(..., save=[(my_fun, every(10, 'step'))])

        are equivalent to::

          sim.hysteresis(..., save=[('averages', every(10, 'step'))])

        To specify when something has to be saved the module ``when``
        is used. The functions ``at`` and ``every``, provided by
        this module, can refer to the following time variables:

          - ``step``: the step number from the beginning of the simulation;
          - ``stage_step``: the step number from the beginning of
            the current stage;
          - ``time``: the simulation time passed from the beginning
            of the simulation (measured in SI_ objects);
          - ``stage_time``: the simulation time passed from the beginning
            of the current stage;
          - ``stage``: the number of the current stage;
          - ``convergence``: a boolean value which is ``True``
            if the convergence criterion is satisfied.
            Use in this way ``at('convergence')``

        Remember that you can combine time specifications using
        the operator | (or) and & (and)::

          every(2, 'stage') & at('convergence') --> only at convergence
                                                    of odd stages
          every(10, 'step') | at('convergence') --> at convergence
                                                    and every 10 steps.

        Some usage examples::

          # Save fields (which implicitly will save the averages as well)
          # when the magnetisation stops changing for each applied field
          # (i.e. save at convergence):
          sim.hysteresis(..., save=[('fields', at('convergence'))])

          # Averages will be saved every 10 steps, fields (and
          # implicitely averages) will be saved at convergence.
          sim.hysteresis(..., save=[('averages', every(10, 'step')),
                                    ('fields', at('convergence'))])

          # Each stage will not last more than 10 ps, even
          # if the magnetisation is not relaxed yet.
          sim.hysteresis(..., do=[('next_stage', at('stage_time', SI(1e-11, "s")))])

          # Exit hysteresis loop simulation if the total number of
          # steps exceeds 1e6, save fields every 100 steps and at
          # convergence before that:
          sim.hysteresis(..., save=[('fields', every(100, 'step') |
                                    at('convergence'))],
                               do =[('exit', at(1e6, 'step'))])

          # Save averages every 0.1 ns (useful for fourier transform)
          # leave after 20 ns (using the related relax_ command)
          sim.relax(save=[('averages', every('time', SI(1e-10, 's')))],
                    do  =[('exit', at('time', SI(20e-9, 's')))])

          # Save averages every nanosecond, and fields every 100 ns.
          sim.relax(save=[('averages',every('time', SI(1e-9, 's'))),
                          ('fields',  every('time', SI(100e-9,'s')))])

          # Save averages every nanosecond, and fields every 100 ns,
          # save restart file every 1000 steps
          sim.relax(save=[('averages',every('time', SI(1e-9, 's'))),
                          ('fields',  every('time', SI(100e-9, 's'))),
                          ('restart', every(1000, 'step'))])

        If ``save`` is not given, averages and fields will be saved whenever
        the stage ends (this is the default behaviour).
    """

    log.debug("simulation_hysteresis(): Entering with H_ext_list=%s, "
              "save=%s, do=%s, convergence_check=%s" 
	      % (H_ext_list,save,do,convergence_check))

    # This function will check for the correctness of the specifications
    # for the list of (thing to save, when) and (thing to do, when).
    # It will return a joint list of tuples (fn, when), where fn is a function
    # to be called with the simulation object as argument.
    thing_when_tuples = \
      _join_save_and_do_lists(save, do,
                              predefined_actions=self.action_abbreviations)

    log.debug("simulation_hysteresis(): thing_when_tuples=%s"
              % thing_when_tuples)

    # Dictionary containing the predicted time for the next saving
    # for each item specified inside the optional argument 'save'
    next_save_time = {}
    for what, _ in thing_when_tuples:
        key = str(what) # If what is a function we convert it
                        # to string and then use it as the key
        if next_save_time.has_key(key):
            msg = (
              "Error in optional argument 'save' or 'do' of method "
              "'hysteresis': the list of (thing_to_save, when) "
              "contains two or more specifications for "
              "thing_to_save = %s. You should remove the duplicate "
              "entry and eventually use the operator | (such as in: "
              "(thing_to_save, when1 | when2))." % key)
            raise NmagUserError, msg
        next_save_time[key] = None

    # We need this in comparisons of times (this is to solve
    # bugs in comparisons related to truncation errors)
    negligible_time = SI(1e-20, "s")
    match_tolerances = {'time': negligible_time,
                        'stage_time': negligible_time}
    def my_next_time(event, clock):
        return _next_time(event, clock, tols=match_tolerances)

    def my_next_deltas(event, clock, suggest=None):
        return _next_deltas(event, clock, suggest=suggest,
                            tols=match_tolerances)

    progress_file_name = self.name + "_progress.txt"

    # Continue from the last restart file if required
    if self._restarting:
        log.info("Hysteresis loop: restarting from a previously saved "
                 "configuration...")
        self.load_restart_file()
        self._restarting = False # To avoid affecting next calls to hysteresis
    else:
        log.info("Hysteresis loop: starting a new simulation.")
        log.info("Hysteresis loop: check file '%s' for progress data"
                 % progress_file_name)

    # Loop over the given fields
    stage = self.clock['stage']
    self.clock["exit_hysteresis"] = False
    for H_ext in H_ext_list[stage-1:]:
        log.info("hysteresis: starting new stage: field = %s"
                 %  str_of_SI_vector(H_ext))
        self.do_next_stage(stage=stage)
        stage = None # Next time, just increase the stage counter
        self.clock["stage_end"] = False
        if H_ext:
          self.set_H_ext(H_ext)
        self.reinitialise(initial_time=0)

        # First of all we run over the list of things to save
        # and take note about when we should save what
        for what, when in thing_when_tuples:
            key = str(what)
            next_save_time[key] = my_next_time(when, self.clock)
            log.debug("hysteresis: will save %s at %s" %
                      (what, next_save_time[key]))
            #if when.match_time(self.clock1): (get_saver(what))(self)

        # Simulate one stage: loop until convergence!
        # NOTE: we avoid something like 'while self.converged()',
        #       because we want to check for saving fields
        #       if convergence is reached!
        while True:
            self.clock["stage_end"] = converged = self.is_converged()
            log.debug("hysteresis loop, stage %d, converged = %s" 
                      % (self.clock['stage'],str(converged)))
            # Find out the next time we need to check for convergence
            deltas = my_next_deltas(convergence_check, self.clock)
            log.debug("Time to next event: deltas = %s", str(deltas))

            # We now see what needs to be saved. The strategy
            # is the following: if the expected time for saving
            # has changed, then it means that we need to save.
            # time passed the time scheduled for saving!
            for what, when in thing_when_tuples:
                key = str(what)
                time_matches = when.match_time(self.clock)
                nst = my_next_time(when, self.clock)
                # BUG: the comparison nst != next_save_time[key]
                #      will misteriously fail sometimes. The reason
                #      is that (a - b) + b != a for some a and b!
                #      This is due to truncation errors!
                if time_matches or nst != next_save_time[key]:
                    log.debug("hysteresis: analysing %s: time planned "\
                              "for saving was %s, now is %s. Matching? %s"
                              % (what, str(next_save_time[key]),
                              str(nst), str(time_matches)))

                    log.info("hysteresis: saving %s at id=%s,step=%s. "
                             "Details:%s" % (what, self.clock['id'],
                             self.clock['step'], _repr_of_clock(self.clock)))
                    what(self)

                next_save_time[key] = nst
                deltas = my_next_deltas(when, self.clock, suggest=deltas)
                # NOTE: we are doing two times the call to the method
                #       'next_time' of the class When: this is not ideal!

            (delta_step, delta_time, delta_real_time) = deltas
            log.debug("hysteresis: current time is %s"
                      % (str(self.clock['time'])))
            log.debug("predicted advance: "
                      "delta_step=%s, delta_time=%s, delta_real_time=%s"
                      % (str(delta_step), str(delta_time),
                          str(delta_real_time)))

            if delta_time == None:
                # This is not ideal, we want to run forever,
                # but the advance_time method does not allow
                # such a speficication!
                target_time = self.max_time_reached
            else:
                target_time = self.clock['stage_time'] + delta_time

                if delta_step == None: delta_step = -1

            if self.clock["exit_hysteresis"]:
                log.debug("Exit from the hysteresis loop has been forced "
                          "using the tag 'exit': exiting now!")
                return

            if self.clock["stage_end"]:
                log.debug("Reached end of stage in hysteresis command, "
                          "converged=%s, exiting now!" % converged)
                break

            log.debug("About to call advance time with target_time=%s "
                      "and max_it=%s" % (str(target_time),delta_step))
            time_reached = self.advance_time(target_time, max_it=delta_step)
            if time_reached > 0.99*self.max_time_reached:
                msg = ("Simulation time reached %s: are you starting from "
                       "a zero torque configuration?" % self.max_time_reached)
                raise NmagUserError, msg

            # Write some progress data into progress file
            try:
                _update_progress_file(self, H_ext, progress_file_name,
                                      progress_message_minimum_delay)
            except:
                pass # Do not stop the simulation for such a stupid reason!

simulation_hysteresis.__argdoclong__ = """
(
self,
H_ext_list,
save=[('averages', 'fields', at('stage_end'))],
do=[],
convergence_check=every(5, 'step')
)
"""

