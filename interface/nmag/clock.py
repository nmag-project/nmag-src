# Nmag micromagnetic simulator
# Copyright (C) 2010 University of Southampton
# Hans Fangohr, Thomas Fischbacher, Matteo Franchin and others
#
# WEB:     http://nmag.soton.ac.uk
# CONTACT: nmag@soton.ac.uk
#
# AUTHOR(S) OF THIS FILE: Matteo Franchin
# LICENSE: GNU General Public License 2.0
#          (see <http://www.gnu.org/licenses/>)

from nsim.si_units import SI

def fmt_time(t, fmt_ps="%.2f", fmt_ns="%.2f"):
    t_ps = float(t/SI(1e-12, "s"))
    return ("%s ps" % (fmt_ps % t_ps)
            if t_ps < 100.0 else "%s ns" % (fmt_ns % (t_ps/1000.0)))

class SimulationClock(object):
    # Initial values for all the members of the clock
    zero_vals = \
      {             "id": -1,
                 "stage": 1,
                  "step": 0,
                  "time": SI(0.0, "s"),
            "stage_step": 0,
            "stage_time": SI(0.0, "s"),
             "real_time": SI(0.0, "s"),
             "stage_end": False,
           "convergence": False,
       "exit_hysteresis": False,
       "zero_stage_time": SI(0.0, "s"),
       "zero_stage_step": 0,
       "time_reached_su": 0.,
       "time_reached_si": SI(0.0, "s"),
       "last_step_dt_su": 0.,
       "last_step_dt_si": SI(0.0, "s")}

    """
    This object specifies all the parameters which define the current time
    in the simulation, such as the simulation time, step number, ...
    In particular:
     id   : unique identifier for data saved. Everytime any data is
            saved, this number will be increased by one.
     stage: the stage number. The stage counter increases whenever
            the field is changed;
     step: the step number. The total number of steps performed
           by this instance of the simulation (always increases);
     stage_step: step number from the beginning of the current stage;
     zero_stage_step: the value of 'step' at the beginning of the stage;
     time: the simulation time. The total simulation time
           which was simulated by this instance of the simulator
           (always increases)
     stage_time: the simulation time from the beginning of the stage;
     zero_stage_time: the value of 'time' at the beginning of the stage;
     real_time: the real time used for advancing time;
     last_step_dt: last time step's length
    """
    def __init__(self, **args):
        # Initialise the clock object
        zero_vals = SimulationClock.zero_vals
        for key, zero in zero_vals.iteritems():
            setattr(self, key, args.get(key, zero))

        # Make sure args contains only known arguments
        for key in args:
            if key not in zero_vals:
                raise TypeError("SimulationClock got an unexpected argument "
                                "'%s'" % key)

    def __repr__(self):
        return ("SimulationClock(%s)"
                % ", ".join(["%s=%s" % (key, repr(getattr(self, key)))
                            for key in self.zero_vals]))

    def __getitem__(self, item_key):
        # Just for compatibility. Should be removed later on
        return getattr(self, item_key)

    def __setitem__(self, item_key, value):
        # Just for compatibility. Should be removed later on
        return setattr(self, item_key, value)

    def inc_stage(self, stage=None):
        """Advance the clock to the next stage."""
        if stage == None:
            self.stage += 1
        else:
            self.stage = stage
        self.stage_step = 0
        self.stage_time = SI(0.0, "s")
        self.convergence = False
        self.zero_stage_step = self.step
        self.zero_stage_time = self.time

    def __str__(self):
        ft = fmt_time
        rows = ((("ID", None, self.id), ("Step", None, self.step),
                 ("Time", ft, self.time),
                 ("Last step size", ft, self.last_step_dt_si)),
                 #("Real time", None, "N/A")),
                (None, ("Stage", None, self.stage),
                 ("Stage-step", None, self.stage_step),
                 ("Stage-time", ft, self.stage_time)),
                (None, ("Convergence", None, self.convergence),
                 ("Stage-end", None, self.stage_end),
                 ("Exit hysteresis", None, self.exit_hysteresis)))

        row_strs = []
        col_widths = []
        for row in rows:
            col_strs = []
            for nr_col, col in enumerate(row):
                desc, fmt, value = col if col != None else ("", None, "")
                value_str = fmt(value) if fmt != None else str(value)
                col_str = (desc, value_str)
                col_strs.append(col_str)
                col_width = len(col_str)
                if nr_col < len(col_widths):
                    wdesc, wvalue = col_widths[nr_col]
                    col_widths[nr_col] = \
                      (max(wdesc, len(desc)), max(wvalue, len(value_str)))
                else:
                    assert nr_col == len(col_widths)
                    col_widths.append((len(desc), len(value_str)))
            row_strs.append(col_strs)

        s = ""
        width = 0
        for row_str in row_strs:
            line = ""
            for nr_col, col in enumerate(row_str):
                sep = "=" if len(col[0]) > 0 else " "
                desc = col[0].rjust(col_widths[nr_col][0])
                value = col[1].ljust(col_widths[nr_col][1])
                line += "%s%s%s | " % (desc, sep, value)
            s += line + "\n"
            width = max(width, len(line))
        sep = "="*width
        return "%s\n%s%s" % (sep, s, sep)
