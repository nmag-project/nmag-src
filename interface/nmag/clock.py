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
    def __init__(self, id=-1):
        self.id = id
        self.stage = 1
        self.step = 0
        self.time = SI(0.0, "s")
        self.stage_step = 0
        self.stage_time = SI(0.0, "s")
        self.real_time = SI(0.0, "s")
        self.stage_end = False
        self.convergence = False
        self.exit_hysteresis = False
        self.zero_stage_time = SI(0.0, "s")
        self.zero_stage_step = 0
        self.time_reached_su = 0.
        self.time_reached_si = SI(0.0, "s")
        self.last_step_dt_su = 0.
        self.last_step_dt_si = SI(0.0, "s")

    def __getitem__(self, item_key):
        # Just for compatibility. Should be removed later on
        return getattr(self, item_key)

    def __setitem__(self, item_key, value):
        # Just for compatibility. Should be removed later on
        return setattr(self, item_key, value)

    def __str__(self):
        ft = fmt_time
        rows = ((("Step", None, self.step), ("Time", ft, self.time),
                 ("Last step size", ft, self.last_step_dt_si),
                 ("Real time", None, "N/A")),
                (("Stage", None, self.stage),
                 ("Stage-step", None, self.stage_step),
                 ("Stage-time", ft, self.stage_time)),
                (("Convergence", None, self.convergence),
                 ("Stage-end", None, self.stage_end),
                 ("Exit hysteresis", None, self.exit_hysteresis)))

        row_strs = []
        col_widths = []
        for row in rows:
            col_strs = []
            for nr_col, col in enumerate(row):
                desc, fmt, value = col
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
                desc = col[0].rjust(col_widths[nr_col][0])
                value = col[1].ljust(col_widths[nr_col][1])
                line += "%s=%s | " % (desc, value)
            s += line + "\n"
            width = max(width, len(line))
        sep = "="*width
        return "%s\n%s%s\n" % (sep, s, sep)
