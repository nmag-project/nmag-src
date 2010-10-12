"""
Here we try to address convergence issues.
We provide a class which takes ||dm/dt||_max as input and decides
whether the simulation has converged or not.
The class also analyzes the values it received and returns suggestions
about whether the tolerances need to be improved or not.
"""

# Standard modules
import logging
log = logging.getLogger('nmag')

from nsim.circular import CircularList

class Convergence:
    def __init__(self, countdown=2, statistics=5,
                 improve_factor=0.5, worsen_factor=1.5,
                 max_factor=1.0, min_factor=1.0e-4,
                 log_size=50):

        self.improve_factor = improve_factor
        self.worsen_factor = worsen_factor
        self.max_factor = max_factor
        self.min_factor = min_factor
        self.statistics = statistics
        self.countdown_initial = countdown
        self._reset()
        self.log = CircularList(log_size)

    def _reset(self):
        self.num = 0
        self.sum_dev_max_dmdt = 0.0
        self.sum_osc_max_dmdt = 0.0
        self.previous_max_dmdt = None
        self.countdown = self.countdown_initial

    def _quality(self, max_dmdt):
        convergence_quality = None
        if self.previous_max_dmdt != None:
            dev = max_dmdt - self.previous_max_dmdt
            osc = abs(dev)
            self.sum_osc_max_dmdt += osc
            self.sum_dev_max_dmdt += dev
            self.num += 1

            if self.num >= self.statistics and self.sum_osc_max_dmdt > 0.0:
                convergence_quality = \
                  abs(self.sum_dev_max_dmdt/self.sum_osc_max_dmdt)

                self.num = 0
                self.sum_dev_max_dmdt = 0.0
                self.sum_osc_max_dmdt = 0.0

        self.previous_max_dmdt = max_dmdt
        return convergence_quality

    def check(self, step, max_dm_dt, stopping_dm_dt, tol_factor):
        """
        Returns True when convergence has been reached.
        """
        log.debug("Entering is_converged()")
        #print "max_dm_dt=%s < %s=stopping_dm_dt ? %s" \
        #      % (max_dm_dt, stopping_dm_dt, max_dm_dt < stopping_dm_dt)
        print max_dm_dt, stopping_dm_dt
        if max_dm_dt < stopping_dm_dt:
            self.countdown -= 1
            if self.countdown < 1:
                log.debug("is_converged(): True")
                # Since we are converging, we don't want to interfere
                # and change the tolerances now! We therefore return
                # new_tol_factor = None
                self._reset()
                return True, None

            log.debug("is_converged(): Trueish (%d times converged but need "
                      "%d times to stop)" % (self.countdown, self.countdown))
            return False, None

        self.countdown = self.countdown_initial
        log.debug("is_converged(): False")

        convergence_quality = self._quality(max_dm_dt)
        self.log.append((step, max_dm_dt, stopping_dm_dt, convergence_quality))

        if convergence_quality == None:
            return False, None

        else:
            new_tol_factor = None
            if convergence_quality == 1.0:
                new_tol_factor = tol_factor*self.worsen_factor
            elif convergence_quality < 0.5:
                new_tol_factor = tol_factor*self.improve_factor
            if new_tol_factor != None:
                new_tol_factor = min(self.max_factor,
                                     max(self.min_factor, new_tol_factor))
                if new_tol_factor == tol_factor: new_tol_factor = None
        return False, new_tol_factor

    def get_log(self):
        l = self.log.get_list()
        s = "# Convergence log\n"
        s += "# step, ax dm/dt, stoppping dm/dt, conv. quality\n"
        for step, max_dm_dt, stopping_dm_dt, conv_quality in l:
            s += "%s %s %s %s\n" % (step, max_dm_dt, stopping_dm_dt,
                                    conv_quality)
        return s

