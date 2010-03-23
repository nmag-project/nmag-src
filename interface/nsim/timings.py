import time

timers = []

class Timer:
    def __init__(self, name):
        self.name = name
        self.counters = {}
        timers.append(self)

    def start(self, identifier):
        """Start the timer counter identified by ``identifier``.
        Creates it if it does not exist."""
        if not self.counters.has_key(identifier):
            self.counters[identifier] = (0.0, time.time(), 0)

        else:
            prev = self.counters[identifier]
            if prev[1] != None:
                print "Timer %s: double start for timer %s" % (self.name,
                                                               identifier)
            else:
                self.counters[identifier] = (prev[0], time.time(), prev[2])

    def stop(self, identifier):
        """Stop the timer counter identified by ``identifier``."""
        try:
            t, t0, num_stops = self.counters[identifier]
            self.counters[identifier] = (t + time.time() - t0,
                                         None,
                                         num_stops+1)

        except:
            print "Timer %s: stop without start for timer %s" % (self.name,
                                                                 identifier)
    def get_state(self, identifier):
        """Get statistics about the given time counter."""
        if self.counters.has_key(identifier):
            t, s, n = self.counters[identifier]
            if s == None:
                return ("stopped", t, n)

            else:
                return ("started", t, n)

        else:
            return ("undefined", None, 0)

    def __repr__(self):
        s = "Timer '%s':\n" % self.name
        if len(self.counters) < 1:
            return s

        max_len = max([len(c) for c in self.counters])
        for c in self.counters:
            state, t, n = self.get_state(c)
            s += ("  %s: %.4f - %s (%s times)\n"
                  % (c.rjust(max_len), t, state, n))
        return s

def show_timers():
    """Shows the state of all the created timers and all their counters."""
    for tmr in timers:
        print tmr

