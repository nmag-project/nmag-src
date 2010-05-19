"""Time specification utilities for specifying when to do things
(such as saving fields or averages)."""

def float_is_integer(f):
    return 1.0 + (f - round(f))*0.5 == 1.0

def _at_repr(arguments):
    identifier, value = arguments
    return "at(%s, %s)" % (identifier, str(value))

def _at_match_time(this_time, arguments):
    identifier, value = arguments
    return this_time[identifier] == value

def _at_next_time(identifier, this_time, arguments):
    at_identifier, value = arguments
    if at_identifier != identifier: return True
    t = this_time[identifier]
    if type(t) == bool:
        return t == value
    else:
        if t < value:
            return value
        else:
            return False

def _every_repr(arguments):
    identifier, when = arguments
    delta, first, last = when
    opts = ""
    if first != 0: opts += ", first=%s" % str(first)
    if last != None: opts += ", last=%s" % str(last)
    return "every(%s, '%s'%s)" % (str(delta), identifier, opts)

def _every_match_time(this_time, arguments):
    identifier, when = arguments
    t = this_time[identifier]
    delta, first, last = when
    if first != None and t < first: return False
    if last != None and t > last: return False
    if delta == None: return True
    assert(first != None)
    if float_is_integer(float((t - first)*1.0/delta)): return True
    return False

def _every_next_time(identifier, this_time, arguments):
    this_identifier, when = arguments
    if this_identifier != identifier:
        return _every_match_time(this_time, arguments)
    t = this_time[identifier]
    delta, first, last = when
    if last != None and t >= last: return False
    if first != None:
        if t < first and delta != None: return first
    if delta == None: return True
    assert(first != None)
    pos = float(((t - first)*1.0)/delta)
    if float_is_integer(pos):
        next = delta*int(round(pos) + 1) + first
    else:
        next = delta*(int(pos) + 1) + first
    if last != None and next > last: return False
    return next

def _or_repr(arguments):
    arg1, arg2 = arguments
    return "%s | %s" % (_repr(arg1), _repr(arg2))

def _or_match_time(this_time, arguments):
    arg1, arg2 = arguments
    return _match_time(this_time, arg1) or _match_time(this_time, arg2)

def _or_next_time(identifier, this_time, arguments):
    arg1, arg2 = arguments
    nt1 = _next_time(identifier, this_time, arg1)
    nt2 = _next_time(identifier, this_time, arg2)
    nt1_is_bool = (type(nt1) == bool)
    nt2_is_bool = (type(nt2) == bool)
    if nt1_is_bool and nt2_is_bool: return nt1 | nt2
    if nt1_is_bool: return nt2
    if nt2_is_bool: return nt1
    return min(nt1, nt2)

def _and_repr(arguments):
    arg1, arg2 = arguments
    return "%s & %s" % (_repr(arg1), _repr(arg2))

def _and_match_time(this_time, arguments):
    arg1, arg2 = arguments
    return _match_time(this_time, arg1) and _match_time(this_time, arg2)

def _and_next_time(identifier, this_time, arguments):
    arg1, arg2 = arguments
    save_t = this_time[identifier]
    both_match = False
    while not both_match:
        nt1 = _next_time(identifier, this_time, arg1)
        nt2 = _next_time(identifier, this_time, arg2)
        if nt1 == False or nt2 == False:
            this_time[identifier] = save_t
            return False
        if nt1 == True:
            this_time[identifier] = save_t
            return nt2
        if nt2 == True:
            this_time[identifier] = save_t
            return nt1

        if nt1 > nt2:
            ntmax, ntmin = nt1, nt2
            argmax, argmin = arg1, arg2
        else:
            ntmax, ntmin = nt2, nt1
            argmax, argmin = arg2, arg1

        this_time[identifier] = ntmax
        both_match = _match_time(this_time, argmin)

    this_time[identifier] = save_t
    return ntmax

_repr_dict = {'at':_at_repr,
              'every':_every_repr,
              'or':_or_repr,
              'and':_and_repr,
              'never':lambda a: 'never'}
def _repr(spec):
    command, arguments = spec
    return (_repr_dict[command])(arguments)

_match_time_dict = {'at':_at_match_time,
                    'every':_every_match_time,
                    'or':_or_match_time,
                    'and':_and_match_time,
                    'never':lambda a, b: False}
def _match_time(this_time, spec):
    command, arguments = spec
    return (_match_time_dict[command])(this_time, arguments)

_next_time_dict = {'at':_at_next_time,
                   'every':_every_next_time,
                   'or':_or_next_time,
                   'and':_and_next_time,
                   'never':lambda a, b, c: False}
def _next_time(identifier, this_time, spec):
    command, arguments = spec
    return (_next_time_dict[command])(identifier, this_time, arguments)

class When:
    """Class used to express when a certain thing should be done."""
    def __init__(self, spec):
        self.spec = spec

    def __repr__(self):
        return _repr(self.spec)

    def __str__(self):
        return _repr(self.spec)

    def match_time(self, this_time):
        return _match_time(this_time, self.spec)

    def next_time(self, identifier, this_time, tols=None):
        nt = _next_time(identifier, this_time, self.spec)
        if tols != None and tols.has_key(identifier) and type(nt) != bool:
            tol = tols[identifier]
            tt = this_time[identifier]
            if tol > 0.0 and abs(nt - tt) < tol:
                this_time[identifier] = nt + tol
                nt = _next_time(identifier, this_time, self.spec)
                this_time[identifier] = tt
                return nt

        return nt

    def __or__(self, other):
        return When(('or', (self.spec, other.spec)))

    def __and__(self, other):
        """
          The followings are known issues which relates to the & operator.
          Don't use the operator & with non independent variables!
          Usually it does not make too much sense anyway!
          Let's see some examples of things that you should not do:

            every(10, 'step') & every(5, 'stage_step')
            every(10, 'step') & every(SI(1e-9, "s"), 'time')

          These examples will produce apparently crazy results, because
          step, stage_step, time and stage_time are dependend variables.
          Other crazy things can be done with the operator &. Example:

            w = every(2, 'step') & every(2, 'step', first=1)
            w.next_time('step', time_object)

          The evaluation of the w When object will lead to an infinite
          loop.
        """
        return When(('and', (self.spec, other.spec)))

def at(identifier, value=True):
    """Function to specify that an action should be performed
    exactly at a certain point in time.
    Examples: at('convergence'), at('step', 10), at('time', 1.23)
    """
    return When(('at', (identifier, value)))

def every(arg1, arg2=None, first=0, last=None):
    """
    Function to specify that an action should be performed periodically.
    Examples::
      every(10, 'step')
      every(5, 'step', first=10, last=100)
      every('step', first=15)
    """
    # I want both every(10, 'step') and every('step', 10) to be valid.
    # I therefore swap the arguments if needed.
    # This is probably not a good idea. But it makes things easier for now!
    if type(arg1) == str:
        identifier, delta = arg1, arg2
    else:
        identifier, delta = arg2, arg1
    if type(identifier) != str:
        raise ValueError("Bad usage of the function every: you should "
                         "specify an identifier. Example: every(10, 'step')")
    if last != None and last <= first:
        raise ValueError("Bad usage of the function every: the value of the "
                         "optional argument last must be greater than "
                         "the value of the optional argument first, however "
                         "first=%s, last=%s" % (str(first), str(last)))
    if delta <= 0:
        raise ValueError("Bad usage of the function every: the delta value "
                         "must be positive, but you specified something like "
                         "every(-1, 'step') or every(0, 'step')")
    return When(('every', (identifier, (delta, first, last))))

never = When(('never', (None, None)))

if __name__ == "__main__":
    # If this is the main, we execute this little test
    from nsim.si_units import SI
    time = {'stage':0, 'step':0, 'time':SI(0.0, 's'),
            'stage_step':0, 'stage_time':0.0, 'real_time':0.0,
            'convergence':False}

    # WARNING: be careful with the & operator!
    # & operator is implemented using brute force: it will search
    # matching events. This could result in infinite loops, if there
    # are no matching events!
    # Example: every(2, 'step') & every(2, 'step', first=1)
    # the resulting intersection is empty!

    w = every(2, 'step', last=10) & every(5, 'step', first=10)
    w = every(2, 'step', last=21) & every(4, 'step', first=10)
    w = every(2, 'step', last=21) & every(4, 'step', first=10) | at('step', 15)


    # every(100, 'step') + every(SI('0.1 ns'), 'stage_time')

    if True:
        w = every(SI(1.5e-12, 's'), 'time')
        from nsim.si_units import SI
        time['time'] = SI(0.0e-12, "s")
        w = every('time', SI(100.0e-12, "s")) | every('time', SI(30.0e-12, "s"))
        w = every('time', SI(100.0e-12, "s")) | at('convergence')
        id = 'time'
        this = SI(0, "s")
    else:
        id = 'step'
        this = 0

    for i in range(0,21):
        time[id] = this
        next = w.next_time(id, time)
        print "%s %s:" % (id, str(this)), "match_time=<%s>" % str(w.match_time(time)), \
              "-- next_time=<%s>" % str(next)
        this = next
        if next == False: break


    #w = at('convergence') & every(2, 'stage')


    #H_list = [[0, 0, 0], ...]
    #save_list = save('averages', every(10, 'stage_step')) and \
                #save('fields', at('convergence'))]
    #sim.hysteresis(H_list, save_list)



