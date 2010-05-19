import math

def with_units(units):
    if units == None:
        return lambda number: number
    else:
        return lambda number: number*units

def _append_floats(l, first, second, last, units=None):
    add_units = with_units(units)
    def err_msg(msg):
      return "float_list contains [..., %g, %g, \"...\", %g, ...]," \
              " but this is wrong: %s" % (first, second, last, msg)

    step = second - first
    interval = last - first
    num_steps = interval / step
    if num_steps <= 1.0:
        raise ValueError(err_msg("%g should lie between %g and %g!"
                                 % (second, first, last)))

    if abs(interval/round(num_steps) - step)/64 + interval != interval:
        int_num_steps = int(num_steps) + 1
    else:
        int_num_steps = int(round(num_steps))

    for i in range(2, int_num_steps):
        l.append(add_units(first + i*step))

    l.append(add_units(last))

def float_set(float_list, units=None):
    """Provides an easy way to specify sequences of numbers.
       Examples:
         # SYNTAX 1: [first, second, [], last]
         float_set([0, 1, [], 5])
          --> [0, 1, 2, 3, 4, 5]
         # SYNTAX 2: [first, [num_steps], last] syntax
         float_set([0, [5], 5])
          --> [0.0, 1.0, 2.0, 3.0, 4.0, 5.0]
         # TWO SUSPENDED LISTS
         float_set([0, 0.1, [], 0.5, 1, [], 3])
          --> [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0]
         # MIX OF THE TWO USAGES
         float_set([0, [3], 3, 4, [], 6])
          --> [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
    """
    add_units = with_units(units)

    fs = []
    eye_tail_1 = None
    eye_tail_2 = None
    status = 0
    for token in float_list:
        if status == 0:
            if token == "..." or token == []:
                if eye_tail_1 == None or eye_tail_2 == None:
                    raise ValueError("float_list has wrong syntax! I expect "
                                     "something like [1, 2, \"...\", 10], "
                                     "but you gave something like "
                                     "[1, \"...\", 10]")
                status = 1
            elif type(token) == list:
                if len(token) == 1 and type(token[0]) == int:
                    num_steps = token[0]
                    if num_steps < 1:
                        raise ValueError("the number of steps must be a "
                                         "positive integer: you gave "
                                         "[..., %f, [%i], ...]"
                                         % (eye_tail_1, token[0]))
                    status = 2
                else:
                    raise ValueError("float_list contains a syntax error. "
                                     "Hint: the right syntax is to range "
                                     "from a to b in n steps is [a, [n], b]. "
                                     "n must be integer.")
            else:
                eye_tail_2 = eye_tail_1
                eye_tail_1 = float(token)
                fs.append(add_units(eye_tail_1))
        elif status == 1:
            final_float = float(token)
            _append_floats(fs, eye_tail_2, eye_tail_1, final_float, units)
            eye_tail_2 = None
            eye_tail_1 = final_float
            status = 0
        elif status == 2:
            initial_float = eye_tail_1
            final_float = float(token)
            # final_float is float: there is no need for using float()
            step_size = (final_float - initial_float)/num_steps
            initial_float += step_size
            for i in range(num_steps):
                fs.append(initial_float + i*step_size)
            eye_tail_2 = None
            eye_tail_1 = final_float
            status = 0
    if status != 0:
        raise ValueError('float_list cannot end with "...", [] or [integer]')
    return fs

def vector_scalar_product(vector1, vector2):
    tot = None
    for v1i, v2i in zip(vector1, vector2):
        if tot == None:
            tot = v1i*v2i
        else:
            tot += v1i*v2i
    return tot

def vector_sq_norm(vector): return vector_scalar_product(vector, vector)

def vector_norm(vector):
    return (vector_scalar_product(vector, vector))**0.5

def vector_scale(vector, scale):
    return [scale*vi for vi in vector]

def vector_direction(vector): return vector_scale(vector, 1.0/vector_norm(vector))

def vector_set(norm_list, direction, units=None, normalize=True):
    """Provides a convenient way to specify a list of vectors which share
       the same direction. This function takes the given direction
       (`direction`) and the given list of norms (`norm_list`)
       as input parameters and generates the list of the corresponding vectors.
       If `normalize` is True, then the direction is normalized
       before being used. `units` provides a multiplicative factor
       which should be used to scale the vectors.
       The syntax for the parameter `norm_list` is the same as for
       the function `float_set`. Refer to the documentation of `float_set`
       for more help.
       Examples:

          IN: nmag.vector_set(norm_list=[0, [4], 4],
                              direction=[1, 2, 3],
                              normalize=False)
          OUT:
          [[0.0, 0.0, 0.0],
          [1.0, 2.0, 3.0],
          [2.0, 4.0, 6.0],
          [3.0, 6.0, 9.0],
          [4.0, 8.0, 12.0]]

          IN:  nmag.vector_set(norm_list=[0, [4], 4], direction=[1, 2, 3])
          OR:  nmag.vector_set(norm_list=[0, 1, [], 4], direction=[1, 2, 3])
          OUT:
          [[0.0, 0.0, 0.0],
          [0.2672612419124244, 0.53452248382484879, 0.80178372573727319],
          [0.53452248382484879, 1.0690449676496976, 1.6035674514745464],
          [0.80178372573727319, 1.6035674514745464, 2.4053511772118195],
          [1.0690449676496976, 2.1380899352993952, 3.2071349029490928]]

          IN: nmag.vector_set(norm_list=[0, 2],
                              direction=[1, 2],
                              normalize=False,
                              units=3)
          OUT: [[0.0, 0.0], [6.0, 12.0]]
    """
    add_units = with_units(units)
    if normalize == True:
        unit_vector = vector_direction(direction)
    else:
        unit_vector =  direction
    nl = float_set(norm_list)
    vs = [[add_units(norm*x) for x in unit_vector] for norm in nl]
    return vs

def projection(axis, vector, normalize=True):
    axis_norm = vector_norm(axis)
    unit_vector = [ai/axis_norm for ai in axis]
    return vector_scalar_product(unit_vector, vector)

def intersections(value, xy_data):
    intersections = []
    was_greater = None
    old_x = None
    old_y = None
    for x, y in xy_data:
        if y > value:
            is_greater = True
        elif y < value:
            is_greater = False
        else:
            is_greater = None
            intersections.append(x)
        if is_greater != None and was_greater != None:
            if is_greater != was_greater:
                old_y
                ix = old_x + (value - old_y)*(x - old_x)/(y - old_y)
                intersections.append(ix)
        was_greater = is_greater
        old_x = x
        old_y = y
    return intersections


#print float_set([0, 1, "...", 5, 5.25, "...", 7])

#print vector_set([0, 1, "...", 10], [1.0, 1.0, 1.0], normalize=True)

#print intersections(1.0, [[0.0, 5.0], [2.0, 3.0], [3, 4], [4, 1], [5.0, 5.0]])
