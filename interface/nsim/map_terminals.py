import nsim
import types
import numpy
from nsim.si_units import SI

def map_terminals(fun, obj):
    if type(obj) == types.ListType:
        return map(lambda x: map_terminals(fun, x), obj)
    elif type(obj) == types.TupleType:
        return tuple(map(lambda x: map_terminals(fun, x), obj))
    else:
        return fun(obj)

def SI_vector(v):
    """
    This function deals with the problem of specifying units in
    scalar, vectors, matrices, etc.
    This function is written to provide a unified convention
    for nsim. These are equivalent ways to spefify a 4-D vector
    of lengths in meters:

        EXAMPLE 1: ([1, 2, 3, 4], SI("m"))
        EXAMPLE 2: [SI(1, "m"), SI(2, "m"), SI(3, "m"), SI(4, "m")]

    If the unit is not specified then SI("") is assumed.
    The unit can be specified in two ways:

        1) Tuple of (vector, units);
        2) vector of SI objects.

    The function returns a tuple (out_obj, units) where out_obj is obtained
    removing the units from the original provided object obj and units
    are the units. out_obj has the same shape of out.
    """

    def invalid_syntax():
        raise ValueError("Invalid syntax for vector. Examples of valid syntax "
                         "are [SI(1), SI(2), SI(3), ...] and ([1, 2, 3, ...], "
                         "SI(\"\")).")

    # First we assume that v is in the form 2): vector/matrix of SI objects

    # 'Assignments do not copy data -- they just bind names to objects'
    #                                                        Guido van Rossum
    # This is why we define 'u' to be a list containing a value:
    # this is the only way we can modify that value inside the function
    # check_units.
    u = []
    def check_units(scalar):
        if type(scalar) == SI:
            if len(u) == 0:
                u.append(scalar)
            else:
                if type(u[0]) != SI:
                    u[0] = "_SI"
                    return 0.0
                if not scalar.is_compatible_with(u[0]):
                    raise ValueError("Your vector/matrix contains scalars "
                                     "with incompatible units: %s" % v)
            return scalar._value
        else:
            if len(u) == 0:
                u.append(scalar)
            else:
                if type(u[0]) == SI or type(u[0]) == str:
                    invalid_syntax()
            return scalar

    new_body = map_terminals(check_units, v)

    if type(u[0]) == SI:
        units = u[0].copy()
        units._value = 1.0
        return (new_body, units)
    else:
        try:
            body, units = v
            if type(units) == SI:
                return (body, units)
        except:
            pass
        invalid_syntax()

if __name__ == "main":
    #print map_terminals(lambda x: x+x, [(1, 2, 3), 4])
    #print map_terminals(lambda x: x+x, numpy.array([1, 2, 3, 4]))

    print SI_vector(([1.23, 2, 4, 5, 6], SI("")))
    print SI_vector(SI(3.1415926))

    vec2d = (SI(1), SI(2))
    print SI_vector(vec2d)

    print SI_vector(([1, 2, 3, 4], SI("m")))
    print SI_vector([SI(1, "m"), SI(2, "m"), SI(3, "m"), SI(4, "m")])
    print SI_vector([1, 2, SI(4, "m")])
