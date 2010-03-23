'''Somewhat generic tools for vector manipulation.'''

import math
#import operator

def normalised_vector(vector):
    # This is pretty ugly
    # inv_norm = 1/math.sqrt(reduce(lambda tot, xi: tot + xi*xi, vector, 0.0))

    # This is much prettier
    #inv_norm = 1/math.sqrt(reduce(operator.add, map(operator.mul, vector, vector)))

    # This is even prettier, and about 30% faster, but only works for
    # 3D vectors, which should be enough for our purposes, shouldn't
    # it ?
    inv_norm = 1/math.sqrt(vector3_dot(vector, vector))
    return [vi*inv_norm for vi in vector]

def vector3_dot(v, w):
    return v[0]*w[0] + v[1]*w[1] + v[2]*w[2]

def vector3_cross(v, w):
    return (v[1]*w[2]-v[2]*w[1], v[2]*w[0]-v[0]*w[2], v[0]*w[1]-v[1]*w[0])
