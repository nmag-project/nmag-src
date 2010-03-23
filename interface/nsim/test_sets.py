"""
This module tests the mini-language provided by the sets.py module (float_set).
When a bug is found in the sets.py module this should be converted
into a regression test, which should be added here.
Then the bug has to be fixed. This series of tests actually
defines the behaviour of the mini-language, better than any documentation.
"""
import sets as s

def piece(s, pos, length=3):
    i = max(0, pos)
    f = min(len(s), pos+length)
    return  "[... " + ", ".join(map(str, s[i:f])) + " ...]"

def err_msg(s1, s2, pos, length=3):
    return "At position %d the two sets are:\n SET1 = %s\n SET2 = %s\n" \
           % (pos+1, piece(s1, pos, length), piece(s2, pos, length))

def sets_are_equal(s1, s2):
    i = 0
    for x1, x2 in zip(s1, s2):
        i += 1
        if abs(x1 - x2) > 1e-10:
            return (False,
                    "At position %d sets differ: %s != %s. " % (i, x1, x2)
                    + err_msg(s1, s2, i-3, 5))
    if len(s1) == len(s2): return (True, "")
    return (False, "sets have different size %d != %d" % (len(s1), len(s2)))

# ----------------------------------------------------------------------------- #
def test_syntax_suspensions():
    
    for a,b in (([0, 1, [], 9],           range(10)),
                ([9, 8, [], 0],           range(9, -1, -1)),
                ([9, 8, [], 0, 1, [], 9], range(9, 0, -1) + range(10))):
        yield check_plain, a, b
        
    for a,b in (([0,  0.1, [],  0.5 ],  [0.0,  0.1,  0.2,  0.3,  0.4,  0.5 ]),
                ([0,  0.1, [],  0.45],  [0.0,  0.1,  0.2,  0.3,  0.4,  0.45]),
                ([0,  0.1, [],  0.2 ],  [0.0,  0.1,  0.2]),
                ([0, -0.1, [], -0.45],  [0.0, -0.1, -0.2, -0.3, -0.4, -0.45]),
                ([0,  0.1, [],  0.5, 1, [], 3], 
                 [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0])):
        yield check_with_sets_are_equal, a, b

    # The following were originally in tests named test_syntax_loop_N,
    # but I don't see how they differ from the first lot (jacek 2007-10-10)
    for a,b in (([0, [9], 9], range(10)),
                ([9, [9], 0], range(9, -1, -1))):
        yield check_plain, a, b

def check_plain(a,b):
    assert s.float_set(a) == b

def check_with_sets_are_equal(a, b):
    success, msg = sets_are_equal(s.float_set(a), b)
    assert success, msg
# ----------------------------------------------------------------------------- #

def test_mix_1():
    assert (s.float_set([0, [3], 3, 4, [], 6]) ==
            [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0])

def test_mix_2():
    input_data = [0.05, 0.1, [], 1.0]
    good_data = [0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5,
                 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1.0]
    output_data = s.float_set(input_data)
    success, msg = sets_are_equal(output_data, good_data)
    assert success, msg

def test_mix_3():
    input_data = [0.05, 0.1, [], 1.0, 1.1, [], 2.0, 2.2, [],
                  3.8, 3.82, [], 4.2, 4.25, [], 4.5, 4.6, [],
                  5.0, 5.5, [], 8.0]
    good_data = [0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5,
                 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1.0,
                 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0,
                 2.2, 2.4, 2.6, 2.8, 3.0, 3.2, 3.4, 3.6, 3.8,
                 3.82, 3.84, 3.86, 3.88, 3.9 , 3.92, 3.94, 3.96, 3.98,
                 4.0, 4.02, 4.04, 4.06, 4.08, 4.10, 4.12, 4.14, 4.16,
                 4.18, 4.20,
                 4.25, 4.3, 4.35, 4.4, 4.45, 4.5,
                 4.6, 4.7, 4.8, 4.9,
                 5.0, 5.5, 6.0, 6.5, 7.0, 7.5, 8.0]
    output_data = s.float_set(input_data)
    success, msg = sets_are_equal(output_data, good_data)
    assert success, msg

