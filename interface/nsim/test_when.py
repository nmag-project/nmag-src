"""
This module tests the mini-language provided by the when.py module.
When a bug is found in the when.py module this should be converted
into a regression test, which should be added here.
Then the bug has to be fixed. This series of tests actually
defines better than any other documentation the behaviour
of the mini-language.
"""
import when as w

t_obj = {'x':0, 'y':0}

def test_match_x():
    step_first_last_list = [(2, 3, 22), (2, 3, 23), (3, 0, None)]
    for step_first_last in step_first_last_list:
        x_step, x_first, x_last = step_first_last
        w_obj = w.every('x', x_step, first=x_first, last=x_last)
        for x in range(40):
            t_obj['x'] = x
            matched = w_obj.match_time(t_obj)
            if x < x_first or (x_last != None and x > x_last):
                matched_expected = False
            else:
                matched_expected = (x-x_first) % x_step == 0
            if matched != matched_expected:
                msg = "%s, x=%d: " % (str(w_obj), x) + \
                      "match_time returned %s, expected value is %s" \
                      % (str(matched), str(matched_expected))
                assert False, msg

def test_next_x():
    step_first_last_list = [(2, 3, 22), (2, 3, 23), (3, 0, None)]
    for step_first_last in step_first_last_list:
        x_step, x_first, x_last = step_first_last
        w_obj = w.every('x', x_step, first=x_first, last=x_last)
        for x in range(40):
            t_obj['x'] = x
            next_x = w_obj.next_time('x', t_obj)
            if x < x_first:
                next_x_expected = x_first
            else:
                next_x_expected = ((x-x_first)/x_step + 1)*x_step+x_first
            if x_last != None and next_x_expected > x_last:
                next_x_expected = False
            if next_x != next_x_expected:
                msg = "%s, x=%d: " % (str(w_obj), x) + \
                      "next_time returned %s, expected value is %s" \
                      % (str(next_x), str(next_x_expected))
                assert False, msg

def test_match_x_and_y():
    x_step = 2
    y_step = 3
    w_obj = w.every('x', x_step) & w.every('y', y_step)
    for x in range(10):
        t_obj['x'] = x
        for y in range(10):
            t_obj['y'] = y
            matched = w_obj.match_time(t_obj)
            matched_expected = (x % x_step == 0) and (y % y_step == 0)
            if matched != matched_expected:
                msg = "%s, x=%d, y=%d: " % (str(w_obj), x, y) + \
                      "match_time returned %s, expected value is %s" \
                      % (str(matched), str(matched_expected))
                assert False, msg

def test_next_x_and_y():
    x_step = 2
    y_step = 3
    w_obj = w.every('x', x_step) & w.every('y', y_step)
    for x in range(10):
        t_obj['x'] = x
        for y in range(10):
            t_obj['y'] = y
            next_y = w_obj.next_time('y', t_obj)
            if x % x_step == 0:
                next_y_expected = (int(y)/y_step + 1)*y_step
            else:
                next_y_expected = False
            if next_y != next_y_expected:
                msg = "%s, x=%d, y=%d: " % (str(w_obj), x, y) + \
                      "next_time returned y=%s, expected value is y=%s" \
                      % (str(next_y), str(next_y_expected))
                assert False, msg

def test_next_x_and_y_or_z():
    x0_y_step = 10
    x_y_step = 2
    w_obj =   (w.at('x', 0) & w.every('y', x0_y_step)) \
            | (w.every('x', 1, first=1) & w.every('y', x_y_step))
    for x in range(4):
        t_obj['x'] = x
        for y in range(100):
            t_obj['y'] = y
            next_y = w_obj.next_time('y', t_obj)
            if x == 0:
                next_y_expected = (int(y)/x0_y_step + 1)*x0_y_step
            else:
                next_y_expected = (int(y)/x_y_step + 1)*x_y_step
            if next_y != next_y_expected:
                msg = "%s, x=%d, y=%d: " % (str(w_obj), x, y) + \
                      "next_time returned y=%s, expected value is y=%s" \
                      % (str(next_y), str(next_y_expected))
                assert False, msg

#def testEmpty(self):
    #pass
    #self.assertEqual( r.reverseWords(''),'')

#def testExactlyOneArgument(self):
    #pass
    #self.assertRaises( TypeError, r.reverseWords)
    #self.assertRaises( TypeError, r.reverseWords, 'One', 'Two')

