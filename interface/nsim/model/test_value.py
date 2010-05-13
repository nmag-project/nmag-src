from nsim.model import Value

def x_evalx(x, xs, evalxs):
    xs.append(x)
    evalxs.append(eval(x))
    return (xs, evalxs)

def assert_eq(a, b):
    assert a == b, "Expected %s, but got %s" % (b, a)

def expect_exception(fn, exc):
    try:
        fn()
    except exc:
        pass
    else:
        assert False, "Expected a %s exception" % exc

def test_all():
    xs, evalxs = x_evalx("Value()", [], [])
    x_evalx("Value(1)", xs, evalxs)
    x_evalx("Value('Py', 1).set('Co', 2)", xs, evalxs)
    x_evalx("Value(3).set('Py', 1).set('Co', 2)", xs, evalxs)

    for x, evalx in zip(xs, evalxs):
        my_x = str(evalx)
        assert_eq(my_x, x)

    assert_eq(evalxs[1].as_constant(), 1)
    assert_eq(evalxs[2].as_constant('Py'), 1)
    assert_eq(evalxs[2].as_constant('Co'), 2)
    assert_eq(evalxs[3].as_constant('Py'), 1)
    assert_eq(evalxs[3].as_constant('Co'), 2)
    assert_eq(evalxs[3].as_constant('Fe'), 3)

    assert_eq(evalxs[3].get_set_plan(['Fe', 'Py', 'Co', 'Dy'], 1.0),
              [('Fe', 3, None), ('Py', 1, None), ('Co', 2, None),
               ('Dy', 3, None)])

    expect_exception(evalxs[3].get_set_plan, ValueError)
    expect_exception(lambda: evalxs[0].get_set_plan(['Py']), ValueError)

if __name__ == "__main__":
    test_all()
