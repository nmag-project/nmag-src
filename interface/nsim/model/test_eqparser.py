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

from eqparser import parse
from eqtree import SimplifyContext

def condensed_string(s):
    return s.replace(" ", "").replace("\n", "")

def compare_strings(s1, s2):
    return condensed_string(s1) == condensed_string(s2)

def test_consistency():
    print "Testing that parsed tree can be translated to original string"
    strings = ["%range j:3, k:3; \nH_t(j) <- H_ext(j) - m*H_exch(j, 1) + m/H_anis(j);",
               "%range j:3, k:3; \nH_t(j) <- H_ext(j) - (m*H_exch(j, 1) + m/H_anis(j))*x;",
               "\na <- b + c;",
               "\na <- 1.0*b*c;",
               "\na <- 1.0 - 1.0*b*c;",
               "\na <- -a - 1.0*b*c;",
               "\na <- -1.23;",
               "\na <- -1.23 - 1.0*b*c;",
               "%local m(1); %range a:1, b:2; %range c:3; \na <- b;",
               "%local m(1, 2), bla(4, 5); %local h(12); %range a:1, b:2; "
               "%range c:3; \na <- b;"]
    for string in strings:
        backnforth = str(parse(string))
        assert backnforth == string, ("Original='%s' but parsed ='%s'."
                                      % (string, backnforth))
        print "passed"

def test_simplify():
    print "Test that simplification works"
    strings = [("a <- 0;", "a <- 0.0;"),
               ("abc <- def;", "abc <- def;"),
               ("a <- 0*(b + c);", "a <- 0.0;"),
               ("a <- 1*(b + c);", "a <- (b + c);"),
               ("a <- 2*(b + c);", "a <- 2.0*(b + c);"),
               ("a <- 2*(b + c)*2.5;", "a <- 5.0*(b + c);"),
               ("a <- 2*(b + c)*2.5/5.0;", "a <- (b + c);"),
               ("a <- 1*(1*b + c*1)*1/1;", "a <- (b + c);"),
               ("a <- 1*(1*b + c*1)*0/1;", "a <- 0.0;"),
               ("a <- (2);", "a <- 2.0;"),
               ("a <- 1*(0.5 + b - (5 - 4)/2);", "a <- b;"),
               ("a <- -1*-1;", "a <- 1.0;"),
               ("a <- --1;", "a <- 1.0;"),]
    for string, result in strings:
        my_result = str(parse(string).simplify()).replace("\n", "")
        assert my_result == result, ("Simplified of '%s' is '%s', but '%s' "
                                     "is expected."
                                     % (string, my_result, result))
        print "passed"

def test_simplify_quantities():
    from quantity import Constant, SpaceField, Quantities
    zero = Constant("zero", def_on_material=False, value=0.0)
    gamma = Constant("gamma", def_on_material=False, value=1.23)

    m = SpaceField("m", [3], def_on_material=True)
    H_ext = SpaceField("H_ext", [3])
    context = SimplifyContext(quantities=Quantities([gamma, m, H_ext, zero]))
    strings = [("m(0) <- -zero*(m(1)*H_ext(2) - m(2)*H_ext(1));",
                "m(0) <- 0.0;")]
    for string, result in strings:
        parse_tree = parse(string).simplify(context=context)
        my_result = str(parse_tree).replace("\n", "")
        assert my_result == result, ("Simplified of '%s' is '%s', but '%s' "
                                     "is expected."
                                     % (string, my_result, result))
        print "passed"

def test_llg():
    from quantity import Constant, SpaceField, Quantities
    C1 = Constant("C1", def_on_material=False, value=-0.17688)
    C2 = Constant("C2", def_on_material=False, value=-0.08844)
    C3 = Constant("C3", def_on_material=False, value=0.1)
    C4 = Constant("C4", def_on_material=False, value=0.0)
    C5 = Constant("C5", def_on_material=False, value=0.0)
    m = SpaceField("m", [3], def_on_material=True)
    dmdt = SpaceField("dmdt", [3], def_on_material=True)
    dm_dcurrent = SpaceField("dm_dcurrent", [3], def_on_material=False)
    pin = SpaceField("pin", def_on_material=False)
    H_total = SpaceField("H_total", [3], def_on_material=True)
    quantities = Quantities([C1, C2, C3, C4, C5, m, dmdt, dm_dcurrent, pin,
                             H_total])

    eq_rhs = \
      """%range i:3, j:3, k:3, p:3, q:3;
      dmdt(i) <- C1 * eps(i,j,k) * m(j) * H_total(k) * pin
               + C2 * eps(i,j,k) * m(j) * eps(k,p,q) * m(p) * H_total(q) * pin
               + C3 * (1.0 - m(j)*m(j)) * m(i) * pin
               + C4 * eps(i,j,k) * m(j) * eps(k,p,q) * m(p) * dm_dcurrent(q) * pin
               + C5 * eps(i,j,k) * m(j) * dm_dcurrent(k) * pin;"""

    result = ("%range i:3, j:3, k:3, p:3, q:3;"
              "dmdt_Py(i) <- -0.17688 * eps(i,j,k) * m_Py(j) * H_total_Py(k) * pin "
              "+ -0.08844 * eps(i,j,k) * m_Py(j) * eps(k,p,q) * m_Py(p) "
              "  * H_total_Py(q) * pin "
              "+ 0.1 * (1.0 - m_Py(j)*m_Py(j)) * m_Py(i) * pin;")

    context = SimplifyContext(quantities=quantities, material='Py')
    parse_tree = parse(eq_rhs).simplify(context=context)
    my_result = str(parse_tree).replace("\n", "")
    assert compare_strings(my_result, result), \
     ("Simplified of '%s' is '%s', but '%s' is expected."
      % (eq_rhs, my_result, result))
    print "passed"

def test_llg_multimaterial():
    from quantity import Constant, SpaceField, Quantities
    C1 = Constant("C1", def_on_material=False, value=-0.17681384)
    C2 = Constant("C2", def_on_material=False, value=-0.08840692)
    C3 = Constant("C3", def_on_material=False, value=0.1)
    C4 = Constant("C4", def_on_material=False, value=0.0)
    C5 = Constant("C5", def_on_material=False, value=0.0)
    m = SpaceField("m", [3], def_on_material=True)
    dmdt = SpaceField("dmdt", [3], def_on_material=True)
    dm_dcurrent = SpaceField("dm_dcurrent", [3], def_on_material=False)
    pin = SpaceField("pin", def_on_material=False)
    H_total = SpaceField("H_total", [3], def_on_material=True)
    quantities = Quantities([C1, C2, C3, C4, C5, m, dmdt, dm_dcurrent, pin,
                             H_total])

    eq_rhs = """%range i:3, j:3, k:3, p:3, q:3;
      dmdt(i) <- C1 * eps(i,j,k) * m(j) * H_total(k) * pin
               + C2 * eps(i,j,k) * m(j) * eps(k,p,q) * m(p) * H_total(q) * pin
               + C3 * (1.0 - m(j)*m(j)) * m(i) * pin
               + C4 * eps(i,j,k) * m(j) * eps(k,p,q) * m(p) * dm_dcurrent(q) * pin
               + C5 * eps(i,j,k) * m(j) * dm_dcurrent(k) * pin;"""

    result = condensed_string("""%range i:3, j:3, k:3, p:3, q:3;
      dmdt_Py(i) <-
          -0.17681384*eps(i,j,k)*m_Py(j)*H_total_Py(k)*pin
        + -0.08840692*eps(i,j,k)*m_Py(j)*eps(k,p,q)*m_Py(p)*H_total_Py(q)*pin
        + 0.1*(1.0 - m_Py(j)*m_Py(j))*m_Py(i)*pin;

      dmdt_Co(i) <-
          -0.17681384*eps(i,j,k)*m_Co(j)*H_total_Co(k)*pin
        + -0.08840692*eps(i,j,k)*m_Co(j)*eps(k,p,q)*m_Co(p)*H_total_Co(q)*pin
        + 0.1*(1.0 - m_Co(j)*m_Co(j))*m_Co(i)*pin;""")

    context = SimplifyContext(quantities=quantities, material=['Py', 'Co'])
    parse_tree = parse(eq_rhs).simplify(context=context)
    my_result = condensed_string(str(parse_tree))
    assert compare_strings(my_result, result), \
     ("Simplified of '%s' is '%s', but '%s' is expected."
      % (eq_rhs, my_result, result))
    print "passed"

if __name__ == "__main__":
    test_consistency()
    test_simplify()
    test_simplify_quantities()
    test_llg()
    test_llg_multimaterial()
