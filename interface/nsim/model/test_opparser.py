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

from opparser import parse
from optree import OpSimplifyContext
from value import Value

def condensed_string(s):
    return s.replace(" ", "").replace("\n", "")

def compare_strings(s1, s2):
    return condensed_string(s1) == condensed_string(s2)

def test_consistency():
    print "Testing that parsed tree can be translated to original string"
    strings = [" 1.25*<rho||d/dxj m(j)> + 4.5*<rho||D/Dxj m(j)>, j:3",
               "(-24.0583053511)*<d/dxj H_exch_Py(k)||d/dxj m_Py(k)>,j:3, k:3",
               "<a(i, j)||d/dxj b(k)>, i:1, j:2, k:3",
               "<a(i, j)||d/dxj b(k)> + <c(i)||d/dxj b(k, j)>, i:1, j:2, k:3",
               "2*<a||d/dxj b> + (-1.23)*<c||d/dxj b>, j:2",]
    for string in strings:
        backnforth = str(parse(string))
        assert compare_strings(backnforth, string), \
          "Original='%s' but parsed ='%s'." % (string, backnforth)
        print "passed"

def test_simplify():
    print "Testing simplification"
    strings = [] #("a <- 0;", "a <- 0.0;"),
               #("abc <- def;", "abc <- def;"),
               #("a <- 0*(b + c);", "a <- 0.0;"),
               #("a <- 1*(b + c);", "a <- (b + c);"),
               #("a <- 2*(b + c);", "a <- 2.0*(b + c);"),
               #("a <- 2*(b + c)*2.5;", "a <- 5.0*(b + c);"),
               #("a <- 2*(b + c)*2.5/5.0;", "a <- (b + c);"),
               #("a <- 1*(1*b + c*1)*1/1;", "a <- (b + c);"),
               #("a <- 1*(1*b + c*1)*0/1;", "a <- 0.0;"),
               #("a <- (2);", "a <- 2.0;"),
               #("a <- 1*(0.5 + b - (5 - 4)/2);", "a <- b;"),
               #("a <- -1*-1;", "a <- 1.0;"),
               #("a <- --1;", "a <- 1.0;"),]
    for string, result in strings:
        my_result = str(parse(string).simplify())
        assert compare_strings(my_result, result), \
          ("Simplified of '%s' is '%s', but '%s' is expected."
           % (string, my_result, result))
        print "passed"

def test_simplify_quantities():
    print "Testing simplification of quantities"
    from quantity import Constant, SpaceField, Quantities
    zero = Constant("zero", subfields=False, value=Value(0.0))
    C = Constant("C", subfields=True,
                 value=Value("mat1", -1.25).set("mat2", -2.5))

    a = SpaceField("a", [3], subfields=True)
    b = SpaceField("b", [3])
    H_exch = SpaceField("H_exch", [3], subfields=True)
    m = SpaceField("m", [3], subfields=True)
    context = OpSimplifyContext(quantities=Quantities([C, a, b, H_exch, m]),
                                material=["mat1", "mat2"])
    strings = [("<a||b>", "<a_mat1||b> + <a_mat2||b>"),
               ("C*<d/dxj H_exch(k)||d/dxj m(k)>, j:1, k:3",
                "  (-1.25)*<d/dxj H_exch_mat1(k)||d/dxj m_mat1(k)> "
                "+ (-2.5)*<d/dxj H_exch_mat2(k)||d/dxj m_mat2(k)>,j:1, k:3")]

    for string, result in strings:
        parse_tree = parse(string).simplify(context=context)
        my_result = str(parse_tree)
        assert compare_strings(my_result, result), \
          ("Simplified of '%s' is '%s', but '%s' is expected."
           % (string, my_result, result))
        print "passed"

#def test_llg():
    #print "Testing LLG single material"
    #from quantity import Constant, SpaceField, Quantities
    #C1 = Constant("C1", subfields=False, value=Value(-0.17688))
    #C2 = Constant("C2", subfields=False, value=Value(-0.08844))
    #C3 = Constant("C3", subfields=False, value=Value(0.1))
    #C4 = Constant("C4", subfields=False, value=Value(0.0))
    #C5 = Constant("C5", subfields=False, value=Value(0.0))
    #m = SpaceField("m", [3], subfields=True)
    #dmdt = SpaceField("dmdt", [3], subfields=True)
    #dm_dcurrent = SpaceField("dm_dcurrent", [3], subfields=False)
    #pin = SpaceField("pin", subfields=False)
    #H_total = SpaceField("H_total", [3], subfields=True)
    #quantities = Quantities([C1, C2, C3, C4, C5, m, dmdt, dm_dcurrent, pin,
                             #H_total])

    #eq_rhs = \
      #"""%range i:3, j:3, k:3, p:3, q:3;
      #dmdt(i) <- C1 * eps(i,j,k) * m(j) * H_total(k) * pin
               #+ C2 * eps(i,j,k) * m(j) * eps(k,p,q) * m(p) * H_total(q) * pin
               #+ C3 * (1.0 - m(j)*m(j)) * m(i) * pin
               #+ C4 * eps(i,j,k) * m(j) * eps(k,p,q) * m(p) * dm_dcurrent(q) * pin
               #+ C5 * eps(i,j,k) * m(j) * dm_dcurrent(k) * pin;"""

    #result = ("%range i:3, j:3, k:3, p:3, q:3;"
              #"dmdt_Py(i) <- -0.17688 * eps(i,j,k) * m_Py(j) * H_total_Py(k) * pin "
              #"+ -0.08844 * eps(i,j,k) * m_Py(j) * eps(k,p,q) * m_Py(p) "
              #"  * H_total_Py(q) * pin "
              #"+ 0.1 * (1.0 - m_Py(j)*m_Py(j)) * m_Py(i) * pin;")

    #context = OpSimplifyContext(quantities=quantities, material='Py')
    #parse_tree = parse(eq_rhs).simplify(context=context)
    #my_result = str(parse_tree).replace("\n", "")
    #assert compare_strings(my_result, result), \
     #("Simplified of '%s' is '%s', but '%s' is expected."
      #% (eq_rhs, my_result, result))
    #print "passed"

#def test_llg_multimaterial():
    #print "Testing LLG multi-material"
    #from quantity import Constant, SpaceField, Quantities
    #C1 = Constant("C1", subfields=False, value=Value(-0.17681384))
    #C2 = Constant("C2", subfields=False, value=Value(-0.08840692))
    #C3 = Constant("C3", subfields=False, value=Value(0.1))
    #C4 = Constant("C4", subfields=False, value=Value(0.0))
    #C5 = Constant("C5", subfields=False, value=Value(0.0))
    #m = SpaceField("m", [3], subfields=True)
    #dmdt = SpaceField("dmdt", [3], subfields=True)
    #dm_dcurrent = SpaceField("dm_dcurrent", [3], subfields=False)
    #pin = SpaceField("pin", subfields=False)
    #H_total = SpaceField("H_total", [3], subfields=True)
    #quantities = Quantities([C1, C2, C3, C4, C5, m, dmdt, dm_dcurrent, pin,
                             #H_total])

    #eq_rhs = """%range i:3, j:3, k:3, p:3, q:3;
      #dmdt(i) <- C1 * eps(i,j,k) * m(j) * H_total(k) * pin
               #+ C2 * eps(i,j,k) * m(j) * eps(k,p,q) * m(p) * H_total(q) * pin
               #+ C3 * (1.0 - m(j)*m(j)) * m(i) * pin
               #+ C4 * eps(i,j,k) * m(j) * eps(k,p,q) * m(p) * dm_dcurrent(q) * pin
               #+ C5 * eps(i,j,k) * m(j) * dm_dcurrent(k) * pin;"""

    #result = condensed_string("""%range i:3, j:3, k:3, p:3, q:3;
      #dmdt_Py(i) <-
          #-0.17681384*eps(i,j,k)*m_Py(j)*H_total_Py(k)*pin
        #+ -0.08840692*eps(i,j,k)*m_Py(j)*eps(k,p,q)*m_Py(p)*H_total_Py(q)*pin
        #+ 0.1*(1.0 - m_Py(j)*m_Py(j))*m_Py(i)*pin;

      #dmdt_Co(i) <-
          #-0.17681384*eps(i,j,k)*m_Co(j)*H_total_Co(k)*pin
        #+ -0.08840692*eps(i,j,k)*m_Co(j)*eps(k,p,q)*m_Co(p)*H_total_Co(q)*pin
        #+ 0.1*(1.0 - m_Co(j)*m_Co(j))*m_Co(i)*pin;""")

    #context = OpSimplifyContext(quantities=quantities, material=['Py', 'Co'])
    #parse_tree = parse(eq_rhs).simplify(context=context)
    #my_result = condensed_string(str(parse_tree))
    #assert compare_strings(my_result, result), \
     #("Simplified of '%s' is '%s', but '%s' is expected."
      #% (eq_rhs, my_result, result))
    #print "passed"

if __name__ == "__main__":
    test_consistency()
    test_simplify()
    test_simplify_quantities()
    #test_llg()
    #test_llg_multimaterial()
