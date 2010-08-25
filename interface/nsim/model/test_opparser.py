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

from opparser import parse, parse_region_logic
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
               "2*<a||d/dxj b> + (-1.23)*<c||d/dxj b>, j:2",
               "-<rho||d/dxj m(j)>, j:3",
               (" -<d/dxj phi[not outer] || d/dxj phi[not outer]>;"
                "phi[outer]=phi[outer], j:3"),
               ("<d/dxj phi[not outer] || d/dxj phi[outer]>; "
                "(L||R)=(*||phi[outer]), j:3")]
    for string in strings:
        backnforth = str(parse(string))
        assert compare_strings(backnforth, string), \
          "Original='%s' but parsed ='%s'." % (string, backnforth)
        print "passed"

def test_simplify():
    print "Testing simplification"
    strings = [("0*<a||b>", ""),
               ("0*<a||b> + 1*<a||b>", "<a||b>"),]
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
    rho = SpaceField("rho", [])
    M_sat = Constant("M_sat", [], subfields=True,
                     value=Value("mat1", 1e6).set("mat2", 0.5e6))
    m = SpaceField("m", [3], subfields=True)
    qs = [C, a, b, H_exch, m, rho, M_sat]
    context = OpSimplifyContext(quantities=Quantities(qs),
                                material=["mat1", "mat2"])
    strings = [("<a||b>", "<a_mat1||b> + <a_mat2||b>"),
               ("C*<d/dxj H_exch(k)||d/dxj m(k)>, j:1, k:3",
                "  (-1.25)*<d/dxj H_exch_mat1(k)||d/dxj m_mat1(k)> "
                "+ (-2.5)*<d/dxj H_exch_mat2(k)||d/dxj m_mat2(k)>,j:1, k:3"),
                ("M_sat*<rho||d/dxj m(j)> + M_sat*<rho||D/Dxj m(j)>, j:3",
                 " 1000000.0*<rho||d/dxj m_mat1(j)> + "
                 "1000000.0*<rho||D/Dxj m_mat1(j)> + "
                 "500000.0*<rho||d/dxj m_mat2(j)> + "
                 "500000.0*<rho||D/Dxj m_mat2(j)>, j:3")]

    for string, result in strings:
        parse_tree = parse(string).simplify(context=context)
        my_result = str(parse_tree)
        assert compare_strings(my_result, result), \
          ("Simplified of '%s' is '%s', but '%s' is expected."
           % (string, my_result, result))
        print "passed"

def test_region_logic():
    print "Testing region logic parser"
    strings = [("123", "123"),
               ("outer", "outer"),
               ("not outer", "not outer"),
               ("not inside and not outside", "not inside and not outside"),
               ("some=123", "123"),
               ("some=thing", "thing"),
               ("some=123 and not 321", "123 and not 321"),
               ("all=123 and not 321", "all=123 and not 321"),
               ("not (in and not out)", "not (in and not out)"),]

    for string, result in strings:
        parse_tree = parse_region_logic(string) #.simplify(context=context)
        my_result = str(parse_tree)
        assert compare_strings(my_result, result), \
          ("Simplified of '%s' is '%s', but '%s' is expected."
           % (string, my_result, result))
        print "passed"

if __name__ == "__main__":
    test_consistency()
    test_simplify()
    test_simplify_quantities()
    test_region_logic()
