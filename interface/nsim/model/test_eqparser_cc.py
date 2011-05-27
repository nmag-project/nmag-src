from eqparser import parse
from eqtree import EqSimplifyContext
from value import Value

def condensed_string(s):
    return s.replace(" ", "").replace("\n", "")

def compare_strings(s1, s2):
    return condensed_string(s1) == condensed_string(s2)

def test_llg():
    print "Testing LLG single material"
    from quantity import Constant, SpaceField, Quantities
    C1 = Constant("C1", subfields=False, value=Value(-0.125))
    C2 = Constant("C2", subfields=False, value=Value(-0.0625))
    C3 = Constant("C3", subfields=False, value=Value(0.25))
    C4 = Constant("C4", subfields=False, value=Value(0.0))
    C5 = Constant("C5", subfields=False, value=Value(0.0))
    m = SpaceField("m", [3], subfields=True)
    dmdt = SpaceField("dmdt", [3], subfields=True)
    dm_dcurrent = SpaceField("dm_dcurrent", [3], subfields=False)
    pin = SpaceField("pin", subfields=False)
    H_total = SpaceField("H_total", [3], subfields=True)
    quantities = Quantities([C1, C2, C3, C4, C5, m, dmdt, dm_dcurrent, pin,
                             H_total])

    eq_rhs = """
      %range i:3;
      (dmdt(i) <-
         (  C1 * (eps(i,j,k) * m(j) * H_total(k))_(j:3,k:3)
          + C2 * (  eps(i,j,k) * m(j)
                  * eps(k,p,q) * m(p) * H_total(q))_(j:3,k:3,p:3,q:3)
          + C3 * (1.0 - (m(j)*m(j))_(j:3)) * m(i)
          + C4 * (  eps(i,j,k) * m(j)
                  * eps(k,p,q) * m(p) * dm_dcurrent(q))_(j:3,k:3,p:3,q:3)
          + C5 * (eps(i,j,k) * m(j) * dm_dcurrent(k))_(j:3,k:3))*pin)_(i:3);"""

    #eq_rhs = """dmdt(0) <- (-m(i))_(i:3);"""

    eq_ccode = """
    if (have_dmdt_a) {
      dmdt_a(0) = (-0.125*(m_a(1)*H_total_a(2) + -1.0*m_a(2)*H_total_a(1)) + -0.0625*(m_a(1)*m_a(0)*H_total_a(1) + -1.0*m_a(1)*m_a(1)*H_total_a(0) + m_a(2)*m_a(0)*H_total_a(2) + -1.0*m_a(2)*m_a(2)*H_total_a(0)) + 0.25*(1.0 - (m_a(0)*m_a(0) + m_a(1)*m_a(1) + m_a(2)*m_a(2)))*m_a(0))*pin;
    };
    if (have_dmdt_a) {
      dmdt_a(1) = (-0.125*(-1.0*m_a(0)*H_total_a(2) + m_a(2)*H_total_a(0)) + -0.0625*(-1.0*m_a(0)*m_a(0)*H_total_a(1) + m_a(0)*m_a(1)*H_total_a(0) + m_a(2)*m_a(1)*H_total_a(2) + -1.0*m_a(2)*m_a(2)*H_total_a(1)) + 0.25*(1.0 - (m_a(0)*m_a(0) + m_a(1)*m_a(1) + m_a(2)*m_a(2)))*m_a(1))*pin;
    };
    if (have_dmdt_a) {
      dmdt_a(2) = (-0.125*(m_a(0)*H_total_a(1) + -1.0*m_a(1)*H_total_a(0)) + -0.0625*(-1.0*m_a(0)*m_a(0)*H_total_a(2) + m_a(0)*m_a(2)*H_total_a(0) + -1.0*m_a(1)*m_a(1)*H_total_a(2) + m_a(1)*m_a(2)*H_total_a(1)) + 0.25*(1.0 - (m_a(0)*m_a(0) + m_a(1)*m_a(1) + m_a(2)*m_a(2)))*m_a(2))*pin;
    };"""

    context = EqSimplifyContext(quantities=quantities, material='a')
    context.expand_indices = True
    parse_tree = parse(eq_rhs).simplify(context=context)
    my_result = parse_tree.get_ccode() #.replace("\n", "")
    assert compare_strings(my_result, eq_ccode), \
     ("Simplified of '%s' is '%s', but '%s' is expected."
      % (eq_rhs, my_result, eq_ccode))
    print "passed"

def test_llg_multimaterial():
  pass


if __name__ == "__main__":
    test_llg()
    test_llg_multimaterial()
