
import nsim
from nsim.si_units import SI
from nsim.si_units import str_of_SI_vector

def test_str_of_SI_vector():
    H_ext=[SI(1000000,['m',-1.0,'A',1.0]), SI(0,['m',-1.0,'A',1.0]), SI(0,['m',-1.0,'A',1.0])]
    assert str_of_SI_vector([SI(0, "A/m"), SI(100, "A/m")]) == "[0 100]<A/m>"
    assert str_of_SI_vector([0, SI(123, "A/m")]) == "[0 123]<A/m>"
    assert str_of_SI_vector([0, SI(123, "A/m"), 0]) == "[0 123 0]<A/m>"
    assert str_of_SI_vector(H_ext) == "[1e+06 0 0]<A/m>"
    assert str_of_SI_vector([SI(1,'A/m')]) == "[1]<A/m>"
    assert str_of_SI_vector(None) == "None"


test_str_of_SI_vector()
    
