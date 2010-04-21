from eqparser import parse

def test_consistency():
    strings = ["%range j:3, k:3; \nH_t(j) <- H_ext(j) - m*H_exch(j, 1) + m/H_anis(j);",
               "\na <- b + c;"]
    for string in strings:
        backnforth = str(parse(string))
        assert backnforth == string, ("Original='%s' but parsed ='%s'"
                                      % (string, backnforth))

if __name__ == "__main__":
    test_consistency()
