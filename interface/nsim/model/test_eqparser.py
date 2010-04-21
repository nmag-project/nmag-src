from eqparser import parse

def test_consistency():
    strings = ["%range j:3, k:3; \nH_t(j) <- H_ext(j) - m*H_exch(j, 1) + m/H_anis(j);",
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
        assert backnforth == string, ("Original='%s' but parsed ='%s'"
                                      % (string, backnforth))
        print "passed"

if __name__ == "__main__":
    #p = parse("%range j:35; a <- b;")
    #print p
    #p.debug()
    #import sys
    #sys.exit(0)
    test_consistency()
