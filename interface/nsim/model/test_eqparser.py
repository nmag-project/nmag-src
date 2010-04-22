from eqparser import parse

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
    strings = [("a <- 0*(b + c);", "a <- 0.0;"),
               ("a <- 1*(b + c);", "a <- (b + c);"),
               ("a <- 2*(b + c);", "a <- 2.0*(b + c);"),
               ("a <- 2*(b + c)*2.5;", "a <- 5.0*(b + c);"),
               ("a <- 2*(b + c)*2.5/5.0;", "a <- (b + c);"),
               ("a <- 1*(1*b + c*1)*1/1;", "a <- (b + c);"),
               ("a <- 1*(1*b + c*1)*0/1;", "a <- 0.0;"),]
    for string, result in strings:
        my_result = str(parse(string).simplify()).replace("\n", "")
        assert my_result == result, ("Simplified of '%s' is '%s', but '%s' "
                                     "is expected."
                                     % (string, my_result, result))
        print "passed"

if __name__ == "__main__":
    #p = parse("%range j:35; a <- b;")
    #print p
    #p.debug()
    #import sys
    #sys.exit(0)
    test_consistency()
    test_simplify()
