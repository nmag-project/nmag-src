import re

try:
    import ocaml
    parse = ocaml.parse_physical_dimensions

except:
    scanner = re.Scanner([(r"\s+", None),
                          (r"[-+]?[0-9]+", lambda _, t: ("INT", int(t))),
                          (r"[a-zA-Z_][a-zA-Z0-9]*",
                           lambda _, t: ("UNIT", t)),
                          (r"\^", lambda _, t: "POW"),
                          (r"/", lambda _, t: "DIV"),
                          (r"[(]", lambda _, t: "LPAR"),
                          (r"[)]", lambda _, t: "RPAR")])


    def parse_unitpow(ts, known_units):
        pows = [0]*len(known_units)
        m = len(ts)
        i = 0
        while i < m:
            this = ts[i]
            if this[0] == "INT" and this[1] == 1:
                # such as in 1/s
                i += 1
                continue

            elif this[0] != "UNIT":
                raise "Parse error"

            next = None
            if i+1 < m:
                next = ts[i+1]
            if next == "POW":
                integer = ts[i + 2]
                if integer[0] != "INT":
                    raise "Parse error"
                ki = known_units.index(this[1])
                pows[ki] += integer[1]
                i += 3
            else:
                ki = known_units.index(this[1])
                pows[ki] += 1
                i += 1
        return pows

    def parse(dimension, known_units):
        tokens, remainder = scanner.scan(dimension)
        if len(remainder) > 0:
            raise "Parse error"
        try:
            div_pos = tokens.index("DIV")
            left = tokens[:div_pos]
            right = tokens[div_pos+1:]
        except:
            left = tokens
            right = []
        pows_up = parse_unitpow(left, known_units)
        pows_down = parse_unitpow(right, known_units)
        c = [pu - pd for pu, pd in zip(pows_up, pows_down)]
        return c

