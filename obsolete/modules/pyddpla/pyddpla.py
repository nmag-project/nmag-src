""" Python-side extensions for pyddpla

    (C) 2009 Dr. Thomas Fischbacher
"""

def region(nr=1,
           tensors=[],
           properties=[]):
    import re
    properties=properties[:]
    # "magical" properties (automatically defined):
    if (not "INSIDE" in properties) and nr >0:
        properties.append("INSIDE")
    if (not "OUTSIDE" in properties) and nr <0:
        properties.append("OUTSIDE")

    rx_tensor=re.compile(r"^\s*([a-zA-Z0-9_]+)(#([0-9]+))?(\((\s*[0-9]+\s*(,\s*[0-9]+)*)\))?\s*$")

    def translate_tensor(str):
        x=re.search(rx_tensor,str)
        if not x:
            raise ValueError, "Bad tensor spec: %s"%repr(str)
        (stem, str_opt_order_all, str_opt_order, \
         str_paren_ixranges, str_ixranges, tail) = x.groups()
        ixranges=re.findall("[0-9]+",str_ixranges)
        order=1
        if str_opt_order <> "":
            order=int(str_opt_order)
        
        result=[stem, order]
        result.extend([int(r) for r in ixranges])
        return tuple(result)

    return {"NR":nr,
            "TENSORS":[translate_tensor x for x in tensors],
            "PROPERTIES":properties,
            }

