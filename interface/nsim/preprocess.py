"""
A simple string preprocessor mechanism.
"""

import re

_var_re = re.compile("[$][^$]*[$]")

def preprocess(src, var, max_recursion=10, quiet=False):
    """
    A string preprocessor.
    The function takes a source string src and replaces all the variables
    with the corresponding values. The names of the variables and their values
    are specified using the dictionary var. The substitution is recursive:

      var = {"VAR1":"1.0", "VAR2":"2*$VAR2$"}
      preprocess("$VAR1$, $VAR2$", var)

    will return the string "1.0, 2*1.0".
    There is a maximum recursion level, though.
    This is intended to avoid infinite recursion, such as what would happen
    with var = {"INFINITY":"$INFINITY$+1"}.
    'quiet' controls whether an error should be reported or not, in case
    the variable is not found or the maximum recursion depth is reached.
    """
    def substitutor(v):
        try:
            whole_var_name = v.group(0)
            var_name = whole_var_name[1:-1]
        except:
            raise "Wrong variable specification: '%s'" % var
        if var.has_key(var_name):
            subst = str(var[var_name])
            if "$" in subst:
                if max_recursion < 1:
                    if quiet: return whole_var_name
                    raise "preprocess reached the maximum recursion depth!"
                subst = preprocess(subst, var, max_recursion=max_recursion-1,
                                   quiet=quiet)
            return subst
        if quiet: return whole_var_name
        raise "Variable '%s' not found!" % var_name

    return re.sub(_var_re, substitutor, src)

def concat_terms(term, index_vals, concat="", quiet=False):
    """
    Concat the same term for a set of different values
    of the provided indices.

      concat_term("+$i$ ", [[("i", i)] for i in range(5)])

    returns the string "+0+1+2+3+4".

      idx = []
      colors = ["red", "blue"]
      sizes = ["big", "small"]
      for c in colors:
          for s in sizes:
              idx.append([("color", c), ("size", s)])
      print concat_terms("$color$_$size$", idx, concat=" + ")

    returns the string "red_big + red_small + blue_big + blue_small"
    """
    # Collect all the indices names
    index_dict = {}
    for index_val in index_vals:
        for (index_name, _) in index_val:
            index_dict[index_name] = None

    terms = []
    for index_val in index_vals:
        for (index_name, index_value) in index_val:
            index_dict[index_name] = index_value
        terms.append(preprocess(term, index_dict, quiet=quiet))

    return concat.join(terms)


if __name__ == "__main__":
    print preprocess("$VAR1$, $VAR2$", {"VAR1":"2*$VAR2$", "VAR2":"$VAR1$"}, quiet=True)
    print concat_terms("$i$", [[("i", i)] for i in range(5)], concat=" + ")

    idx = []
    colors = ["red", "blue"]
    sizes = ["big", "small"]
    for c in colors:
        for s in sizes:
            idx.append([("color", c), ("size", s)])
    print concat_terms("$color$_$size$", idx, concat=" + ")
