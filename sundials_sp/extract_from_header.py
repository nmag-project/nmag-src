all_lib_and_funcs = {}

all_lib_and_funcs["/usr/include/cvode.h"] = (
  "libsundials_cvode",
  [
    # All these functions:
    #   "CVodeGetNumRhsEvals", "CVodeGetNumLinSolvSetups",
    #   "CVodeGetNumErrTestFails", "CVodeGetLastOrder", "CVodeGetCurrentOrder",
    #   "CVodeGetCurrentTime",
    # do not need to be exported, because the function CVodeGetIntegratorStats
    # returns all their output.
    "CVode", "CVodeFree", "CVodeCreate", "CVodeMalloc", "CVodeReInit",
    "CVodeSetFdata", "CVodeSetMaxOrd", "CVodeSetMaxNumSteps",
    "CVodeSetInitStep", "CVodeSetMinStep", "CVodeSetMaxStep",
    "CVodeSetTolerances", "CVodeSetStopTime", "CVodeGetNumSteps",
    "CVodeGetLastStep", "CVodeGetCurrentStep", "CVodeGetActualInitStep",
    "CVodeGetWorkSpace", "CVodeGetNumStabLimOrderReds",
    "CVodeGetTolScaleFactor", "CVodeGetErrWeights", "CVodeGetEstLocalErrors",
    "CVodeGetNumNonlinSolvIters", "CVodeGetNumNonlinSolvConvFails",
    "CVodeGetIntegratorStats", "CVodeGetNonlinSolvStats", "CVodeGetRootInfo",
    "CVodeGetNumGEvals", "CVodeGetReturnFlagName",
  ]
)

all_lib_and_funcs["/usr/include/cvode/cvode_spils.h"] = (
  "libsundials_cvode",
  ["CVSpilsSetJacTimesVecFn", "CVSpilsSetPreconditioner",
   "CVSpilsGetNumLinIters", "CVSpilsGetNumConvFails",
   "CVSpilsGetNumPrecEvals", "CVSpilsGetNumPrecSolves",
   "CVSpilsGetNumJtimesEvals", "CVSpilsGetNumRhsEvals"
  ]
)

all_lib_and_funcs["/usr/local/include/nvector_serial.h"] = (
  "libsundials_nvec_serial",
  ["N_VMake_Serial", "N_VDestroy_Serial"]
)


all_lib_and_funcs["/usr/local/include/nvector_parallel.h"] = (
  "libsundials_nvec_parallel",
  ["N_VMake_Parallel", "N_VDestroy_Parallel"]
)

all_lib_and_funcs["/usr/include/cvode/cvode_spgmr.h"] = (
  "libsundials_cvode",
  ["CVSpgmr"])

import sys, re

def extract_from_header(header_file, library_name, used_functions, fntable):
  # Read file into the variable content
  f = open(header_file, 'r')
  content = f.read()
  f.close()

  # This is the only point where the strategy reveals a weakness :(
  content = content.replace('extern "C" {', '')

  # Remove preprocessor directives
  preproc_re = re.compile("\n#.*")
  content = re.sub(preproc_re, '', content)

  # Remove comments from C-header file
  comment_re = re.compile("[/][*][^*]*[*]+([^/*][^*]*[*]+)*[/]")
  content = re.sub(comment_re, '', content)

  # Removes end of line characters
  #content = content.replace('\n', '')

  ls = content.split(";")
  found = {}
  for l in ls:
    for fn in used_functions:
      if fn in l:
        if (l[l.find(fn)+len(fn)]).isalpha(): continue # Searching just for words
        dn = "dyn_%s" % fn
        tn = "(*%s)" % dn
        decl = "static " + l.strip().replace(fn, tn) + " = 0;\n"
        fntable.append([decl, library_name, fn, dn])
        found[fn] = True

  for fn in used_functions:
    if not found.has_key(fn):
      print "Warning: %s not found in %s!" % (fn, header_file)

  for function_name in used_functions:
    re.compile("$[.]+%s[.]*" % function_name)

fntable = []
for header in all_lib_and_funcs:
  lib_name, used_functions = all_lib_and_funcs[header]
  extract_from_header(header, lib_name, used_functions, fntable)

out = open("sundials_fntable.c", "w")
out.write(
"""
/* This file was generated automatically by extract_from_header.py
   and needs to be reviewed with care before being used in the sundials
   ocaml module.
 */

"""
)

# Write the declaration of the dyn_ symbols (pointers to functions)
for x in fntable: out.write(x[0])

out.write(
"""
typedef struct {
    void **library;
    char *sym_name;
    void **target;
} SundialsFnTable;

static SundialsFnTable symbols_table[] = {
"""
)


for _, lib_name, fun_name, dyn_name in fntable:
  out.write("  {& %s, \"%s\", (void **) & %s},\n" % (lib_name, fun_name, dyn_name))

out.write(
"""  {(void **) 0, (char *) 0, (void *) 0}
};
"""
)

out.close()
