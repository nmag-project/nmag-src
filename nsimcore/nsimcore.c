#include <stdio.h>

#include "Python.h"

#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/fail.h"
#include "caml/callback.h"
#include "caml/custom.h"
#include "caml/alloc.h"


PyMODINIT_FUNC
initnsimcore(void)
{
  char *dummy_argv[] = {0};
  caml_startup(dummy_argv);

  value *create_nsimcore_module = caml_named_value("create_nsimcore_module");
  if (create_nsimcore_module == NULL) {
    fprintf(stderr, "Cannot find create_nsimcore_module.\n");
    return;
  }
  callback(*create_nsimcore_module, Val_unit);
}

