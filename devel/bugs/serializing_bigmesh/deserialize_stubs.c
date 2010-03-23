#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/intext.h>
#include <caml/bigarray.h>

/* See the discussion in:
   http://caml.inria.fr/pub/ml-archives/caml-list/2004/06/2176c54608c3c39e2dbbd9365c2fc6bb.en.html
 */

CAMLprim value caml_marshal_to_bigarray(value v, value flags)
{
  char *buf;
  long len;
  output_value_to_malloc(v, flags, &buf, &len);
  return alloc_bigarray(BIGARRAY_UINT8 | BIGARRAY_C_LAYOUT | BIGARRAY_MANAGED, 
                        1, buf, &len);

  /* Note that BIGARRAY_MANAGED ensures that the finalizer
     will free() the buffer array!
  */
}

CAMLprim value caml_demarshal_from_bigarray(value b)
{
  struct caml_bigarray *b_arr = Bigarray_val(b);
  return input_value_from_block(b_arr->data, b_arr->dim[0]);
}
