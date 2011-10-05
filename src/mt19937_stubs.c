/* 
   (C) 2005 Dr. Thomas Fischbacher

   Mersenne-Twister random numbers.
   
 */

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

#include <stdlib.h>
/* For malloc() */

#include "mt19937.h"

static void finalize_mt19937_rng(value block)
{
  /* Judging from otherlibs/sythreads/win32.c's caml_thread_finalize
     function in the ocaml sources, we do not have to make block a GC
     root...
   */

  mt19937_rng *c;

  c=(mt19937_rng *)Field(block,1);

  free(c);
}


CAMLprim value caml_mt19937_new_rng(value ml_seed)
{
  CAMLparam1(ml_seed);

  mt19937_rng *ctx=mt19937_new_rng(malloc);

  mt19937_sgenrand(ctx,Int_val(ml_seed));

  CAMLlocal1(block);

  block=alloc_final(2,finalize_mt19937_rng,
		    sizeof(mt19937_rng *),
		    sizeof(mt19937_rng *));
  
  Field(block,1)=(value)ctx;

  CAMLreturn(block);
}


CAMLprim value caml_mt19937_init(value ml_ctx, value ml_seed)
{
  CAMLparam2(ml_ctx, ml_seed);
  mt19937_rng *ctx;

  ctx=(mt19937_rng *)(Field(ml_ctx,1));

  mt19937_sgenrand(ctx,Int_val(ml_seed));
  
  CAMLreturn(Val_unit);
}

/* We may want to think about providing access to mt19937_lsgenrand()
   to initialize the RNG with more entropy. However, a "good"
   interface (which allows us to have full control over entropy) would
   either have to use int32 parameters, which are a bit awkward to use,
   or we would have to re-map bits, which also is awkward.

   For now, we just do not provide a second initialization function,
   as there should be little need for one anyway. (After all, if you
   need true entropy, take a RNG source provided by the kernel, like
   Linux' /dev/urandom or /dev/random)
*/

CAMLprim value caml_mt19937_genrand_double(value ml_ctx, value ml_max)
{
  CAMLparam2(ml_ctx, ml_max);
  mt19937_rng *ctx;

  double max,result;

  max=Double_val(ml_max);

  ctx=(mt19937_rng *)Field(ml_ctx,1);

  result=max*mt19937_genrand_double(ctx);
  
  CAMLreturn(copy_double(result));
}

/* Note: the function above performs number consing.

   To alleviate that problem somewhat, we provide a second interface
   which fills a pre-allocated array of double-floats with random numbers.

 */

CAMLprim value caml_mt19937_genrand_double_fillarray(value ml_ctx, value ml_max, value ml_arr)
{
  CAMLparam3(ml_ctx, ml_max, ml_arr);
  mt19937_rng *ctx;
  int nr_entries,i;
  double rnd,max;

  max=Double_val(ml_max);

  ctx=(mt19937_rng *)Field(ml_ctx,1);

  nr_entries=Wosize_val(ml_arr)/Double_wosize;

  for(i=0;i<nr_entries;i++)
    {
      rnd=max*mt19937_genrand_double(ctx);
      Store_double_field(ml_arr,i,rnd);
    }
  
  CAMLreturn(Val_unit);
}


CAMLprim value caml_mt19937_genrand_int(value ml_ctx, value ml_max)
{
  CAMLparam2(ml_ctx,ml_max);
  mt19937_rng *ctx;
  unsigned int rnd,max;

  max=Int_val(ml_max);

  ctx=(mt19937_rng *)Field(ml_ctx,1);

  rnd=mt19937_genrand_uint(ctx);
  rnd&=0x3fffffff;
  /* On 32-bit machines, OCaml uses 31-bit signed integers, i.e.
     the largest integer is 2^30-1.

     For definiteness, we declare that we always return a value
     in the range [0..2^30-1], even on 64 bit machines. Thus,
     our code will produce identical results across all platforms.

   */
  
  CAMLreturn(Val_int(rnd%max));
  /* Note: when max is around 2^30-1, the distribution will be quite uneven! */
}

