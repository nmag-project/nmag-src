/* 
   (C) 2005 Dr. Thomas Fischbacher
 */

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/intext.h>
#include <caml/bigarray.h>


#include <stdlib.h>
/* Mostly for malloc(), should we need it */

/* Some other generally used includes: */
#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include <stdint.h>
#include <signal.h>

#include <netinet/in.h> 
/* ^ This is to find out byte order */


CAMLprim value caml_7bit_encode_double_into_string(value ml_target, value ml_x)
{
  CAMLparam2(ml_target,ml_x);

  double x;
  int accumulator,acc_fill_state,i,k,ix_inc;
  unsigned char *c, v;

  x=Double_val(ml_x);
  

  /* We assume an ISO8859-x charset and use a subset of the printable
     characters to put 7 valid bits into 1 byte. This means we will
     need two extra bytes to encode one double-float (8 bytes).
     
     The range of printable characters is 33-126 plus 192-255
     (boundaries included).

     Furthermore, we take care that byte order is respected, so that we
     can exchange data files between little-endian and big-endian machines.
     (NOTE: has to be tested!)
  */
  
  c=(unsigned char *)&x;
  accumulator=acc_fill_state=0;

  if(256==htons(256)) /* machine uses NBO */
    {
      ix_inc=1;
    }
  else
    {
      c=&c[7];
      ix_inc=-1;
    }

  for(i=k=0;i<8;i++)
    {

      accumulator = (accumulator<<8) + *c;
      c+=ix_inc;
      /* Note that this is slightly tricky as we may generate a pointer
	 that points outside the string. The C standard actually does not 
	 permit having pointers into nowhere around, but this does not
	 matter here, as we will at worst point into the OCaml header word!
       */

      acc_fill_state+=8;

      while(acc_fill_state>7)
	{
	  v= accumulator>>(acc_fill_state-7);
	  accumulator-= v<<(acc_fill_state-7);
	  acc_fill_state-=7;
	  
	  v+=33;
	  if(v>126)v+=(192-127);
	  Byte(ml_target,k++)=v;
	}
    }

  /* There still is stuff in the accumulator at the end. */

  v=accumulator;
  v+=33;
  if(v>126)v+=(192-127);
  Byte(ml_target,k)=v;

  CAMLreturn(Val_unit);
}

CAMLprim value caml_7bit_decode_string_to_double(value ml_string)
{
  CAMLparam1(ml_string);

  double x=0.0;
  int accumulator,acc_fill_state,i,k,ix_inc;
  unsigned char *c, v, v_store;

    
  c=(unsigned char *)&x;
  accumulator=acc_fill_state=0;

  if(256==htons(256)) /* machine uses NBO */
    {
      ix_inc=1;
    }
  else
    {
      c=&c[7];
      ix_inc=-1;
    }

  for(k=0;k<10;k++)
    {
      v=Byte(ml_string,k);
      if(v>=192)v-=(192-127);
      v-=33;

      accumulator=(accumulator<<7)+v;

      acc_fill_state+=7;

      while(acc_fill_state>8)
	{
	  v_store = accumulator>>(acc_fill_state-8);
	  accumulator -= v_store<<(acc_fill_state-8);
	  acc_fill_state -= 8;
	  
	  *c=v_store;
	  c+=ix_inc;
	}
    }

  /* There still is stuff in the accumulator at the end. */
  *c=accumulator;

  CAMLreturn(copy_double(x));
}

CAMLprim value caml_debugprint_entity(value ml_x)
{
  CAMLparam1(ml_x);
  fprintf(stderr,"CAML value: %lx sizeof(value)=%d sizeof(void*)=%d\n",ml_x,sizeof(value),sizeof(void*));fflush(stderr);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_execution_count(value ml_dummy)
{
  CAMLparam1(ml_dummy);
  uint64_t count=0;
  double xcount=0.0;
 
#ifdef __tune_i686__
  /* XXX actually, this #ifdef is somewhat inappropriate. Should use something better! */

  __asm__ volatile("rdtsc\n\t" : "=A"(count));
#endif
 
  xcount=(double)count;

  CAMLreturn(copy_double(xcount));
}


/* Dump a float into a string. Note: we use native format,
   which might imply endian-ness problems.
*/
CAMLprim value caml_put_double_into_string(value ml_str, value ml_x, value ml_change_byteorder)
{
  CAMLparam3(ml_str,ml_x, ml_change_byteorder);
  double x;
  char *s_dst,*s_src;
  int i, do_change_byteorder;

  do_change_byteorder=Bool_val(ml_change_byteorder);

  if(string_length(ml_str)<8)
    CAMLreturn(Val_unit);	
  /* String too short - maybe we should raise an exception,
     but we just do nothing (rather than crashing the heap) */

  x=Double_val(ml_x);
  s_dst=String_val(ml_str);
  s_src=(char*)&x;
  
  if(do_change_byteorder)
    {
      for(i=0;i<8;i++) 
	{
	  s_dst[7-i]=s_src[i];
	}
    }
  else
    {
      for(i=0;i<8;i++) 
	{
	  s_dst[i]=s_src[i];
	}
    }

  
  CAMLreturn(Val_unit);
}

CAMLprim value caml_read_double_from_string(value ml_str,
					    value ml_change_byteorder)
{
  CAMLparam2(ml_str,ml_change_byteorder);
  double xd;
  float xf;
  char *s;
  int i, do_change_byteorder, len;

  do_change_byteorder=Bool_val(ml_change_byteorder);

  s=String_val(ml_str);
  len=string_length(ml_str);

  if(len==8) /* double-float */
    {
      for(i=0;i<8;i++)
	{
	  ((char *)(&xd))[i]=s[do_change_byteorder?7-i:i];
	}
      CAMLreturn(copy_double(xd));
    }
  else if(len==4) /* single-float */
    {
      for(i=0;i<4;i++)
	{
	  ((char *)(&xf))[i]=s[do_change_byteorder?3-i:i];
	}
      xd=(double)xf;
      CAMLreturn(copy_double(xd));
    }
  else
    {
      CAMLreturn(copy_double(1.0/0.0));
      /* Hope this is a portable way to get a silent NaN... */
    }
}


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

CAMLprim value caml_reset_signal_handler(value ml_nr_signal)
{
  CAMLparam1(ml_nr_signal);
  signal(Int_val(ml_nr_signal),SIG_DFL);
  CAMLreturn(Val_unit);
}
