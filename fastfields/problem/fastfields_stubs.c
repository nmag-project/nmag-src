
/* (C) 2006 Dr. Thomas Fischbacher */

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

#include <caml/bigarray.h>

#include <dlfcn.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#define Store_c_field(block,offset,x) (Field(block,offset)=(value)x)

typedef int field_function(double *,double *);

static void finalize_dl_handle(value block)
{
  void *dlh;
  dlh=(void *)Field(block,1);

  if(dlh)
    {
      dlclose(dlh);
    }
}

CAMLprim value ocaml_c_wrapped_mkdtemp(value ml_template)
{
  CAMLparam1(ml_template);
  char *s, *s_loc, *s_ret;
  int len;
  s=String_val(ml_template);
  len=strlen(s);

  if(0==(s_loc=(char *)malloc(len+1)))
    {
      fprintf(stderr,"AIEE: malloc failure - aborting!()\n");
      exit(1);
    }

  strncpy(s_loc,String_val(ml_template),len+1);
  s_ret=mkdtemp(s_loc);


  CAMLlocal1(ml_ret);
  ml_ret=copy_string(s_ret);
  free(s_loc);
  
  CAMLreturn(ml_ret);
}


CAMLprim value ocaml_c_fastfield_null_funpointer(value ml_unit)
{
  CAMLparam1(ml_unit);
  CAMLlocal1(block);
  
  block=alloc(1, Abstract_tag);
  
  Store_c_field(block,0,0);
  CAMLreturn(block);
}

CAMLprim value ocaml_c_fastfield_dlopen(value ml_str)
{
  CAMLparam1(ml_str);
  CAMLlocal1(block);
  
  block=alloc_final(2, &finalize_dl_handle,1,100);
  fprintf(stderr,"Calling dlopen(\"%s\")\n",String_val(ml_str));fflush(stderr); /* DDD */
  Store_c_field(block,1,dlopen(String_val(ml_str),RTLD_NOW));
  fprintf(stderr,"Called dlopen(\"%s\")\n",String_val(ml_str));fflush(stderr);
  /* Note: dlopen may have returned the NULL handle. This is okay. */
  CAMLreturn(block);
}

CAMLprim value ocaml_c_fastfield_dlclose(value ml_dl_handle)
{
  CAMLparam1(ml_dl_handle);
  void *dl_handle=(void *)Field(ml_dl_handle,1);

  if(dl_handle)
    {
      fprintf(stderr,"Calling dlclose()\n");fflush(stderr); /* DDD */
      dlclose(dl_handle);
      fprintf(stderr,"Called dlclose()\n");fflush(stderr); /* DDD */
      Store_field(ml_dl_handle,1,0);
    }
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_c_fastfield_dlsym(value ml_dl_handle, value ml_sym)
{
  CAMLparam2(ml_dl_handle,ml_sym);
  void *dl_handle=(void *)Field(ml_dl_handle,1);
  void *funptr;
  
  CAMLlocal1(ml_funptr);

  ml_funptr=alloc(1, Abstract_tag);
  Store_c_field(ml_funptr,0,0);
  
  if(dl_handle)
    {
      fprintf(stderr,"Calling dlsym(handle,\"%s\")\n",String_val(ml_sym));fflush(stderr); /* DDD */
      funptr=(void *)dlsym(dl_handle,String_val(ml_sym));
      fprintf(stderr,"Called dlsym(\"%s\") => 0x%08x\n",String_val(ml_sym),funptr);fflush(stderr); /* DDD */
      Store_c_field(ml_funptr,0,funptr);
    }

  CAMLreturn(ml_funptr);
}


CAMLprim value ocaml_c_fastfield_eval(value ml_funptr, value ml_arr_in, value ml_arr_out)
{
  CAMLparam3(ml_funptr, ml_arr_in, ml_arr_out);
  int success;
  field_function *fun;

  fun=(field_function *)Field(ml_funptr,0);

  if(fun==0)
    {
      CAMLreturn(Val_bool(0));
    }

#ifdef ARCH_ALIGN_DOUBLE
  {
    fprintf(stderr,"The fastfields module does not (yet) support platforms which have ARCH_ALIGN_DOUBLE defined. Exiting.\n");
    exit(1);
  }
#endif
  /*
    See the discussion of this in the thread "C interface style question" in fa.caml
    http://groups.google.com/group/fa.caml/browse_thread/thread/5c2c56f94be1c37d/4d67a0a52a989dce#4d67a0a52a989dce

    or (caml weekly news)

   http://alan.petitepomme.net/cwn/2006.02.14.html#5
  */

  success=fun(&(Double_field(ml_arr_in,0)),&(Double_field(ml_arr_out,0)));
  /* This is stretching the rules a bit concerning the use of Double_field... */

  CAMLreturn(Val_bool(success));
}


CAMLprim value ocaml_c_fastfield_eval_bigarray(value ml_funptr,
					 value ml_arr_in,
					 value ml_arr_out)
{
  CAMLparam3(ml_funptr, ml_arr_in, ml_arr_out);
  int success;
  field_function *fun;

  fun=(field_function *)Field(ml_funptr,0);

  if(fun==0)
    {
      CAMLreturn(Val_bool(0));
    }

  success=fun(Data_bigarray_val(ml_arr_in),Data_bigarray_val(ml_arr_out));
  
  CAMLreturn(Val_bool(success));
}

/* XXX NOTE (mf):
 * It seems I cannot avoid such preprocessor-tricks: I cannot define the macro
 * GCC_FLAGS_SHLIB as a string in the command line. This is because I'm using
 * ocaml to compile C-code and it passes arguments to C-compiler in this way
 * "arguments". So, if I replace arguments with -DMACRO="value" I get some
 * problems, since after expansion: "arguments" --> "-DMACRO="value"".
 * And it seems also that \" doesn't work.
 */
#define STRINGIFY_AFTER_EXPANSION(str) #str
#define STRINGIFY(str) STRINGIFY_AFTER_EXPANSION(str)

CAMLprim value gcc_flags_shlibs(value ml_unit) {
  CAMLparam1(ml_unit);
  char *flags = STRINGIFY(GCC_FLAGS_SHLIB);
  CAMLlocal1(ret_flags);
  ret_flags = copy_string(flags);
  CAMLreturn(ret_flags);
}
