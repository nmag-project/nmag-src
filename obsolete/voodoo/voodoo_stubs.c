/* 
   (C) 2006 Dr. Thomas Fischbacher

 */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/alloc.h>

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

#include <gtk/gtk.h>

#include <GL/glu.h>


CAMLprim value caml_voodoo_gtk_main_iteration_do(value do_block)
{
  CAMLparam1(do_block);
  gtk_main_iteration_do(Bool_val(do_block));
  
  CAMLreturn(Val_unit);
}


