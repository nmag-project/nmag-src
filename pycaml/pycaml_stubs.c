/*
 * (C) arty 2002
 * This software is covered under the GNU lesser general public license

 Heavy modifications (Bugfixes!) done by T.F.

 See

 http://mail.python.org/pipermail/doc-sig/2001-July/001956.html

 for a discussion of result value stealing and a list of functions where
 this becomes relevant.

 */

#include "Python.h"
#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/fail.h"
#include "caml/callback.h"
#include "caml/custom.h"
#include "caml/alloc.h"
#include "caml/bigarray.h"

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "pycaml_stubs.h"
/* ^ T.F. */

#define true 1
#define false 0
/* ^ T.F. says: arty's code occasionally sucks. */

/* When doing python2.5, we need a few "shells" for functions that
   happened to get turned into macros:
*/
static int wrapped_PyRun_SimpleString(const char *s) {
  return PyRun_SimpleStringFlags(s,0);
}

static int wrapped_PyRun_AnyFile(FILE *f,const char *s) {
  return PyRun_AnyFileExFlags(f,s,0,0);
}

static int wrapped_PyRun_SimpleFile(FILE *f,const char *s) {
  return PyRun_SimpleFileExFlags(f,s,0,0);
}

static int wrapped_PyRun_InteractiveOne(FILE *f,const char *s) {
  return PyRun_InteractiveOneFlags(f,s,0);
}

static int wrapped_PyRun_InteractiveLoop(FILE *f,const char *s) {
  return PyRun_InteractiveLoopFlags(f,s,0);
}

static int wrapped_PyRun_AnyFileEx(FILE *f,const char *s, int c) {
  return PyRun_AnyFileExFlags(f,s,c,0);
}

static int wrapped_PyRun_SimpleFileEx(FILE *f,const char *s, int c) {
  return PyRun_SimpleFileExFlags(f,s,c,0);
}

static PyObject * wrapped_PyRun_String(const char *s, int n, PyObject *g, PyObject *l) {
  return PyRun_StringFlags(s,n,g,l,0);
}

static PyObject * wrapped_PyRun_File(FILE *f, const char *s,int n, PyObject *g, PyObject *l) {
  return PyRun_FileExFlags(f,s,n,g,l, 0,0);
}

static PyObject * wrapped_PyRun_FileEx(FILE *f, const char *s,int n, PyObject *g, PyObject *l, int c) {
  return PyRun_FileExFlags(f,s,n,g,l,c,0);
}

static PyObject * wrapped_Py_CompileString(const char *s1, const char *s2,int n) {
  return Py_CompileStringFlags(s1,s2,n,0);
}

static PyObject * wrapped_PyEval_CallObjectWithKeywords(PyObject *o1,
							PyObject *o2,
							PyObject *o3)
{
  return PyEval_CallObjectWithKeywords(o1,o2,o3);
}

static PyObject * wrapped_PyEval_CallObject(PyObject *o1,
					    PyObject *o2)
{
  return PyEval_CallObject(o1,o2);
}



static PyObject * wrapped_PyRange_New(long n1, long n2, long n3, int n4) {
  /* PyRange_New was undocumented in Python 2.3, deprecated in Python 2.4, and
     removed in Python 2.5. We still need a sort-of wrapper here, as not having
     this would require major changes to Arty's code...
   */
  Py_INCREF(Py_None);
  return Py_None;
}

/* Also in Python 2.6, at some point, some functions were turned into macros
 */
static PyObject *
  wrapped_PyImport_ImportModuleEx(char *name, PyObject *globals,
                                  PyObject *locals, PyObject *fromlist) {
  return PyImport_ImportModuleLevel(name, globals, locals, fromlist, -1);
}

PyObject* OurPy_InitEmptyModule(const char* name) {
  /* If we ever end up creating more than one module with this
     function, we should really check whether it will lead to sharing
     of namespaces between the modules. */
  static PyMethodDef the_module_methods[] = {
    {NULL, NULL, 0, NULL}        /* Sentinel */
  };
  return Py_InitModule(name, the_module_methods);
}


static void *getcustom( value v ) { return *((void **)Data_custom_val(v)); }

static void pydecref( value v ) {
    if( getcustom(v) )
      {
	/* printf("GC - pydecref obj 0x%08x to refcount=%d\nOBJ=",getcustom(v),((PyObject *)getcustom(v))->ob_refcnt-1);
	   PyObject_Print((PyObject *)getcustom(v),stdout,0);
	   printf("END OBJ\n");
	   fflush(stdout);
	*/
	Py_DECREF((PyObject *)getcustom(v));
      }
}

static int pycompare( value v1, value v2 ) {
    int result;
    if( getcustom(v1) && !getcustom(v2) ) return -1;
    if( getcustom(v2) && !getcustom(v1) ) return 1;
    if( !getcustom(v1) && !getcustom(v2) ) return 0;
    PyObject_Cmp((PyObject *)getcustom(v1),
		 (PyObject *)getcustom(v2),&result);
    return result;
}

static long pyhash( value v ) {
    if( getcustom(v) ) return PyObject_Hash((PyObject *)getcustom(v));
    else return 0;
}

static unsigned long pydeserialize( void *dst ) {
    return 0;
}

struct custom_operations pyops = {
    "PythonObject",
    pydecref,
    pycompare,
    pyhash,
    custom_serialize_default,
    pydeserialize
};

struct custom_operations fnops = {
    "FuncPointer",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
};



static value pywrap( PyObject *obj ) {
    CAMLparam0();
    CAMLlocal1(v);

    if(obj)Py_INCREF(obj);

    v = alloc_custom( &pyops, sizeof( PyObject * ), 100, 30000000 );
    *((PyObject **)Data_custom_val(v)) = obj;
    CAMLreturn(v);
}

/* T.F.: we may want to pywrap in such a way that we steal the reference: */
static value pywrap_steal( PyObject *obj ) {
    CAMLparam0();
    CAMLlocal1(v);

    v = alloc_custom( &pyops, sizeof( PyObject * ), 100, 30000000 );
    *((PyObject **)Data_custom_val(v)) = obj;
    CAMLreturn(v);
}

static value pywrap_maybe_steal( int do_steal, PyObject *obj ) {
    CAMLparam0();
    CAMLlocal1(v);

    if(obj && !do_steal)Py_INCREF(obj);

    v = alloc_custom( &pyops, sizeof( PyObject * ), 100, 30000000 );
    *((PyObject **)Data_custom_val(v)) = obj;
    CAMLreturn(v);
}


static value funcwrap( void *obj ) {
    CAMLparam0();
    CAMLlocal1(v);
    v = alloc_custom( &fnops, sizeof( void * ), 100, 30000000 );
    *((void **)Data_custom_val(v)) = obj;
    CAMLreturn(v);
}

static PyObject *pyunwrap( value v ) {
    return *((PyObject **)Data_custom_val(v));
}

static void camldestr( void *v ) {
    value *valptr = (value *)v;
    /* printf("DDD camlwrap remove_global_root(0x%08x)\n",valptr);fflush(stdout); */
    remove_global_root(valptr);
    free( v );
}

static void camldestr_pill( void *v, void *unused_dummy_receiving_ocamlpill_token ) {
    value *valptr = (value *)v;
    /* printf("DDD camlwrap remove_global_root(0x%08x)\n",valptr);fflush(stdout); */
    remove_global_root(valptr);
    free( v );
}


/* T.F. Extension: the pill token is a subtle hack:

   One problem is that, as it seems, there are
   some python objects around which would be regarded as
   being of OtherType in original PyCaml.

   As these are opaque, we do not really have a good way
   to discern them from OCaml pills, and passing such an
   opaque value (which cannot be investigated systematically)
   where an ocaml pill is expected is bound to result
   in crashes (which we want to avoid).

   How to get rid of this? We somehow have to be able to properly
   identify OCaml Pills and extend the python types with CamlType.

   We do this by using the PyCObject_Check to determine c-object type,
   and abuse the closure parameter that is passed on to the destructor
   (which can be queried independently, and actually is not used by our
   destructor) as a token designating OCaml pills.

 */
static const char* ocamlpill_token="CAML";

static PyObject *camlwrap( value val, void *aux_str, int size ) {
    value *v = (value *)malloc(sizeof(value) + size);
    *v = val;
    memcpy((void *)v+sizeof(value),aux_str,size);
    register_global_root(v);
    /* printf("DDD camlwrap register_global_root(0x%08x)\n",v);fflush(stdout); */
    return PyCObject_FromVoidPtr(v,camldestr);
}

static PyObject *camlwrap_pill( value val, void *aux_str, int size ) {
    value *v = (value *)malloc(sizeof(value) + size);
    *v = val;
    memcpy((void *)v+sizeof(value),aux_str,size);
    register_global_root(v);
    return PyCObject_FromVoidPtrAndDesc(v,(void*)ocamlpill_token,camldestr_pill);
}


static void *caml_aux( PyObject *obj ) {
    value *v = (value *)PyCObject_AsVoidPtr( obj );
    return (void *)v+sizeof(value);
}

/*
PyObject *pycall_callback_buggy( PyObject *obj, PyObject *args ) {
    value out;
    value *v;

    if( !PyCObject_Check(obj) ) {
	Py_INCREF(Py_None);
	return Py_None;
    }
    v = (value *)PyCObject_AsVoidPtr( obj );
    out = callback(*v,pywrap(args));
    return pyunwrap(out);
}
*/

/* T.F.: - I think the definition above is flawed...
   Looking at the definitions of OCAML macros in memory.h,
   this is how I suppose it should work:
*/

static PyObject *pycall_callback( PyObject *obj, PyObject *args ) {
  CAMLparam0();
  CAMLlocal3(ml_out,ml_func,ml_args);
  PyObject *out;

    if( !PyCObject_Check(obj) ) {
	Py_INCREF(Py_None);
	CAMLreturn(Py_None);
    }

    ml_func = *(value *)PyCObject_AsVoidPtr( obj );
    ml_args=pywrap(args);
    ml_out = callback(ml_func,ml_args);
    out=pyunwrap(ml_out);
    /* T.F.:
       The result which we have now is borrowed - most probably,
       there is only one reference to it which says
       "I am reachable through the ML heap".
       We have to properly transfer ownership, and hence
       see that we own that reference:
    */
    if(out)Py_INCREF(out);	/* NOTE: may be 0! */
    CAMLreturn(out);
}

typedef void  (*type_1)( void );
typedef void  (*type_2)( int );
typedef void  (*type_3)( char * );
typedef int   (*type_4)( void );
typedef int   (*type_5)( char * );
typedef int   (*type_6)( FILE *, char * );
typedef int   (*type_7)( FILE *, char *, int );
typedef char *(*type_8)( void );
typedef PyObject* (*type_9)(char*, int, PyObject *, PyObject *);
typedef PyObject* (*type_10)(FILE *, char*, int, PyObject *, PyObject *);
typedef PyObject* (*type_11)(FILE *, char*, int, PyObject *, PyObject *, int );
typedef PyObject* (*type_12)(char*, char*, int);
typedef int (*type_13)(PyObject *, FILE *, int);
typedef PyObject * (*type_14)(PyObject *);
typedef PyObject * (*type_15)(PyObject *, PyObject *, int);
typedef PyObject * (*type_16)(PyObject *, char *);
typedef PyObject * (*type_17)(PyObject *, PyObject *);
typedef int (*type_18)(PyObject *);
typedef int (*type_19)(PyObject *, PyObject *);
typedef int (*type_20)(PyObject *, PyObject *, int);
typedef int (*type_21)(PyObject *, char *, PyObject *);
typedef int (*type_22)(PyObject *, char *);
typedef int (*type_23)(PyObject **, PyObject **);
typedef int (*type_24)(PyObject *, PyObject *, PyObject *);
typedef long (*type_25)(PyObject *);
typedef char *(*type_26)(PyObject *);
typedef void (*type_27)(PyObject **, PyObject *);
typedef PyObject *(*type_28)(char *);
typedef PyObject *(*type_29)(void);
typedef void (*type_30)(PyObject *);
typedef int (*type_31)(PyObject *, int *, PyObject **, PyObject **);
typedef PyObject *(*type_32)(char*, char**, int);
typedef PyObject *(*type_33)(Py_UNICODE*, int, int);
typedef PyObject *(*type_34)(long);
typedef long (*type_35)(void);
typedef PyObject *(*type_36)(double);
typedef double (*type_37)(PyObject *);
typedef PyObject *(*type_38)(PyObject*, char**);
typedef PyObject *(*type_39)(int size);
typedef PyObject *(*type_40)(PyObject *, int);
typedef int (*type_41)(PyObject *, int, PyObject *);
typedef PyObject *(*type_42)(PyObject* start, PyObject* stop, PyObject* step);
typedef int (*type_43)(PySliceObject *r, int length, int *start, int *stop, int *step);
typedef PyObject *(*type_44)(long, long, long, int);
typedef void (*type_45)(PyObject *, PyObject *);
typedef void (*type_46)(PyObject *, const char *);
typedef void (*type_47)(PyObject **, PyObject **, PyObject **);
typedef void (*type_48)(PyObject *, PyObject *, PyObject *);
typedef PyObject *(*type_49)(PyObject *, char *);
typedef PyObject *(*type_50)(char *,PyObject *,char *);
typedef PyObject *(*type_51)(char *,PyObject *,PyObject *,PyObject *);
typedef int (*type_52)(PyObject *obj,const char **buffer,int *buffer_len);
typedef int (*type_53)(PyObject *o, int i1, int i2, PyObject *v);
typedef int (*type_54)(PyObject *o, int i1, int i2);


value pygencall( value format, value arg ) {
  CAMLparam2(format,arg);
    CAMLlocal1(rv);
    FILE *f;
    int fd;
    int x;
    int ret_int;
    const char *rvs;
    /* T.F. Bugfix - was char *rvs,
       while we actually have to give a guarantee
       not to modify the string! */
    int fmt = Int_val(Field(format,1));
    PyObject *ob1,*ob2,*ob3;
    void *func = getcustom(Field(format,0));
    int do_steal=Bool_val(Field(format,3));

    rv = Val_unit;

    switch( fmt ) {
    case 1:
      ((type_1)func)();
      CAMLreturn(Val_unit);

    case 2:
      ((type_2)func)(Int_val(arg));
      CAMLreturn(Val_unit);

    case 3:
      ((type_3)func)(String_val(arg));
      CAMLreturn(Val_unit);

    case 4:
      CAMLreturn(Int_val(((type_4)func)()));

    case 5:
      CAMLreturn(Int_val(((type_5)func)
			 (String_val(arg))));

    case 6:
    case 7:
    case 10:
    case 11:
      fd = dup(Int_val(Field(arg,0)));
      f = fdopen(fd,"r+");
      switch( fmt ) {
      case 6:
	rv = Val_int(((type_6)func)
		     (f,String_val(Field(arg,1))));
	break;

      case 7:
	rv = Val_int(((type_7)func)
		     (f,
		      String_val(Field(arg,1)),
		      Int_val(Field(arg,2))));
	break;

      case 10:
	rv = pywrap_maybe_steal(do_steal,((type_10)func)
				(f,
				 String_val(Field(arg,1)),
				 Int_val(Field(arg,2)),
				 pyunwrap(Field(arg,3)),
				 pyunwrap(Field(arg,4))));
	break;

      case 11:
	rv = pywrap_maybe_steal(do_steal,((type_11)func)
				(f,
				 String_val(Field(arg,1)),
				 Int_val(Field(arg,2)),
				 pyunwrap(Field(arg,3)),
				 pyunwrap(Field(arg,4)),
				 Int_val(Field(arg,5))));
	break;
      }

      fclose( f );
      CAMLreturn( rv );

    case 8:
      CAMLreturn(copy_string(((type_8)func)()));

    case 9:
      CAMLreturn(pywrap_maybe_steal(do_steal,((type_9)func)
				    (String_val(Field(arg,0)),
				     Int_val(Field(arg,1)),
				     pyunwrap(Field(arg,2)),
				     pyunwrap(Field(arg,3)))));

    case 12:
      CAMLreturn(pywrap_maybe_steal(do_steal,((type_12)func)
				    (String_val(Field(arg,0)),
				     String_val(Field(arg,1)),
				     Int_val(Field(arg,2)))));

    case 13:
      fd = dup(Int_val(Field(arg,1)));
      f = fdopen(fd,"r+");
      rv = Val_int(((type_13)func)
		   (pyunwrap(Field(arg,0)),
		    f,
		    Int_val(Field(arg,2))));
      fclose( f );
      CAMLreturn(rv);
      /* <- T.F. this line was just missing!
	 Actually, it would have been done in the end,
	 after the switch/case, but it is nicer and clearer
	 to do it here.
      */
      break;

    case 14:
      CAMLreturn(pywrap_maybe_steal(do_steal,((type_14)func)(pyunwrap(arg))));

    case 15:
      CAMLreturn(pywrap_maybe_steal(do_steal,((type_15)func)
				    (pyunwrap(Field(arg,0)),
				     pyunwrap(Field(arg,1)),
				     Int_val(Field(arg,2)))));

    case 16:
      CAMLreturn(pywrap_maybe_steal(do_steal,((type_16)func)
				    (pyunwrap(Field(arg,0)),
				     String_val(Field(arg,1)))));

    case 17:
      CAMLreturn(pywrap_maybe_steal(do_steal,((type_17)func)(pyunwrap(Field(arg,0)),pyunwrap(Field(arg,1)))));

    case 18:
      CAMLreturn(Val_int(((type_18)func)(pyunwrap(arg))));

    case 19:
      CAMLreturn(Val_int(((type_19)func)
			 (pyunwrap(Field(arg,0)),
			  pyunwrap(Field(arg,1)))));

    case 20:
      CAMLreturn(Val_int(((type_20)func)
			 (pyunwrap(Field(arg,0)),
			  pyunwrap(Field(arg,1)),
			  Int_val(Field(arg,2)))));

    case 21:
      CAMLreturn(Val_int(((type_21)func)
			 (pyunwrap(Field(arg,0)),
			  String_val(Field(arg,1)),
			  pyunwrap(Field(arg,2)))));

    case 22:
      CAMLreturn(Val_int(((type_22)func)
			 (pyunwrap(Field(arg,0)),
			  String_val(Field(arg,1)))));

      /* T.F.: XXX warnings, as the implementation looks *highly* suspicious! */
    case 23:
      printf("XXX WARNING: using pycaml code that looks highly suspicious, probably is wrong!\n");fflush(stdout);
      ob1 = pyunwrap(Field(arg,0)); ob2 = pyunwrap(Field(arg,1));
      ret_int = ((type_23)func)( &ob1, &ob2 );
      if( ret_int == -1 ) CAMLreturn((value)1); /* that cannot be right! */
      else {
	rv = alloc_tuple(2);
	/* T.F. Bugfix:
	   Field(rv,0) = pywrap(ob1);
	   Field(rv,1) = pywrap(ob2);

	   Changed to:
	*/
	Store_field(rv,0,pywrap(ob1));
	Store_field(rv,1,pywrap(ob2));
	/* How about stealing references here?!? */
	CAMLreturn(rv);
      }

    case 24:
      CAMLreturn(Int_val(((type_24)func)
			 (pyunwrap(Field(arg,0)),
			  pyunwrap(Field(arg,1)),
			  pyunwrap(Field(arg,2)))));

    case 25:
      CAMLreturn(copy_int64(((type_25)func)(pyunwrap(arg))));

    case 26:
      CAMLreturn(copy_string(((type_26)func)(pyunwrap(arg))));

    case 27:
      fprintf(stderr,"XXX This piece of code should not be callable!\n");fflush(stderr);
      CAMLreturn(Val_unit);

    case 28:
      CAMLreturn(pywrap_maybe_steal(do_steal,((type_28)func)(String_val(arg))));

    case 29:
      CAMLreturn(pywrap_maybe_steal(do_steal,((type_29)func)()));

    case 30:
      ((type_30)func)(pyunwrap(arg));
      CAMLreturn(Val_unit);
    case 31:
      printf("XXX WARNING: using pycaml code that looks highly suspicious, probably is wrong!\n");fflush(stdout);

      x = Int_val(Field(arg,1));
      ret_int = ((type_31)func)
	(pyunwrap(Field(arg,0)),
	 &x,
	 &ob1, &ob2);
      if( !ret_int ) CAMLreturn((value)1);
      else {
	rv = alloc_tuple(3);
	/* T.F. Bugfix
	   Field(rv,0) = pywrap(ob1);
	   Field(rv,1) = pywrap(ob2);
	   Field(rv,2) = Val_int(x);
	*/
	Store_field(rv,0,pywrap(ob1));
	Store_field(rv,1,pywrap(ob2));
	Store_field(rv,2,Val_int(x));

	CAMLreturn(rv);
      }

      /* case 32: string -> int */

      /* case 33: unicode ... need to do something coherent */

    case 34:
      CAMLreturn(pywrap_maybe_steal(do_steal,((type_34)func)(Int64_val(arg))));

    case 35:
      CAMLreturn(copy_int64(((type_35)func)()));

    case 36:
      CAMLreturn(pywrap_maybe_steal(do_steal,((type_36)func)(Double_val(arg))));

    case 37:
      CAMLreturn(copy_double(((type_37)func)
			     (pyunwrap(arg))));

      /* case 38: string -> float */

    case 39:
      CAMLreturn(pywrap_maybe_steal(do_steal,((type_39)func)(Int_val(arg))));

    case 40:
      CAMLreturn(pywrap_maybe_steal(do_steal,((type_40)func)
				    (pyunwrap(Field(arg,0)),
				     Int_val(Field(arg,1)))));

    case 41:
      /* PyTuple_SetItem, PySequence_SetItem, PyMapping_SetItem, PyMapping_SetItemString -
	 do_steal here means:
	 "do we steal the arg[2] reference (which is an OCaml reference)?"
	 If our function is stealing (which only PyTuple_SetItem is),
	 we first have to get a reference...
      */
      {
	PyObject *x=pyunwrap(Field(arg,2));
	if(do_steal)Py_INCREF(x);
	CAMLreturn(Val_int(((type_41)func)
			   (pyunwrap(Field(arg,0)),
			    Int_val(Field(arg,1)),
			    x)));
      }

    case 42:
      CAMLreturn(pywrap_maybe_steal(do_steal,((type_42)func)
				    (pyunwrap(Field(arg,0)),
				     pyunwrap(Field(arg,1)),
				     pyunwrap(Field(arg,2)))));

    case 43: {
      int start,end,step;

      ret_int = ((type_43)func)
	((PySliceObject *)pyunwrap(Field(arg,0)),
	 Int_val(Field(arg,1)),
	 &start, &end, &step);
      if( !ret_int ) CAMLreturn((value)1);
      else {
	rv = alloc_tuple(3);
	/* T.F. bugfix
	   Field(rv,0) = start;
	   Field(rv,1) = end;
	   Field(rv,2) = step;
	*/
	Store_field(rv,0,Val_int(start));
	Store_field(rv,1,Val_int(end));
	Store_field(rv,2,Val_int(step));
	CAMLreturn(rv);
      }
    }

    case 44:
      CAMLreturn(pywrap_maybe_steal(do_steal,((type_44)func)
				    (Int_val(Field(arg,0)),
				     Int_val(Field(arg,1)),
				     Int_val(Field(arg,2)),
				     Int_val(Field(arg,3)))));

    case 45:
      ((type_45)func)
	(pyunwrap(Field(arg,0)),
	 pyunwrap(Field(arg,1)));
      CAMLreturn(Val_unit);

    case 46:
      ((type_46)func)
	(pyunwrap(Field(arg,0)),String_val(Field(arg,1)));
      CAMLreturn(Val_unit);

    case 47:
      ob1 = pyunwrap(Field(arg,0));
      ob2 = pyunwrap(Field(arg,1));
      ob3 = pyunwrap(Field(arg,2));
      ((type_47)func)(&ob1,&ob2,&ob3);
      rv = alloc_tuple(3);
      /* T.F. bugfix
	 Field(rv,0) = pywrap(ob1);
	 Field(rv,1) = pywrap(ob2);
	 Field(rv,2) = pywrap(ob3);
      */

      /* XXX T.F. problem: if this interface is used for PyErr_Fetch,
	 then ob1,ob2,ob3 may be NULL here! We should map this properly
	 to python!
      */
      Store_field(rv,0,pywrap_maybe_steal(do_steal,ob1));
      Store_field(rv,1,pywrap_maybe_steal(do_steal,ob2));
      Store_field(rv,2,pywrap_maybe_steal(do_steal,ob3));
      CAMLreturn(rv);

    case 48:
      ((type_48)func)
	(pyunwrap(Field(arg,0)),
	 pyunwrap(Field(arg,1)),
	 pyunwrap(Field(arg,2)));
      CAMLreturn(Val_unit);

    case 49:
      CAMLreturn(pywrap_maybe_steal(do_steal,((type_49)func)
				    (pyunwrap(Field(arg,0)),
				     String_val(Field(arg,1)))));

    case 50:
      CAMLreturn(pywrap_maybe_steal(do_steal,((type_50)func)
				    (String_val(Field(arg,0)),
				     pyunwrap(Field(arg,1)),
				     String_val(Field(arg,2)))));

    case 51:
      CAMLreturn(pywrap_maybe_steal(do_steal,((type_51)func)
				    (String_val(Field(arg,0)),
				     pyunwrap(Field(arg,1)),
				     pyunwrap(Field(arg,2)),
				     pyunwrap(Field(arg,3)))));

    case 52:
      ((type_52)func)(pyunwrap(arg),&rvs,&ret_int);
      rv = copy_string(rvs);
      CAMLreturn(rv);

    case 53:
      CAMLreturn(Val_int(((type_53)func)
			 (pyunwrap(Field(arg,0)),
			  Int_val(Field(arg,1)),
			  Int_val(Field(arg,2)),
			  pyunwrap(Field(arg,3)))));

    case 54:
      CAMLreturn(Val_int(((type_54)func)
			 (pyunwrap(Field(arg,0)),
			  Int_val(Field(arg,1)),
			  Int_val(Field(arg,2)))));
    }
    CAMLreturn(rv);
}

#ifdef DONT_COMPILE_THIS
/* 1 */
DL_IMPORT(void) Py_Initialize(void);
DL_IMPORT(void) Py_Finalize(void);
DL_IMPORT(void) PyErr_Print(void);
/* 2 */
DL_IMPORT(void) Py_Exit(int);
DL_IMPORT(void) PyErr_PrintEx(int);
/* 3 */
DL_IMPORT(void) Py_SetProgramName(char *);
DL_IMPORT(void) Py_SetPythonHome(char *);
/* 4 */
DL_IMPORT(int) Py_IsInitialized(void);
/* 5 */
DL_IMPORT(int) PyRun_SimpleString(char *);
/* 6 */
DL_IMPORT(int) PyRun_AnyFile(FILE *, char *);
DL_IMPORT(int) PyRun_SimpleFile(FILE *, char *);
DL_IMPORT(int) PyRun_InteractiveOne(FILE *, char *);
DL_IMPORT(int) PyRun_InteractiveLoop(FILE *, char *);
DL_IMPORT(int) Py_FdIsInteractive(FILE *, char *);
/* 7 */
DL_IMPORT(int) PyRun_AnyFileEx(FILE *, char *, int);
DL_IMPORT(int) PyRun_SimpleFileEx(FILE *, char *, int);
/* 8 */
DL_IMPORT(char*) Py_GetProgramName(void);
DL_IMPORT(char*) Py_GetPythonHome(void);
DL_IMPORT(char*) Py_GetProgramFullPath(void);
DL_IMPORT(char*) Py_GetPrefix(void);
DL_IMPORT(char*) Py_GetExecPrefix(void);
DL_IMPORT(char*) Py_GetPath(void);
DL_IMPORT(char*) Py_GetVersion(void);
DL_IMPORT(char*) Py_GetPlatform(void);
DL_IMPORT(char*) Py_GetCopyright(void);
DL_IMPORT(char*) Py_GetCompiler(void);
DL_IMPORT(char*) Py_GetBuildInfo(void);
/* 9 */
DL_IMPORT(PyObject*) PyRun_String(char*, int, PyObject *, PyObject *);
/* 10 */
DL_IMPORT(PyObject*) PyRun_File(FILE *, char*, int, PyObject *, PyObject *);
/* 11 */
DL_IMPORT(PyObject*) PyRun_FileEx(FILE *, char*, int, PyObject *, PyObject *, int );
/* 12 */
DL_IMPORT(PyObject*) Py_CompileString(char*, char*, int); /* FUNCTION 30 */

/* Generic operations on objects */
/* 13 */
extern DL_IMPORT(int) PyObject_Print(PyObject *, FILE *, int);
/* 14 */
extern DL_IMPORT(PyObject *) PyObject_Repr(PyObject *);
extern DL_IMPORT(PyObject *) PyObject_Str(PyObject *);
extern DL_IMPORT(PyObject *) PyObject_Unicode(PyObject *);
/* 15 */
extern DL_IMPORT(PyObject *) PyObject_RichCompare(PyObject *, PyObject *, int);
/* 16 */
extern DL_IMPORT(PyObject *) PyObject_GetAttrString(PyObject *, char *);
/* 17 */
extern DL_IMPORT(PyObject *) PyObject_GetAttr(PyObject *, PyObject *);
/* 18 */
extern DL_IMPORT(int) PyObject_IsTrue(PyObject *);
extern DL_IMPORT(int) PyObject_Not(PyObject *);
extern DL_IMPORT(int) PyCallable_Check(PyObject *);
/* 19 */
extern DL_IMPORT(int) PyObject_Compare(PyObject *, PyObject *);
extern DL_IMPORT(int) PyObject_HasAttr(PyObject *, PyObject *);
/* 20 */
extern DL_IMPORT(int) PyObject_RichCompareBool(PyObject *, PyObject *, int);
/* 21 */
extern DL_IMPORT(int) PyObject_SetAttrString(PyObject *, char *, PyObject *);
/* 22 */
extern DL_IMPORT(int) PyObject_HasAttrString(PyObject *, char *);
/* 23 */
extern DL_IMPORT(int) PyNumber_Coerce(PyObject **, PyObject **);
extern DL_IMPORT(int) PyNumber_CoerceEx(PyObject **, PyObject **);
/* 24 */
extern DL_IMPORT(int) PyObject_SetAttr(PyObject *, PyObject *, PyObject *);
/* 25 */
extern DL_IMPORT(long) PyObject_Hash(PyObject *);

/* Strings */
/* 18 */
extern DL_IMPORT(int) PyString_Size(PyObject *);
/* 26 */
extern DL_IMPORT(char *) PyString_AsString(PyObject *);
/* 27 */
extern DL_IMPORT(void) PyString_Concat(PyObject **, PyObject *);
extern DL_IMPORT(void) PyString_ConcatAndDel(PyObject **, PyObject *);
/* 28 */
extern DL_IMPORT(PyObject *) PyString_FromString(const char *);
/* 17 */
extern DL_IMPORT(PyObject *) PyString_Format(PyObject *, PyObject *);

/* Dictionaries */
/* 29 */
extern DL_IMPORT(PyObject *) PyDict_New(void);
/* 17 */
extern DL_IMPORT(PyObject *) PyDict_GetItem(PyObject *mp, PyObject *key);
/* 24 */
extern DL_IMPORT(int) PyDict_SetItem(PyObject *mp, PyObject *key, PyObject *item);
/* 19 */
extern DL_IMPORT(int) PyDict_DelItem(PyObject *mp, PyObject *key);
/* 30 */
extern DL_IMPORT(void) PyDict_Clear(PyObject *mp);
/* 31 */
extern DL_IMPORT(int) PyDict_Next
	(PyObject *mp, int *pos, PyObject **key, PyObject **value);
/* 14 */
extern DL_IMPORT(PyObject *) PyDict_Keys(PyObject *mp);
extern DL_IMPORT(PyObject *) PyDict_Values(PyObject *mp);
extern DL_IMPORT(PyObject *) PyDict_Items(PyObject *mp);
extern DL_IMPORT(PyObject *) PyDict_Copy(PyObject *mp);
/* 18 */
extern DL_IMPORT(int) PyDict_Size(PyObject *mp);
/* 16 */
extern DL_IMPORT(PyObject *) PyDict_GetItemString(PyObject *dp, char *key);
/* 22 */
extern DL_IMPORT(int) PyDict_DelItemString(PyObject *dp, char *key);
/* 21 */
extern DL_IMPORT(int) PyDict_SetItemString(PyObject *dp, char *key, PyObject *item);

/* Integer */
/* 32 */
extern DL_IMPORT(PyObject *) PyInt_FromString(char*, char**, int);
/* 33 */
extern DL_IMPORT(PyObject *) PyInt_FromUnicode(Py_UNICODE*, int, int);
/* 34 */
extern DL_IMPORT(PyObject *) PyInt_FromLong(long);
/* 25 */
extern DL_IMPORT(long) PyInt_AsLong(PyObject *);
/* 35 */
extern DL_IMPORT(long) PyInt_GetMax(void);
/* Long */
/* 34 */
extern DL_IMPORT(PyObject *) PyLong_FromLong(long);
/* 36 */
extern DL_IMPORT(PyObject *) PyLong_FromDouble(double);
/* 25 */
extern DL_IMPORT(long) PyLong_AsLong(PyObject *);

/* Float */
/* 36 */
extern DL_IMPORT(PyObject *) PyFloat_FromDouble(double);
/* 37 */
extern DL_IMPORT(double) PyFloat_AsDouble(PyObject *);

/* Modules */
/* 28 */
extern DL_IMPORT(PyObject *) PyModule_New(char *);
/* 14 */
extern DL_IMPORT(PyObject *) PyModule_GetDict(PyObject *);
/* 26 */
extern DL_IMPORT(char *) PyModule_GetName(PyObject *);
extern DL_IMPORT(char *) PyModule_GetFilename(PyObject *);

/* 39 */
extern DL_IMPORT(PyObject *) PyTuple_New(int size);
/* 18 */
extern DL_IMPORT(int) PyTuple_Size(PyObject *);
/* 40 */
extern DL_IMPORT(PyObject *) PyTuple_GetItem(PyObject *, int);
/* 41 */
extern DL_IMPORT(int) PyTuple_SetItem(PyObject *, int, PyObject *);
/* 13 */
extern DL_IMPORT(PyObject *) PyTuple_GetSlice(PyObject *, int, int);

/* 42 */
DL_IMPORT(PyObject *) PySlice_New(PyObject* start, PyObject* stop, PyObject* step);
/* 43 */
DL_IMPORT(int) PySlice_GetIndices(PySliceObject *r, int length, int *start, int *stop, int *step);
/* 44 */
DL_IMPORT(PyObject *) PyRange_New(long, long, long, int);

/* Error handling definitions */

/* 30 */
DL_IMPORT(void) PyErr_SetNone(PyObject *);
/* 45 */
DL_IMPORT(void) PyErr_SetObject(PyObject *, PyObject *);
/* 46 */
DL_IMPORT(void) PyErr_SetString(PyObject *, const char *);
/* 29 */
DL_IMPORT(PyObject *) PyErr_Occurred(void);
/* 1 */
DL_IMPORT(void) PyErr_Clear(void);
/* 47 */
DL_IMPORT(void) PyErr_Fetch(PyObject **, PyObject **, PyObject **);
/* 48 */
DL_IMPORT(void) PyErr_Restore(PyObject *, PyObject *, PyObject *);

/* Error testing and normalization */
/* 19 */
DL_IMPORT(int) PyErr_GivenExceptionMatches(PyObject *, PyObject *);
/* 18 */
DL_IMPORT(int) PyErr_ExceptionMatches(PyObject *);
/* 47 */
DL_IMPORT(void) PyErr_NormalizeException(PyObject**, PyObject**, PyObject**);

/* Classes */
/* 42 */
extern DL_IMPORT(PyObject *) PyClass_New(PyObject *, PyObject *, PyObject *);
/* 42 */
extern DL_IMPORT(PyObject *) PyInstance_New(PyObject *, PyObject *,
                                            PyObject *);
/* 17 */
extern DL_IMPORT(PyObject *) PyInstance_NewRaw(PyObject *, PyObject *);
/* 42 */
extern DL_IMPORT(PyObject *) PyMethod_New(PyObject *, PyObject *, PyObject *);
/* 14 */
extern DL_IMPORT(PyObject *) PyMethod_Function(PyObject *);
extern DL_IMPORT(PyObject *) PyMethod_Self(PyObject *);
extern DL_IMPORT(PyObject *) PyMethod_Class(PyObject *);

/* Module */
/* 28 */
extern DL_IMPORT(PyObject *) PyModule_New(char *);
/* 14 */
extern DL_IMPORT(PyObject *) PyModule_GetDict(PyObject *);
/* 26 */
extern DL_IMPORT(char *) PyModule_GetName(PyObject *);
extern DL_IMPORT(char *) PyModule_GetFilename(PyObject *);
/* 35 */
DL_IMPORT(long) PyImport_GetMagicNumber(void);
/* 49 */
DL_IMPORT(PyObject *) PyImport_ExecCodeModule(char *name, PyObject *co);
/* 50 */
DL_IMPORT(PyObject *) PyImport_ExecCodeModuleEx(char *name, PyObject *co, char *pathname);
/* 29 */
DL_IMPORT(PyObject *) PyImport_GetModuleDict(void);
/* 28 */
DL_IMPORT(PyObject *) PyImport_AddModule(char *name);
DL_IMPORT(PyObject *) PyImport_ImportModule(char *name);
/* 51 */
DL_IMPORT(PyObject *) PyImport_ImportModuleEx(char *name, PyObject *globals, PyObject *locals, PyObject *fromlist);
/* 28 */
DL_IMPORT(PyObject *) PyImport_Import(PyObject *name);
/* 14 */
DL_IMPORT(PyObject *) PyImport_ReloadModule(PyObject *m);
/* 1 */
DL_IMPORT(void) PyImport_Cleanup(void);
/* 5 */
DL_IMPORT(int) PyImport_ImportFrozenModule(char *);

/* Interface to random parts in ceval.c */
/* 42 */
DL_IMPORT(PyObject *) wrapped_PyEval_CallObjectWithKeywords(PyObject *, PyObject *, PyObject *);
/* 17 */
DL_IMPORT(PyObject *) wrapped_PyEval_CallObject(PyObject *, PyObject *);

/* 29 */
DL_IMPORT(PyObject *) PyEval_GetBuiltins(void);
DL_IMPORT(PyObject *) PyEval_GetGlobals(void);
DL_IMPORT(PyObject *) PyEval_GetLocals(void);
DL_IMPORT(PyObject *) PyEval_GetFrame(void);
/* 4 */
DL_IMPORT(int) PyEval_GetRestricted(void);

/* Abstract layer */
/* 14 */
DL_IMPORT(PyObject *) PyObject_Type(PyObject *o);
/* 18 */
DL_IMPORT(int) PyObject_Size(PyObject *o);
/* 17 */
DL_IMPORT(PyObject *) PyObject_GetItem(PyObject *o, PyObject *key);
/* 24 */
DL_IMPORT(int) PyObject_SetItem(PyObject *o, PyObject *key, PyObject *v);
/* 17 */
DL_IMPORT(int) PyObject_DelItem(PyObject *o, PyObject *key);
/* 52 */
DL_IMPORT(int) PyObject_AsCharBuffer(PyObject *obj,const char **buffer,int *buffer_len);
DL_IMPORT(int) PyObject_AsReadBuffer(PyObject *obj,const void **buffer,int *buffer_len);
DL_IMPORT(int) PyObject_AsWriteBuffer(PyObject *obj,void **buffer,int *buffer_len);
/* 18 */
DL_IMPORT(int) PyNumber_Check(PyObject *o);
/* 17 */
DL_IMPORT(PyObject *) PyNumber_Add(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_Subtract(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_Multiply(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_Divide(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_Remainder(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_Divmod(PyObject *o1, PyObject *o2);
/* 42 */
DL_IMPORT(PyObject *) PyNumber_Power(PyObject *o1, PyObject *o2,PyObject *o3);
/* 14 */
DL_IMPORT(PyObject *) PyNumber_Negative(PyObject *o);
DL_IMPORT(PyObject *) PyNumber_Positive(PyObject *o);
DL_IMPORT(PyObject *) PyNumber_Absolute(PyObject *o);
DL_IMPORT(PyObject *) PyNumber_Invert(PyObject *o);
/* 17 */
DL_IMPORT(PyObject *) PyNumber_Lshift(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_Rshift(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_And(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_Xor(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_Or(PyObject *o1, PyObject *o2);
/* 14 */
DL_IMPORT(PyObject *) PyNumber_Int(PyObject *o);
DL_IMPORT(PyObject *) PyNumber_Long(PyObject *o);
DL_IMPORT(PyObject *) PyNumber_Float(PyObject *o);
/* 17 */
DL_IMPORT(PyObject *) PyNumber_InPlaceAdd(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_InPlaceSubtract(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_InPlaceMultiply(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_InPlaceDivide(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_InPlaceRemainder(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_InPlaceLshift(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_InPlaceRshift(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_InPlaceAnd(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_InPlaceXor(PyObject *o1, PyObject *o2);
DL_IMPORT(PyObject *) PyNumber_InPlaceOr(PyObject *o1, PyObject *o2);
/* 42 */
DL_IMPORT(PyObject *) PyNumber_InPlacePower(PyObject *o1, PyObject *o2,PyObject *o3);
/* 18 */
DL_IMPORT(int) PySequence_Check(PyObject *o);
DL_IMPORT(int) PySequence_Size(PyObject *o);
DL_IMPORT(int) PySequence_Length(PyObject *o);
/* 17 */
DL_IMPORT(PyObject *) PySequence_Concat(PyObject *o1, PyObject *o2);
/* 40 */
DL_IMPORT(PyObject *) PySequence_Repeat(PyObject *o, int count);
DL_IMPORT(PyObject *) PySequence_GetItem(PyObject *o, int i);
/* 13 */
DL_IMPORT(PyObject *) PySequence_GetSlice(PyObject *o, int i1, int i2);
/* 41 */
DL_IMPORT(int) PySequence_SetItem(PyObject *o, int i, PyObject *v);
/* 20 */
DL_IMPORT(int) PySequence_DelItem(PyObject *o, int i);
/* 53 */
DL_IMPORT(int) PySequence_SetSlice(PyObject *o, int i1, int i2, PyObject *v);
/* 54 */
DL_IMPORT(int) PySequence_DelSlice(PyObject *o, int i1, int i2);
/* 14 */
DL_IMPORT(PyObject *) PySequence_Tuple(PyObject *o);
DL_IMPORT(PyObject *) PySequence_List(PyObject *o);
/* 16 */
DL_IMPORT(PyObject *) PySequence_Fast(PyObject *o, const char* m);
/* 19 */
DL_IMPORT(int) PySequence_Count(PyObject *o, PyObject *value);
DL_IMPORT(int) PySequence_Contains(PyObject *o, PyObject *value);
DL_IMPORT(int) PySequence_In(PyObject *o, PyObject *value);
DL_IMPORT(int) PySequence_Index(PyObject *o, PyObject *value);
/* 17 */
DL_IMPORT(PyObject *) PySequence_InPlaceConcat(PyObject *o1, PyObject *o2);
/* 22 */
DL_IMPORT(PyObject *) PySequence_InPlaceRepeat(PyObject *o, int count);
/* 18 */
DL_IMPORT(int) PyMapping_Check(PyObject *o);
DL_IMPORT(int) PyMapping_Size(PyObject *o);
DL_IMPORT(int) PyMapping_Length(PyObject *o);
/* 16 */
DL_IMPORT(int) PyMapping_HasKeyString(PyObject *o, char *key);
/* 19 */
DL_IMPORT(int) PyMapping_HasKey(PyObject *o, PyObject *key);
/* 16 */
DL_IMPORT(PyObject *) PyMapping_GetItemString(PyObject *o, char *key);
/* 41 */
DL_IMPORT(int) PyMapping_SetItemString(PyObject *o, char *key, PyObject *value);


#ifdef  MAYBE_RUN
DL_IMPORT(void) init_exceptions(void);
DL_IMPORT(void) fini_exceptions(void);
DL_IMPORT(void) _PyImport_Init(void);
DL_IMPORT(void) _PyImport_Fini(void);
DL_IMPORT(void) PyMethod_Fini(void);
DL_IMPORT(void) PyFrame_Fini(void);
DL_IMPORT(void) PyCFunction_Fini(void);
DL_IMPORT(void) PyTuple_Fini(void);
DL_IMPORT(void) PyString_Fini(void);
DL_IMPORT(void) PyInt_Fini(void);
DL_IMPORT(void) PyFloat_Fini(void);
DL_IMPORT(PyObject*) _PyBuiltin_Init(void);
DL_IMPORT(PyObject*) _PySys_Init(void);
DL_IMPORT(struct symtable *) Py_SymtableString(char*, char*, int);
DL_IMPORT(struct _node *) PyParser_SimpleParseString(char*, int);
DL_IMPORT(struct _node *) PyParser_SimpleParseFile(FILE *, char*, int);
DL_IMPORT(int) Py_AtExit(void (*func)(void));
DL_IMPORT(void) Py_EndInterpreter(PyThreadState *);
DL_IMPORT(PyThreadState *) Py_NewInterpreter(void);
DL_IMPORT(int) (*PyOS_InputHook)(void);
DL_IMPORT(int) PyOS_CheckStack(void);
DL_IMPORT(char) *(*PyOS_ReadlineFunctionPointer)(char*);
DL_IMPORT(PyOS_sighandler_t) PyOS_getsig(int);
DL_IMPORT(PyOS_sighandler_t) PyOS_setsig(int, PyOS_sighandler_t);
DL_IMPORT(void) PyOS_FiniInterrupts(void);
DL_IMPORT(char*) PyOS_Readline(char*);
#endif
 /* MAYBE_RUN */
#endif
/* DONT_COMPILE_THIS */

/* Value -> Pyobject */

value pywrapvalue( value cb ) {
    CAMLparam1(cb);
    CAMLreturn(pywrap_steal(camlwrap(cb,NULL,0)));
    /* T.F.: camlwrap already gives us a reference. We steal that. */
}

/* For pills, we use an extension: */
value pywrapvalue_pill( value cb ) {
    CAMLparam1(cb);
    CAMLreturn(pywrap_steal(camlwrap_pill(cb,NULL,0)));
}


value pycaml_seterror(value ml_err,value ml_str)
{
  CAMLparam2(ml_err,ml_str);
  PyObject *err;
  int nr_err;

  nr_err=Int_val(ml_err);

  switch(nr_err) {
  case 0:
    err=PyExc_Exception;
    break;
  case 1:
    err=PyExc_StandardError;
    break;
  case 2:
    err=PyExc_ArithmeticError;
    break;
  case 3:
    err=PyExc_LookupError;
    break;
  case 4:
    err=PyExc_AssertionError;
    break;
  case 5:
    err=PyExc_AttributeError;
    break;
  case 6:
    err=PyExc_EOFError;
    break;
  case 7:
    err=PyExc_EnvironmentError;
    break;
  case 8:
    err=PyExc_FloatingPointError;
    break;
  case 9:
    err=PyExc_IOError;
    break;
  case 10:
    err=PyExc_ImportError;
    break;
  case 11:
    err=PyExc_IndexError;
    break;
  case 12:
    err=PyExc_KeyError;
    break;
  case 13:
    err=PyExc_KeyboardInterrupt;
    break;
  case 14:
    err=PyExc_MemoryError;
    break;
  case 15:
    err=PyExc_NameError;
    break;
  case 16:
    err=PyExc_NotImplementedError;
    break;
  case 17:
    err=PyExc_OSError;
    break;
  case 18:
    err=PyExc_OverflowError;
    break;
  case 19:
    err=PyExc_ReferenceError;
    break;
  case 20:
    err=PyExc_RuntimeError;
    break;
  case 21:
    err=PyExc_SyntaxError;
    break;
  case 22:
    err=PyExc_SystemExit;
    break;
  case 23:
    err=PyExc_TypeError;
    break;
  case 24:
    err=PyExc_ValueError;
    break;
  case 25:
    err=PyExc_ZeroDivisionError;
    break;
  default:
    err=PyExc_StandardError;
  }

  PyErr_SetString(err,String_val(ml_str));
  CAMLreturn(Val_unit);
}

value pyunwrapvalue( value cb ) {
    CAMLparam1(cb);
    value *v;
    v = (value *)PyCObject_AsVoidPtr( pyunwrap(cb) );
    CAMLreturn(*v);
}

/* Create the function table */

typedef struct _python_func_table {
    void *func;
    int format;
    int steal_result;
    char *desc;
} python_func_table;

python_func_table the_python_func_table[] = {
/* 1 */
  { (void *)Py_Initialize, 1,1, "Py_Initialize" },
  { (void *)Py_Finalize, 1,1, "Py_Finalize" },
  { (void *)PyErr_Print, 1,1, "PyErr_Print" },
/* 2 */
  { (void *)Py_Exit, 2,1, "Py_Exit" },
  { (void *)PyErr_PrintEx, 2,1, "PyErr_PrintEx" },
/* 3 */
  { (void *)Py_SetProgramName, 3,1, "Py_SetProgramName" },
  { (void *)Py_SetPythonHome, 3,1, "Py_SetPythonHome" },
/* 4 */
  { (void *)Py_IsInitialized, 4,1, "Py_IsInitialized" },
/* 5 */
  { (void *)wrapped_PyRun_SimpleString, 5,1, "PyRun_SimpleString" },
/* 6 */
  { (void *)wrapped_PyRun_AnyFile, 6,1, "PyRun_AnyFile" },
  { (void *)wrapped_PyRun_SimpleFile, 6,1, "PyRun_SimpleFile" },
  { (void *)wrapped_PyRun_InteractiveOne, 6,1, "PyRun_InteractiveOne" },
  { (void *)wrapped_PyRun_InteractiveLoop, 6,1, "PyRun_InteractiveLoop" },
  { (void *)Py_FdIsInteractive, 6,1, "Py_FdIsInteractive" },
/* 7 */
  { (void *)wrapped_PyRun_AnyFileEx, 7,1, "PyRun_AnyFileEx" },
  { (void *)wrapped_PyRun_SimpleFileEx, 7,1, "PyRun_SimpleFileEx" },
/* 8 */
  { (void *)Py_GetProgramName, 8,1, "Py_GetProgramName" },
  { (void *)Py_GetPythonHome, 8,1, "Py_GetPythonHome" },
  { (void *)Py_GetProgramFullPath, 8,1, "Py_GetProgramFullPath" },
  { (void *)Py_GetPrefix, 8,1, "Py_GetPrefix" },
  { (void *)Py_GetExecPrefix, 8,1, "Py_GetExecPrefix" },
  { (void *)Py_GetPath, 8,1, "Py_GetPath" },
  { (void *)Py_GetVersion, 8,1, "Py_GetVersion" },
  { (void *)Py_GetPlatform, 8,1, "Py_GetPlatform" },
  { (void *)Py_GetCopyright, 8,1, "Py_GetCopyright" },
  { (void *)Py_GetCompiler, 8,1, "Py_GetCompiler" },
  { (void *)Py_GetBuildInfo, 8,1, "Py_GetBuildInfo" },
/* 9 */
  { (void *)wrapped_PyRun_String, 9,1, "PyRun_String" },
/* 10 */
  { (void *)wrapped_PyRun_File, 10,1, "PyRun_File" },
/* 11 */
  { (void *)wrapped_PyRun_FileEx, 11,1, "PyRun_FileEx" },
/* 12 */
  { (void *)wrapped_Py_CompileString, 12,1, "Py_CompileString" },

/* Object */
/* 13 */
  { (void *)PyObject_Print, 13,1, "PyObject_Print" },
/* 14 */
  { (void *)PyObject_Repr, 14,1, "PyObject_Repr" },
  { (void *)PyObject_Str, 14,1, "PyObject_Str" },
  { (void *)PyObject_Unicode, 14,1, "PyObject_Unicode" },
/* 15 */
  { (void *)PyObject_RichCompare, 15,1, "PyObject_RichCompare" },
/* 16 */
  { (void *)PyObject_GetAttrString, 16,1, "PyObject_GetAttrString" },
/* 17 */
  { (void *)PyObject_GetAttr, 17,1, "PyObject_GetAttr" },
/* 18 */
  { (void *)PyObject_IsTrue, 18,1, "PyObject_IsTrue" },
  { (void *)PyObject_Not, 18,1, "PyObject_Not" },
  { (void *)PyCallable_Check, 18,1, "PyCallable_Check" },
/* 19 */
  { (void *)PyObject_Compare, 19,1, "PyObject_Compare" },
  { (void *)PyObject_HasAttr, 19,1, "PyObject_HasAttr" },
/* 20 */
  { (void *)PyObject_RichCompareBool, 20,1, "PyObject_RichCompareBool" },
/* 21 */
  { (void *)PyObject_SetAttrString, 21,1, "PyObject_SetAttrString" },
/* 22 */
  { (void *)PyObject_HasAttrString, 22,1, "PyObject_HasAttrString" },
/* 23 */
  { (void *)PyNumber_Coerce, 23,1, "PyNumber_Coerce" },
  { (void *)PyNumber_CoerceEx, 23,1, "PyNumber_CoerceEx" },
/* 24 */
  { (void *)PyObject_SetAttr, 24,1, "PyObject_SetAttr" },
/* 25 */
  { (void *)PyObject_Hash, 25,1, "PyObject_Hash" },

/* Strings */
/* 18 */
  { (void *)PyString_Size, 18,1, "PyString_Size" },
/* 26 */
  { (void *)PyString_AsString, 26,1, "PyString_AsString" },
/* 27 - disabled by T.F. due to refcounting problems! */
  /*
    { (void *)PyString_Concat, 27,1, "PyString_Concat" },
    { (void *)PyString_ConcatAndDel, 27,1, "PyString_ConcatAndDel" },
  */
/* 28 */
  { (void *)PyString_FromString, 28,1, "PyString_FromString" },
/* 17 */
  { (void *)PyString_Format, 17,1, "PyString_Format" },

/* Dictionaries */
/* 29 */
  { (void *)PyDict_New, 29,1, "PyDict_New" },
/* 17 */
  { (void *)PyDict_GetItem, 17,0, "PyDict_GetItem" },
/* 24 */
  { (void *)PyDict_SetItem, 24,1, "PyDict_SetItem" },
/* 19 */
  { (void *)PyDict_DelItem, 19,1, "PyDict_DelItem" },
/* 30 */
  { (void *)PyDict_Clear, 30,1, "PyDict_Clear" },
/* 31 */
  { (void *)PyDict_Next, 31,1, "PyDict_Next" },
/* 14 */
  { (void *)PyDict_Keys, 14,1, "PyDict_Keys" },
  { (void *)PyDict_Values, 14,1, "PyDict_Values" },
  { (void *)PyDict_Items, 14,1, "PyDict_Items" },
  { (void *)PyDict_Copy, 14,1, "PyDict_Copy" },
/* 18 */
  { (void *)PyDict_Size, 18,1, "PyDict_Size" },
/* 16 */
  { (void *)PyDict_GetItemString, 16,0, "PyDict_GetItemString" },
/* 22 */
  { (void *)PyDict_DelItemString, 22,1, "PyDict_DelItemString" },
/* 21 */
  { (void *)PyDict_SetItemString, 21,1, "PyDict_SetItemString" },

/* Integer */
/* 34 */
  { (void *)PyInt_FromLong, 34,1, "PyInt_FromLong" },
/* 25 */
  { (void *)PyInt_AsLong, 25,1, "PyInt_AsLong" },
/* 35 */
  { (void *)PyInt_GetMax, 35,1, "PyInt_GetMax" },

/* Float */
/* 36 */
  { (void *)PyFloat_FromDouble, 36,1, "PyFloat_FromDouble" },
/* 37 */
  { (void *)PyFloat_AsDouble, 37,1, "PyFloat_AsDouble" },

/* Modules */
/* 28 */
  { (void *)PyModule_New, 28,1, "PyModule_New" },
/* 14 */
  { (void *)PyModule_GetDict, 14,0, "PyModule_GetDict" },
/* 26 */
  { (void *)PyModule_GetName, 26,1, "PyModule_GetName" },
  { (void *)PyModule_GetFilename, 26,1, "PyModule_GetFilename" },

/* 39 */
  { (void *)PyTuple_New, 39,1, "PyTuple_New" },
/* 18 */
  { (void *)PyTuple_Size, 18,1, "PyTuple_Size" },
/* 40 */
  { (void *)PyTuple_GetItem, 40,0, "PyTuple_GetItem" },
/* 41 */
  { (void *)PyTuple_SetItem, 41,1, "PyTuple_SetItem" },
/* 13 */
  { (void *)PyTuple_GetSlice, 13,1, "PyTuple_GetSlice" },

/* 42 */
  { (void *)PySlice_New, 42,1, "PySlice_New" },
/* 43 */
  { (void *)PySlice_GetIndices, 43,1, "PySlice_GetIndices" },
/* 44 */
  { (void *)wrapped_PyRange_New, 44,1, "PyRange_New" },

/* Error handling definitions */

/* 30 */
  { (void *)PyErr_SetNone, 30,1, "PyErr_SetNone" },
/* 45 */
  { (void *)PyErr_SetObject, 45,1, "PyErr_SetObject" },
/* 46 */
  { (void *)PyErr_SetString, 46,1, "PyErr_SetString" },
/* 29 */
  { (void *)PyErr_Occurred, 29,0, "PyErr_Occurred" },
/* 1 */
  { (void *)PyErr_Clear, 1,1, "PyErr_Clear" },
/* 47 */
  { (void *)PyErr_Fetch, 47,1, "PyErr_Fetch" },
/* 48 */
  { (void *)PyErr_Restore, 48,1, "PyErr_Restore" },

/* Error testing and normalization */
/* 19 */
  { (void *)PyErr_GivenExceptionMatches, 19,1, "PyErr_GivenExceptionMatches" },
/* 18 */
  { (void *)PyErr_ExceptionMatches, 18,1, "PyErr_ExceptionMatches" },
/* 47 */
  { (void *)PyErr_NormalizeException, 47,1, "PyErr_NormalizeException" },

/* Classes */
/* 42 */
  { (void *)PyClass_New, 42,1, "PyClass_New" },
/* 42 */
  { (void *)PyInstance_New, 42,1, "PyInstance_New" },

/* 17 */
  { (void *)PyInstance_NewRaw, 17,1, "PyInstance_NewRaw" },
/* 42 */
  { (void *)PyMethod_New, 42,1, "PyMethod_New" },
/* 14 */
  { (void *)PyMethod_Function, 14,0, "PyMethod_Function" },
  /* T.F. NOTE: the "0" is important! From: refcounts.dat! */
  { (void *)PyMethod_Self, 14,0, "PyMethod_Self" },
  { (void *)PyMethod_Class, 14,0, "PyMethod_Class" },

/* Module */
/* 28 */
  { (void *)PyModule_New, 28,1, "PyModule_New" },
/* 14 */
  { (void *)PyModule_GetDict, 14,0, "PyModule_GetDict" },
/* 26 */
  { (void *)PyModule_GetName, 26,1, "PyModule_GetName" },
  { (void *)PyModule_GetFilename, 26,1, "PyModule_GetFilename" },
/* 35 */
  { (void *)PyImport_GetMagicNumber, 35,1, "PyImport_GetMagicNumber" },
/* 49 */
  { (void *)PyImport_ExecCodeModule, 49,1, "PyImport_ExecCodeModule" },
/* 50 */
  { (void *)PyImport_ExecCodeModuleEx, 50,1, "PyImport_ExecCodeModuleEx" },
/* 29 */
  { (void *)PyImport_GetModuleDict, 29,0, "PyImport_GetModuleDict" },
/* 28 */
  { (void *)PyImport_AddModule, 28,0, "PyImport_AddModule" },
  { (void *)PyImport_ImportModule, 28,1, "PyImport_ImportModule" },
/* 51 */
  { (void *)wrapped_PyImport_ImportModuleEx, 51,1, "PyImport_ImportModuleEx" },
/* 28 */
  { (void *)PyImport_Import, 28,1, "PyImport_Import" },
/* 14 */
  { (void *)PyImport_ReloadModule, 14,1, "PyImport_ReloadModule" },
/* 1 */
  { (void *)PyImport_Cleanup, 1,1, "PyImport_Cleanup" },
/* 5 */
  { (void *)PyImport_ImportFrozenModule, 5,1, "PyImport_ImportFrozenModule" },

/* Interface to random parts in ceval.c */
/* 42 */
  { (void *)wrapped_PyEval_CallObjectWithKeywords, 42,1, "PyEval_CallObjectWithKeywords" },
/* 17 */
  { (void *)wrapped_PyEval_CallObject, 17,1, "PyEval_CallObject" },

/* 29 */
  { (void *)PyEval_GetBuiltins, 29,0, "PyEval_GetBuiltins" }, /* T.F.: Here, had to consult Python/ceval.c - also for the functions below... */
  { (void *)PyEval_GetGlobals, 29,0, "PyEval_GetGlobals" },
  { (void *)PyEval_GetLocals, 29,0, "PyEval_GetLocals" },
  { (void *)PyEval_GetFrame, 29,0, "PyEval_GetFrame" },
/* 4 */
  { (void *)PyEval_GetRestricted, 4,0, "PyEval_GetRestricted" },

/* Abstract layer */
/* 14 */
  { (void *)PyObject_Type, 14,1, "PyObject_Type" },
/* 18 */
  { (void *)PyObject_Size, 18,1, "PyObject_Size" },
/* 17 */
  { (void *)PyObject_GetItem, 17,1, "PyObject_GetItem" },
/* 24 */
  { (void *)PyObject_SetItem, 24,1, "PyObject_SetItem" },
/* 17 */
  { (void *)PyObject_DelItem, 17,1, "PyObject_DelItem" },
/* 52 */
  { (void *)PyObject_AsCharBuffer, 52,1, "PyObject_AsCharBuffer" },
  { (void *)PyObject_AsReadBuffer, 52,1, "PyObject_AsReadBuffer" },
  { (void *)PyObject_AsWriteBuffer, 52,1, "PyObject_AsWriteBuffer" },
/* 18 */
  { (void *)PyNumber_Check, 18,1, "PyNumber_Check" },
/* 17 */
  { (void *)PyNumber_Add, 17,1, "PyNumber_Add" },
  { (void *)PyNumber_Subtract, 17,1, "PyNumber_Subtract" },
  { (void *)PyNumber_Multiply, 17,1, "PyNumber_Multiply" },
  { (void *)PyNumber_Divide, 17,1, "PyNumber_Divide" },
  { (void *)PyNumber_Remainder, 17,1, "PyNumber_Remainder" },
  { (void *)PyNumber_Divmod, 17,1, "PyNumber_Divmod" },
/* 42 */
  { (void *)PyNumber_Power, 42,1, "PyNumber_Power" },
/* 14 */
  { (void *)PyNumber_Negative, 14,1, "PyNumber_Negative" },
  { (void *)PyNumber_Positive, 14,1, "PyNumber_Positive" },
  { (void *)PyNumber_Absolute, 14,1, "PyNumber_Absolute" },
  { (void *)PyNumber_Invert, 14,1, "PyNumber_Invert" },
/* 17 */
  { (void *)PyNumber_Lshift, 17,1, "PyNumber_Lshift" },
  { (void *)PyNumber_Rshift, 17,1, "PyNumber_Rshift" },
  { (void *)PyNumber_And, 17,1, "PyNumber_And" },
  { (void *)PyNumber_Xor, 17,1, "PyNumber_Xor" },
  { (void *)PyNumber_Or, 17,1, "PyNumber_Or" },
/* 14 */
  { (void *)PyNumber_Int, 14,1, "PyNumber_Int" },
  { (void *)PyNumber_Long, 14,1, "PyNumber_Long" },
  { (void *)PyNumber_Float, 14,1, "PyNumber_Float" },
/* 17 */
  { (void *)PyNumber_InPlaceAdd, 17,1, "PyNumber_InPlaceAdd" },
  { (void *)PyNumber_InPlaceSubtract, 17,1, "PyNumber_InPlaceSubtract" },
  { (void *)PyNumber_InPlaceMultiply, 17,1, "PyNumber_InPlaceMultiply" },
  { (void *)PyNumber_InPlaceDivide, 17,1, "PyNumber_InPlaceDivide" },
  { (void *)PyNumber_InPlaceRemainder, 17,1, "PyNumber_InPlaceRemainder" },
  { (void *)PyNumber_InPlaceLshift, 17,1, "PyNumber_InPlaceLshift" },
  { (void *)PyNumber_InPlaceRshift, 17,1, "PyNumber_InPlaceRshift" },
  { (void *)PyNumber_InPlaceAnd, 17,1, "PyNumber_InPlaceAnd" },
  { (void *)PyNumber_InPlaceXor, 17,1, "PyNumber_InPlaceXor" },
  { (void *)PyNumber_InPlaceOr, 17,1, "PyNumber_InPlaceOr" },
/* 42 */
  { (void *)PyNumber_InPlacePower, 42,1, "PyNumber_InPlacePower" },
/* 18 */
  { (void *)PySequence_Check, 18,1, "PySequence_Check" },
  { (void *)PySequence_Size, 18,1, "PySequence_Size" },
  { (void *)PySequence_Length, 18,1, "PySequence_Length" },
/* 17 */
  { (void *)PySequence_Concat, 17,1, "PySequence_Concat" },
/* 40 */
  { (void *)PySequence_Repeat, 40,1, "PySequence_Repeat" },
  { (void *)PySequence_GetItem, 40,1, "PySequence_GetItem" },
/* 13 */
  { (void *)PySequence_GetSlice, 13,1, "PySequence_GetSlice" },
/* 41 */
  { (void *)PySequence_SetItem, 41,0, "PySequence_SetItem" },
/* 20 */
  { (void *)PySequence_DelItem, 20,1, "PySequence_DelItem" },
/* 53 */
  { (void *)PySequence_SetSlice, 53,1, "PySequence_SetSlice" },
/* 54 */
  { (void *)PySequence_DelSlice, 54,1, "PySequence_DelSlice" },
/* 14 */
  { (void *)PySequence_Tuple, 14,1, "PySequence_Tuple" },
  { (void *)PySequence_List, 14,1, "PySequence_List" },
/* 16 */
  { (void *)PySequence_Fast, 16,1, "PySequence_Fast" },
/* 19 */
  { (void *)PySequence_Count, 19,1, "PySequence_Count" },
  { (void *)PySequence_Contains, 19,1, "PySequence_Contains" },
  { (void *)PySequence_In, 19,1, "PySequence_In" },
  { (void *)PySequence_Index, 19,1, "PySequence_Index" },
/* 17 */
  { (void *)PySequence_InPlaceConcat, 17,1, "PySequence_InPlaceConcat" },
/* 22 */
  { (void *)PySequence_InPlaceRepeat, 22,1, "PySequence_InPlaceRepeat" },
/* 18 */
  { (void *)PyMapping_Check, 18,1, "PyMapping_Check" },
  { (void *)PyMapping_Size, 18,1, "PyMapping_Size" },
  { (void *)PyMapping_Length, 18,1, "PyMapping_Length" },
/* 16 */
  { (void *)PyMapping_HasKeyString, 16,1, "PyMapping_HasKeyString" },
/* 19 */
  { (void *)PyMapping_HasKey, 19,1, "PyMapping_HasKey" },
/* 16 */
  { (void *)PyMapping_GetItemString, 16,1, "PyMapping_GetItemString" },
/* 41 */
  { (void *)PyMapping_SetItemString, 41,0, "PyMapping_SetItemString" },
/* 28 */
  { (void *)OurPy_InitEmptyModule, 28,0, "OurPy_InitEmptyModule" },

/* End */
{ NULL, 0 }
};

value pygetfuncarray( value unit ) {
    CAMLparam1(unit);
    CAMLlocal2(retv,tuplev);
    int i;
    int total_funcs;

    for( i = 0; the_python_func_table[i].func; i++ ) ; total_funcs = i;

    retv = alloc(total_funcs,0);

    for( i = 0; i < total_funcs; i++ ) {
	tuplev = alloc_tuple( 4 );
	Store_field(tuplev,0,funcwrap((void *)the_python_func_table[i].func));
	Store_field(tuplev,1,Val_int(the_python_func_table[i].format));
	Store_field(tuplev,2,Val_int(i));
	Store_field(tuplev,3,Val_bool(the_python_func_table[i].steal_result));
	Store_field(retv,i,tuplev);
    }

    CAMLreturn(retv);
}

enum PycamlTypeLabels {
    TupleType = 0,
    StringType,
    IntType,
    FloatType,
    ListType,
    BoolType,
    NoneType,
    CallableType,
    ModuleType,
    ClassType,
    TypeType,
    DictType,
    NullType,
    CamlpillType,
    OtherType
};

value pytype( value obj ) {
    CAMLparam1(obj);
    PyObject *pobj = pyunwrap( obj );
    if( !pobj ) CAMLreturn(NullType);
    else if( PyTuple_Check( pobj ) ) CAMLreturn(Val_int(TupleType));
    else if( PyString_Check( pobj ) ) CAMLreturn(Val_int(StringType));
    else if( PyBool_Check( pobj ) ) CAMLreturn(Val_int(BoolType));
    else if( PyInt_Check( pobj ) ) CAMLreturn(Val_int(IntType));
    else if( PyFloat_Check( pobj ) ) CAMLreturn(Val_int(FloatType));
    else if( PyList_Check( pobj ) ) CAMLreturn(Val_int(ListType));
    else if( pobj == Py_None ) CAMLreturn(Val_int(NoneType));
    else if( PyCallable_Check( pobj ) ) CAMLreturn(Val_int(CallableType));
    else if( PyModule_Check( pobj ) ) CAMLreturn(Val_int(ModuleType));
    else if( PyClass_Check( pobj ) ) CAMLreturn(Val_int(ClassType));
    else if( PyType_Check( pobj ) ) CAMLreturn(Val_int(TypeType));
    else if( PyDict_Check( pobj ) ) CAMLreturn(Val_int(DictType));
    else if( PyCObject_Check( pobj ) )
      {
	void *desc=PyCObject_GetDesc(pobj);
	if(desc==(void *)ocamlpill_token)
	  {
	    CAMLreturn(Val_int(CamlpillType));
	  }
	else
	  {
	    CAMLreturn(Val_int(OtherType));
	  }
      }
    else CAMLreturn(Val_int(OtherType));
}

value pynull( value unit ) {
    CAMLparam1(unit);
    CAMLreturn(pywrap(0));
}

value pynone( value unit ) {
    CAMLparam1(unit);
    CAMLreturn(pywrap(Py_None));
}

value pytuple_fromarray( value array ) {
    CAMLparam1(array);
    PyObject *tuple = PyTuple_New(Wosize_val(array));
    int i;
    int x;

    for( i = 0; i < Wosize_val(array); i++ )
      {
	PyObject *entry;
	entry=pyunwrap(Field(array,i));
	/* T.F.:
	   entry's reference count was increased by one because it is visible
	   from within OCaml (and OCaml's GC will take care of decreasing
	   this again upon finalization. But now, we do add another use to
	   entry. So, we have to increase its reference count manually:

	   Note that even if tuple contained some python value before
	   (which it does not), we do not have to Py_DECREF the
	   reference count on the old entry, as the PyTuple_SetItem/PyList_SetItem
	   does this of its own! Nice, isn't it?
	*/
	Py_INCREF(entry);
	x = PyTuple_SetItem(tuple,i,entry);
      }

    CAMLreturn(pywrap_steal(tuple));
}

value pytuple_toarray( value array ) {
    CAMLparam1(array);
    PyObject *obj = pyunwrap(array);
    int i;
    CAMLlocal1(rv);

    rv = alloc_tuple( PySequence_Size(obj) );
    /* XXX T.F.: actually, using alloc_tuple to get an array is not overly aesthetic... */

    for( i = 0; i < PySequence_Size(obj); i++ )
	Store_field(rv,i,pywrap_steal(PySequence_GetItem(obj,i)));

    CAMLreturn(rv);
}

value pywrap_closure( value closure ) {
    CAMLparam1(closure);
    PyMethodDef ml;
    PyObject *obj;
    PyMethodDef *ml_def;
    ml.ml_name = "anonymous_closure";
    ml.ml_meth = pycall_callback;
    ml.ml_flags = 1;
    ml.ml_doc = "Anonymous closure";
    obj = camlwrap(closure,&ml,sizeof(ml));
    ml_def = (PyMethodDef *)caml_aux(obj);
    CAMLreturn(pywrap_steal(PyCFunction_New(ml_def,obj)));
}

/*
value pymodule_initmodule( value name, value funclist ) { ... }
Removed by T.F., as this piece of code seemed quite buggy....
*/

/* -- T.F. Extensions -- */

/*
  In case of "You used it the wrong way" or "this should not happen",
  we want to be able to just bailout from python into ocaml with an
  exception.


  Note: this is not being used, as we decided to do error handling the
  other way round: python is in charge, ocaml lies "beneath the surface",
  so we effectively always bailout into python.
 */

static int pycaml_raise_error(int type, char *message)
{
  CAMLlocal1(ex);
  ex=alloc_tuple(3);
  Store_field(ex,0,Val_int(type));
  Store_field(ex,1,copy_string(message));
  raise_with_arg(*caml_named_value("ocaml_exn_pycaml"),ex);
}

/* These were missing */
value pybool_frombool(value x) {
  CAMLparam1(x);
  PyObject *py_tf = (Bool_val(x)) ? Py_True : Py_False;

  /* As Py_True/Py_False are expected to behave like Py_None,
     we use pywrap() here, rather than pywrap_steal()
   */
  CAMLreturn(pywrap(py_tf));
}

value pybool_asbool(value py_x) {
  CAMLparam1(py_x);
  PyObject *obj = pyunwrap(py_x);
  int tf;

  tf=(obj == Py_True)?1:0;

  CAMLreturn(Val_bool(tf));
}

/* This is just an adjusted copy of pytuple_fromarray. */
value pylist_fromarray( value array ) {
    CAMLparam1(array);
    PyObject *list = PyList_New(Wosize_val(array));
    int i;
    int x;

    for( i = 0; i < Wosize_val(array); i++ )
      {
	PyObject *entry;
	entry=pyunwrap(Field(array,i));
	/* T.F.: See pytuple_fromarray code comments! */
	Py_INCREF(entry);
	x = PyList_SetItem(list,i,entry);
      }
    CAMLreturn(pywrap_steal(list));
}

/* We also need it the other way round */
value pylist_toarray( value pylist ) {
    CAMLparam1(pylist);
    PyObject *obj = pyunwrap(pylist);
    int i,len;
    CAMLlocal1(rv);

    rv = alloc_tuple( PySequence_Size(obj) );

    len=PySequence_Size(obj);

    for( i = 0; i < len; i++ )
	Store_field(rv,i,pywrap_steal(PySequence_GetItem(obj,i)));

    CAMLreturn(rv);
}

value pylist_set( value pylist, value index, value v ) {
  CAMLparam3(pylist,index,v);
  PyObject *list, *new_entry;

  list = pyunwrap(pylist);
  new_entry=pyunwrap(v);
  Py_INCREF(new_entry);
  PyList_SetItem(list,Int_val(index),new_entry);

  CAMLreturn(Val_unit);
}

value pylist_get( value pylist, value index) {
  CAMLparam2(pylist,index);
  PyObject *list = pyunwrap(pylist);

  /* T.F.: According to the Python docs, we own the reference produced by
     PySequence_GetItem. Hence, we have to steal that reference...
   */
  CAMLreturn(pywrap_steal(PySequence_GetItem(list,Int_val(index))));
}



/* It's nice to have this variant of pywrap_closure */
value pywrap_closure_docstring(value docstring, value closure) {
  CAMLparam2(docstring, closure);
  PyMethodDef ml;
  PyObject *obj;
  PyMethodDef *ml_def;
  ml.ml_name = "anonymous_closure";
  ml.ml_meth = pycall_callback;
  ml.ml_flags = 1;
  ml.ml_doc = String_val(docstring);
  obj = camlwrap(closure,&ml,sizeof(ml));
  ml_def = (PyMethodDef *)caml_aux(obj);
  CAMLreturn(pywrap_steal(PyCFunction_New(ml_def,obj)));
}

/* Using pyrun_interactiveloop the way it was in the original code
   may work on some systems, but then just by chance. We have to
   do this in a cleaner way:
 */
value pycaml_prompt(value ml_unit) {
  CAMLparam1(ml_unit);

  PyRun_InteractiveLoop(stdin,"<stdin>");

  CAMLreturn(Val_unit);
}


/* The function below is highly useful for debugging! */
value pyrefcount(value pyobj) {
  CAMLparam1(pyobj);
  PyObject *obj = pyunwrap(pyobj);

  CAMLreturn(Val_int(obj->ob_refcnt));
}

/***************************************************************************
 *                         RAW NumPy ARRAY SUPPORT                         *
 ***************************************************************************/
#include "configuration.h"

#ifdef NUMPY_INCLUDE_PATH
#  define HAVE_NUMPY_ARRAYOBJECT_H
#endif

#ifdef HAVE_NUMPY_ARRAYOBJECT_H
#  include "numpy/arrayobject.h"
#endif

#ifdef HAVE_NUMPY_ARRAYOBJECT_H
/* To be called before using any function from the NumPy API */
static void init_numarr_package(void) {
  static int numpy_is_initialised = 0;
  if (numpy_is_initialised)
    return;
  import_array();
  numpy_is_initialised = 1;
}
#endif

value raw_have_numarr(value ml_unit) {
  CAMLparam1(ml_unit);
#ifdef HAVE_NUMPY_ARRAYOBJECT_H
  CAMLreturn(Val_int((int) 1));
#else
  CAMLreturn(Val_int((int) 0));
#endif
}

#ifdef HAVE_NUMPY_ARRAYOBJECT_H
static int numpytype_from_ba_type(int ba_type) {
  switch (ba_type & CAML_BA_KIND_MASK) {
  case    CAML_BA_FLOAT32: return PyArray_FLOAT32;
  case    CAML_BA_FLOAT64: return PyArray_FLOAT64;
  case      CAML_BA_SINT8: return PyArray_INT8;
  case      CAML_BA_UINT8: return PyArray_UINT8;
  case     CAML_BA_SINT16: return PyArray_INT16;
  case     CAML_BA_UINT16: return PyArray_UINT16;
  case      CAML_BA_INT32: return PyArray_INT32;
  case      CAML_BA_INT64: return PyArray_INT64;
  case   CAML_BA_CAML_INT: return -1;
  case CAML_BA_NATIVE_INT: return -1;
  case  CAML_BA_COMPLEX32: return PyArray_COMPLEX64;
  case  CAML_BA_COMPLEX64: return PyArray_COMPLEX128;
  default: return -1;
  }
}

static int numpytype_to_ba_type(int numpytype) {
  switch (numpytype) {
  case       PyArray_INT8: return CAML_BA_SINT8;
  case      PyArray_UINT8: return CAML_BA_UINT8;
  case      PyArray_INT16: return CAML_BA_SINT16;
  case     PyArray_UINT16: return CAML_BA_UINT16;
  case      PyArray_INT32: return CAML_BA_INT32;
  case      PyArray_INT64: return CAML_BA_INT64;
  case    PyArray_FLOAT32: return CAML_BA_FLOAT32;
  case    PyArray_FLOAT64: return CAML_BA_FLOAT64;
  case  PyArray_COMPLEX64: return CAML_BA_COMPLEX32;
  case PyArray_COMPLEX128: return CAML_BA_COMPLEX64;
  default: return -1;
  }
}

static int numpytype_from_eltype(int eltype) {
  switch(eltype) {
  case 0: return PyArray_INT;
  case 1: return PyArray_FLOAT;
  case 2: return PyArray_DOUBLE;
  default:
    caml_invalid_argument("Pycaml(NumPy): Unsupported NumPy array type!");
    assert(0); /* this function shouldn't return */
    return 0; /* to keep CC happy */
  }
}

/* Note the PyArray_INT32, PyArray_INT64, etc. are defined in terms of
 * PyArray_INT, PyArray_LONG, etc.
 */
static int eltype_from_numpytype(int numpytype) {
  switch(numpytype) {
  case PyArray_INT: return 0;
  case PyArray_FLOAT: return 1;
  case PyArray_DOUBLE: return 2;
  default:
    caml_invalid_argument("Pycaml(NumPy): Unsupported NumPy array type!");
    assert(0);
    return 0;
  }
}

#else
static void numpy_na(void) {
  caml_invalid_argument("Pycaml(NumPy): Pycaml was compiled without NumPy "
                        "support!");

}
#endif

value pyarray_create(value element_type, value dim) {
  CAMLparam2(element_type, dim);
#ifdef HAVE_NUMPY_ARRAYOBJECT_H
  int arr_type = numpytype_from_eltype(Int_val(element_type));
  npy_intp dims[] = {Int_val(dim)};
  PyObject *numarr = NULL;
  init_numarr_package();
  numarr = PyArray_SimpleNew(1, dims, arr_type);
  CAMLreturn(pywrap_steal(numarr));
#else
  CAMLreturn(pywrap(Py_None));
#endif
}

value pyarray_set(value pyarray, value index, value value_to_set) {
  CAMLparam3(pyarray, index, value_to_set);
#ifdef HAVE_NUMPY_ARRAYOBJECT_H
  PyObject *arr = pyunwrap(pyarray);
  int arr_type = PyArray_TYPE(arr);

  if (!PyArray_Check(arr)) {
    caml_invalid_argument("Pycaml.numarr_set: the given pyobject is not a "
                          "NumPy array!");

  } else {
    npy_intp *dims = PyArray_DIMS(arr),
             i = Int_val(index);
    if (i >= 0 && i < dims[0]) {
      void *data = PyArray_DATA(arr);
      switch(arr_type) {
      case PyArray_INT:
        ((int *) data)[i] = Int_val(value_to_set);
        CAMLreturn(Val_unit);
      case PyArray_FLOAT:
        ((float *) data)[i] = (float) Double_val(value_to_set);
        CAMLreturn(Val_unit);
      case PyArray_DOUBLE:
        ((double *) data)[i] = Double_val(value_to_set);
        CAMLreturn(Val_unit);
      default:
        caml_invalid_argument("Pycaml.numarr_set: the given NumPy array does "
                              "have an unsupported element type!");
      }

    } else {
      caml_invalid_argument("Pycaml.numarr_set: index out of bounds!");
    }
  }
#else
  numpy_na();
#endif
  CAMLreturn(Val_unit);
}

value pyarray_get(value pyarray, value index) {
  CAMLparam2(pyarray, index);
#ifdef HAVE_NUMPY_ARRAYOBJECT_H
  PyObject *arr = pyunwrap(pyarray);
  int arr_type = PyArray_TYPE(arr);

  if (!PyArray_Check(arr)) {
    caml_invalid_argument("Pycaml.numarr_get: the given pyobject is not a "
                          "NumPy array!");

  } else {
    npy_intp *dims = PyArray_DIMS(arr),
             i = Int_val(index);
    if (i >= 0 && i < dims[0]) {
      void *data = PyArray_DATA(arr);
      switch(arr_type) {
      case PyArray_INT:    CAMLreturn(Val_int(((int *) data)[i]));
      case PyArray_FLOAT:  CAMLreturn(copy_double(((float *) data)[i]));
      case PyArray_DOUBLE: CAMLreturn(copy_double(((double *) data)[i]));
      default:
        caml_invalid_argument("Pycaml.numarr_get: the given NumPy array does "
                              "have an unsupported element type!");
      }

    } else {
      caml_invalid_argument("Pycaml.numarr_get: index out of bounds!");
    }
  }
#else
  numpy_na();
#endif
  assert(0);
  CAMLreturn(Val_unit); /* just to make the CC happy */
}

value pyarray_kind(value pyarray) {
  CAMLparam1(pyarray);
#ifdef HAVE_NUMPY_ARRAYOBJECT_H
  PyObject *arr = pyunwrap(pyarray);
  CAMLreturn(Val_int(eltype_from_numpytype(PyArray_TYPE(arr))));
#else
  numpy_na();
  CAMLreturn(Val_unit);
#endif
}

value pyarray_length(value pyarray) {
  CAMLparam1(pyarray);
#ifdef HAVE_NUMPY_ARRAYOBJECT_H
  PyObject *arr = pyunwrap(pyarray);
  npy_intp *dims = PyArray_DIMS(arr);
  CAMLreturn(Val_int(dims[0]));
#else
  numpy_na();
  CAMLreturn(Val_unit);
#endif
}

value pytensor_create(value element_type, value dims) {
  CAMLparam2(element_type, dims);
#ifdef HAVE_NUMPY_ARRAYOBJECT_H
  PyObject *numarr = NULL;
  int dims_size = Wosize_val(dims);
  npy_intp *my_dims = (npy_intp *) malloc(sizeof(npy_intp)*dims_size);
  if (my_dims != NULL) {
    int i, tensor_type;
    for(i = 0; i < dims_size; i++)
      my_dims[i] = Int_val(Field(dims, i));

    init_numarr_package();
    tensor_type = numpytype_from_eltype(Int_val(element_type));
    numarr = PyArray_SimpleNew(dims_size, my_dims, tensor_type);
    free(my_dims);
    CAMLreturn(pywrap_steal(numarr));

  } else
    caml_invalid_argument("Pycaml.pytensor_create: error.");

#else
  numpy_na();
#endif
  CAMLreturn(Val_unit);
}

value pytensor_create_from_bigarray_raw(value ml_ba) {
  CAMLparam1(ml_ba);
#ifdef HAVE_NUMPY_ARRAYOBJECT_H
  PyObject *numarr = NULL;
  struct caml_ba_array *ba;
  int rank;
  npy_intp *my_dims;
  int ba_type, na_type;

  ba=Caml_ba_array_val(ml_ba);
  rank = ba->num_dims;

  if((ba->flags &  CAML_BA_LAYOUT_MASK) !=  CAML_BA_C_LAYOUT) {
    caml_invalid_argument("Pycaml.pytensor_create_from_bigarray: bigarray must have C layout!");
  }

  ba_type = (ba->flags & BIGARRAY_KIND_MASK);
  na_type = numpytype_from_ba_type(ba_type);

  /* Don't know what this was - so, let's just create a byte array... */
  if (na_type == -1)
    na_type = PyArray_UINT8;

  my_dims = (npy_intp *) malloc(sizeof(npy_intp)*rank);
  if (my_dims != NULL) {
    int i;
    for(i = 0; i < rank; i++)
      my_dims[i] = ba->dim[i];

    init_numarr_package();

    numarr = PyArray_SimpleNewFromData(rank, my_dims, na_type, ba->data);
    free(my_dims);

    CAMLreturn(pywrap_steal(numarr));

  } else
    caml_invalid_argument("Pycaml.pytensor_create_from_bigarray: error.");

#else
  fprintf(stderr,"DDD pytensor_create_from_bigarray_raw - NO NUMPY\n");fflush(stderr);
  numpy_na();
#endif
  CAMLreturn(Val_unit);
}


/* This function works only with arrays created by us:
 * does not support stripes etc.
 */
value raw_pytensor_set(value pytensor, value indices, value value_to_set) {
  CAMLparam3(pytensor, indices, value_to_set);
#ifdef HAVE_NUMPY_ARRAYOBJECT_H
  PyObject *numarr = pyunwrap(pytensor);
  int tensor_type = PyArray_TYPE(numarr),
      int_tensor = (tensor_type == PyArray_INT);
  if (!PyArray_Check(numarr)) {
    CAMLreturn(Val_int(1)); /* not a NumPy array */

  } else if (!int_tensor && tensor_type != PyArray_DOUBLE) {
    CAMLreturn(Val_int(2)); /* items are not double float numbers */

  } else {
    void *data = PyArray_DATA(numarr);
    int indices_size = Wosize_val(indices), i;
    npy_intp *dims = PyArray_DIMS(numarr);
    long flat_index = 0;
    for(i = 0; i < indices_size; i++) {
      long this_index = Int_val(Field(indices, i)),
           max_index = dims[i];
      if (this_index < 0 || this_index >= max_index) {
        CAMLreturn(Val_int(3)); /* index out of bounds */
      }
      flat_index = flat_index*max_index + this_index;
    }

    if (int_tensor)
      ((int *) data)[flat_index] = Int_val(value_to_set);
    else
      ((double *) data)[flat_index] = Double_val(value_to_set);
    CAMLreturn(Val_int(0));
  }
#endif
  CAMLreturn(Val_int(-1));
}

value pytensor_to_ba_raw(value v_pytensor) {
  CAMLparam1(v_pytensor);

#ifdef HAVE_NUMPY_ARRAYOBJECT_H
  PyObject *pytensor = pyunwrap(v_pytensor);
  int is_fortran_array, is_c_array;

  CAMLlocal1(v_ba);

  init_numarr_package();

  is_fortran_array = PyArray_ISFARRAY(pytensor);
  is_c_array = PyArray_ISCARRAY(pytensor);

  if (is_fortran_array || is_c_array) {
    /* Behaved array in Fortran or C order */
    int nd = PyArray_NDIM(pytensor);
    npy_intp *dims = PyArray_DIMS(pytensor);
    void *data = PyArray_BYTES(pytensor);

    if (nd <= CAML_BA_MAX_NUM_DIMS) {
      /* BA parameters */
      int numpytype = numpytype_to_ba_type(PyArray_TYPE(pytensor));

      if (numpytype != -1) {
        int i, ba_flags = 0;
        intnat ml_dims[CAML_BA_MAX_NUM_DIMS];

        for (i = 0; i < nd; i++)
          ml_dims[i] = dims[i];

        ba_flags = (numpytype & CAML_BA_KIND_MASK);
        ba_flags |= (is_c_array) ? CAML_BA_C_LAYOUT : CAML_BA_FORTRAN_LAYOUT;

        v_ba = caml_ba_alloc(ba_flags, nd, data, ml_dims);
        CAMLreturn(v_ba);

      } else
        caml_invalid_argument("Pycaml(NumPy): Unrecognized array type!");

    } else
      caml_invalid_argument("Pycaml(NumPy): Numpy array has too many "
                            "dimensions to be converted to a Bigarray!");

  } else
    caml_invalid_argument("Pycaml(NumPy): Array should be contiguous "
                          "and behaved!");

  assert(0);
#else
  caml_invalid_argument("Pycaml(NumPy): Missing NumPy support.");

#endif
  CAMLreturn(Val_unit);
}

value pytensor_of_pyobject(value v_kind, value v_pyobj) {
  CAMLparam2(v_kind, v_pyobj);
#ifdef HAVE_NUMPY_ARRAYOBJECT_H
  PyObject *pyobj = pyunwrap(v_pyobj);

  init_numarr_package();

  if (PyArray_Check(pyobj)) {
    if (eltype_from_numpytype(PyArray_TYPE(pyobj)) == Int_val(v_kind))
      CAMLreturn(v_pyobj);

    else
      caml_invalid_argument("Numpy array has wrong element type!");

  } else
    caml_invalid_argument("Object is not a numpy array!");

#endif
  CAMLreturn(v_pyobj);
}
