(* (C) 2006 Dr. Thomas Fischbacher *)

(** Fastfields "call-a-c-code-string-as-function" library. *)

(**
{2 What it does}

   This library allows one to very conveniently "use a string
   containing C code as a callable function", so that parts of the
   behaviour of a program that otherwise would have to be provided at
   compile time can be configured at run-time.

   This functionality for now is only provided for "fields",
   i.e. mappings from [R^n] (position) to [R^m] (some vector field value).
   This is in part for similicity reasons, and in part because this
   should cover the largest class of problems where one would use the
   technique employed here.

   Under the hood, this module calls the C compiler at run time and
   then uses the dynamic linking loader ([dlopen] and friends) to
   generate and load object code. A weak hash is used to keep track of
   functions that have become garbage and recompilation is done lazily
   (on demand).

   A prototypical application is to specify the node density of a mesh
   as a function for a mesh generator.
*)

(**
{2 Interface}
*)

val version : unit -> string
(** This function returns an unique code version string *)

exception Fastfields_eval_problem;;

(** User-provided C code implementing a field is supposed to work with two
   [double*] pointers, usually named [position] and [result], which provide
   input and output arguments. Furthermore, the user-provided block of code
   may abort the calculation early via [return 0], which is mapped
   to this exception on the OCaml side.
 *)

type c_field;;

(** We might want to provide different calling interfaces to a given
    piece of C code - e.g. modifying OCaml float arrays or
    bigarrays, or mapping float arrays to float arrays.
    Hence, C code is not directly mapped to an OCaml function,
    but to some opaque intermediate entity which then can be dressed up with
    different calling interfaces.
 *)

type c_manipulator;;

(** Another interface for a different kind of function. Normally,
    the user should not have to know about this, as it is
    for quite esoteric applications.
 *)

val init : unit -> unit
val finish : unit -> unit

(** [init] and [finish] are idempotent initialization and finalization
   functions that get called automatically if required. Normally, the
   user should not have to worry about them. They prepare the temporary
   directory where the compiler will place its files and libraries.
 *)

val set_directory : string -> unit
val set_cc : string -> unit
val set_cc_opts : string -> unit
val set_cc_debug_handle: out_channel option -> out_channel option

(** These functions set the template pathname for [mkdtemp] to make
    the temporary directory, as well as the compiler name and options for
    dynamically calling the C compiler.

    [set_cc_debug_handle] will register a file handle option as a
    debug-output to log compiler calls. Set to [None] (default) to
    disable this feature. Return value is the previous debug handle.
 *)

val register_include : ?library_file:bool -> string -> unit

(** We may want to include extra header files to provide definitions
for user-specified C code. This is done with [register_include], which
will also take care that no includes are multiplicated unnecessarily.

The parameter [library_file] defaults to [true] controls whether
the include generated should be of the form [#include <...>] or
[#include "..."]. Note that in the latter case, a full path should
be provided, as the intermediate C source file is placed in a
special temporary directory.
*)

val add_header_def : string -> unit

(** 

  *)

val do_relink_if_necessary: unit -> unit

(** Implementing (lazy) recompilation-on-demand-only is easy
    as long as we only deal with wrapping up C strings as
    OCaml-callable functions. But as soon as we allow users
    of this module to build and pass around pointers to
    compiled C code, it is up to them to help us getting
    relinking-on-demand done properly. This is what we provide
    [do_relink_if_necessary] for. Any library implementor
    must make sure to call this immediately before passing a
    C manipulator to a C function that may extract the
    C function pointer.
*)

val c_register_field :
  ?in_name:string ->
  ?out_name:string ->
  ?extra_defines:(string * string list * string) list ->
  string -> c_field

(** This is the central function in this library that maps a string to
   an opaque callable C entity. The optional parameters [in_name] and
   [out_name] default to [position] and [result] and provide the names of
   the input and output [double*] arguments. [extra_defines] is a list of
   [(define_name,define_args,define_RHS)] that provides additional
   [#define/#undef] entries to be wrapped around the definition
   of this function. The idea is to allow alias-names like
   [#define x_pos position[0]], which may also be used as lvalues.
 *)

val c_register_field_manipulator :
  ?buffer_name:string ->
  ?posbuffer_name:string ->
  ?extra_defines:(string * string list * string) list ->
  string -> c_manipulator
(**
   This is almost the same as the previous function,
   only that this expects two arrays of pointers to memory locations
   where numbers are to be stored: (double **in, double **out)

   The uses for [c_field_manipulator] values are somewhat esoteric -
   this is mainly of interest only for building other applications in
   top of the fastfields module. It is expected that only very few
   users will have a reason to look deeper into this.

   XXX right now, there is no calling interface for this.
   What would we like to provide / what do we need?
   
   Given: two sets of vectors (we may assume these are available
   as bigarrays - as they are non-moveable, and may
   just as well be ML-interfaced), and two sets of index sets
   (int array array); call the C function in such a way that it
   has access to all the slots in the in- and out- vector entries
   described by the corresponding index sets.
 *)


val c_field_evaluator_mapping_float_array_to_float_array :
  ?allocate_every_result:bool ->
  int -> c_field ->
  (float array -> float array)

val c_field_evaluator_modifying_float_array :
  c_field ->
  (position:float array -> result:float array -> unit)

val c_field_evaluator_modifying_bigarray :
  c_field ->
  (position:(float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  result:(float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  unit)

(** These three functions dress up a [c_field] with calling
conventions, so that it can be used as a function operating on the
values of a [~position] and [~result] named-argument array, or a
function mapping float arrays to float arrays.
(We then also have to specify the length of the output array.)

The [allocate_every_result] parameter defaults to [true] and controls
whether every evaluation should allocate its own result vector, or
whether one single vector should be allocated beforehand and re-used
over and over again. The default variant is the robust one. Only
change this if you know what you are doing!
*)

(** Esoteric Add-Ons *)

type c_funptr

val _c_manipulator_funptr: c_manipulator -> c_funptr

(** Users of this library should not have to deal with this. This is
   only for implementors who build other low-level libraries on top of
   the fastfields module.
 *)

val opt_remove_stale_c_functions: bool ref;;

(** XXX Document me! *)

