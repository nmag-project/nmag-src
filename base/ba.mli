(* Nmag micromagnetic simulator
   Copyright (C) 2010 University of Southampton
   Hans Fangohr, Thomas Fischbacher, Matteo Franchin and others

   WEB:     http://nmag.soton.ac.uk
   CONTACT: nmag@soton.ac.uk

   AUTHOR(S) OF THIS FILE: Matteo Franchin
   LICENSE: GNU General Public License 2.0
            (see <http://www.gnu.org/licenses/>)

  Types and functionality at the base of the Nsim package.
 *)

type c_layout = Bigarray.c_layout
val c_layout: c_layout Bigarray.layout

(* Abbreviations for creating arrays of floating point numbers (64 bits) *)
type float_elt = Bigarray.float64_elt
type float_kind = (float, float_elt) Bigarray.kind

type farray  = (float, float_elt, c_layout) Bigarray.Array1.t
type farray2 = (float, float_elt, c_layout) Bigarray.Array2.t
type farray3 = (float, float_elt, c_layout) Bigarray.Array3.t

val float_ba: float_kind

val create_farray1: int -> farray
val create_farray2: int -> int -> farray2
val create_farray3: int -> int -> int -> farray3

(* Abbreviations for creating arrays of native integers *)
type int_elt = Bigarray.int_elt
type int_kind = (int, int_elt) Bigarray.kind

type iarray  = (int, int_elt, c_layout) Bigarray.Array1.t
type iarray2 = (int, int_elt, c_layout) Bigarray.Array2.t
type iarray3 = (int, int_elt, c_layout) Bigarray.Array3.t

val int_ba: int_kind

val create_iarray1: int -> iarray
val create_iarray2: int -> int -> iarray2
val create_iarray3: int -> int -> int -> iarray3

(* Abbreviations for creating arrays of short integers (32 bits) *)
type short_elt = Bigarray.int32_elt
type short_kind = (int32, short_elt) Bigarray.kind

type sarray  = (int32, short_elt, c_layout) Bigarray.Array1.t
type sarray2 = (int32, short_elt, c_layout) Bigarray.Array2.t
type sarray3 = (int32, short_elt, c_layout) Bigarray.Array3.t

val short_ba: short_kind

val create_sarray1: int -> sarray
val create_sarray2: int -> int -> sarray2
val create_sarray3: int -> int -> int -> sarray3
