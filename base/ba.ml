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

(* We use C layout *)
type c_layout = Bigarray.c_layout
let c_layout = Bigarray.c_layout

(* Abbreviations for creating arrays of floating point numbers *)
type float_elt = Bigarray.float64_elt
type float_kind = (float, float_elt) Bigarray.kind

type farray  = (float, float_elt, c_layout) Bigarray.Array1.t;;
type farray2 = (float, float_elt, c_layout) Bigarray.Array2.t;;
type farray3 = (float, float_elt, c_layout) Bigarray.Array3.t;;

let float_ba = Bigarray.float64

let create_farray1 = Bigarray.Array1.create float_ba c_layout;;
let create_farray2 = Bigarray.Array2.create float_ba c_layout;;
let create_farray3 = Bigarray.Array3.create float_ba c_layout;;

(* Abbreviations for creating arrays of native integers *)
type int_elt = Bigarray.int_elt
type int_kind = (int, int_elt) Bigarray.kind

type iarray  = (int, int_elt, c_layout) Bigarray.Array1.t;;
type iarray2 = (int, int_elt, c_layout) Bigarray.Array2.t;;
type iarray3 = (int, int_elt, c_layout) Bigarray.Array3.t;;

let int_ba = Bigarray.int

let create_iarray1 = Bigarray.Array1.create int_ba c_layout;;
let create_iarray2 = Bigarray.Array2.create int_ba c_layout;;
let create_iarray3 = Bigarray.Array3.create int_ba c_layout;;

(* Abbreviations for creating arrays of short integers *)
type short_elt = Bigarray.int32_elt
type short_kind = (int32, short_elt) Bigarray.kind

type sarray  = (int32, short_elt, c_layout) Bigarray.Array1.t;;
type sarray2 = (int32, short_elt, c_layout) Bigarray.Array2.t;;
type sarray3 = (int32, short_elt, c_layout) Bigarray.Array3.t;;

let short_ba = Bigarray.int32

let create_sarray1 = Bigarray.Array1.create short_ba c_layout;;
let create_sarray2 = Bigarray.Array2.create short_ba c_layout;;
let create_sarray3 = Bigarray.Array3.create short_ba c_layout;;

(*
module I32 =
  struct
    type elt = Bigarray.int_elt
    type map = (int, elt) Bigarray.kind
    type t

  end

I32.array1
*)
