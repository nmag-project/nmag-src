(* Nmag micromagnetic simulator
   Copyright (C) 2010 University of Southampton
   Hans Fangohr, Thomas Fischbacher, Matteo Franchin and others

   WEB:     http://nmag.soton.ac.uk
   CONTACT: nmag@soton.ac.uk

   AUTHOR(S) OF THIS FILE: Matteo Franchin
   LICENSE: GNU General Public License 2.0
            (see <http://www.gnu.org/licenses/>)

  Here we provide access to 'float' and 'int32' 'bigarray's similar to the one
  for the builtin 'array's. We also provide some new C functions in order to
  get as much speed as possible (reading/writing int32 arrays would be
  otherwise too slow). As a result, if an array of int is created usually with
  Array.init n (fun i -> ...) a Bigarray of int32 can be created with

    I32.init1 n (fun i -> ...)

  The resulting array will occupy half the memory on 64 bit machines and will
  be faster to read and write.
 *)

type c_layout = Bigarray.c_layout
val c_layout: c_layout Bigarray.layout

type i32_ml_elt = int
type f64_ml_elt = float

type i32array1 = (int32, Bigarray.int32_elt, c_layout) Bigarray.Array1.t
type i32array2 = (int32, Bigarray.int32_elt, c_layout) Bigarray.Array2.t
type i32array3 = (int32, Bigarray.int32_elt, c_layout) Bigarray.Array3.t
type farray1 = (float, Bigarray.float64_elt, c_layout) Bigarray.Array1.t
type farray2 = (float, Bigarray.float64_elt, c_layout) Bigarray.Array2.t
type farray3 = (float, Bigarray.float64_elt, c_layout) Bigarray.Array3.t

module I32:
  sig
    type ml_elt = i32_ml_elt
    type array1 = i32array1
    type array2 = i32array2
    type array3 = i32array3
    val create1: int -> array1
    val create2: int -> int -> array2
    val create3: int -> int -> int -> array3
    val dim1: array1 -> int
    val dim2: array2 -> int*int
    val dim3: array3 -> int*int*int
    val unsafe_get1: array1 -> int -> ml_elt
    val unsafe_get2: array2 -> int -> int -> ml_elt
    val unsafe_get3: array3 -> int -> int -> int -> ml_elt
    val unsafe_set1: array1 -> int -> ml_elt -> unit
    val unsafe_set2: array2 -> int -> int -> ml_elt -> unit
    val unsafe_set3: array3 -> int -> int -> int -> ml_elt -> unit
    val set1: array1 -> int -> ml_elt -> unit
    val get1: array1 -> int -> ml_elt
    val set_all1: array1 -> (int -> ml_elt) -> unit
    val set_all2: array2 -> (int -> int -> ml_elt) -> unit
    val set_all3: array3 -> (int -> int -> int -> ml_elt) -> unit
    val iter1: array1 -> (int -> ml_elt -> unit) -> unit
    val iter2: array2 -> (int -> int -> ml_elt -> unit) -> unit
    val iter3: array3 -> (int -> int -> int -> ml_elt -> unit) -> unit
    val init1: int -> (int -> ml_elt) -> array1
    val init2: int -> int -> (int -> int -> ml_elt) -> array2
    val init3: int -> int -> int -> (int -> int -> int -> ml_elt) -> array3
  end

module F:
  sig
    type ml_elt = f64_ml_elt
    type array1 = farray1
    type array2 = farray2
    type array3 = farray3
    val create1: int -> array1
    val create2: int -> int -> array2
    val create3: int -> int -> int -> array3
    val dim1: array1 -> int
    val dim2: array2 -> int*int
    val dim3: array3 -> int*int*int
    val unsafe_get1: array1 -> int -> ml_elt
    val unsafe_get2: array2 -> int -> int -> ml_elt
    val unsafe_get3: array3 -> int -> int -> int -> ml_elt
    val unsafe_set1: array1 -> int -> ml_elt -> unit
    val unsafe_set2: array2 -> int -> int -> ml_elt -> unit
    val unsafe_set3: array3 -> int -> int -> int -> ml_elt -> unit
    val set1: array1 -> int -> ml_elt -> unit
    val get1: array1 -> int -> ml_elt
    val set_all1: array1 -> (int -> ml_elt) -> unit
    val set_all2: array2 -> (int -> int -> ml_elt) -> unit
    val set_all3: array3 -> (int -> int -> int -> ml_elt) -> unit
    val iter1: array1 -> (int -> ml_elt -> unit) -> unit
    val iter2: array2 -> (int -> int -> ml_elt -> unit) -> unit
    val iter3: array3 -> (int -> int -> int -> ml_elt -> unit) -> unit
    val init1: int -> (int -> ml_elt) -> array1
    val init2: int -> int -> (int -> int -> ml_elt) -> array2
    val init3: int -> int -> int -> (int -> int -> int -> ml_elt) -> array3
  end
