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

(* ML types corresponding to the Bigarray types *)
type i32_ml_elt = int
type f64_ml_elt = float

(* Handled types of bigarrays *)
type i32array1 = (int32, Bigarray.int32_elt, c_layout) Bigarray.Array1.t;;
type i32array2 = (int32, Bigarray.int32_elt, c_layout) Bigarray.Array2.t;;
type i32array3 = (int32, Bigarray.int32_elt, c_layout) Bigarray.Array3.t;;
type farray1 = (float, Bigarray.float64_elt, c_layout) Bigarray.Array1.t;;
type farray2 = (float, Bigarray.float64_elt, c_layout) Bigarray.Array2.t;;
type farray3 = (float, Bigarray.float64_elt, c_layout) Bigarray.Array3.t;;

(* Optimised C procedures to access i32 arrays *)
external i32_unsafe_get1: i32array1 -> int -> i32_ml_elt
  = "caml_ba_int32_get1"
external i32_unsafe_get2: i32array2 -> int -> int -> i32_ml_elt
  = "caml_ba_int32_get2"
external i32_unsafe_get3: i32array3 -> int -> int -> int -> i32_ml_elt
  = "caml_ba_int32_get3"
external i32_unsafe_set1: i32array1 -> int -> i32_ml_elt -> unit
  = "caml_ba_int32_set1"
external i32_unsafe_set2: i32array2 -> int -> int -> i32_ml_elt -> unit
  = "caml_ba_int32_set2"
external i32_unsafe_set3: i32array3 -> int -> int -> int -> i32_ml_elt -> unit
  = "caml_ba_int32_set3"

module type ACCESSOR =
  sig
    type ml_elt
    type array1
    type array2
    type array3

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
  end

module BaFunctor =
  functor (Acc: ACCESSOR) ->
    struct
      include Acc

      let set1 ba i v =
        if i >= 0 && i < (Acc.dim1 ba)
        then Acc.unsafe_set1 ba i v
        else raise (Invalid_argument "Index out of bounds.")

      let get1 ba i =
        if i >= 0 && i < (Acc.dim1 ba)
        then Acc.unsafe_get1 ba i
        else raise (Invalid_argument "Index out of bounds.")

      let set_all1 ba fn =
        let nm1 = (Acc.dim1 ba) - 1 in
          for i = 0 to nm1; do
            Acc.unsafe_set1 ba i (fn i)
          done

      let set_all2 ba fn =
        let (n1, n2) = Acc.dim2 ba in
          for i1 = 0 to n1 - 1; do
            for i2 = 0 to n2 - 1; do
              Acc.unsafe_set2 ba i1 i2 (fn i1 i2)
            done
          done

      let set_all3 ba fn =
        let (n1, n2, n3) = Acc.dim3 ba in
          for i1 = 0 to n1 - 1; do
            for i2 = 0 to n2 - 1; do
              for i3 = 0 to n3 - 1; do
                Acc.unsafe_set3 ba i1 i2 i3 (fn i1 i2 i3)
              done
            done
          done

      let iter1 ba fn =
        let nm1 = (Acc.dim1 ba) - 1 in
          for i = 0 to nm1; do
            let () = fn i (Acc.unsafe_get1 ba i) in ()
          done

      let iter2 ba fn =
        let (n1, n2) = Acc.dim2 ba in
          for i1 = 0 to n1 - 1; do
            for i2 = 0 to n2 - 1; do
              let () = fn i1 i2 (Acc.unsafe_get2 ba i1 i2) in ()
            done
          done

      let iter3 ba fn =
        let (n1, n2, n3) = Acc.dim3 ba in
          for i1 = 0 to n1 - 1; do
            for i2 = 0 to n2 - 1; do
              for i3 = 0 to n3 - 1; do
                let () = fn i1 i2 i3 (Acc.unsafe_get3 ba i1 i2 i3) in ()
              done
            done
          done
(*
      let iter21 ba fn =
        let n1, _ = dim2 ba in
          for i1 = 0 to n1 - 1; do
            let slice = Bigarray.Array2.slice_left ba i1 in
            let () = fn i1 slice in ()
          done

      let iter31 ba fn =
        let n1, _, _ = dim3 ba in
          for i1 = 0 to n1 - 1; do
            let slice = Bigarray.Array3.slice_left_2 ba i1 in
            let () = fn i1 slice in ()
          done

      let iter32 ba fn =
        let n1, n2, _ = dim3 ba in
          for i1 = 0 to n1 - 1; do
            for i2 = 0 to n2 - 1; do
              let slice = Bigarray.Array3.slice_left_1 ba i1 i2 in
              let () = fn i1 i2 slice in ()
            done
          done
*)
      let init1 n fn =
        let ba = Acc.create1 n in
        let () = set_all1 ba fn
        in ba

      let init2 n1 n2 fn =
        let ba = Acc.create2 n1 n2 in
        let () = set_all2 ba fn
        in ba

      let init3 n1 n2 n3 fn =
        let ba = Acc.create3 n1 n2 n3 in
        let () = set_all3 ba fn
        in ba

    end

module I32Accessor =
  struct
    type ml_elt = int
    type array1 = i32array1
    type array2 = i32array2
    type array3 = i32array3

    let create1 = Bigarray.Array1.create Bigarray.int32 c_layout
    let create2 = Bigarray.Array2.create Bigarray.int32 c_layout
    let create3 = Bigarray.Array3.create Bigarray.int32 c_layout
    let dim1 = Bigarray.Array1.dim
    let dim2 ba = (Bigarray.Array2.dim1 ba, Bigarray.Array2.dim2 ba)
    let dim3 ba = (Bigarray.Array3.dim1 ba, Bigarray.Array3.dim2 ba,
                   Bigarray.Array3.dim3 ba)
    let unsafe_get1 = i32_unsafe_get1
    let unsafe_get2 = i32_unsafe_get2
    let unsafe_get3 = i32_unsafe_get3
    let unsafe_set1 = i32_unsafe_set1
    let unsafe_set2 = i32_unsafe_set2
    let unsafe_set3 = i32_unsafe_set3
  end

module I32 = BaFunctor(I32Accessor)

module FAccessor =
  struct
    type ml_elt = float
    type array1 = farray1
    type array2 = farray2
    type array3 = farray3

    let create1 = Bigarray.Array1.create Bigarray.float64 c_layout
    let create2 = Bigarray.Array2.create Bigarray.float64 c_layout
    let create3 = Bigarray.Array3.create Bigarray.float64 c_layout
    let dim1 = Bigarray.Array1.dim
    let dim2 ba = (Bigarray.Array2.dim1 ba, Bigarray.Array2.dim2 ba)
    let dim3 ba = (Bigarray.Array3.dim1 ba, Bigarray.Array3.dim2 ba,
                   Bigarray.Array3.dim3 ba)
    let unsafe_get1 = Bigarray.Array1.unsafe_get
    let unsafe_get2 = Bigarray.Array2.unsafe_get
    let unsafe_get3 = Bigarray.Array3.unsafe_get
    let unsafe_set1 = Bigarray.Array1.unsafe_set
    let unsafe_set2 = Bigarray.Array2.unsafe_set
    let unsafe_set3 = Bigarray.Array3.unsafe_set
  end

module F = BaFunctor(FAccessor)
