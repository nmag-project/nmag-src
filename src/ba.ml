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
type f_ml_elt = float

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

external i32_get1: i32array1 -> int -> i32_ml_elt
  = "caml_ba_s_int32_get1"
external i32_get2: i32array2 -> int -> int -> i32_ml_elt
  = "caml_ba_s_int32_get2"
external i32_get3: i32array3 -> int -> int -> int -> i32_ml_elt
  = "caml_ba_s_int32_get3"
external i32_set1: i32array1 -> int -> i32_ml_elt -> unit
  = "caml_ba_s_int32_set1"
external i32_set2: i32array2 -> int -> int -> i32_ml_elt -> unit
  = "caml_ba_s_int32_set2"
external i32_set3: i32array3 -> int -> int -> int -> i32_ml_elt -> unit
  = "caml_ba_s_int32_set3"

(* Optimised C procedures to access float arrays *)
external f_unsafe_get1: farray1 -> int -> f_ml_elt
  = "caml_ba_double_get1"
external f_unsafe_get2: farray2 -> int -> int -> f_ml_elt
  = "caml_ba_double_get2"
external f_unsafe_get3: farray3 -> int -> int -> int -> f_ml_elt
  = "caml_ba_double_get3"
external f_unsafe_set1: farray1 -> int -> f_ml_elt -> unit
  = "caml_ba_double_set1"
external f_unsafe_set2: farray2 -> int -> int -> f_ml_elt -> unit
  = "caml_ba_double_set2"
external f_unsafe_set3: farray3 -> int -> int -> int -> f_ml_elt -> unit
  = "caml_ba_double_set3"

external f_get1: farray1 -> int -> f_ml_elt
  = "caml_ba_s_double_get1"
external f_get2: farray2 -> int -> int -> f_ml_elt
  = "caml_ba_s_double_get2"
external f_get3: farray3 -> int -> int -> int -> f_ml_elt
  = "caml_ba_s_double_get3"
external f_set1: farray1 -> int -> f_ml_elt -> unit
  = "caml_ba_s_double_set1"
external f_set2: farray2 -> int -> int -> f_ml_elt -> unit
  = "caml_ba_s_double_set2"
external f_set3: farray3 -> int -> int -> int -> f_ml_elt -> unit
  = "caml_ba_s_double_set3"

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
    val get1: array1 -> int -> ml_elt
    val get2: array2 -> int -> int -> ml_elt
    val get3: array3 -> int -> int -> int -> ml_elt
    val set1: array1 -> int -> ml_elt -> unit
    val set2: array2 -> int -> int -> ml_elt -> unit
    val set3: array3 -> int -> int -> int -> ml_elt -> unit
    val slice2: array2 -> int -> array1
    val slice31: array3 -> int -> int -> array1
    val slice32: array3 -> int -> array2
  end

module BaFunctor =
  functor (Acc: ACCESSOR) ->
    struct
      include Acc

      let set_all1 ba fn =
        let nm1 = (dim1 ba) - 1 in
          for i = 0 to nm1 do
            unsafe_set1 ba i (fn i)
          done

      let set_all2 ba fn =
        let (n1, n2) = dim2 ba in
          for i1 = 0 to n1 - 1 do
            for i2 = 0 to n2 - 1 do
              unsafe_set2 ba i1 i2 (fn i1 i2)
            done
          done

      let set_all3 ba fn =
        let (n1, n2, n3) = dim3 ba in
          for i1 = 0 to n1 - 1 do
            for i2 = 0 to n2 - 1 do
              for i3 = 0 to n3 - 1 do
                unsafe_set3 ba i1 i2 i3 (fn i1 i2 i3)
              done
            done
          done

      let iter1 ba fn =
        let nm1 = (dim1 ba) - 1 in
          for i = 0 to nm1 do
            let () = fn i (unsafe_get1 ba i) in ()
          done

      let iter2 ba fn =
        let (n1, n2) = dim2 ba in
          for i1 = 0 to n1 - 1 do
            for i2 = 0 to n2 - 1 do
              let () = fn i1 i2 (unsafe_get2 ba i1 i2) in ()
            done
          done

      let iter3 ba fn =
        let (n1, n2, n3) = dim3 ba in
          for i1 = 0 to n1 - 1 do
            for i2 = 0 to n2 - 1 do
              for i3 = 0 to n3 - 1 do
                let () = fn i1 i2 i3 (unsafe_get3 ba i1 i2 i3) in ()
              done
            done
          done

      let iter21 ba fn =
        let n1, _ = dim2 ba in
          for i1 = 0 to n1 - 1 do
            let slice = slice2 ba i1 in
            let () = fn i1 slice in ()
          done

      let iter32 ba fn =
        let n1, _, _ = dim3 ba in
          for i1 = 0 to n1 - 1 do
            let slice = slice32 ba i1 in
            let () = fn i1 slice in ()
          done

      let iter31 ba fn =
        let n1, n2, _ = dim3 ba in
          for i1 = 0 to n1 - 1 do
            for i2 = 0 to n2 - 1 do
              let slice = slice31 ba i1 i2 in
              let () = fn i1 i2 slice in ()
            done
          done

      let init1 n fn =
        let ba = create1 n in
        let () = set_all1 ba fn
        in ba

      let init2 n1 n2 fn =
        let ba = create2 n1 n2 in
        let () = set_all2 ba fn
        in ba

      let init3 n1 n2 n3 fn =
        let ba = create3 n1 n2 n3 in
        let () = set_all3 ba fn
        in ba

      let to_ml1 a1 =
        Array.init (dim1 a1) (fun i1 -> unsafe_get1 a1 i1)

      let from_ml2 a2 =
        let n1 = Array.length a2 in
        let n2 = if n1 >= 1 then Array.length a2.(0) else 0 in
          init2 n1 n2 (fun i1 i2 -> a2.(i1).(i2))

      let to_ml2 a2 =
        let n1, n2 = dim2 a2
        in Array.init n1
             (fun i1 -> Array.init n1 (fun i2 -> unsafe_get2 a2 i1 i2))
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
    let get1 = i32_get1
    let get2 = i32_get2
    let get3 = i32_get3
    let set1 = i32_set1
    let set2 = i32_set2
    let set3 = i32_set3
    let slice2 = Bigarray.Array2.slice_left
    let slice31 = Bigarray.Array3.slice_left_1
    let slice32 = Bigarray.Array3.slice_left_2
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
    let unsafe_get1 = f_unsafe_get1
    let unsafe_get2 = f_unsafe_get2
    let unsafe_get3 = f_unsafe_get3
    let unsafe_set1 = f_unsafe_set1
    let unsafe_set2 = f_unsafe_set2
    let unsafe_set3 = f_unsafe_set3
    let get1 = f_get1
    let get2 = f_get2
    let get3 = f_get3
    let set1 = f_set1
    let set2 = f_set2
    let set3 = f_set3
    let slice2 = Bigarray.Array2.slice_left
    let slice31 = Bigarray.Array3.slice_left_1
    let slice32 = Bigarray.Array3.slice_left_2
  end

module F = BaFunctor(FAccessor)

type matrix = farray2

(* At the moment we multiply a bigarray matrix by an Array.t vector and return
   an Array.t vector. *)
let matrix_x_vec ?store_result mx v =
  let dim_le, dim_ri = F.dim2 mx in
  let () = (if dim_ri <> Array.length v
            then failwith "matrix/vector size mismatch!"
            else ())
  in
  let result =
    match store_result with
    | None -> Array.make dim_le 0.0
    | Some x -> x
  in
  let rec sprod_row row pos so_far =
    if pos = dim_ri
    then so_far
    else sprod_row row (1+pos) (so_far +. v.(pos)*.(F.unsafe_get1 row pos))
  in
  begin
    for i=0 to dim_le-1 do
      result.(i) <- sprod_row (F.slice2 mx i) 0 0.0;
    done;
    result;
  end
