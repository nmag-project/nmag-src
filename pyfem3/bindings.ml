(* Nmag micromagnetic simulator
 * Copyright (C) 2011 University of Southampton
 * Hans Fangohr, Thomas Fischbacher, Matteo Franchin and others
 *
 * WEB:     http://nmag.soton.ac.uk
 * CONTACT: nmag@soton.ac.uk
 *
 * AUTHOR(S) OF THIS FILE: Jacek Generowicz and Matteo Franchin
 * LICENSE: GNU General Public License 2.0
 *          (see <http://www.gnu.org/licenses/>)
 *
 * This file provides facilities to generate wrappers between OCaml
 * and Python.
 *
 *)


open Pycaml

(**************** shared **********************************************)
module type CONVERTER = sig
  type caml_type
  val pycaml_pyobject_type : pyobject_type
  val from_py : pyobject -> caml_type
  val to_py   : caml_type -> pyobject
end

module CONV_U = struct
  type caml_type = unit
  let pycaml_pyobject_type = NoneType
  let from_py _ = ()
  let to_py = pynone
end

module CONV_B = struct
  type caml_type = bool
  let pycaml_pyobject_type = BoolType
  let from_py = pybool_asbool
  let to_py = pybool_frombool
end

module CONV_I = struct
  type caml_type = int
  let pycaml_pyobject_type = IntType
  let from_py = pyint_asint
  let to_py = pyint_fromint
end

module CONV_F = struct
  type caml_type = float
  let pycaml_pyobject_type = FloatType
  let from_py = pyfloat_asdouble
  let to_py = pyfloat_fromdouble
end

module CONV_S = struct
  type caml_type = string
  let pycaml_pyobject_type = StringType
  let from_py = pystring_asstring
  let to_py = pystring_fromstring
end

module CONV_L(Elt:CONVERTER) = struct
  type caml_type = Elt.caml_type array
  let pycaml_pyobject_type = ListType
  let from_py pylist = Array.map Elt.from_py (pylist_toarray pylist)
  let to_py camllist = pylist_fromarray (Array.map Elt.to_py camllist)
end

module CONV_T2 (ITEM1:CONVERTER) (ITEM2:CONVERTER) = struct
  type caml_type = ITEM1.caml_type * ITEM2.caml_type
  let pycaml_pyobject_type = TupleType
  let from_py pytuple =
    match (pytuple_toarray pytuple) with
      [|arg1; arg2|] -> (ITEM1.from_py arg1, ITEM2.from_py arg2)
    | arr ->
      let size = Array.length arr in
      let msg = Printf.sprintf "Expected 2-tuple, but tuple has size %d." size
      in failwith msg
  let to_py (arg1, arg2) = pytuple2 ((ITEM1.to_py arg1), (ITEM2.to_py arg2))
end

(* For now we map Python(NumPy)<-->OCaml(Bigarray) by making copies, and not
   passing the original data *)
module CONV_FA = struct
  type caml_type =
    (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Genarray.t
  let pycaml_pyobject_type = OtherType
  let from_py pyobj =
    let pyt = pytensor_of_pyobject double_items pyobj in
    let ba = pytensor_to_ba_unsafe pyt in
      Bigarray.Genarray.create
        Bigarray.float64 Bigarray.c_layout (Bigarray.Genarray.dims ba)
  let to_py ba =
    pytensor_to_pyobject
      (pytensor_init double_items (Bigarray.Genarray.dims ba)
         (fun indices -> Bigarray.Genarray.get ba indices))
end

module CONV_FUN1 (ARG1: CONVERTER) (RES: CONVERTER) = struct
  type caml_type = ARG1.caml_type -> RES.caml_type
  let pycaml_pyobject_type = CallableType
  let from_py pyfunc =
    (fun mlarg ->
       RES.from_py
         (pyeval_callobject(pyfunc, pytuple_fromarray [|ARG1.to_py mlarg|])))
  let to_py camlfun = 
    python_pre_interfaced_function [|ARG1.pycaml_pyobject_type|]
      (fun pyargs -> RES.to_py (camlfun (ARG1.from_py pyargs.(0))))
end

module CONV_FUN2
         (ARG1: CONVERTER) (ARG2: CONVERTER) (RES: CONVERTER) = struct
  type caml_type = ARG1.caml_type -> ARG2.caml_type -> RES.caml_type
  let pycaml_pyobject_type = CallableType
  let from_py pyfunc = failwith "Not implemented yet."
  let to_py camlfun =
    python_pre_interfaced_function
      [|ARG1.pycaml_pyobject_type; ARG2.pycaml_pyobject_type|]
      (fun pyargs -> RES.to_py (camlfun (ARG1.from_py pyargs.(0))
                                        (ARG2.from_py pyargs.(1))))
end

module CONV_FUN3
         (ARG1: CONVERTER) (ARG2: CONVERTER) (ARG3: CONVERTER)
         (RES: CONVERTER) = struct
  type caml_type =
    ARG1.caml_type -> ARG2.caml_type -> ARG3.caml_type -> RES.caml_type
  let pycaml_pyobject_type = CallableType
  let from_py pyfunc = failwith "Not implemented yet."
  let to_py camlfun =
    python_pre_interfaced_function
      [|ARG1.pycaml_pyobject_type; ARG2.pycaml_pyobject_type;
        ARG3.pycaml_pyobject_type|]
      (fun pyargs -> RES.to_py (camlfun (ARG1.from_py pyargs.(0))
                                        (ARG2.from_py pyargs.(1))
                                        (ARG3.from_py pyargs.(2))))
end

module CONV_FUN4
         (ARG1: CONVERTER) (ARG2: CONVERTER) (ARG3: CONVERTER)
         (ARG4: CONVERTER) (RES: CONVERTER) = struct
  type caml_type =
    ARG1.caml_type -> ARG2.caml_type -> ARG3.caml_type -> ARG4.caml_type
    -> RES.caml_type
  let pycaml_pyobject_type = CallableType
  let from_py pyfunc = failwith "Not implemented yet."
  let to_py camlfun =
    python_pre_interfaced_function
      [|ARG1.pycaml_pyobject_type; ARG2.pycaml_pyobject_type;
        ARG3.pycaml_pyobject_type; ARG4.pycaml_pyobject_type|]
      (fun pyargs ->
         RES.to_py (camlfun (ARG1.from_py pyargs.(0)) (ARG2.from_py pyargs.(1))
                            (ARG3.from_py pyargs.(2)) (ARG4.from_py pyargs.(3))))
end

(**********************************************************************)


(*
(* ********** Start of original pyfem3.ml ********** *)


(* open Pycaml;; *)

(* Specify a pill type: integer array *)

let array_pill_name = "ocaml array";;


let () = register_ocamlpill_types
    [| array_pill_name;
    |]
;;

let _sample_array_pill = [| 1 |];;


let (ocamlpill_from_array, array_from_ocamlpill) =
  make_ocamlpill_wrapper_unwrapper array_pill_name _sample_array_pill;;

(* End of specification of integer array pill type *)

(* Generic utilities for creating Python extension modules. Should be
   moved to Pycaml *)

let my_register_for_python mod_dict stuff =
  Array.iter
    (fun (python_name, value) ->
       ignore(pydict_setitemstring (mod_dict, python_name, value)))
    stuff;;

let my_register_pre_functions_for_python mod_dict stuff =
  Array.iter
    (fun (python_name, pre_fun) ->
       ignore(pydict_setitemstring (mod_dict, python_name, pre_fun python_name)))
    stuff;;

(* End of generic utilities *)

(* Wrappers for functionality provided in ML *)

(* This is being replaced by our more automatic wrapping utilities
let _py_square =
  python_pre_interfaced_function [|FloatType|]
    (fun py_args ->
       let ml_arg = pyfloat_asdouble py_args.(0) in
       let ml_result = Functionality.square ml_arg in
       let py_result = pyfloat_fromdouble ml_result
       in
         py_result)
;;
*)

let _py_make_consecutive_integers = 
  python_pre_interfaced_function [| IntType |]
    (fun py_args ->
       let ml_arg = pyint_asint py_args.(0) in
       let ml_result = Functionality.make_consecutive_integers ml_arg in
       let py_result = ocamlpill_from_array ml_result
       in
         py_result)
;;

let _py_add_up =
  python_pre_interfaced_function [| CamlpillType |]
    (fun py_args ->
      let ml_arg = array_from_ocamlpill py_args.(0) in
      let ml_result = Functionality.add_up ml_arg in
      let py_result = pyint_fromint ml_result in
      py_result)
;;

module FF = Convertable_fun1 (Converter_float) (Converter_float)
module IL = Converter_list(Converter_int)
module IL_IL = Convertable_fun1
  (IL)
  (Converter_list(Converter_int))

let create_nsimcore_module () = 
  (*
  let _ =
    register_for_python
      [|("mesher_defaults",ocamlpill_from_mesher_defaults_int !opt_mesher_defaults);
	("mesher_default_gendriver",ocamlpill_from_mg_gendriver default_gendriver);
	("empty_element",_py_empty_element);
      |]
in
*)
  let _py_mod_ocaml = ourpy_initemptymodule "functionality" in
  let _py_mod_ocaml_dict = pymodule_getdict _py_mod_ocaml in
    my_register_for_python _py_mod_ocaml_dict
      [|("square", FF.to_py Functionality.square);
	("reverse", IL_IL.to_py Functionality.reverse);
       (* ("make_consecutive_integers", _py_make_consecutive_integers);
        ("add_up", _py_add_up);*)|];;

let _ = Callback.register "create_nsimcore_module" create_nsimcore_module;;


*)
