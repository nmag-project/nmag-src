#use "topfind";;
#require "pycaml";;

open Pycaml;;

(*
let () = Gc.set
  {Gc.minor_heap_size = 32768; Gc.major_heap_increment = 61440;
   Gc.space_overhead = 80; Gc.verbose = 0x3ff; Gc.max_overhead = 500;
   Gc.stack_limit = 262144}
;;
*)

let _py_intfloattuple_array =
  python_pre_interfaced_function
    ~doc:"Return an array with n tuples with ints and floats from 0 n-1 (int,float)"
    [|IntType|]
    (fun py_args ->
       let n = pyint_asint py_args.(0)
       in let a = Array.init n (fun x -> pytuple2 (pyint_fromint n,pyfloat_fromdouble (float_of_int x)))
       in pylist_fromarray a)
in
  register_pre_functions_for_python
    [|("intfloattuple_array", _py_intfloattuple_array) |]
;;

let _py_float_array =
  python_pre_interfaced_function
    ~doc:"Return an array with n floats from 0. to float_of_int(n)-.1."
    [|IntType|]
    (fun py_args ->
       let n = pyint_asint py_args.(0)
       in let a = Array.init n (fun x -> pyfloat_fromdouble (float_of_int x))
       in pylist_fromarray a)
in
  register_pre_functions_for_python
    [|("float_array", _py_float_array) |]
;;


Printf.printf "within python, try for example 'ocaml.float_array(10)'\n%!";;

python_eval "execfile('sizetest.py')";;




