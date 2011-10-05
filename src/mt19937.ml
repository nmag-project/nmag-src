(* 
   (C) 2005 Dr. Thomas Fischbacher
*)

type rng

external make: int -> rng = "caml_mt19937_new_rng"

external init: rng -> int -> unit = "caml_mt19937_init"

external float: rng -> float -> float = "caml_mt19937_genrand_double"

external float_fillarray: rng -> float -> float array -> unit = "caml_mt19937_genrand_double_fillarray"

external int: rng -> int -> int = "caml_mt19937_genrand_int"

let version () = "$Id$";;
