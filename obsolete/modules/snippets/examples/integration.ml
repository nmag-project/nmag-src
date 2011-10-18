#use "topfind";;
#require "snippets";;

open Snippets;;

(* integrate \int x^2 dx from a to b *)

(* function to integrate *)
let f xarray =
  let x = xarray.(0) in x*.x;;

let a = Array.create 1 0.0  (* lower bound *)
and b = Array.create 1 1.0 in (* upper bound *)
numerically_integrate_float_over_box (a,b) f;;

(* Note: this example looks a bit tedious because it is one dimensional, and
it therefore seems unnecessary to convert from arrays to floats. The
array-framework is necessary for higher dimensions. *)
