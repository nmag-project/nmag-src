#use "topfind";;
#require "snippets";;

open Snippets;;


let dim = 3 in 
  Printf.printf "Volume of %d dimensional sphere is %f\n" dim (volume_d_sphere dim);;

let dim = 3 in 
  Printf.printf "Packing fraction of sphere in  %d dimensions is %f\n" dim (sphere_packing_ratio_lattice_type_d dim);;

let dims = [1;2;3] in
  let f dim =
    Printf.printf "Volume of simplex in %d dimensions is %f\n" dim (volume_regular_simplex dim)
  in
  List.iter f dims;;

(* 1d: line from 0 to 1 -> 1
   2d: triangle with side length1 -> sqrt(3)/4
   3d: tetrahedron with side length 1 -> 1/12*sqrt(2)*1^3
   ...
*)
