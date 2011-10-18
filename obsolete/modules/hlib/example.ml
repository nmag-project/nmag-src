(* 
   HLib bug: does not depend on liblapack!

   Start this way to tell linker to also include liblapack.
   Then, the mpi_petsc line below can be removed.

   LD_PRELOAD=/usr/lib/liblapack.so.3 ocaml
*)

#use "topfind";;

#load "bigarray.cma";;
#require "mpi_petsc";;			(* we ONLY do this to force inclusion of liblapack! Weird! *)
#require "hlib";;

(* let () = Hlib.hlib_init "/home/fangohr/build/HLib-1.3/Library/.libs/libhmatrix-1.3.so";;
*)

(*let () = Hlib.hlib_init "/home/tf/HLib-1.3/Library/.libs/libhmatrix-1.3.so";;*)

let () = Hlib.hlib_init "/home/aknittel/HLib-1.3/Library/.libs/libhmatrix-1.3.so";;

Gc.full_major();; (* check heap *)

let (vertices, triangles) = 
  ( [| [| 0.; 0.; 0. |];
       [| 1.; 0.; 0. |];
       [| 0.; 1.; 0. |];
       [| 0.; 0.; 1. |] |],
    [| [| 0; 2; 1 |];
       [| 0; 1; 3 |];
       [| 0; 3; 2 |];
       [| 1; 2; 3 |] |] 
  );;

let hmatrix = Hlib.make_hmatrix_from_oriented_triangles vertices triangles;;

let () = Hlib.write_hmatrix "/tmp/myhmatrix.bin" hmatrix;;

let hmatrix2 = Hlib.read_hmatrix "/tmp/myhmatrix.bin";;

let v_in  = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout 4;;
let v_out = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout 4;;

(*let a = ref 1 ;;
let () = !a :=2 ;;*)

let test_hmx v =
  begin
    for i=0 to Array.length v-1 do
      v_in.{i} = v.(i)
    done;
    Hlib.apply_hmatrix hmatrix v_out v_in;
    for i=0 to Array.length v-1 do
      v.(i) <- v_out.{i}
    done;
    v
1  end
;;

test_hmx [|1.;0.;0.;0.|];;

let () = begin
  v_in.{0} <- 1.;
  v_in.{1} <- 0.;
  v_in.{2} <- 0.;
  v_in.{3} <- 0.;
end;;

let () = Hlib.apply_hmatrix hmatrix v_out v_in;;

let () = Printf.printf "v_out = %f %f %f %f\n" v_out.{0} v_out.{1}  v_out.{2}  v_out.{3};;  

