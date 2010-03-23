#use "topfind";;
#require "mpi_petsc";;

open Mpi_petsc;;

petsc_init [|"foo"|] "foo" "foo";;

let v0 = vector_pack (Array.init 10 (fun n -> float_of_int (n*n)));;

let m0 = 
  let m = matrix_create 10 10 "my matrix" in
  begin
    for i=0 to 10-1 do
      for j=0 to 10-1 do
	matrix_set m i j (float_of_int (i+j))
      done;
    done;
    matrix_assemble m;
    m
  end
;;

let ml_m0 = matrix_extract m0;;


let v1 = 
  let result = vector_pack (Array.make 10 0.0) in
  begin
    matrix_times_vector m0 v0 result;
    result
  end
;;

let ml_v1 = vector_extract v1;;

(* petsc_finalize();; *)
