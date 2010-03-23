(* (C) 2005 Dr. Thomas Fischbacher *)

(*
ocamlfind ocamlopt -linkpkg -package mpi_petsc -cc mpicc mpi_test_tf_basic.ml


#use "topfind";;
#require "mpi_petsc";;
*)

let _ = Mpi_petsc.mpi_init Sys.argv

let size = Mpi_petsc.comm_size Mpi_petsc.comm_world;;
let myrank = Mpi_petsc.comm_rank Mpi_petsc.comm_world;;

let _ = Printf.printf "*** Node %d: comm_size = %d\n%!" myrank size;;

let _ =
  if myrank <> 0 
  then
    let _ = Printf.printf "Node %d entering slave mode.\n%!" myrank in
    let n = Mpi_petsc.receive_float 0 Mpi_petsc.any_tag Mpi_petsc.comm_world in
    let result = n*.n in
    let sent = Mpi_petsc.send_float result 0 0 Mpi_petsc.comm_world in
      ()
  else
    for i=1 to size-1 do
      Mpi_petsc.send_float 10.0 i 0 Mpi_petsc.comm_world;
      let r = Mpi_petsc.receive_float i Mpi_petsc.any_tag Mpi_petsc.comm_world in
	Printf.printf "Master: %f -> %f\n%!" 10.0 r
    done
;;

