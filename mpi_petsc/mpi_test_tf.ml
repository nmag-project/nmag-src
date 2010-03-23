(* (C) 2005 Dr. Thomas Fischbacher *)

(*
ocamlfind ocamlopt -linkpkg -package mpi_petsc -cc mpicc mpi_test_tf.ml


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
    let _ = Printf.printf "Node %d entering slave mode.\n" myrank in
    let rec process () =
      let (task,arg) = Mpi_petsc.receive 0 Mpi_petsc.any_tag Mpi_petsc.comm_world in
      let result = task arg in
      let sent = Mpi_petsc.send result 0 0 Mpi_petsc.comm_world in
	process ()
    in
      process ()
  else ()
;;

let _rpc_msg_count=ref 0;;

let eval_on node func x =
  let lz = lazy (func x) in
    (* Note: we only do this to enforce typechecking
       and ensure func can be applied to x!
    *)
  let sent = Mpi_petsc.send (func,x) node 0 Mpi_petsc.comm_world in
  let recv = Mpi_petsc.receive node Mpi_petsc.any_tag Mpi_petsc.comm_world in
  let result x =
    if x
    then recv
    else Lazy.force lz
  in result true
;;

Printf.printf "XYZ\n";;

let ma = eval_on 1 Array.make_matrix 5 in
let m = ma 6 0.0 in
  Printf.printf "ENTRY: %f\n%!" m.(2).(3)
;;
