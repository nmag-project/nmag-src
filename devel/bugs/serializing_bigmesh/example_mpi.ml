 
(* 
ocamlfind ocamlopt -I ../../../snippets -I ../../../mpi_petsc -I ../../../mesh -package snippets,mesh,unix,bigarray,mpi_petsc -o example_mpi example_mpi.ml -linkpkg
mpirun -np 2 ./example_mpi 
 *)

open Snippets;;
open Mesh;;

Printf.printf "DDD args: %s\n%!" (string_array_to_string Sys.argv);;

let _ = Mpi_petsc.mpi_init Sys.argv;;

let b = Mpi_petsc.petsc_init
  [|"foo";|]
  (Printf.sprintf "%s/.petscrc" (Unix.getenv "HOME"))
  "[help message]" true
in Printf.printf "Petsc Init %s\n%!" (if b then "succeeded" else "failed")
;;

let comm = Mpi_petsc.petsc_get_comm_world();;
let comm_size = Mpi_petsc.comm_size comm;;
let comm_rank = Mpi_petsc.comm_rank comm;;

let () = Printf.printf "Started MPI - comm=%d/%d\n%!" comm_size comm_rank;;

let () =
  if comm_rank == 0
  then	(* master *)
    let Some bigmesh = Mesh.read_mesh "/tmp/meshbigbar28000nodes.nmesh" in
    (* let Some bigmesh = Mesh.read_mesh "/tmp/meshbigbar10000nodes.nmesh" in *)
    let () = Mpi_petsc.send bigmesh 1 0 comm in
    let () = Printf.printf "MASTER sent mesh!\n%!" in
      ()
  else
    let () = Printf.printf "SLAVE waiting for mesh.\n%!" in
    let m = Mpi_petsc.receive 0 0 comm in
    let () = Printf.printf "SLAVE received mesh!\n%!" in
    let () = Printf.printf "Mesh nr-vertices = %d nr-simplices = %d\n%!" (Array.length m.mm_points) (Array.length m.mm_simplices) in
      ()
;;

