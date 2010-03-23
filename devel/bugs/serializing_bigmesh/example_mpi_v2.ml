 
(* 
ocamlfind ocamlopt -I ../../../snippets -I ../../../mpi_petsc -I ../../../mesh -package snippets,mesh,unix,bigarray,mpi_petsc -o example_mpi_v2 example_mpi_v2.ml -linkpkg
mpirun -np 2 ./example_mpi_v2 

Output (debug_example=false):

DDD master serialized bigmesh size=70093269 sum=830659452
MASTER sent mesh!
DDD BATF ba={84 95 a6 be ... 80 41 41 40}
DDD BATF ba={84 95 a6 be ... 80 41 41 40}
MASTER demarshalled own mesh! (nr vertices=28561)
tf@eta:~/ocaml/devel/bugs/serializing_bigmesh$ SLAVE received mesh (bigarray size=70093269 sum=830659452)!
DDD BEGIN check-heap
DDD END check-heap
SLAVE post check_heap!
p1_17051:  p4_error: interrupt SIGSEGV: 11

Output (debug_example=true):

$ mpirun -np 2 `pwd`/example_mpi_v2 
Started MPI - comm=0/2
Started MPI - comm=1/2
SLAVE waiting for mesh.
DDD BAFF ba={84 95 a6 be ... 80 41 41 40}
DDD master serialized bigmesh size=70093269 sum=830659452
MASTER sent mesh!
Fatal error: exception Failure("input_value: code mismatch")
tf@eta:~/ocaml/devel/bugs/serializing_bigmesh$ DDD BATF ba={84 95 a6 be ... 80 41 41 40}
SLAVE received mesh (bigarray size=70093269 sum=830659452)!
DDD BEGIN check-heap
DDD END check-heap
SLAVE post check_heap!
Fatal error: exception Failure("input_value: code mismatch")

*)

open Snippets;;
open Mesh;;

let debug_example = true;;

let ba_checksum ba =
  let len = Bigarray.Array1.dim ba in
  let rec walk pos sum =
    if pos = len then sum
    else walk (pos+1) (sum + (Char.code ba.{pos}) lxor pos)
  in walk 0 0
;;

let ba_to_file ba filename =
  let fh = open_out filename in
  let buf = String.make 4096 '\000' in
  let len = Bigarray.Array1.dim ba in
  let ddd = Printf.printf "DDD BATF ba={%02x %02x %02x %02x ... %02x %02x %02x %02x}\n%!"
    (Char.code ba.{0})  (Char.code ba.{1})  (Char.code ba.{2})  (Char.code ba.{3})
    (Char.code ba.{len-4})  (Char.code ba.{len-3})  (Char.code ba.{len-2})  (Char.code ba.{len-1})
  in
  let rec transfer_and_write offset =
    if offset = len then ()
    else
      let target = min (offset+4096) len in
      let nr_todo = target-offset in
      let () =
	for i=0 to nr_todo-1 do
	  buf.[i] <- ba.{offset+i}
	done
      in
      let () = output_string fh (if nr_todo = 4096 then buf else String.sub buf 0 nr_todo) in
	transfer_and_write target
  in
  let () = transfer_and_write 0 in
  let () = close_out fh in
    ()
;;

let ba_from_file filename =
  let len = (Unix.stat filename).Unix.st_size in
  let ba = 
    Bigarray.Array1.create Bigarray.char Bigarray.c_layout len
  in
  let fh = open_in filename in
  let () =
    for i=0 to len-1 do
      ba.{i} <- input_char fh
    done
  in
  let () = close_in fh in
  let ddd = Printf.printf "DDD BAFF ba={%02x %02x %02x %02x ... %02x %02x %02x %02x}\n%!"
    (Char.code ba.{0})  (Char.code ba.{1})  (Char.code ba.{2})  (Char.code ba.{3})
    (Char.code ba.{len-4})  (Char.code ba.{len-3})  (Char.code ba.{len-2})  (Char.code ba.{len-1})
  in
    ba
;;

let _ = Mpi_petsc.mpi_init Sys.argv;;

let comm = Mpi_petsc.get_comm_world();;
let comm_size = Mpi_petsc.comm_size comm;;
let comm_rank = Mpi_petsc.comm_rank comm;;

let () = Printf.printf "Started MPI - comm=%d/%d\n%!" comm_rank comm_size;;

let check_heap () =
  begin
    Printf.printf "DDD BEGIN check-heap\n%!";
    Gc.minor();
    Gc.major();
    Gc.full_major();
    Printf.printf "DDD END check-heap\n%!";
  end
;;

(* This seems to work: serializing intermediately to a bigarray! *)

let () =
  if comm_rank == 0
  then	(* master *)
    let s_bigmesh =
      if debug_example
      then
	(* debug: load serialized bigarray from file *)
	ba_from_file "/tmp/ba.master"
      else
	(* nsim: load mesh from ascii file *)
	let Some bigmesh = Mesh.read_mesh "/tmp/meshbigbar28000nodes.nmesh" in
	  marshal_to_bigarray bigmesh [Marshal.Closures]
    in
    (* let Some bigmesh = Mesh.read_mesh "/tmp/meshbigbar10000nodes.nmesh" in *)
    let () = Printf.printf "DDD master serialized bigmesh size=%d sum=%d\n"
      (Bigarray.Array1.dim s_bigmesh) (ba_checksum s_bigmesh)
      (* Should produce the line:
	 DDD master serialized bigmesh size=70093269 sum=830661008
      *)
    in
    let () = Mpi_petsc.send s_bigmesh 1 0 comm in
    let () = Printf.printf "MASTER sent mesh!\n%!" in
    let () = (if debug_example then () else ba_to_file s_bigmesh "/tmp/ba.master") in
    let bigmesh2 = demarshal_from_bigarray s_bigmesh in
    let () = Printf.printf "MASTER demarshalled own mesh! (nr vertices=%d)\n%!" (Array.length bigmesh2.mm_points) in
      ()
  else
    let () = Printf.printf "SLAVE waiting for mesh.\n%!" in
    let s_m = Mpi_petsc.receive 0 0 comm in
    let len = Bigarray.Array1.dim s_m in
    let () = ba_to_file s_m "/tmp/ba.slave" in
    let () = Printf.printf "SLAVE received mesh (bigarray size=%d sum=%d)!\n%!" len (ba_checksum s_m) in
    let () = check_heap () in
    let () = Printf.printf "SLAVE post check_heap!\n%!" in
    let m = demarshal_from_bigarray s_m in
      (* As it seems, the problematic step in both cases is demarshalling the mesh! *)
    let () = Printf.printf "SLAVE demarshalled mesh!\n%!" in
    let () = Printf.printf "Mesh nr-vertices = %d nr-simplices = %d\n%!"
      (Array.length m.mm_points) (Array.length m.mm_simplices)
    in
      ()
;;

