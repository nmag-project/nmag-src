
(* Debugging petsc_matrix_call_on_rows... *)

#use "topfind";;
#require "unix";;
#require "mpi_petsc";;

let _ = 
  Mpi_petsc.petsc_init 
    [|"foo"|]
    (let home = Unix.getenv "HOME" in
     let petscrc_file = Printf.sprintf "%s/.petscrc" home in
       petscrc_file)
    "[help message]"
    true (* Enable petsc signal handler *)
;;

let mxsize=10;;

let mx = Mpi_petsc.matrix_create mxsize mxsize "Matrix_M";;


(* first test, write "laplace operator" like structure *) 
for i=0 to mxsize-1 do
  begin
    Mpi_petsc.matrix_set mx i i 2.0;
    if i>0 
    then
      begin
	Mpi_petsc.matrix_set mx (i-1) i (-1.0);
	Mpi_petsc.matrix_set mx i (i-1) (-1.0);
      end
    else ()
  end
done;;


(* second test: write column number in each entry of the matrix *)
if true then
  begin
    for i=0 to mxsize-1 do
      begin
	for j=0 to mxsize-1 do
	  Mpi_petsc.matrix_set mx i j (float_of_int j);
	done
      end;
    done
  end;;



Mpi_petsc.matrix_assemble mx true;;

Mpi_petsc.petsc_matrix_call_on_rows mx
  (fun nr_row ba_indices ba_vals ->
     begin
       Printf.printf "=== ROW %4d ===\n%!" nr_row;
       for i=0 to (Bigarray.Array1.dim ba_indices)-1 do
	 Printf.printf " %4d -> %16.10f\n%!" (Nativeint.to_int ba_indices.{i}) ba_vals.{i}
       done
     end)
;;

Gc.minor();;
Gc.full_major();;
