(* Nmag micromagnetic simulator
 * Copyright (C) 2011 University of Southampton
 * Hans Fangohr, Thomas Fischbacher, Matteo Franchin and others
 *
 * WEB:     http://nmag.soton.ac.uk 
 * CONTACT: nmag@soton.ac.uk
 *
 * AUTHOR(S) OF THIS FILE: Matteo Franchin
 * LICENSE: GNU General Public License 2.0
 *          (see <http://www.gnu.org/licenses/>)
 *
 * Exporting PETSc functionality to Python.
 *
 *)

open Pycaml;;
open Bindings;;

(* Create wrapper/unwrapper for PETSc vector *)
let (pyobj_from_petsc_vec, pyobj_to_petsc_vec) =
       register_pyobj "PETSc vector" (Mpi_petsc.vector_dummy ());;

(* Create wrapper/unwrapper for PETSc matrix *)
let (pyobj_from_petsc_mat, pyobj_to_petsc_mat) =
       register_pyobj "PETSc matrix" (Mpi_petsc.matrix_dummy ());;

(* Converters for auto-bindings *)
module CONV_V = struct
  type caml_type = Mpi_petsc.vector
  let pycaml_pyobject_type = CamlpillType
  let from_py = pyobj_to_petsc_vec
  let to_py = pyobj_from_petsc_vec
end

module CONV_M = struct
  type caml_type = Mpi_petsc.matrix
  let pycaml_pyobject_type = CamlpillType
  let from_py = pyobj_to_petsc_mat
  let to_py = pyobj_from_petsc_mat
end

module CONV_V_U = CONV_FUN1 (CONV_V) (CONV_U)
module CONV_V_I_F_U = CONV_FUN3 (CONV_V) (CONV_I) (CONV_F) (CONV_U)
module CONV_V_F_U = CONV_FUN2 (CONV_V) (CONV_F) (CONV_U)
module CONV_I_S_V = CONV_FUN2 (CONV_I) (CONV_S) (CONV_V)
module CONV_V_V = CONV_FUN1 (CONV_V) (CONV_V)
module CONV_V_V_U = CONV_FUN2 (CONV_V) (CONV_V) (CONV_U)
module CONV_V_V_V_U = CONV_FUN3 (CONV_V) (CONV_V) (CONV_V) (CONV_U)
module CONV_F_F_V_V_U = CONV_FUN4 (CONV_F) (CONV_F) (CONV_V) (CONV_V) (CONV_U)
module CONV_M_U = CONV_FUN1 (CONV_M) (CONV_U)
module CONV_B_M_M = CONV_FUN2 (CONV_B) (CONV_M) (CONV_M)
module CONV_B_M_M_U = CONV_FUN3 (CONV_B) (CONV_M) (CONV_M) (CONV_U)
module CONV_M_B_U = CONV_FUN2 (CONV_M) (CONV_B) (CONV_U)
module CONV_M_I_I_U = CONV_FUN3 (CONV_M) (CONV_I) (CONV_I) (CONV_U)
module CONV_M_V_V_U = CONV_FUN3 (CONV_M) (CONV_V) (CONV_V) (CONV_U)
module CONV_M_V_V_V_U = CONV_FUN4 (CONV_M) (CONV_V) (CONV_V) (CONV_V) (CONV_U)
module CONV_M_F_U = CONV_FUN2 (CONV_M) (CONV_F) (CONV_U)
module CONV_M_S_U = CONV_FUN2 (CONV_M) (CONV_S) (CONV_U)
module CONV_S_M = CONV_FUN1 (CONV_S) (CONV_M)
module CONV_M_I_I_F_U = CONV_FUN4 (CONV_M) (CONV_I) (CONV_I) (CONV_F) (CONV_U)
module CONV_S_S_I_I_M = CONV_FUN4 (CONV_S) (CONV_S) (CONV_I) (CONV_I) (CONV_M)

let my_matrix_create mname mtype rows cols =
  let comm = Mpi_petsc.petsc_get_comm_self () in
    Mpi_petsc.matrix_create_raw comm mtype mname (rows, cols, -1, -1)
;;

let my_vector_as_numpy_array =
  python_pre_interfaced_function
    [|CamlpillType|]
    (fun pyargs ->
       let vec = CONV_V.from_py pyargs.(0) in
       let ret_ba = ref None in
       let () =
         Mpi_petsc.with_petsc_vector_as_bigarray vec
           (fun ba -> ret_ba := Some (CONV_FA.to_py (Bigarray.genarray_of_array1 ba)))
       in
         match !ret_ba with
           None -> failwith "pypetsc.ml, my_vector_as_numpy_array: shouldn't happen"
         | Some x -> x)
;;

let register_bindings () =
  register_pre_functions_for_python
    [|("VecCreate", CONV_I_S_V.to_py Mpi_petsc.vector_create);
      ("VecSetValue", CONV_V_I_F_U.to_py Mpi_petsc.vector_set);
      ("VecAddValue", CONV_V_I_F_U.to_py Mpi_petsc.vector_inc);
      ("VecAssemble", CONV_V_U.to_py Mpi_petsc.vector_assemble);
      ("VecAssemblyBegin", CONV_V_U.to_py Mpi_petsc.vector_assembly_begin);
      ("VecAssemblyEnd", CONV_V_U.to_py Mpi_petsc.vector_assembly_end);
      ("VecZero", CONV_V_U.to_py Mpi_petsc.vector_zero);
      ("VecScale", CONV_V_F_U.to_py Mpi_petsc.vector_scale);
      ("VecDuplicate", CONV_V_V.to_py Mpi_petsc.vector_duplicate);
      ("VecCopy", CONV_V_V_U.to_py Mpi_petsc.vector_copy);
      ("VecAXPBY", CONV_F_F_V_V_U.to_py Mpi_petsc.vector_AXPBY);
      ("VecPointwiseMult", CONV_V_V_V_U.to_py Mpi_petsc.vector_pointwise_mult);
      ("VecPointwiseDivide", CONV_V_V_V_U.to_py Mpi_petsc.vector_pointwise_divide);
      ("VecAsNumpyArray", my_vector_as_numpy_array);
      ("MatCreate", CONV_S_S_I_I_M.to_py my_matrix_create);
      ("MatSetPreallocation", CONV_M_I_I_U.to_py Mpi_petsc.matrix_set_prealloc);
      ("MatDuplicate", CONV_B_M_M.to_py Mpi_petsc.matrix_duplicate);
      ("MatCopy", CONV_B_M_M_U.to_py Mpi_petsc.matrix_copy);
      ("MatSetValue", CONV_M_I_I_F_U.to_py Mpi_petsc.matrix_set);
      ("MatAddValue", CONV_M_I_I_F_U.to_py Mpi_petsc.matrix_inc);
      ("MatAssemble", CONV_M_B_U.to_py Mpi_petsc.matrix_assemble);
      ("MatAssemblyBegin", CONV_M_B_U.to_py Mpi_petsc.matrix_assembly_begin);
      ("MatAssemblyEnd", CONV_M_B_U.to_py Mpi_petsc.matrix_assembly_end);
      ("MatReincarnate", CONV_M_U.to_py Mpi_petsc.matrix_reincarnate);
      ("MatZeroEntries", CONV_M_U.to_py Mpi_petsc.matrix_zero_entries);
      ("MatMult", CONV_M_V_V_U.to_py Mpi_petsc.matrix_times_vector);
      ("MatMultAdd", CONV_M_V_V_V_U.to_py Mpi_petsc.matrix_mult_add);
      ("MatMultTranspose", CONV_M_V_V_U.to_py Mpi_petsc.matrix_transpose_times_vector);
      ("MatScale", CONV_M_F_U.to_py Mpi_petsc.matrix_scale);
      ("MatAddIdentity", CONV_M_F_U.to_py Mpi_petsc.matrix_add_identity);
      ("MatSaveToFile", CONV_M_S_U.to_py Mpi_petsc.matrix_write_on_file);
      ("MatReadFromFile", CONV_S_M.to_py Mpi_petsc.matrix_read_from_file);
      

      |]
;;
