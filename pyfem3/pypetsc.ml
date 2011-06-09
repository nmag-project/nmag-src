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

let register_pyobj desc_str dummy =
  let () = register_ocamlpill_types [|desc_str|]
  in make_ocamlpill_wrapper_unwrapper desc_str dummy
;;

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
module CONV_V_F_U = CONV_FUN2 (CONV_V) (CONV_F) (CONV_U)
module CONV_M_U = CONV_FUN1 (CONV_M) (CONV_U)
module CONV_M_V_V_U = CONV_FUN3 (CONV_M) (CONV_V) (CONV_V) (CONV_U)
module CONV_I_S_V = CONV_FUN2 (CONV_I) (CONV_S) (CONV_V)

let register_bindings () =
  register_pre_functions_for_python
    [|("mat_zero_entries", CONV_M_U.to_py Mpi_petsc.matrix_zero_entries);
      ("mat_mult", CONV_M_V_V_U.to_py Mpi_petsc.matrix_times_vector);
      ("vec_create", CONV_I_S_V.to_py Mpi_petsc.vector_create);
      ("vec_zero", CONV_V_U.to_py Mpi_petsc.vector_zero);
      ("vec_scale", CONV_V_F_U.to_py Mpi_petsc.vector_scale);
      ("vec_assemble", CONV_V_U.to_py Mpi_petsc.vector_assemble);
      ("vec_assembly_begin", CONV_V_U.to_py Mpi_petsc.vector_assembly_begin);
      ("vec_assembly_end", CONV_V_U.to_py Mpi_petsc.vector_assembly_end);|]
;;

(*external vector_zero: vector -> unit = "caml_petsc_vec_zero";;
external vector_scale: vector -> float -> unit = "caml_petsc_vec_scale";;


external vector_assemble: vector -> unit = "caml_petsc_vec_assemble";;
external vector_assembly_begin: vector -> unit = "caml_petsc_vec_assembly_begin";;
external vector_assembly_end: vector -> unit = "caml_petsc_vec_assembly_end";;
*)
