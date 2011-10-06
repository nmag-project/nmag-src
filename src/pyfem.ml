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
open Mesh;;
open Fem;;
open Pypetsc;;

let my_sample_element = make_element 1 ("T",[||]) 1;;
let my_sample_mwe =
  make_mwe "sample mwe" (fun sx -> my_sample_element) dummy_mesh;;
let my_sample_vector = Mpi_petsc.vector_dummy ();;
let my_sample_field = FEM_field (my_sample_mwe, None, my_sample_vector);;
let my_sample_cofield = FEM_cofield (my_sample_mwe, None, my_sample_vector);;

(* Create wrapper/unwrapper for FEM field *)
let pysym_field = "FEM Field";;
let (pyobj_from_fem_field, pyobj_to_fem_field) =
       register_pyobj pysym_field my_sample_field;;

(* Create wrapper/unwrapper for FEM field *)
let pysym_cofield = "FEM Co-Field";;
let (pyobj_from_fem_cofield, pyobj_to_fem_cofield) =
       register_pyobj pysym_cofield my_sample_cofield;;

(* Converters for auto-bindings *)
module CONV_FIELD = struct
  type caml_type = float fem_field
  let pycaml_pyobject_type = CamlpillType
  let from_py = pyobj_to_fem_field
  let to_py = pyobj_from_fem_field
end

module CONV_COFIELD = struct
  type caml_type = float fem_cofield
  let pycaml_pyobject_type = CamlpillType
  let from_py = pyobj_to_fem_cofield
  let to_py = pyobj_from_fem_cofield
end

let my_extract_vec_from_field (FEM_field (_, _, v)) = v;;
let my_extract_vec_from_cofield (FEM_cofield (_, _, v)) = v;;

let my_build_probe_matrix matrix_name field dof_stem positions =
  let fun_matrix_create rows cols =
    Mpi_petsc.matrix_create rows cols matrix_name in
  let fun_matrix_add_entry = Mpi_petsc.matrix_inc in
  let matrix =
    Fem.build_probe_matrix
      fun_matrix_create fun_matrix_add_entry
      field dof_stem positions
  in
  let () = Mpi_petsc.matrix_assemble matrix true
  in
    matrix
;;

module CONV_FIELD_V = CONV_FUN1 (CONV_FIELD) (CONV_V)
module CONV_COFIELD_V = CONV_FUN1 (CONV_COFIELD) (CONV_V)
module CONV_S_F_S_FLL_M =
  CONV_FUN4 (CONV_S) (CONV_FIELD) (CONV_S) (CONV_FLL) (CONV_M)

let register_bindings () =
  register_pre_functions_for_python
    [|("FieldGetVec", CONV_FIELD_V.to_py my_extract_vec_from_field);
      ("CofieldGetVec", CONV_COFIELD_V.to_py my_extract_vec_from_cofield);
      ("FemBuildProbeMatrix", CONV_S_F_S_FLL_M.to_py my_build_probe_matrix);
      |]
;;
