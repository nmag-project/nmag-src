(* Standard micromagnetic problem n. 4
 * UNITS: length in nanometers (10^(-9) m), times in picoseconds (10^(-12) s)
 *)

#use "topfind";;
#require "fem_element";;
#require "mpi_petsc";;
#require "fastfields";;

#require "timestep";;

#use "timestep.ml";;

open Snippets;;
open Fem_element;;
open Mesh;;
open Timestep;;

let ex_mesh =
  mesh_it (fem_geometry_from_bodies ~mesh_exterior:true
	     ([|-5.0;-5.0|],[|5.0;5.0|])
	     [|Body (body_trafo_id 2, bc_ellipsoid [|2.0;2.0|])|])
    !opt_mesher_defaults
    0.8
;;

let rw_mesh ?(filename="debug.mesh") mesh =
  let () = write_mesh filename mesh in
  let re_read = read_mesh filename in
  match re_read with
  | None -> failwith "Could not re-read the mesh!\n"
  | Some m -> m
;;

let rw_ex_mesh = rw_mesh ex_mesh;;


let compare_meshes_simplex_wise f =
  array_all_satisfy (fun x -> x)
    (array_pointwise f ex_mesh.mm_simplices rw_ex_mesh.mm_simplices)
;;

let compare_meshes_point_wise f =
  array_all_satisfy (fun x -> x)
    (array_pointwise f ex_mesh.mm_points rw_ex_mesh.mm_points)
;;

compare_meshes_point_wise (fun p1 p2 -> p1.mp_in_body = p2.mp_in_body);;
(* This tells us we have a problem! *)

array_position_if (fun (p,q) -> p <> q) (array_pointwise (fun p1 p2 -> (p1.mp_in_body,p2.mp_in_body)) ex_mesh.mm_points rw_ex_mesh.mm_points) 0;;

ex_mesh.mm_points.(0).mp_in_body;;

rw_ex_mesh.mm_points.(0).mp_in_body;;

(*********************************** POINTS ***************************************)
(*
     mp_coords: float array;
     mutable mp_id: point_id;
     mutable mp_belongs_to: simplex array;
     mutable mp_in_body: simplex_region list;
*)
Printf.printf "\n\n(******************** check mp_coords  *********************)";;
Array.iter (fun pt -> 
  let array_coords = pt.mp_coords in 
  let () = Printf.printf "\n%!" in
  Printf.printf "%s " (float_array_to_string array_coords)
	   ) ex_mesh.mm_points
;;

Printf.printf "\n\n(******************** check mp_point_id  *******************)";;
Array.iter (fun pt -> 
  let id = pt.mp_id in 
  let () = Printf.printf "\n%!" in
  Printf.printf "%d " id
	   ) ex_mesh.mm_points
;;

Printf.printf "\n\n(******************** check mp_in_body *********************)";;
Array.iter (fun pt -> 
  let array_simplices = pt.mp_belongs_to in 
  let () = Printf.printf "\n%!" in
  
  (* loop over bodies *)
  Array.iter (fun simplex -> Printf.printf "%d " simplex.ms_id ) array_simplices
	   ) ex_mesh.mm_points
;;

Printf.printf "\n\n(******************** check mp_in_body *********************)";;
Array.iter (fun pt -> 
  let list_bodies = pt.mp_in_body in 
  let () = Printf.printf "\n%!" in
  
  (* loop over bodies *)
  List.iter (fun (Body_Nr x) -> Printf.printf "%d " x ) list_bodies
	   ) ex_mesh.mm_points
;;

(********************************** POINTS ARE OK !!! *****************************)
(*********************************** SIMPLICES ************************************)
(*
    {
     mutable ms_id: simplex_id;
     ms_points: point array;
     ms_neighbours: simplex option array;
     ms_neighbour_backrefs: int array;
     ms_face_ids: face_id array;
     mutable ms_in_body: simplex_region;
     mutable ms_cc_midpoint: float array;
     mutable ms_ic_midpoint: float array;
     mutable ms_cc_radius: float;
     mutable ms_ic_radius: float;
     ms_ext_point_coords: float array array option;
     ms_inv_ext_point_coords: float array array option;
     ms_point_coords_det: float;
   }
*)

Printf.printf "\n\n(******************** check ms_id  *********************)";;
Array.iter (fun sx -> 
  let id = sx.ms_id in 
  let () = Printf.printf "\n%!" in
  Printf.printf "%d " id
	   ) ex_mesh.mm_simplices
;;

Printf.printf "\n\n(******************** check ms_points ******************)";;
Array.iter (fun sx -> 
  let pts = sx.ms_points in 
  let () = Printf.printf "\n%!" in
  Printf.printf "%s " (int_array_to_string (Array.map (fun point -> point.mp_id) pts ) )
	   ) ex_mesh.mm_simplices
;;

Printf.printf "\n\n(******************** check ms_neighbours **************)";;
Array.iter (fun sx -> 
  let nbs = sx.ms_neighbours in 
  let () = Printf.printf "\n%!" in
  Array.iter (fun nb -> match nb with
  | Some x -> Printf.printf "%d " x.ms_id 
  | None -> Printf.printf "") nbs 
	   ) ex_mesh.mm_simplices
;;

Printf.printf "\n\n(******************** check ms_neighbour_backrefs *******)";;
Array.iter (fun sx -> 
  let nb_br = sx.ms_neighbour_backrefs in 
  let () = Printf.printf "\n%!" in
  Printf.printf "%s " (int_array_to_string nb_br) 
	     ) ex_mesh.mm_simplices
;;

Printf.printf "\n\n(******************** check ms_face_ids *****************)";;
Array.iter (fun sx -> 
  let faces = sx.ms_face_ids in 
  let () = Printf.printf "\n%!" in
  Array.iter (fun face -> Printf.printf "%s " (int_array_to_string face) ) faces
  	   ) ex_mesh.mm_simplices
;;

Printf.printf "\n\n(******************** check ms_in_body *****************)";;
Array.iter (fun sx -> 
  let body = sx.ms_in_body in 
  let () = Printf.printf "\n%!" in
  (fun (Body_Nr x) -> Printf.printf "%d " x) body
  	   ) ex_mesh.mm_simplices
;;

Printf.printf "\n\n(******************** check ms_cc_midpoint, ms_ic_midpoint *****************)";;
Array.iter (fun sx -> 
  let ic_mp = sx.ms_ic_midpoint in
  let cc_mp = sx.ms_cc_midpoint in
  let () = Printf.printf "\n%!" in
  Printf.printf "%s %s" (float_array_to_string ic_mp) (float_array_to_string cc_mp)
	   ) ex_mesh.mm_simplices
;;

Printf.printf "\n\n(******************** check ms_cc_radius, ms_ic_radius *****************)";;
Array.iter (fun sx -> 
  let ic_r = sx.ms_ic_radius in
  let cc_r = sx.ms_cc_radius in
  let () = Printf.printf "\n%!" in
  Printf.printf "%f %f" ic_r cc_r
	   ) ex_mesh.mm_simplices
;;

Printf.printf "\n\n(******************** check ms_ext_point_coords *****************)";;
Array.iter (fun sx -> 
  let ext_pt = sx.ms_ext_point_coords in
  let () = Printf.printf "\n%!" in
  match ext_pt with
  | Some x -> Array.iter (fun arr -> Printf.printf "%s " (float_array_to_string arr)) x 
  | None -> Printf.printf ""
	   ) ex_mesh.mm_simplices
;;

Printf.printf "\n\n(******************** check ms_inv_ext_point_coords *****************)";;
Array.iter (fun sx -> 
  let ext_pt = sx.ms_inv_ext_point_coords in
  let () = Printf.printf "\n%!" in
  match ext_pt with
  | Some x -> Array.iter (fun arr -> Printf.printf "%s " (float_array_to_string arr)) x 
  | None -> Printf.printf ""
	   ) ex_mesh.mm_simplices
;;

Printf.printf "\n\n(******************** check ms_point_coords_det *****************)";;
Array.iter (fun sx -> 
  let det = sx.ms_point_coords_det in
  let () = Printf.printf "\n%!" in
  Printf.printf "%f " det
	   ) ex_mesh.mm_simplices
;;
