(* Nmag micromagnetic simulator
 * Copyright (C) 2010 University of Southampton
 * Hans Fangohr, Thomas Fischbacher, Matteo Franchin and others
 *
 * WEB:     http://nmag.soton.ac.uk
 * CONTACT: nmag@soton.ac.uk
 *
 * AUTHOR(S) OF THIS FILE: Matteo Franchin
 * LICENSE: GNU General Public License 2.0
 *          (see <http://www.gnu.org/licenses/>)
 *
 * This module defines the datastructures storing simplex information in
 * a mesh. It also contains various routines to build and retrieve such
 * information.
 *
 *)

open Base
open Base.Ba

(*
     (*
        Circum-Circle and In-Circle.

        They are needed at least for the simplex quality test, but
        keeping track of those allows us to do Delaunay triangulation
        in D dimensions without having to refer to D+1 dimensions.
      *)
     mutable ms_cc_midpoint: float array;
     mutable ms_ic_midpoint: float array;
     mutable ms_cc_radius: float;
     mutable ms_ic_radius: float;
*)

type idx = int

type t = {
  msd_mesh0: Mesh0.t;
  (** The mesh to which this simplex info is attached to *)

  mutable msd_ext_point_coords: F.array3 Deferred.t;
  (** Point matrices *)

  mutable msd_inv_ext_point_coords: F.array3 Deferred.t;
  (** Inverse point matrices *)

  mutable msd_point_coords_det: F.array1 Deferred.t;
  (** Determinants of point matrices *)

  mutable msd_ic_cc_data: F.array2 Deferred.t;
  (** incircle and circumcircle midpoints (d*2 floats)
      and radii (1*2 floats) *)
}

type simplex = t * idx
(** The way we structure simplex data now (in this file) is different with
    respect to the old one. Initially, we had an array of Mesh.simplex
    structures, each containing the data related to one single simplex.
    We now split all the fields of Mesh.simplex into separate Bigarray-s,
    which is more memory-efficient (reduce number of ml-object headers).
    We consequently loose the concept of single simplex, as we don't have
    a datastructure similar to Mesh.simplex which contains all the info
    relevant for one single simplex. However, the old code still provides
    functions working on single simplices. We then need to provide a
    replacement for the concept of Mesh.simplex. A Mesh.Simplex.simplex
    is a tuple made of the simplex data (Simplex.t) plus the simplex index.
    It should then be enough to retrieve all data for one single simplex. *)

let my_fill_point_matrices m0 ba =
  let d = Mesh0.get_nr_dims m0 in
    F.iter32 ba
      (fun nr_spx matrix ->
         F.iter21 matrix
           (fun nr_row row ->
              if nr_row = d
              then Bigarray.Array1.fill row 1.0
              else
                F.set_all1 row
                  (fun nr_col ->
                     let point_coords = Mesh0.get_simplex_point m0 nr_spx nr_col
                     in point_coords.{nr_row})))

let my_build_point_matrices m0 () =
  let d = Mesh0.get_nr_dims m0 in
  let n = Mesh0.get_nr_simplices m0 in
  let ba = F.create3 n (d + 1) (d + 1) in
  let () = my_fill_point_matrices m0 ba
  in ba

(* NOTE: in the function above we build the inverses of the extended point
     matrices. Notice that we do not reuse the data stored in
     msd_ext_point_coords, since we want to be independent from that.
 *)
let my_fill_inv_point_matrices_and_dets simplex () =
  let m0 = simplex.msd_mesh0 in
  let d = Mesh0.get_nr_dims m0 in
  let n = Mesh0.get_nr_simplices m0 in
  let compute_det_and_inv = Snippets.det_and_inv (d + 1) in
  let inv_mxs = Deferred.create simplex.msd_inv_ext_point_coords in
  let () = my_fill_point_matrices m0 inv_mxs in
  let dets = Deferred.create simplex.msd_point_coords_det in
  let () =
    F.iter32 inv_mxs
      (fun nr_spx mx ->
         let det, inv_mx = compute_det_and_inv (F.to_ml2 mx) in
           begin
             F.set_all2 mx (fun i1 i2 -> inv_mx.(i1).(i2));
             F.set1 dets nr_spx det;
           end)
  in ()

let init m0 =
  let d = Mesh0.get_nr_dims m0 in
  let dd = d + 1 in
  let n = Mesh0.get_nr_simplices m0 in
  let fa1_creator () = F.create1 n in
  let fa2_creator () = F.create2 n dd in
  let fa3_creator () = F.create3 n dd dd in
  let point_matrices =
    Deferred.init ~creator:(my_build_point_matrices m0) "ext_point_coords" in
  let inv_point_matrices =
    Deferred.init ~creator:fa3_creator "inv_ext_point_coords" in
  let point_coords_det = Deferred.init ~creator:fa1_creator "point_coords_det" in
  let ic_cc_data = Deferred.init ~creator:fa2_creator "ic_cc_data" in
  let simplex =
    { msd_mesh0 = m0;
      msd_ext_point_coords = point_matrices;
      msd_inv_ext_point_coords = inv_point_matrices;
      msd_point_coords_det = point_coords_det;
      msd_ic_cc_data = ic_cc_data; }
  in
  let () =
    Deferred.set_collective_filler2 inv_point_matrices point_coords_det
      (my_fill_inv_point_matrices_and_dets simplex)
  in simplex

let get_num_points sx_data = 1 + Mesh0.get_nr_dims sx_data.msd_mesh0

(** Get the point matrix for the given simplex. *)
let get_point_matrix sx_data =
  let ba3 = Deferred.get sx_data.msd_ext_point_coords in
    (fun sx_id -> Bigarray.Array3.slice_left_2 ba3 sx_id)

(** Get the determinant of the point matrix for the given simplex. *)
let get_point_matrix_det sx_data =
  let ba1 = Deferred.get sx_data.msd_point_coords_det in
    (fun sx_id -> F.get1 ba1 sx_id)

(** Get the inverse point matrix for the given simplex. *)
let get_inv_point_matrix sx_data =
  let ba3 = Deferred.get sx_data.msd_inv_ext_point_coords in
    (fun sx_id -> Bigarray.Array3.slice_left_2 ba3 sx_id)

(** Get the n-th row of the inverse point matrix for the given simplex.
    That corresponds to retrieve the coefficients of the equation of the n-th
    face of the simplex. *)
let get_face_eqn sx_data sx_id =
  let mx = get_inv_point_matrix sx_data sx_id in
    (fun n -> Bigarray.Array2.slice_left mx n)

let dummy = init Mesh0.dummy
