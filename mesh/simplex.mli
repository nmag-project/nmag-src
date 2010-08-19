(* Nmag micromagnetic simulator
   Copyright (C) 2010 University of Southampton
    Hans Fangohr, Thomas Fischbacher, Matteo Franchin and others

    WEB:     http://nmag.soton.ac.uk
    CONTACT: nmag@soton.ac.uk

    AUTHOR(S) OF THIS FILE: Matteo Franchin
    LICENSE: GNU General Public License 2.0
             (see <http://www.gnu.org/licenses/>)

    This module defines the datastructures storing simplex information in
    a mesh. It also contains various routines to build and retrieve such
    information.

    NOTE 1: If you need to get simplex information repeatedly, you should
      partially apply the functions. Example:

      let get_it = Simplex.get_point_matrix sd in
        for i = 0 to n; do
          let mx = get_it i in ...
        done
 *)

type t

type idx = int
(** The simplex index *)

val dummy : t

val init: Mesh0.t -> t
(** Create a simplex object for the given mesh. The simplex object contains
    information about all the simplices in the mesh. *)

val get_num_points: t -> int
(** Get number of points of the simplices *)

val get_point_matrix: t -> idx -> Base.Ba.F.array2
(** Get the point matrix for the given simplex index. See note 1. *)

val get_point_matrix_det: t -> idx -> float
(** Get the determinant of the point matrix for the given simplex index.
    See note 1. *)

val get_inv_point_matrix: t -> idx -> Base.Ba.F.array2
(** Get the inverse of the point matrix for the given simplex index.
    See note 1. *)

(** Get all the inverse point matrice. This is a F.array3 made by
    (# simplices) x (d + 1) x (d + 1) entries (d is the mesh dimension). *)
val get_inv_point_matrices: t -> Base.Ba.F.array3

val get_face_eqn: t -> int -> int -> Base.Ba.F.array1

val get_incircle_midpoint: t -> int -> Base.Ba.F.array1
(** Get the incircle midpoint for the given simplex *)

val get_circumcircle_midpoint: t -> int -> Base.Ba.F.array1
(** Get the circumcircle midpoint for the given simplex *)

val get_incircle_radius: t -> int -> float
(** Get the incircle radius for the given simplex *)

val get_circumcircle_radius: t -> int -> float
(** Get the circumcircle radius for the given simplex *)
