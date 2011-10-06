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
 * This module defines a type which stores just the minimal amount of
 * information necessary to identify univocally a mesh (points, simplices
 * and - optionally - surfaces).
 *)

type t

val init_from_arrays: float array array -> (int * int array) array -> t

val dummy: t

val get_nr_dims: t -> int
(** Return number of dimensions of the mesh *)

val get_nr_points: t -> int
(** Return number of points in the mesh *)

val get_nr_simplices: t -> int
(** Return number of simplices in the mesh *)

val get_points: t -> Base.Ba.F.array2
(** Return the array of points in the mesh *)

val get_simplices: t -> Base.Ba.I32.array2
(** Return the array of simplices in the mesh *)

val get_regions: t -> Base.Ba.I32.array1
(** Return the array of regions (for each simplex) in the mesh *)

val get_simplex_point: t -> int -> int -> Base.Ba.F.array1
(** get_simplex_point m0 nr_spx nr_pnt

    Return the nr_pnt point of the nr_spx simplex of the mesh m0 *)

val get_simplex_points: t -> int -> float array array
(** Return an array of the points (as arrays) of the given simplex *)
