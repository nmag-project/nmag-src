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

type t

val init: Mesh0.t -> t
val get_point_matrix: t -> int -> Base.Ba.F.array2
val get_inv_point_matrix: t -> int -> Base.Ba.F.array2
val get_face_eqn: t -> int -> int -> Base.Ba.F.array1

val dummy : t
