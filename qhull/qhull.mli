(* 
   (C) 2005 Dr. Thomas Fischbacher
*)

(** Qhull interface for Delaunay triangulation *)


(*
  Tests showed that the extra time required to do consistency checks
  is absolutely negligible in comparison to libqhull run time.

  Hence, we do not export this:

val delaunay_nocheck: float array array -> int array array
*)


val version : unit -> string
(** This function returns an unique code version string *)

val delaunay: float array array -> int array array
(** Map an array of points (each being given as an array of float coordinates)
    to an array of simplices.

    A simplex is given as an array of point indices.
*)

val neighbours: float array array -> int array array
(** Map an array of points (each being given as an array
    of float coordinates) to an array of neighbour indices
    of each point.
*)
