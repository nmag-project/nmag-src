(*
   Flexible, general dynamic Delaunay triangulation in arbitrary dimensions.

   (C) 2005 Dr. Thomas Fischbacher

  Note: the code provided here is intended to replace qhull.

  There are at least two good reasons why to do so:

  (1) qhull is fast, but the code sucks beyond belief. Every
  experienced C hacker who had a look at it will presumably want to
  agree.

  (2) If we use qhull, we nevertheless have to re-determine data which
  is qhull-internal from qhull's output. For example, we have to find
  the equations for the hyperplane faces of simplices.

  In the end, this means that we have to re-do a considerable part of
  the relevant logic of qhull anyway, and it is more appropriate to
  have to do the corresponding linear algebra calculations only once,
  not twice, in our code.
  
*)

(*
  Point Coordinates are abstract.

  Points are 
*)

type point_id = int;;

(*

 We may want to introduce a scheme were we use integers for point IDs,
 but subdivide those, so that, e.g. points with bit 0 set are kept on
 machine #1, and points with bit 1 set are kept on machine #2 (Points
 with both bits set are considered belonging to an overlap region and
 are kept on both machines.)
   
 *)

type element_id = int;;

(*
   In the end, these might or might not play a role for the algorithm,
   but they are extremely useful in debugging.
*)

type ('coords_t,'face_eqn_t) point =
    {
     dp_coords: 'coords_t;
     dp_id: point_id;
     mutable dp_belongs_to: ('coords_t,'face_eqn_t) element array;
   }
;;

(*

   Note ad elements: it makes sense to provide a function that re-maps
   coordinates, e.g. pulls back the center of an element to the origin
   of the coordinate system. With this, we can also nicely mesh spaces
   like S^3.

   We furthermore may want to provide "mirage functions": these re-map
   coordinates for elements adjacent across a given face.

   The idea is that this way, we should be able to even mesh
   orbifolds. 

   On the other hand, it is far from clear how to deal with mesh
   changes in the presence of such functions, so that perhaps, it is a
   much better idea to just forget about them, demand that the
   elements close to the orbifold singularity are fixed, and worry
   about the issue in stiffnes matrix assembly.


*)

type ('coords_t,'face_eqn_t) element =
    {
     de_id: element_id;
     de_points: ('coords_t,'face_eqn_t) point array;
     (* sorted by point_id *)

     de_neighbours: ('coords_t,'face_eqn_t) element option array;
     (* a neighbour may be None *)

     de_neighbour_backrefs: int array;
     (* If we cross a face and enter a neighbour, we have to
	know the number of the face from the perspective
	of the neighbour that leads back to us.
      *)

     de_face_eqns: 'face_eqn_t array;
     (* XXX Eventually remove that comment!
	Note that face_eqn_t should include the possibility of having
	a face equation saying "I am not valid".
	But that information is also provided by a de_neighbour of None,
	so this is not that much an issue.
      *)

     (*
	Circum-Circle and In-Circle.

	They are needed at least for the element quality test, but
	keeping track of those allows us to do Delaunay triangulation
	in D dimensions without having to refer to D+1 dimensions.
      *)
     de_cc_midpoint: 'coords_t;
     de_ic_midpoint: 'coords_t;
     de_cc_radius: float;
     de_ic_radius: float;
   }
;;

