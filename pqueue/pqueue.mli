
(* (C) 2008 Dr. Thomas Fischbacher *)

type ('a, 'b, 'c) pqueue

val make : ('a -> 'b) -> ('c, 'b, 'a) pqueue
val is_empty : ('a, 'b, 'c) pqueue -> bool
val nr_entries : ('a, 'b, 'c) pqueue -> int
val push : ('a, 'b, 'c) pqueue -> 'a -> 'c -> unit
val pop : ('a, 'b, 'c) pqueue -> 'a * 'c
val peek : ('a, 'b, 'c) pqueue -> 'a * 'c
val pop_by_name : ('a, 'b, 'c) pqueue -> 'b -> 'a * 'c
val peek_by_name : ('a, 'b, 'c) pqueue -> 'b -> 'a * 'c
val reschedule_by_name : ('a, 'b, 'c) pqueue -> 'b -> 'a -> unit
val iter : ('a, 'b, 'c) pqueue -> ('a -> 'c -> unit) -> unit
val check : (float, 'a, 'b) pqueue -> unit
