(* Nmag micromagnetic simulator
   Copyright (C) 2010 University of Southampton
   Hans Fangohr, Thomas Fischbacher, Matteo Franchin and others

   WEB:     http://nmag.soton.ac.uk
   CONTACT: nmag@soton.ac.uk

   AUTHOR(S) OF THIS FILE: Matteo Franchin
   LICENSE: GNU General Public License 2.0
            (see <http://www.gnu.org/licenses/>)
 *)

(**   Module to handle deferred (lazy) computation.

  We want to achieve the following:
   1. Define a datatype for deferred computations: we can give the recipe to
      create the value (a function fun () -> value) and trigger the creation
      only when we need to access it;
   2. We should be able to clear the value when it is not needed and either
      reset it to the initial state (i.e. the value is not there, but will be
      automatically re-created if needed) or delete it definitely (meaning
      that we do not expect the value to be required anymore);
   3. Be able to find out easily (through logging) in which stage of the
      computation deferred quantities are needed.

   NOTE: Such functionality is currently not provided by the OCaml builtin
     Lazy module.
 *)

exception DeferredDeleted of string
(** Exception raised when accessing a Deferred quantity marked as Deleted *)

type 'a t
(** The type for deferred quantities *)

val init : string -> (unit -> 'a) -> 'a t
(** Create a new deferred quantity *)

val activate_logging : bool -> unit
(* Sets whether the logging is activated or not *)

val set_context : string -> unit
(** Function to set a context name (useful to identify where deferred
    computations happen) *)

val get : 'a t -> 'a
(** Get a deferred quantity (computing it if necessary). *)

val delete : ?msg:string -> 'a t -> unit
(** Delete a deferred quantity so that it won't be accessible anymore. *)

val reset : 'a t -> unit
(** Reset a deferred quantity. The quantity is discarded and recomputed later,
    if necessary. *)

val get_computation_count : 'a t -> int
(** Get the number of times a deferred quantity has been computed *)
