(* (C) 2005 Dr. Thomas Fischbacher *)

(**
   The pyfem module does not export anything, excapt a version string.
   Its sole purpose is to add extra ocaml callbacks to python! 
 *)

val version : unit -> string
(** This function returns an unique code version string *)

