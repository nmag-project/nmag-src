#use "topfind";;
#require "snippets";;

open Snippets;;

let lines = read_file_as_lines "readfile.ml"
  in List.iter (Printf.printf "%s\n") lines;;

(* When executed, this file will print itself. *)

