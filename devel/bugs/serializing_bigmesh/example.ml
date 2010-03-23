
(* 
ocamlfind ocamlopt -I ../../../snippets -I ../../../mesh -package snippets,mesh,unix,bigarray -o example example.ml -linkpkg
 *)

open Snippets;;

let ba_checksum ba =
  let len = Bigarray.Array1.dim ba in
  let rec walk pos sum =
    if pos = len then sum
    else walk (pos+1) (sum + (Char.code ba.{pos})*pos)
  in walk 0 0
;;

let Some bigmesh = Mesh.read_mesh "/tmp/meshbigbar28000nodes.nmesh";;

let ser_bigmesh = marshal_to_bigarray bigmesh [Marshal.Closures];;
let len_bigmesh = Bigarray.Array1.dim ser_bigmesh;;

Printf.printf "Bigmesh serialized size: %d (SUM=%d)\n%!" len_bigmesh (ba_checksum ser_bigmesh);;

let re_bigmesh = demarshal_from_bigarray ser_bigmesh;;

Gc.minor();;
Gc.major();;
Gc.full_major();;
Printf.printf "Done.\n";;


