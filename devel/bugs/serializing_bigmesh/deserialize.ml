(* Primitive example demonstrating a problem(?) in the OCaml deserializer *)

(* ocamlfind ocamlopt -package unix,bigarray -o deserialize deserialize.ml deserialize_stubs.c  -linkpkg *)

external marshal_to_bigarray : 
  'a -> Marshal.extern_flags list -> 
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t 
  = "caml_marshal_to_bigarray"
      
external demarshal_from_bigarray :
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t -> 'a 
  = "caml_demarshal_from_bigarray"


let ba_from_file filename =
  let len = (Unix.stat filename).Unix.st_size in
  let ba = 
    Bigarray.Array1.create Bigarray.char Bigarray.c_layout len
  in
  let fh = open_in filename in
  let () =
    for i=0 to len-1 do
      ba.{i} <- input_char fh
    done
  in
  let () = close_in fh in
    ba
;;

let ba = ba_from_file "/tmp/ba.master";;
let mesh = demarshal_from_bigarray ba;;

let () = Printf.printf "OK!\n%!";;


