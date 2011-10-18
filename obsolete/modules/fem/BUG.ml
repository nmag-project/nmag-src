let gensym =
  let ht_sym_count = Hashtbl.create 10 in
  fun name ->
    let n =
      try
	Hashtbl.find ht_sym_count name
      with
      | Not_found -> 0
    in
    let () = Hashtbl.replace ht_sym_count name (1+n) in
    let sym = Printf.sprintf "%s-%d" name (1+n)
    in sym
;;

type dof_region_spec = (string * bool * (int option array)) array;;
type opt_field_restriction = dof_region_spec option;;

let make_field ?(name=gensym "Field_") ?restriction ?constant_value mwe =
  let restriction:(string * bool * (int option array)) array option = restriction in
	failwith "FOO";;

