
(* let str = "3* <d/dx0 M(0) || d/dx1 M(1) > - 4* <d/dx1 M(2) || d/dx2 M(0) >";; *)

(* let str = " <d/dx0 T || d/dx0 T> + <d/dx1 T || d/dx1 T>";; *)

let str = " <d/dxj T || d/dxj T>, j:2";;

let parse_it str =
  let lexbuf = Lexing.from_string str in
  let result = Diffop_parser.main Diffop_lexer.token lexbuf in
  result
;;

let (pstr_contribs, pstr_summations) = parse_it str;;

Printf.printf "LEN RES: %d LEN SUMS: %d\n" (List.length pstr_contribs) (List.length pstr_summations);;

