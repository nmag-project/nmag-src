
(*
#use "topfind";;
#require "nsim_grammars";;
*)

#load "ddiffop_lexer.cmo";;
#load "ddiffop_parser.cmo";;

(* For testing: *)
let parse_it str =
  let lexbuf = Lexing.from_string str in
  let result = Ddiffop_parser.parse_ddiffop Ddiffop_lexer.token lexbuf in
  result
;;

(* For debugging: *)
let generic_token_stream lexer eof_token str =
  let lexbuf = Lexing.from_string str in
  let rec walk have =
    let next = lexer lexbuf in
      if next = eof_token then List.rev have
      else walk (next::have)
  in walk []
;;


let token_stream = generic_token_stream Ddiffop_lexer.token Ddiffop_parser.EOF;;


(*

Should be able to handle:

parse_it "<rho || D/Dxj m_Py[boundary](j)>, j:3; (L||R) = (*||m_Py)";;

parse_it "(-24.058305)*<d/dxj H_exch_Py(k) || d/dxj m_Py(k)>, j:3,k:3";;

XXX BUG Broken now!

=====================================================

Works:

parse_it "<rho || d/dxj phi>, j:3";;

parse_it "<rho || d/dxj phi>, j:3, k:3";;

token_stream "<rho || D/Dxj m_Py[3](j)>, j:3; (L||R) = (*||m_Py)";;

parse_it "<rho || d/dxj phi[not 3 and not 4 and not 5]>, j:3, k:3";;

*)
