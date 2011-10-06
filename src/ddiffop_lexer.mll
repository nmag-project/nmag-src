{
open Ddiffop_parser   (* The type 'token' is defined there *)
}

  rule token = parse
  [' ' '\t' '\n']     { token lexbuf }     (* skip blanks *)
| '-'? ['0'-'9']+ as lxm { INT(int_of_string lxm) }
(* If this were lex, it would try to find the first rule that gives the longest match. *)
| "and"         { DOF_REGION_AND }
| "or"          { DOF_REGION_OR }
| "not"         { DOF_REGION_NOT }
| "some"        { DOF_REGION_SOME }
| "all"         { DOF_REGION_ALL }
| '-'? ['0'-'9']* '.' ['0'-'9']+ (['e' 'E'] '-'? ['0'-'9']+)? as lxm { FLOAT(float_of_string lxm) }
| '-'? ['0'-'9']+ ['e' 'E'] '-'? ['0'-'9']+ as lxm { FLOAT(float_of_string lxm) }
| ['a'-'z' 'A'-'Z' '0'-'9' '_']+ as lxm {STRING(lxm)} (* Names MAY start with underscores! *)
| "d/dx"         { DDX_VOL }
| "D/Dx"         { DDX_BOUNDARY }
| "(L||R)"       { MXDIM }
(* Obsolete grammar parts that once were used for an early scheme
   to specify boundary conditions:
| "[all"        { DOF_BSPEC("all") }
| "[any"        { DOF_BSPEC("all") }
| "[vol"        { DOF_BSPEC("vol") }
| "[bulk"       { DOF_BSPEC("vol") }
| "[boundary"   { DOF_BSPEC("bdy") }
| "[surface"    { DOF_BSPEC("bdy") }
| "[bdy"        { DOF_BSPEC("bdy") }
*)

| "gauge_fix:"  { GAUGEFIX }
| "periodic:"  { PERIODIC }

| '('            { LPAREN }
| ')'            { RPAREN }

| '+'            { SIGN(1.0) }
| '-'            { SIGN(-.1.0) }
| '*'            { STAR }
| '/'            { SLASH }

| '<'            { LANGLE }
| '>'            { RANGLE }
| '|'            { VBAR }
| '='            { EQUALS }
| ','            { COMMA }
| ';'            { SEMICOLON }
| ':'            { COLON }
| ']'            { RBRACKET }

(* New grammar to deal with boundaries: *)

| '['            { LBRACKET }
| '#'            { HASH }

| eof            { EOF }
