{
open Dimensions_parser   (* The type 'token' is defined there *)
}

  rule token = parse
  [' ' '\t' '\n' '\r']     { token lexbuf }     (* skip whitespace *)
| '-'? ['0'-'9']+ as lxm { INT(int_of_string lxm) }
| ['a'-'z' 'A'-'Z' '0'-'9' '_']+ as lxm {STRING(lxm)} (* Names MAY start with underscores! *)
| "/"         { SLASH }
| "^"         { HAT }
| '('            { LPAREN }
| ')'            { RPAREN }
| eof            { EOF }
