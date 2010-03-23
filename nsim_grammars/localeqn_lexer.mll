{
open Localeqn_parser   (* The type 'token' is defined there *)
}

  rule token = parse
  [' ' '\t' '\n' '\r']     { token lexbuf }     (* skip whitespace *)
| '-'? ['0'-'9']+ as lxm { INT(int_of_string lxm) }
| '-'? ['0'-'9']* '.' ['0'-'9']+ (['e' 'E'] '-'? ['0'-'9']+)? as lxm { FLOAT(float_of_string lxm) }
| '-'? ['0'-'9']+ ['e' 'E'] '-'? ['0'-'9']+ as lxm { FLOAT(float_of_string lxm) }
| ['a'-'z' 'A'-'Z' '0'-'9' '_']+ as lxm {STRING(lxm)} (* Names MAY start with underscores! *)
| "<-"         { ASSIGN }
| "%local"         { LOCAL }
| "%range"         { RANGE }
| '('            { LPAREN }
| ')'            { RPAREN }
| '['            { LBRACKET }
| ']'            { RBRACKET }
| '+'            { SIGN(1.0) }
| '-'            { SIGN(-.1.0) }
| '*'            { STAR }
| '/'            { SLASH }
| ':'            { COLON }
| ','            { COMMA }
| ';'            { SEMICOLON }
| eof            { EOF }
