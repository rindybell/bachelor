{
  open Parser;;
  open Fundamental;;
    
} 

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z' '_' '?' ':']
let mathf = ['+' '-' '*' '/']
let single = ['^' '+' '-' '*' '/' '>' '<']
let equal = ['=']
let dblq = ['\"']
let n_dblq = [^'\"']

rule token = parse
  | "nil"
      { NIL }
  | "#f"
      { BOOL (false) }
  | "#t"
      { BOOL (true) }
  | alpha(digit|alpha)*
      { VAR(Lexing.lexeme lexbuf) }
  | '-'?digit+
      { INT(int_of_string (Lexing.lexeme lexbuf)) }
  | space+
      { token lexbuf }
  | '('
      { LPAREN }
  | ')'
      { RPAREN }
  | single(equal)?
      { VAR (Lexing.lexeme lexbuf) }
  | equal
      { VAR("=") }
  | dblq(n_dblq)*dblq
      { STRING( twin_dblq_to_single_dblq(Lexing.lexeme lexbuf)) }
  | eof
      { EOF }
