{
  open FormatParser;;
  open Fundamental;;
} 

let linefeed = ['\n']
let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z' '_']
let n_dblq = [^'\"']
let childa = ['~']
let nullchar = ['\000']
let notf = ['0'-'9' 'A'-'Z' 'a'-'z' '_' ' ' '\t' '\n' '\r' '+' '-' '*' '/' '^' '+' '-' '*' '/' '>' '<' '=' ':' '(' ')' '.' ',']
rule token = parse
  | "~%"
      { FormatLinefeed }
  | "~D"
      { FormatInt }
  | "~A"
      { FormatString }
  | nullchar
      { NULLCHAR }
  | notf*
      { FormatPrint ( Lexing.lexeme lexbuf) }
  | eof
      { EOF }
