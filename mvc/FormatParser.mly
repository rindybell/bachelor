%{
  open Type
%}

%token FormatInt //数値
%token FormatString //文字列
%token FormatLinefeed // 改行
%token <string> FormatPrint //ノーマルの文字列
%token NULLCHAR // ヌル文字

%token EOF
%token END

%type <token list> formatStatement
%start formatStatement

%%

in_atoms:
| FormatString
    { FormatString }
| FormatInt
    { FormatInt }
| FormatLinefeed
    { FormatLinefeed }
| FormatPrint
    { FormatPrint ($1) }

formatStatement:
| in_atoms formatStatement
    { $1 :: $2 }
| EOF
   { [] }
| END
    { [] }
| NULLCHAR
    { [] }
| error
 	{ failwith
	    (Printf.sprintf
	       "parse error near characters %d-%d"
	       (Parsing.symbol_start ())
	       (Parsing.symbol_end ())) }
	
