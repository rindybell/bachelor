%{
  open Type;;
  open Environment
%}

// 構文解析、字句解析及び実行時両方に使われる識別子
%token <string> VAR /* 変数 */
%token <int> INT /* 数値 */
%token <string> STRING /* 文字列 */

// 構文解析、字句解析でのみ使われる識別子  
%token LPAREN /* 左括弧 */
%token RPAREN /* 右括弧 */
%token NIL /* リストの最後 */
%token EOF /* End Of File */
  
// 実行時でのみ使われる識別子
%token <bool> BOOL /* ブール値を持つ */
%token <token Environment.env_tree * token Type.statement * bool> Closure /* 関数クロージャ*/
%token <token Environment.env_tree * token Type.statement * bool> SubClosure /* 変数束縛中の関数クロージャ (なくてもよさそう) */
%token <token * token> CamoufClosure // 偽装クロージャ
// %token <token Type.statement * bool> BuiltInClosure // 組み込み関数(無限引数か否か)
%token <(token Environment.env_tree -> token)> BuiltInFunction //組み込み関数
// %token <token * token Type.statement * token Environment.env_tree> MultiValue /* 多値計算の際に用いる */
%token <token * token Type.contiflow> MultiValue // 多値計算に用いる
  
%token <token Type.statement> LIST /* token型のリスト */
//%token <token> FINISH /* 関数の戻り値・多値計算の際に用いる */
//%token <token> RETURN /* 同上 */
%token NONE /* 戻り値が存在しないときに使う識別子 */

// 構文解析でのみ使われる識別子
%type <token Type.statement list> statement_list

%start statement_list

%%

in_atoms:
| VAR
    { Atom(VAR($1)) }
| INT
    { Atom(INT($1)) }
| STRING
    { Atom(STRING($1)) }
| BOOL
    { Atom(BOOL($1)) }
| NIL
    { Nil } 


statement:
| LPAREN statement RPAREN statement
    { Cons ($2, $4) }
| LPAREN statement RPAREN
    { Cons ($2, Nil) }
| in_atoms statement
    { Cons ($1, $2) }
| in_atoms
    { Cons ($1, Nil) }
| LPAREN RPAREN
    { Nil }
| error
	{ failwith
	    (Printf.sprintf
	       "parse error near characters %d-%d"
	       (Parsing.symbol_start ())
	       (Parsing.symbol_end ())) }

statement_list:
| LPAREN statement RPAREN statement_list
    { $2 :: $4 }
| LPAREN RPAREN statement_list
    { Nil :: $3 }
| EOF
    { [] }
