(* open *)
open Type;;
open Parser;;

let rec debug_print a_token =
  match a_token with
      VAR x -> Printf.printf "VAR (%s)\n" x
    | INT x -> Printf.printf "INT %d\n" x
    | STRING x -> Printf.printf "STRING %s\n" x
    | BOOL _ -> print_endline "BOOL"
    | Closure _ -> print_endline "Closure"
    | SubClosure _ -> print_endline "SubClosure"
    | MultiValue (x,y) -> 
	Printf.printf "MultiValue (";
	debug_print x;
	print_endline ")"	  
    | LIST _ -> print_endline "LIST"
    | NIL -> print_endline "NIL"
    | EOF -> print_endline "EOF"
    | LPAREN -> print_endline "LPAREN"
    | RPAREN -> print_endline "RPAREN"
    | NONE -> print_endline "NONE"
    | BuiltInFunction(_) -> print_endline "BuiltInFunction"
    | CamoufClosure (x,y) -> 
	print_string "CamoufClosure(";
	debug_print x;
	debug_print y
;;

let rec debug_print_syntax a_syntax =
  match a_syntax with
      Atom b_token ->
	print_string "Atom ";
	(match b_token with
	     LIST c_syntax ->
	       debug_print b_token;
	       debug_print_syntax c_syntax
	   | _ ->
	       debug_print b_token)
    | Cons (b_syntax, c_syntax) ->
	print_string "Cons ";
	debug_print_syntax b_syntax;
	debug_print_syntax c_syntax
    | Nil ->
	print_endline "Nil"
;;

let rec debug_print_syntax_list a_syntax =
  match a_syntax with
      Atom b_token ->
	(match b_token with
	     LIST c_syntax ->
	      debug_print_syntax_list c_syntax
	   | _ -> debug_print b_token)
    | Cons (b_syntax, c_syntax) ->
	debug_print_syntax_list b_syntax;
	debug_print_syntax_list c_syntax
    | Nil ->
	print_endline "Nil"
;;
