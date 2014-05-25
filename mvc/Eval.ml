(* open module *)
open Type;;
open Parser;;
open Lexer;;
open Environment;;
  

let rec defined_eval a_syntax using_variables =
  let return_true_token a_token =
    match a_token with
	VAR x -> using_variables#first_find_all_groups x
      | BuiltInFunction x -> x using_variables
      | _ -> a_token
  in

  match a_syntax with
      Cons (s_syntax, Nil) ->
	defined_eval s_syntax using_variables
(*    | Cons (Cons (s_syntax, t_syntax), Nil) ->
	defined_eval (Cons(s_syntax, t_syntax)) using_variables *)
    | Cons (_, _) ->
	Functions.main_function a_syntax defined_eval using_variables
    | Atom( s_token ) ->
	return_true_token s_token
    | _ ->
	print_endline "This is expected Pars(_, _)";
	exit(1)
;;

