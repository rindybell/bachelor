(* open module *)
open Parser;;
open Lexer;;
open Type;;

let convert_int_to_token a_int = 
  INT a_int
;;

let convert_token_to_int a_token =
  match a_token with
      INT x -> x
    | _ ->
	print_endline "This expression must be INT token.(Converter.ml:convert_token_to_int)";
	exit(1)
;;

let convert_token_to_bool a_token =
    match a_token with
	BOOL x -> x 
      | _ -> 
	  print_endline "convert_token_to_bool expected BOOL _.(Converter.ml.convert_token_to_bool)";
	  exit(1)
;;

let convert_single_to_multi a_token b_contiflow =
  if (b_contiflow = None) then
    a_token
  else
    match a_token with
        MultiValue(c_token, None) ->
	  MultiValue(c_token, b_contiflow)
      | MultiValue(c_token, d_contiflow) ->
	  MultiValue(c_token, Pairs(d_contiflow, b_contiflow))
      | c_token ->
	  MultiValue(c_token, b_contiflow)
;;

let convert_single_to_multi_2 a_token b_contiflow =
  match a_token with
      MultiValue(c_token, None) ->
	MultiValue(c_token, b_contiflow)
    | MultiValue(c_token, d_contiflow) ->
	MultiValue(c_token, Pairs(d_contiflow, b_contiflow))
    | c_token ->
	MultiValue(c_token, b_contiflow)
;;
