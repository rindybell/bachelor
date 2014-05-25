(* open module *)
open Type;;
open Parser;;

(* open public mudule *)
open Sys;;
  

let module_list =
  [
  ]
;;

let rec eval_apply_original a_parser_list =
  match a_parser_list with
      [] ->
	(ignore 1)
    | s :: s_parser_list ->
	ignore (Eval.defined_eval s Toptree.normal_variables );
	eval_apply_original s_parser_list
;;

let rec eval_module a_module_list =
  let open_module l_folder_name m_filename =
    let s_temp_filename = l_folder_name ^ m_filename in
    let t_input_stream = open_in s_temp_filename in
    let u_parser_list = Parser.statement_list Lexer.token (Lexing.from_channel t_input_stream) in
      eval_apply_original u_parser_list
  in
  let rec eval_module_sub l_filename_list m_fun =
    match l_filename_list with
	[] -> (ignore 1)
      | s_filename :: t_filename_list ->
	  m_fun s_filename;
	  eval_module_sub t_filename_list m_fun
  in
  let apply_eval_module_sub l_filename_list =
    match l_filename_list with
	[] -> ignore 1
      | s_folder_name :: t_filename_list -> eval_module_sub t_filename_list (open_module s_folder_name)
  in
    match a_module_list with
	[] -> ignore 1
      |	s_filename_list :: t_module_list ->
	  apply_eval_module_sub s_filename_list;
	  eval_module t_module_list
;;

eval_module module_list;;

let filename =argv.(1);;
let ic = open_in filename;;

(* 構文解析した結果 *)
let parser_list = Parser.statement_list Lexer.token (Lexing.from_channel ic);;

(*Eval.defined_eval parser_list;;*)

let rec eval_apply a_parser_list () =
  match a_parser_list with
      [] -> 
	print_endline "";
	print_endline "------------------------------------------------------";
	print_endline "Run has ended."
    | s :: s_parser_list ->
	let topValue = Eval.defined_eval s Toptree.normal_variables in
	  (match topValue with
	       MultiValue (_, _) ->
		 let temp_data = Cons(Atom(VAR("single")), Cons(Atom(topValue), Nil)) in
		   (ignore (Eval.defined_eval temp_data Toptree.normal_variables));
		   eval_apply s_parser_list ()
	     | _ ->
		 (*Debug.debug_print (Eval.defined_eval s Toptree.normal_variables);*)
		 eval_apply s_parser_list ())
;;

let main () =
  eval_apply parser_list ();;

main();;
