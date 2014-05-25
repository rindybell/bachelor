(* open external module *)
open Toptree;;
open Parser;;
open Environment;;
open Type;;

let primitive_arithmetic a_function_name b_fun c_environment =
  let e_temp_data = c_environment#first_find_all_groups "temp_x" in
  let f_temp_data = c_environment#first_find_all_groups "temp_y" in
    match (e_temp_data, f_temp_data) with
	(INT g_int, INT h_int) ->
	  INT (b_fun g_int h_int)
      | _ ->
	  Printf.printf "%s need int values" a_function_name;
	  exit(1)
;;

normal_variables#add "+" (Closure (normal_variables, Cons(Cons(Atom(VAR("temp_x")), Cons(Atom(VAR("temp_y")), Nil)), 
						Atom(BuiltInFunction (primitive_arithmetic "+" (+)))), true));;
normal_variables#add "-" (Closure (normal_variables, Cons(Cons(Atom(VAR("temp_x")), Cons(Atom(VAR("temp_y")), Nil)), 
						Atom(BuiltInFunction (primitive_arithmetic "-" (-)))), true));;
normal_variables#add "*" (Closure (normal_variables, Cons(Cons(Atom(VAR("temp_x")), Cons(Atom(VAR("temp_y")), Nil)), 
						Atom(BuiltInFunction (primitive_arithmetic "*" ( * )))), true));;
normal_variables#add "/" (Closure (normal_variables, Cons(Cons(Atom(VAR("temp_x")), Cons(Atom(VAR("temp_y")), Nil)), 
						Atom(BuiltInFunction (primitive_arithmetic "/" (/)))), true));;
			  
normal_variables#add "komai" (INT 21);;
