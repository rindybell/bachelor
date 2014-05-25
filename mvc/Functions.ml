(* open public module *)
open Type;;
open Parser;;
open Lexer;;

let error_message a_function_name =
  Printf.printf "There was an error by %s.\n" a_function_name
;;

let error_message_2 a_primitive_name b_function_name =
  Printf.printf "There was an error by %s(%s).\n" b_function_name a_primitive_name
;;

let specified_number_of_error_message a_function_name b_number =
  Printf.printf "The argument of %s is unusual.(The argument must be %d)\n" a_function_name b_number
;;

(* 列の評価 *)
let eval_consecutive_list origin_defined_eval origin_environment origin_syntax =
  let rec eval_consecutive_list_sub l_syntax =
    match l_syntax with
	Cons (m_syntax, Nil) ->
	  let n_temp_data = origin_defined_eval m_syntax origin_environment in
	    n_temp_data
      | Cons (m_syntax, n_syntax) ->
	  let o_temp_data = origin_defined_eval m_syntax origin_environment in
	(match o_temp_data with
	     MultiValue (p_token, None) ->
	       MultiValue (p_token, Pair (n_syntax, origin_environment))
	   | MultiValue (p_token, q_contiflow) ->
	       MultiValue (p_token, Pairs(q_contiflow, Pair (n_syntax, origin_environment)))
	   | _ ->
	       eval_consecutive_list_sub n_syntax)
      | Nil ->
	  NONE
      | Atom (_) ->
      let m_temp_data = origin_defined_eval l_syntax origin_environment in
	m_temp_data
  in
    eval_consecutive_list_sub origin_syntax
;;

let function_list_reverse a_function_name b_syntax =
  let rec function_list_reverse_sub l_syntax m_syntax =
    match l_syntax with 
	Nil -> m_syntax
      | Cons (n_syntax, o_syntax) ->
	  let p_temp_data = Cons (n_syntax, m_syntax) in
	    function_list_reverse_sub o_syntax p_temp_data
      | _ ->
	  error_message "function_list_reverse";
	  exit(1)
  in
    match b_syntax with
	Nil -> Nil
      | Cons (c_syntax, Nil) -> b_syntax
      | Cons (c_syntax, d_syntax) ->
	  let e_temp_data = Cons(c_syntax, Nil) in
	    function_list_reverse_sub d_syntax e_temp_data
      | _ ->
	  error_message a_function_name;
	  exit(1)
;;


let main_function origin_syntax origin_defined_eval origin_environment =
  (* 関数呼び出しのため *)
  let rec function_call_partial a_function_name b_environment c_fun_syntax if_built_in_function d_syntax =
    let rec function_call_sub_any m_fun_syntax n_syntax =
      match (m_fun_syntax, n_syntax) with
	  (Cons (Nil, o_syntax), Nil) -> (* 評価スタート *)
	    eval_consecutive_list origin_defined_eval b_environment o_syntax
	| (Cons (o_syntax, p_syntax), Cons(Atom(q_token), Nil)) ->
	    let r_temp_data = SubClosure (b_environment, m_fun_syntax, if_built_in_function) in
	      MultiValue(CamoufClosure(r_temp_data, q_token), None)
	| (Cons (Cons(Atom(VAR(o_string)), Cons(Atom(VAR(p_string)), Nil)), q_syntax), Cons(r_syntax, Cons(s_syntax, Nil))) ->
	    let u_temp_data = origin_defined_eval r_syntax origin_environment in
	    let v_temp_data = origin_defined_eval s_syntax origin_environment in
	      (match (u_temp_data, v_temp_data) with
		  (MultiValue(w_token, None), MultiValue(x_token, None)) ->
		    b_environment#add o_string w_token;
		    b_environment#add p_string x_token;
		    let y_temp_data = eval_consecutive_list origin_defined_eval b_environment q_syntax in
		      Converter.convert_single_to_multi_2 y_temp_data None
		| (MultiValue(w_token, x_contiflow), MultiValue(y_token, None)) ->
		    b_environment#add o_string w_token;
		    b_environment#add p_string y_token;
		    let z_temp_data = eval_consecutive_list origin_defined_eval b_environment q_syntax in
		    let aa_temp_data = SubClosure(b_environment, c_fun_syntax, if_built_in_function) in
		    let ab_temp_data = MultiValue(z_temp_data, PairArg(AppPair(aa_temp_data, x_contiflow), Cons(Atom(y_token), Nil))) in
		      ab_temp_data
		| (MultiValue(w_token, None), MultiValue(x_token, y_contiflow)) ->
		    b_environment#add o_string w_token;
		    b_environment#add p_string x_token;
		    let z_temp_data = eval_consecutive_list origin_defined_eval b_environment q_syntax in
		    let aa_temp_data = SubClosure(b_environment, c_fun_syntax, if_built_in_function) in
		    let ab_temp_data = MultiValue(z_temp_data, PairArg(AppPair(aa_temp_data, y_contiflow), Cons(Atom(w_token), Nil))) in
		      ab_temp_data
		| (MultiValue(w_token, x_contiflow), MultiValue(y_token, z_contiflow)) ->
		    b_environment#add o_string w_token;
		    b_environment#add p_string y_token;
		    let aa_temp_data = eval_consecutive_list origin_defined_eval b_environment q_syntax in
		    let ab_temp_data = SubClosure(b_environment, c_fun_syntax, if_built_in_function) in
		    let ac_temp_data = MultiValue(aa_temp_data, (Pairs(PairArg(AppPair(ab_temp_data, z_contiflow), Cons(Atom(w_token), Nil)), PairArg(AppPair(ab_temp_data, x_contiflow), Cons(Atom(v_temp_data), Nil))))) in
		      ac_temp_data
		| (MultiValue(w_token, None), x_token) ->
		    b_environment#add o_string w_token;
		    b_environment#add p_string x_token;
 		    let y_temp_data = eval_consecutive_list origin_defined_eval b_environment q_syntax in
		      Converter.convert_single_to_multi_2 y_temp_data None
		| (w_token, MultiValue(x_token, None)) ->
		    b_environment#add o_string w_token;
		    b_environment#add p_string x_token;
 		    let y_temp_data = eval_consecutive_list origin_defined_eval b_environment q_syntax in
		      Converter.convert_single_to_multi_2 y_temp_data None
		| (MultiValue(w_token, x_contiflow), y_token) ->
		    b_environment#add o_string w_token;
		    b_environment#add p_string y_token;
		    let z_temp_data = eval_consecutive_list origin_defined_eval b_environment q_syntax in
		    let aa_temp_data = SubClosure(b_environment, c_fun_syntax, if_built_in_function) in
		    let ab_temp_data = MultiValue(z_temp_data, PairArg(AppPair(aa_temp_data, x_contiflow), Cons(Atom(y_token), Nil))) in
		      ab_temp_data
		| (w_token, MultiValue(x_token, y_contiflow)) ->
		    b_environment#add o_string w_token;
		    b_environment#add p_string x_token;
		    let z_temp_data = eval_consecutive_list origin_defined_eval b_environment q_syntax in
		    let aa_temp_data = SubClosure(b_environment, c_fun_syntax, if_built_in_function) in
		    let ab_temp_data = MultiValue(z_temp_data, PairArg(AppPair(aa_temp_data, y_contiflow), Cons(Atom(w_token), Nil))) in
		      ab_temp_data
		| (w_token, x_token) ->
		    b_environment#add o_string w_token;
		    b_environment#add p_string x_token;
 		    let y_temp_data = eval_consecutive_list origin_defined_eval b_environment q_syntax in
		    y_temp_data)


	| (Cons (Cons(Atom(VAR(o_string)), Cons(Atom(VAR(p_string)), Nil)), q_syntax), Cons(r_syntax, Cons(s_syntax, t_syntax))) ->
	    let u_temp_data = origin_defined_eval r_syntax origin_environment in
	    let v_temp_data = origin_defined_eval s_syntax origin_environment in
	      (match (u_temp_data, v_temp_data) with
		  (MultiValue(w_token, None), MultiValue(x_token, None)) ->
		    b_environment#add o_string w_token;
		    b_environment#add p_string x_token;
		    let y_temp_data = eval_consecutive_list origin_defined_eval b_environment q_syntax in
		      function_call_sub_any c_fun_syntax (Cons(Atom(y_temp_data), t_syntax))
		| (MultiValue(w_token, x_contiflow), MultiValue(y_token, None)) ->
		    b_environment#add o_string w_token;
		    b_environment#add p_string y_token;
		    let z_temp_data = eval_consecutive_list origin_defined_eval b_environment q_syntax in
		    let aa_temp_data = SubClosure(b_environment, c_fun_syntax, if_built_in_function) in
		    let ab_temp_data = MultiValue(z_temp_data, PairArg(AppPair(aa_temp_data, x_contiflow), Cons(Atom(y_token), Nil))) in
		      function_call_sub_any c_fun_syntax (Cons(Atom(ab_temp_data), t_syntax))
		| (MultiValue(w_token, None), MultiValue(x_token, y_contiflow)) ->
		    b_environment#add o_string w_token;
		    b_environment#add p_string x_token;
		    let z_temp_data = eval_consecutive_list origin_defined_eval b_environment q_syntax in
		    let aa_temp_data = SubClosure(b_environment, c_fun_syntax, if_built_in_function) in
		    let ab_temp_data = MultiValue(z_temp_data, PairArg(AppPair(aa_temp_data, y_contiflow), Cons(Atom(w_token), Nil))) in
		      function_call_sub_any c_fun_syntax (Cons(Atom(ab_temp_data), t_syntax))
		| (MultiValue(w_token, x_contiflow), MultiValue(y_token, z_contiflow)) ->
		    b_environment#add o_string w_token;
		    b_environment#add p_string y_token;
		    let aa_temp_data = eval_consecutive_list origin_defined_eval b_environment q_syntax in
		    let ab_temp_data = SubClosure(b_environment, c_fun_syntax, if_built_in_function) in
		    let ac_temp_data = MultiValue(aa_temp_data, (Pairs(PairArg(AppPair(ab_temp_data, z_contiflow), Cons(Atom(w_token), Nil)), PairArg(AppPair(ab_temp_data, x_contiflow), Cons(Atom(v_temp_data), Nil))))) in
		      function_call_sub_any c_fun_syntax (Cons(Atom(ac_temp_data), t_syntax))
		| (MultiValue(w_token, None), x_token) ->
		    b_environment#add o_string w_token;
		    b_environment#add p_string x_token;
 		    let y_temp_data = eval_consecutive_list origin_defined_eval b_environment q_syntax in
		      function_call_sub_any c_fun_syntax (Cons(Atom(y_temp_data), t_syntax))
		| (w_token, MultiValue(x_token, None)) ->
		    b_environment#add o_string w_token;
		    b_environment#add p_string x_token;
 		    let y_temp_data = eval_consecutive_list origin_defined_eval b_environment q_syntax in
		      function_call_sub_any c_fun_syntax (Cons(Atom(y_temp_data), t_syntax))
		| (MultiValue(w_token, x_contiflow), y_token) ->
		    b_environment#add o_string w_token;
		    b_environment#add p_string y_token;
		    let z_temp_data = eval_consecutive_list origin_defined_eval b_environment q_syntax in
		    let aa_temp_data = SubClosure(b_environment, c_fun_syntax, if_built_in_function) in
		    let ab_temp_data = MultiValue(z_temp_data, PairArg(AppPair(aa_temp_data, x_contiflow), Cons(Atom(y_token), Nil))) in
		      function_call_sub_any c_fun_syntax (Cons(Atom(ab_temp_data), t_syntax))
		| (w_token, MultiValue(x_token, y_contiflow)) ->
		    b_environment#add o_string w_token;
		    b_environment#add p_string x_token;
		    let z_temp_data = eval_consecutive_list origin_defined_eval b_environment q_syntax in
		    let aa_temp_data = SubClosure(b_environment, c_fun_syntax, if_built_in_function) in
		    let ab_temp_data = MultiValue(z_temp_data, PairArg(AppPair(aa_temp_data, y_contiflow), Cons(Atom(w_token), Nil))) in
		      function_call_sub_any c_fun_syntax (Cons(Atom(ab_temp_data), t_syntax))
		| (w_token, x_token) ->
		    b_environment#add o_string w_token;
		    b_environment#add p_string x_token;
 		    let y_temp_data = eval_consecutive_list origin_defined_eval b_environment q_syntax in
		      function_call_sub_any c_fun_syntax (Cons(Atom(y_temp_data), t_syntax)))

	| _ ->
	    Debug.debug_print_syntax m_fun_syntax;
	    Debug.debug_print_syntax n_syntax;
	    error_message_2 "function_call_sub_any" a_function_name;
	    exit(1)
    in

    let rec function_call_sub m_fun_syntax n_syntax =
      match (m_fun_syntax, n_syntax) with
	  (Cons (Nil, o_syntax), Nil) -> (* 評価スタート *)
	    eval_consecutive_list origin_defined_eval b_environment o_syntax
	| (Cons (Cons(Atom(VAR(o_string)), p_syntax), q_syntax), Cons(r_syntax, s_syntax)) ->
	    let t_temp_data = origin_defined_eval r_syntax origin_environment in
	      (match t_temp_data with
		   MultiValue(u_token, None) ->		     
		     b_environment#add o_string u_token;
		     let v_temp_syntax = Cons(p_syntax, q_syntax) in
		     let w_temp_data = function_call_sub v_temp_syntax s_syntax in		       
		       (match w_temp_data with
			    MultiValue(_, _) ->
			      w_temp_data
			  | _ ->
			      MultiValue(w_temp_data, None))
		 | MultiValue(u_token, v_contiflow) ->     
		     b_environment#add o_string u_token;
		     let w_temp_syntax = Cons(p_syntax, q_syntax) in
		     let x_temp_data = function_call_sub w_temp_syntax s_syntax in (* x_temp_data は通常の評価結果*)
		       (match x_temp_data with
			    MultiValue(y_token, None) ->
			      MultiValue(y_token, PairArg(AppPair(SubClosure(b_environment, m_fun_syntax, if_built_in_function), v_contiflow), s_syntax))
			  | MultiValue(y_token, x_contiflow) ->
			      MultiValue(y_token, Pairs(x_contiflow, PairArg(AppPair(SubClosure(b_environment, m_fun_syntax, if_built_in_function), v_contiflow), s_syntax)))
			  | _ ->
			      MultiValue(x_temp_data, PairArg(AppPair(SubClosure(b_environment, m_fun_syntax, if_built_in_function), v_contiflow), s_syntax)))
		 | _ -> 
		     let u_temp_syntax = Cons(p_syntax, q_syntax) in
		       b_environment#add o_string t_temp_data;
		       function_call_sub u_temp_syntax s_syntax)
	| (Cons (_, _), Nil) ->
	    SubClosure(b_environment, m_fun_syntax, if_built_in_function)
	| _ ->
	    error_message_2 "function_call_sub" a_function_name;
	    exit(1)
    in
      if if_built_in_function then
	function_call_sub_any c_fun_syntax d_syntax
      else
	function_call_sub c_fun_syntax d_syntax
  in      

  let function_call_from_closure a_function_name b_closure c_syntax =
    match b_closure with
	Closure (d_environment, e_syntax, if_any_arguments) ->
	  let f_temp_environment = new Environment.env_tree in
	    f_temp_environment#set_func_env (Environment.Node d_environment);
	    f_temp_environment#set_parent_env (Environment.Node origin_environment);
	    function_call_partial a_function_name f_temp_environment e_syntax if_any_arguments c_syntax
      | SubClosure (d_environment, e_syntax, if_any_arguments) ->
	  let f_temp_environment = new Environment.env_tree in
	    f_temp_environment#set_parent_env (Environment.Node d_environment);
	    function_call_partial a_function_name f_temp_environment e_syntax if_any_arguments c_syntax
      | _ ->
	  Printf.printf "%s function has error.\n" a_function_name;
	  exit(1)
  in

  let function_call_from_name a_function_name b_syntax =
    let c_temp_data = origin_defined_eval (Atom(VAR(a_function_name))) origin_environment in
      match c_temp_data with
	  Closure (_, _, _) | SubClosure(_, _, _) ->
	      function_call_from_closure a_function_name c_temp_data b_syntax
	| _ ->
	    Printf.printf "function_call_from_name(%s) has error.\n" a_function_name;
	    exit(1)
  in    
    
  (* 続きの計算をする関数 *)
  let rec eval_contiflow a_function_name b_conti = 
    match b_conti with
	Pair (c_syntax, d_environment) ->
	  (* let e_temp_data = origin_defined_eval c_syntax d_environment in *)
	  let e_temp_data = eval_consecutive_list origin_defined_eval d_environment c_syntax in
	    e_temp_data
      | Pairs (c_contiflow, d_contiflow) ->
	  let e_temp_data = eval_contiflow a_function_name c_contiflow in
	    (match e_temp_data with
		 MultiValue(f_token, None) ->
		   MultiValue (f_token, d_contiflow)
	       | MultiValue(f_token, g_contiflow) ->
		   MultiValue(f_token, (Pairs(g_contiflow, d_contiflow)))
	       | _ ->
		   eval_contiflow a_function_name d_contiflow)
      | AppPair (c_token, d_contiflow) ->	  
	  (match c_token with
	       Closure(e_environment, f_syntax, _) | SubClosure(e_environment, f_syntax, _) ->
		 let g_temp_data = eval_contiflow a_function_name d_contiflow in
		 let h_temp_data = Cons(Atom(c_token),Cons(Atom(g_temp_data), Nil)) in
		 let i_temp_data = origin_defined_eval h_temp_data origin_environment in
		   
		   (match i_temp_data with
			MultiValue(j_token, None) ->
			  i_temp_data
			    (* MultiValue(j_token, PairArg(d_contiflow, Atom(g_temp_data))) *)
		      | MultiValue(j_token, k_contiflow) ->
			  i_temp_data
			    (* MultiValue(j_token, Pairs(k_contiflow, PairArg(d_contiflow, Atom(g_temp_data)))) *)
		      | _ ->
			  Printf.printf "eval_contiflow (%s : SubAppPair) is error.\n" a_function_name;
			  exit(1))
	     | _ ->
		 Printf.printf "eval_contiflow (%s : AppPair) is error.\n" a_function_name;
		 exit(1))
      | PairArg (c_contiflow, d_syntax) ->
	  let e_temp_data = eval_contiflow a_function_name c_contiflow in
	    (match d_syntax with
		 Nil -> e_temp_data
	       | _ ->
	    (* Debug.debug_print e_temp_data; *)
	    (match e_temp_data with
		 Closure (_,_,_) | SubClosure(_,_,_) ->
		   let f_temp_data = Cons(Atom(e_temp_data), d_syntax) in
		     origin_defined_eval f_temp_data origin_environment
	       | CamoufClosure (f_closure, j_token) ->
		   let k_temp_data = Cons(Atom(f_closure), Cons(Atom(j_token), d_syntax)) in
		     origin_defined_eval k_temp_data origin_environment
	       | MultiValue(Closure(f_environment, g_syntax, if_any_arguments), None) ->
		   let i_temp_data = Cons(Atom(Closure(f_environment, g_syntax, if_any_arguments)), d_syntax) in
		   let j_temp_data = origin_defined_eval i_temp_data origin_environment in
		     j_temp_data
	       | MultiValue(SubClosure(f_environment, g_syntax, if_any_arguments), None) ->
		   let i_temp_data = Cons(Atom(SubClosure(f_environment, g_syntax, if_any_arguments)), d_syntax) in
		   let j_temp_data = origin_defined_eval i_temp_data origin_environment in
		   j_temp_data
	       | MultiValue(CamoufClosure (f_closure, j_token), None) ->
		   let k_temp_data = Cons(Atom(f_closure), Cons(Atom(j_token), d_syntax)) in
		   let l_temp_data = origin_defined_eval k_temp_data origin_environment in
		     l_temp_data
	       | MultiValue(CamoufClosure (f_closure, j_token), k_contiflow) ->
		   let l_temp_data = Cons(Atom(f_closure), Cons(Atom(j_token), d_syntax)) in
		   let m_temp_data = origin_defined_eval l_temp_data origin_environment in
		   let n_temp_data = PairArg (k_contiflow, d_syntax) in
		   let o_temp_data = Converter.convert_single_to_multi m_temp_data n_temp_data in
		     o_temp_data
	       | MultiValue(Closure(f_environment, g_syntax, if_any_arguments), h_contiflow) ->
		   let i_temp_data = Cons(Atom(Closure(f_environment, g_syntax, if_any_arguments)), d_syntax) in
		   let j_temp_data = origin_defined_eval i_temp_data origin_environment in
		   let k_temp_data = PairArg (h_contiflow, d_syntax) in
		   let l_temp_data = Converter.convert_single_to_multi j_temp_data k_temp_data in
		     l_temp_data
	       | MultiValue(SubClosure(f_environment, g_syntax, if_any_arguments), h_contiflow) ->
		   let i_temp_data = Cons(Atom(SubClosure(f_environment, g_syntax, if_any_arguments)), d_syntax) in
		   let j_temp_data = origin_defined_eval i_temp_data origin_environment in
		   let k_temp_data = PairArg (h_contiflow, d_syntax) in
		   let l_temp_data = Converter.convert_single_to_multi j_temp_data k_temp_data in
		     l_temp_data
	       | _ ->
		   Debug.debug_print e_temp_data;
		   Printf.printf "eval_contiflow (%s : PairArg) is error.\n" a_function_name;
		   exit(1)))
      | _ ->
	  print_endline "eval_contiflow is error.";
	  exit(1)
    in    

  (*
    以下のfunction_fold関数にて、最初に仮引数にb_functionを書いてしまい、
    消しはしたものの、アルファベットの整合性を整えるのが面倒なのでこのままにしてある。
  *)
  let function_fold a_function_name b_bool c_syntax =
    let rec fold_right l_syntax m_token n_token =
      match n_token with
	  MultiValue(o_token, None) ->
	    let p_temp_data = Cons(l_syntax, Cons(Atom(o_token), Cons(Atom(m_token), Nil))) in
	      origin_defined_eval p_temp_data origin_environment 
	| MultiValue(o_token, p_conti) ->
	    let q_temp_data = eval_contiflow a_function_name p_conti in
	    let r_temp_data = fold_right l_syntax m_token q_temp_data in
	    let s_temp_data = Cons(l_syntax, Cons(Atom(o_token), Cons(Atom(r_temp_data), Nil))) in
	      origin_defined_eval s_temp_data origin_environment
	| _ ->
	    print_endline "fold_right has error.";
	    exit(1)
    in
    let rec fold_left l_syntax m_token n_token =
      match n_token with
	  MultiValue(o_token, None) ->
	    let p_temp_data = Cons(l_syntax, Cons(Atom(m_token), Cons(Atom(o_token), Nil))) in
	      origin_defined_eval p_temp_data origin_environment
	| MultiValue(o_token, p_conti) ->
	    let p_temp_data = Cons(l_syntax, Cons(Atom(m_token), Cons(Atom(o_token), Nil))) in
	    let q_temp_data = origin_defined_eval p_temp_data origin_environment in
	    let s_temp_data = eval_contiflow a_function_name p_conti in
	      fold_left l_syntax q_temp_data s_temp_data
	| _ ->
	    print_endline "fold_left has error.";
	    exit(1)
    in
      match c_syntax with
	  Cons(d_syntax, Cons(e_syntax, f_syntax)) ->
	    let g_temp_data = origin_defined_eval e_syntax origin_environment in
	  let h_temp_data = origin_defined_eval f_syntax origin_environment in
	    (match h_temp_data with
		 MultiValue(_,_) ->
		   if b_bool then
		     fold_right d_syntax g_temp_data h_temp_data
		   else
		     fold_left d_syntax g_temp_data h_temp_data 
	       | _ ->
		   print_endline "function_fold is error";
		   exit(1))
	| _ ->
	    print_endline "syntax error.(function_fold)";
	    exit(1)
  in

  let eval_multivalue a_function_name b_function c_token =
    let rec eval_multivalue_sub l_token =
	match l_token with
	    MultiValue (m_token, None) ->
	      b_function m_token
	  | MultiValue (m_token, n_contiflow) ->
	      b_function m_token;
	      let o_temp_data = eval_contiflow a_function_name n_contiflow in
		eval_multivalue_sub o_temp_data
	  | _ ->
	      b_function l_token
    in
      match c_token with
	  MultiValue (d_token, None) ->
	    b_function d_token
	| MultiValue (d_token, e_contiflow) ->
	    b_function d_token;
	    let f_temp_data = eval_contiflow a_function_name e_contiflow in
	      eval_multivalue_sub f_temp_data
	| _ ->
	    error_message_2 "eval_multivalue" a_function_name;
	    exit(1)
  in
    
  (* list operation *)
    (* リスト操作系 *)
  let function_list a_syntax =
    LIST a_syntax
  in
  
  let function_list_car a_syntax =
    let function_list_car_sub l_syntax =
      match l_syntax with
	  Cons (Atom (m_token), n_syntax) ->
	    m_token
	| Nil -> LIST( Nil )
	| _ -> 
	    error_message "function_list_car";
	    exit(1)
    in
      match a_syntax with
	  Cons (Atom(LIST(b_syntax)), Nil) ->
	    function_list_car_sub b_syntax
	| _ ->
	    error_message "function_list_car";
	    exit(1)
  in
   
  let function_list_cdr a_syntax =
    let function_list_cdr_sub l_syntax =
      match l_syntax with
	  Cons (m_syntax, n_syntax) ->
	    LIST n_syntax
	| Nil -> LIST( Nil )
	| _ -> 
	    error_message "function_list_cdr";
	    exit(1)
    in
    match a_syntax with
	Cons (Atom (LIST(b_syntax)), Nil) ->
	  function_list_cdr_sub b_syntax
      | _ ->
	  error_message "function_list_cdr";
	  exit(1)
  in          

  let function_reverse a_function_name b_syntax =
    let function_reverse_sub l_token =
      match l_token with
	  LIST m_token -> 
	    let n_temp_data = function_list_reverse "function_list_reverse" m_token in
	      LIST n_temp_data
	| _ ->
	    error_message "function_reverse";
	    exit(1)
    in
      match b_syntax with
	  Cons (c_syntax, Nil) ->
	    let d_temp_data = origin_defined_eval c_syntax origin_environment in
	      function_reverse_sub d_temp_data
	| _ -> 
	    error_message a_function_name;
	    exit(1)
  in

  (* Independence *)
  let function_define l_function_name m_syntax =
    match m_syntax with
	Cons (Atom (VAR(n_string)), o_syntax) ->
	  let p_temp_data = origin_defined_eval o_syntax origin_environment in
	    origin_environment#add n_string p_temp_data;
	    NONE
      | Cons (Cons (Atom(VAR(n_string)), o_syntax), p_syntax) -> (* 関数 *)
	  let q_temp_data = Closure (origin_environment, Cons (o_syntax, p_syntax), false) in
	    origin_environment#add n_string q_temp_data;
	    NONE
      | _ ->
	  error_message l_function_name;
	  exit(1)
  in

  (* *)  
  let function_lambda l_function_name m_syntax =
    match m_syntax with
      Cons (_, _) ->
	Closure (origin_environment, m_syntax, false)
    | _ ->
      error_message l_function_name;
      exit(1)
  in
  
  let function_return a_function_name b_syntax =
    match b_syntax with
	Cons (c_syntax, Nil) ->
	  let d_temp_data = origin_defined_eval c_syntax origin_environment in
	    MultiValue (d_temp_data, None)
      | _ ->
	  error_message a_function_name;
	  exit(1)
  in
    
  let function_if a_function_name b_syntax =
    let function_if_sub l_token_bool m_syntax =
      match (l_token_bool, m_syntax) with
	  (BOOL n, Cons(o_syntax, Cons(p_syntax, Nil))) ->
	    if n then
	      origin_defined_eval o_syntax origin_environment
	    else
	      origin_defined_eval p_syntax origin_environment
	| _ ->
	    error_message "function_if";
	    exit(1)
    in
      match b_syntax with
	  Cons (c_syntax, d_syntax) ->
	    let e_temp_data = origin_defined_eval c_syntax origin_environment in
	      function_if_sub e_temp_data d_syntax
	| _ ->
	    error_message "function_if";
	    exit(1)
  in
    
  let function_print a_string b_syntax =
    let rec function_print_sub l_token =
      match l_token with
	  MultiValue (m_token, n_contiflow) ->
	    eval_multivalue "print" function_print_sub l_token      
	| INT x ->
	    print_int x;
	    print_string a_string;
	    NONE
	| STRING x ->
	    print_string x;
	    print_string a_string;
	    NONE
	| _ ->	    
	    Debug.debug_print l_token;
	    error_message "function_print";
	    exit(1)
    in
    match b_syntax with
	Cons (c_syntax, _) ->
	  let d_temp_data = origin_defined_eval c_syntax origin_environment in
	    function_print_sub d_temp_data
      | Atom (c_token) ->
	  let d_temp_data = origin_defined_eval (Atom(c_token)) origin_environment in
	    function_print_sub d_temp_data
      | _ ->
	  print_endline "syntax error:function_print";
	  exit(1)
  in

  let function_single a_string b_syntax =
    let rec function_single_sub l_token =
      match l_token with
	  MultiValue (m_token, n_contiflow) ->
	    eval_multivalue "single" function_single_sub l_token      
	| x ->
	    x
    in
    match b_syntax with
	Cons (c_syntax, _) ->
	  let d_temp_data = origin_defined_eval c_syntax origin_environment in
	    function_single_sub d_temp_data
      | Atom (c_token) ->
	  let d_temp_data = origin_defined_eval (Atom(c_token)) origin_environment in
	    function_single_sub d_temp_data
      | _ ->
	  print_endline "syntax error:function_single";
	  exit(1)
  in

  
  let function_format a_function_name b_syntax =
    let rec function_format_sub l_format_list m_syntax =
      match (l_format_list) with
	  [] ->
	    if m_syntax = Nil then
	      NONE
	    else
	      (print_endline "(function_format) There are too many arguments.";
	       exit(1))
	| FormatParser.FormatInt :: o_format_list ->
	    (match m_syntax with
		 Cons(Atom(INT(n_int)), p_syntax) ->
		   print_int n_int;
		   function_format_sub o_format_list p_syntax
	       | _ ->
		 Debug.debug_print_syntax m_syntax;
		   print_endline "format int error";		
		   exit(1))
	| FormatParser.FormatString :: o_format_list ->
	    (match m_syntax with
		 Cons(Atom(STRING(n_string)), p_syntax) ->
		   print_string n_string;
		   function_format_sub o_format_list p_syntax
	       | _ ->
		   print_endline "format string error";
		   exit(1))
	| FormatParser.FormatLinefeed :: o_format_list ->
	    print_endline "";
	    function_format_sub o_format_list m_syntax
	| (FormatParser.FormatPrint (o_string)) :: p_format_list ->
	    print_string o_string;
	    function_format_sub p_format_list m_syntax
	| _ ->
	    print_endline "format_sub error";
	    exit(1)
    in
      match b_syntax with
	  Cons(Atom(BOOL(true)), Cons(Atom(STRING(c_string)), d_syntax)) ->
	    let e_format_list = FormatParser.formatStatement FormatLexer.token (Lexing.from_string c_string) in
	      function_format_sub e_format_list d_syntax
	| _ ->
	    print_endline "format error";
	    exit(1)
  in

  (* depenedence *)
  let eval_int a_fun b_syntax =
    let rec eval_int_sub temp_fun l_syntax =
      match l_syntax with
	  Cons (m_syntax, Nil) ->
	    eval_int_sub temp_fun m_syntax
	| Cons (m_syntax, n_syntax) ->
	    let o_temp_data = eval_int_sub temp_fun m_syntax in
	      eval_int_sub (a_fun o_temp_data) n_syntax
	| Atom (m_syntax) ->
	    let n_temp_data = Converter.convert_token_to_int m_syntax in
	      temp_fun n_temp_data
	| _ ->
	    error_message "eval_int";
	    exit(1)    
    in
      match b_syntax with
	  Cons (c_syntax, Nil) ->
	    (origin_defined_eval c_syntax origin_environment)
	| Cons (c_syntax, d_syntax) ->
	     let e_temp_data = Converter.convert_token_to_int (origin_defined_eval c_syntax origin_environment) in
	     let f_temp_data = eval_int_sub (a_fun e_temp_data) d_syntax in
	       Converter.convert_int_to_token f_temp_data
	| Atom (c_syntax) ->
	    origin_defined_eval (Atom c_syntax) origin_environment
	| _ ->
	    error_message "eval_int";
	    exit(1)
	     
  in

  let function_judge a_fun b_syntax =
    let rec function_judge_sub l_fun m_syntax =
      match m_syntax with
	  Cons (n_syntax, Nil) ->
	    let p_temp_data = origin_defined_eval n_syntax origin_environment in
	      if l_fun p_temp_data then
		BOOL true
	      else
		BOOL false

	| Cons (n_syntax, o_syntax) ->
	    let p_temp_data = origin_defined_eval n_syntax origin_environment in
	      if l_fun p_temp_data then
		function_judge_sub (a_fun p_temp_data) o_syntax
	      else
		BOOL false
	| _ ->
	    error_message "function_judge";
	    exit(1)
    in	
      match b_syntax with
	Cons (c_syntax, Nil) ->
	  error_message "function_judge";
	  exit(1)
      | Cons (c_syntax, d_syntax) ->
	  let e_temp_data = origin_defined_eval c_syntax origin_environment in
	    function_judge_sub (a_fun e_temp_data) d_syntax
      | _ ->
	  error_message "function_judge";
	  exit(1)
  in      

  let function_nullq a_function_name b_syntax =
    match b_syntax with
	Atom (LIST (Nil)) | Cons(Atom(LIST (Nil)), Nil) ->
	  BOOL true
      | Atom (_) | Cons(Atom(_), Nil) ->
	  BOOL false
      | _ ->
	  print_endline "function null? has error.";
	  exit(1)
  in

  let any_syntax_apply_function a_function_name b_fun c_syntax =
    let rec any_syntax_apply_function_sub l_syntax =
      match l_syntax with
	  Nil -> Nil
	| Atom (m_token) ->
	    let n_temp_data = origin_defined_eval l_syntax origin_environment in
	      Cons (Atom (n_temp_data), Nil)
	| Cons (m_syntax, n_syntax) ->
	    let o_temp_data = origin_defined_eval m_syntax origin_environment in
	    let p_temp_data = any_syntax_apply_function_sub n_syntax in
	      Cons (Atom(o_temp_data), p_temp_data)
    in
    let e_temp_data = any_syntax_apply_function_sub c_syntax in
      b_fun e_temp_data
  in
    
  let specified_number_of_syntax_apply_function a_function_name b_fun c_number d_syntax =
    let rec specified_number_of_syntax_apply_function_sub l_syntax m_number =
      if m_number < 0 then
	(specified_number_of_error_message a_function_name c_number;
	 exit(1))
      else
	match (l_syntax, m_number) with
	    (Nil, 0) -> Nil
	  | (Atom(_), 1) ->
	      let n_temp_data = origin_defined_eval l_syntax origin_environment in
		Cons(Atom(n_temp_data), Nil)
	  | (Cons (m_syntax, n_syntax), _) ->
	      let o_temp_data = origin_defined_eval m_syntax origin_environment in
	      let p_temp_data = specified_number_of_syntax_apply_function_sub n_syntax (m_number - 1) in
		Cons (Atom(o_temp_data), p_temp_data)
	  | _ ->
	      specified_number_of_error_message a_function_name c_number;
	      exit(1)
    in
    let e_temp_data = specified_number_of_syntax_apply_function_sub d_syntax c_number in
      b_fun e_temp_data
  in
     
  let return_defined_function a_function_name =
    match a_function_name with
	(*"+" -> any_syntax_apply_function a_function_name (eval_int (+))*)
	"%" -> any_syntax_apply_function a_function_name (eval_int (mod))
      | "single" -> any_syntax_apply_function a_function_name (function_single a_function_name)
	  
      (* 関数宣言 *)
      | "define" -> function_define a_function_name
      | "lambda" -> function_lambda a_function_name

      (* 多値計算構文 *)
      | "return" -> specified_number_of_syntax_apply_function a_function_name (function_return a_function_name) 1
	  
      (* 条件分岐 *)
      | "if" -> function_if a_function_name 
	  
      (* 出力系 *)
      | "print" -> any_syntax_apply_function a_function_name (function_print(" ")) 
      | "println" -> specified_number_of_syntax_apply_function a_function_name (function_print ("\n")) 1
      | "format" -> any_syntax_apply_function a_function_name (function_format a_function_name)
	  
      (* リスト処理 *)
      | "list" -> any_syntax_apply_function a_function_name (function_list)
      | "car" -> specified_number_of_syntax_apply_function a_function_name (function_list_car) 1
      | "cdr" -> specified_number_of_syntax_apply_function a_function_name (function_list_cdr) 1
      | "reverse" -> specified_number_of_syntax_apply_function a_function_name (function_reverse a_function_name) 1

      (* 畳み込み関数 *)
      | "foldr" -> function_fold a_function_name true
      | "foldl" -> function_fold a_function_name false
	  
      (* 比較 *)
      | "=" -> any_syntax_apply_function a_function_name (function_judge (=))
      | ">" -> any_syntax_apply_function a_function_name (function_judge (>))
      | "<" -> any_syntax_apply_function a_function_name (function_judge (<))
      | ">=" -> any_syntax_apply_function a_function_name (function_judge (>=))
      | "<=" -> any_syntax_apply_function a_function_name (function_judge (<=))
      | "null?" -> specified_number_of_syntax_apply_function a_function_name (function_nullq a_function_name) 1

      (* 列処理 *)
      | "begin" -> eval_consecutive_list origin_defined_eval origin_environment 
      | _ ->
	  any_syntax_apply_function a_function_name (function_call_from_name a_function_name)
  in
    match origin_syntax with
	Cons (Atom (VAR (b_function_name)), c_syntax) ->
	  let d_temp_data = (return_defined_function b_function_name) c_syntax in
	    d_temp_data
      | Cons(closure_syntax, b_syntax) ->
	  let origin_temp_closure = origin_defined_eval closure_syntax origin_environment in
	    (match origin_temp_closure with
		 Closure (_,_,_) | SubClosure(_,_,_) ->
		   let c_temp_data = any_syntax_apply_function "non name" (function_call_from_closure "non_name" origin_temp_closure) b_syntax in
		     c_temp_data
	       | _ ->
		   Debug.debug_print origin_temp_closure;
		   Printf.printf "Closure is required.";
		   exit(1))
      | Atom(_) | Nil ->
	  error_message "main_function";
	  exit(1)
;;
