module String_map = Map.Make(String);;

type 'a env_node = 
    Nothing
  | Node of 'a
;;

(* debug *)
let is_print = function
    true -> print_endline "true"
  | false -> print_endline "false"
;;

(* 
   以下のvariables_treeクラスにおける
   parent_variables 及び、present_variablesは、
   あくまでも上記のvariables_node型であって、
   String_map型ではない。
   故に、present_variables等を扱う場合には、
   match _ with 等で取り出さなければならない
*)

(* token 型で使う *)
class ['a] env_tree = object(self)
  val mutable parent_env:('a env_tree env_node) = Nothing; (* env_tree env_node *)
  val mutable func_env:('a env_tree env_node)  = Nothing; (* env_tree env_node *)
  val mutable present_tree:('a String_map.t) = String_map.empty; (* String_map *)
  
  method set_func_env a_node =
    if a_node != Nothing then
      func_env <- a_node
  method set_parent_env a_node =
    if a_node != Nothing then
      parent_env <- a_node
  method set_grand_parent_env a_node = (* 最上位に追加する *)
    match parent_env with
	Node x ->
	  x#set_grand_parent_env a_node
      | Nothing ->
	  parent_env <- a_node
  method add a_string_key (b_token:'a) =
    present_tree <- (String_map.add a_string_key b_token present_tree)

  method mem_present a_string_key =
    String_map.mem a_string_key present_tree

  method env_mem a_string_key b_env =
    match b_env with
	Node x ->
	  x#mem_present a_string_key
      | Nothing ->
	  false

  method find_present a_string_key =
    if self#mem_present a_string_key then
      String_map.find a_string_key present_tree
    else
      (Printf.printf "%s is not exist.(Variables.ml:find)\n" a_string_key;
       exit(1))

  method env_find a_string_key b_env =
    match b_env with
	Node x ->
	  x#find_present a_string_key
      | _ ->
	  Printf.printf "%s is not exist.(Variables.ml:find)\n" a_string_key;
	  exit(1)
	    
  method find_all_groups a_string_key =
    if (self#mem_present a_string_key) then
      self#find_present a_string_key
    else if (self#env_mem a_string_key func_env) then
      self#env_find a_string_key func_env
    else
      match parent_env with
	  Node x ->
	    x#find_all_groups a_string_key
	| Nothing ->
	    Printf.printf "%s is not exist.(Variables.ml:find_all_groups\n" a_string_key;
	    exit(1)
      
  method first_find_all_groups a_string_key =
    (*print_endline a_string_key; *)
    if (self#mem_present a_string_key) then
      self#find_present a_string_key
    else
      match parent_env with
	  Node x ->
	    x#find_all_groups a_string_key
	| Nothing ->
	    Printf.printf "%s is not exist.(Variables.ml:first_find_all_groups\n" a_string_key;
	    exit(1)
	            
end;;
