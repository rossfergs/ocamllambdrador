
let rec interpret_program input_string : unit =
  let open Scope in
  let parse_tree = Parser.parse input_string in
  let rec aux nodes scope idx =
    if idx >= List.length nodes then
      ()
    else
      let new_scope = interpret_statement (List.nth nodes idx) scope in
    aux nodes new_scope (idx+1)
    in
  let empty_scope = {inner_scope = String_map.empty; outer_scope = None} in
  aux parse_tree empty_scope 0;
  print_newline ();

and interpret_statement node scope =
  let open Scope in
  let open Parse_node in
  match node with
  | Print_Node e -> interpret_print e scope
  | Let_Node { namespace = name; block = blck} -> 
    Scope.bind (String_map.add name blck scope.inner_scope) scope
  | Expr e -> interpret_print e scope

and interpret_print node scope =
  let () = match node with
  | Integer_Node n -> 
    print_int n
  | _ -> print_endline "not yep implemented printing expression type"
  in
  scope

and interpret_function _node _scope = 
  let rec _set_params f_name taken given params =
    match taken, given with
    | [], _ | _, [] -> params
    | t :: rt, g :: rg ->
      _set_params f_name rt rg (Scope.String_map.add t g params)
  in
  raise (Error.InterpreterError "intepret function needs implemented")

and interpret_expression node scope =
  let open Parse_node in
  let get_type = function
  | Float_Node _ -> "Float"
  | Integer_Node _ -> "Int"
  | String_Node _ -> "String"
  | _ ->
    raise (Error.InterpreterError "Invalid expression type")
  in

  let get_binary_result_type l r =
    match get_type l, get_type r with
    | a, b when a == b -> l, r
    | "String", "String" -> l, r
    | "String", n | n, "String" -> 
      let msg = "Cannot apply operator from type String and " ^ n in
      raise (Error.InterpreterError msg)
    | "Float", _ | _, "Float" -> to_float_node l, to_float_node r
    | "Int", "Int" -> l, r
    | _, _ -> 
      raise (Error.InterpreterError "Unknown expression result")
  in

  let add_nodes left_node right_node = match get_binary_result_type left_node right_node with
  | Integer_Node l, Integer_Node r -> Integer_Node (l+r)
  | Float_Node l, Float_Node r -> Float_Node (l+.r)
  | String_Node l, String_Node r -> String_Node (l^r)
  | _ -> raise (Error.InterpreterError "Invalid nodes to apply addition to")
  in

  let sub_nodes left_node right_node = match get_binary_result_type left_node right_node with
  | Integer_Node l, Integer_Node r -> Integer_Node (l-r)
  | Float_Node l, Float_Node r -> Float_Node (l-.r)
  | _ -> raise (Error.InterpreterError "Invalid nodes to apply subtraction to")
  in

  let mult_nodes left_node right_node = match get_binary_result_type left_node right_node with
  | Integer_Node l, Integer_Node r -> Integer_Node (l*r)
  | Float_Node l, Float_Node r -> Float_Node (l*.r)
  | _ -> raise (Error.InterpreterError "Invalid nodes to apply multiplication to")
  in

  let () = print_endline "implement expression stuff" in
  match node with 
  | Add_Node (l, r) -> add_nodes l r
  | Sub_Node (l, r) -> sub_nodes l r
  | Mult_Node (l, r) -> mult_nodes l r
  | Integer_Node _ | Float_Node _ | String_Node _ -> node
  | Variable_Node _ -> interpret_function node scope
  | Div_Node (_, _) -> raise (Error.InterpreterError "Division not implemented")
