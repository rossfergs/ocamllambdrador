(* 
  interpret all the statements given to the interpreter
  current_scope is an option of a scope, None indicates no
  pre existing scope, Some scope indicates a preexising scope 's' (in case of REPL)
*)
let rec interpret_program input_string current_scope : Scope.scope =
  let open Scope in
  let parse_tree = Parser.parse input_string in
  let new_scope = match current_scope with
    | None -> {inner_scope = String_map.empty; outer_scope = None}
    | Some s -> s 
  in
  let rec aux nodes scope idx =
    if idx >= List.length nodes then
      scope
    else
      let new_scope = interpret_statement (List.nth nodes idx) scope in
    aux nodes new_scope (idx+1)
    in
  aux parse_tree new_scope 0

(* interpreting a statement, returns the scope effected by the statement *)
and interpret_statement node scope =
  let open Scope in
  let open Parse_node in
  match node with
    | Print_Node e -> interpret_print e scope
    | Let_Node { namespace = name; block = block} -> 
      Scope.bind (String_map.add name block scope.inner_scope) scope
    | Expr e -> interpret_print e scope

(* interpreting a print statement, returns the scope given to it *)
and interpret_print node scope =
  let open Parse_node in
  let eval_node = interpret_expression node scope in
  let () = match eval_node with
    | Integer_Node n -> print_int n; print_newline ()
    | String_Node s -> print_endline s
    | Float_Node f -> print_float f; print_newline ()
    | _ -> print_string "not yep implemented printing expression type"
  in
  scope

and interpret_function node scope = 
  let rec _set_params taken given params =
    match taken, given with
      | [], [] -> params
      | [] , _ :: _ -> 
        raise (Error.InterpreterError "Too many params given")
      | _ :: _, [] -> 
        raise (Error.InterpreterError "Not enough params given")
      | taken_head :: taken_tail, given_head :: given_tail ->
        let block = match given_head with
          | Parse_node.Variable_Node {namespace; parameters} when parameters = [] -> 
            Scope.get namespace scope
          | n -> Parse_node.{parameters = []; statements = []; expression = n}
          in
          _set_params  
            taken_tail 
            given_tail 
            (Scope.String_map.add taken_head block params)
  in
  let _func = match node with
    | Parse_node.Variable_Node {namespace; _} -> Scope.get namespace scope
    | _ -> raise (Error.InterpreterError "this shouldnt happen... interpret_function")
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

  let apply operation left right =
    let eval_left = interpret_expression left scope in
    let eval_right = interpret_expression right scope in
    operation eval_left eval_right
  in

  match node with 
    | Add_Node (l, r) -> apply add_nodes l r
    | Sub_Node (l, r) -> apply sub_nodes l r
    | Mult_Node (l, r) -> apply mult_nodes l r
    | Integer_Node _ | Float_Node _ | String_Node _ -> node
    | Variable_Node _ -> interpret_function node scope
    | Div_Node _ -> raise (Error.InterpreterError "Division not implemented")

let interpret user_input current_scope : Scope.scope option =
  try
    Some(interpret_program user_input current_scope)
  with e ->
    Error.print_error e;
    None
