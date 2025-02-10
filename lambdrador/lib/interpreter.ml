let get_type = let open Parse_node in function
  | Float_Node _ -> "Float"
  | Integer_Node _ -> "Int"
  | String_Node _ -> "String"
  | Bool_Node _ -> "Bool"
  | _ ->
    raise (Error.Interpreter_Error "Invalid expression type")


let get_binary_result_type l r =
  let open Parse_node in
  match get_type l, get_type r with
    | a, b when a == b -> l, r
    | "String", n | n, "String" -> 
      let msg = "Cannot apply operator from type String and " ^ n in
      raise (Error.Interpreter_Error msg)
    | "Bool", n | n, "Bool" ->
      let msg = "Cannot apply operator from type Bool and " ^ n in
      raise (Error.Interpreter_Error msg)
    | "Float", _ | _, "Float" -> to_float_node l, to_float_node r
    | _, _ -> 
      raise (Error.Interpreter_Error "Unknown expression result")


(*
  interpret all the statements given to the interpreter
  current_scope is an option of a scope, None indicates no
  pre existing scope, Some scope indicates a preexising scope 's' (in case of REPL)
*)
let rec interpret_program input_string current_scope : Scope.scope =
  let parse_tree = Parser.parse input_string in
  let new_scope = match current_scope with
    | None -> Scope.empty
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
      (* FIXME: let x = x - 1; currently breaks the scope, investigate *)
      (* let block_eval = 
        {parameters = []; statements = []; expression = interpret_block block scope}
      in *)
      let s = Scope.bind (String_map.add name block scope.inner_scope) scope in
      s
      | Expr e -> interpret_print e scope


(* interpreting a print statement, returns the scope given to it *)
and interpret_print node scope =
  let open Parse_node in
  let eval_node = interpret_expression node scope in
  let () = match eval_node with
    | Integer_Node n -> print_int n; print_newline ()
    | String_Node s -> print_endline s
    | Float_Node f -> print_float f; print_newline ()
    | Bool_Node b -> if b then print_endline "true" else print_endline "false"
    | Eq_Node _ -> print_string "whuy?"
    | Leq_Node _ -> print_string "dafaw"
    | _ -> print_string "not yet implemented printing expression type"
  in
  scope


(* intepreting a variable node, which is basically a function xd *)
and interpret_function node scope = 
  (* function to set the parameters of the function call *)
  let rec set_params taken given params =
    match taken, given with
      | [], [] ->
          params
      | [] , _ :: _ ->
        raise (Error.Interpreter_Error "Too many params given")
      | _ :: _, [] ->
        raise (Error.Interpreter_Error "Not enough params given")
      | taken_head :: taken_tail, given_head :: given_tail ->
        let block = match given_head with
          | Parse_node.Variable_Node {namespace; parameters} when parameters = [] ->
            Scope.get namespace scope
          | n ->
            Parse_node.{parameters = []; statements = []; expression = n}
        in
        let open Scope in
        set_params
          taken_tail
          given_tail
          (bind (String_map.add taken_head block params.inner_scope) params)
  in
  (* getting the block assocaited with the function namespace and its parameters *)
  let func, given_params = match node with
    | Parse_node.Variable_Node {namespace; parameters = params} -> Scope.get namespace scope, params
    | _ -> raise (Error.Interpreter_Error "non-namespace expression being interpreted as function")
  in
  let function_scope = set_params func.parameters given_params scope in
  interpret_block func function_scope


and interpret_block node scope =
  let rec interpret_block_impl statements block_scope = match statements with
    | [] -> block_scope
    | statement :: remainder ->
      let new_scope = interpret_statement statement block_scope in
    interpret_block_impl remainder new_scope
    in
  let new_scope = interpret_block_impl node.statements scope in
  interpret_expression node.expression new_scope


and interpret_equals left_node right_node scope =
  let open Parse_node in
  let left_eval = interpret_expression left_node scope in
  let right_eval = interpret_expression right_node scope in
  match get_binary_result_type left_eval right_eval with
    | Bool_Node l, Bool_Node r ->
      Bool_Node (l = r)
    | Integer_Node l, Integer_Node r ->
      Bool_Node (l = r)
    | Float_Node l, Float_Node r ->
      Bool_Node (l = r)
    | String_Node l, String_Node r ->
      Bool_Node (String.equal l r)
    | _ -> raise (Error.Interpreter_Error "Invalid types for equals")


and interpret_not_equals left_node right_node scope =
  let open Parse_node in
  match interpret_equals left_node right_node scope with
    | Bool_Node b -> Bool_Node (not b)
    | _ -> raise (Error.Interpreter_Error "invalid output from interpreting equals")


and interpret_greater_than left_node right_node scope =
  let open Parse_node in
  let left_eval = interpret_expression left_node scope in
  let right_eval = interpret_expression right_node scope in
  match get_binary_result_type left_eval right_eval with
  | Integer_Node l, Integer_Node r ->
      Bool_Node (l > r)
  | Float_Node l, Float_Node r ->
      Bool_Node (l > r)
  | String_Node _, String_Node _ ->
      raise (Error.Interpreter_Error "Cannot perform '>' operation on type string")
  | _ -> raise (Error.Interpreter_Error "invalid output from interpreting greater than")


and interpret_less_than left_node right_node scope =
  let open Parse_node in
  let left_eval = interpret_expression left_node scope in
  let right_eval = interpret_expression right_node scope in
  match get_binary_result_type left_eval right_eval with
    | Bool_Node l, Bool_Node r ->
      Bool_Node (l < r)
  | Integer_Node l, Integer_Node r ->
      Bool_Node (l < r)
  | Float_Node l, Float_Node r ->
      Bool_Node (l < r)
  | String_Node _, String_Node _ ->
      raise (Error.Interpreter_Error "Cannot perform '<' operation on type string")
  | _ -> raise (Error.Interpreter_Error "invalid output from interpreting less than")


and interpret_greater_than_or_equal_to left_node right_node scope =
  let open Parse_node in
  let left_eval = interpret_expression left_node scope in
  let right_eval = interpret_expression right_node scope in
  match get_binary_result_type left_eval right_eval with
  | Bool_Node l, Bool_Node r ->
      Bool_Node (l >= r)
  | Integer_Node l, Integer_Node r ->
      Bool_Node (l >= r)
  | Float_Node l, Float_Node r ->
      Bool_Node (l >= r)
  | String_Node _, String_Node _ ->
      raise (Error.Interpreter_Error "Cannot perform '>=' operation on type string")
  | _ -> raise (Error.Interpreter_Error "invalid output from interpreting greater than or equal to")


and interpret_less_than_or_equal_to left_node right_node scope =
  let open Parse_node in
  let left_eval = interpret_expression left_node scope in
  let right_eval = interpret_expression right_node scope in
  match get_binary_result_type left_eval right_eval with
  | Bool_Node l, Bool_Node r ->
      Bool_Node (l <= r)
  | Integer_Node l, Integer_Node r ->
      Bool_Node (l <= r)
  | Float_Node l, Float_Node r ->
      Bool_Node (l <= r)
  | String_Node _, String_Node _ ->
      raise (Error.Interpreter_Error "Cannot perform '<=' operation on type string")
  | _ -> raise (Error.Interpreter_Error "invalid output from interpreting less than or equal to")


and interpret_predicate node scope =
  let open Parse_node in
  match node with
    | Eq_Node (left, right) ->
        interpret_equals left right scope
    | Not_Eq_Node (left, right) ->
        interpret_not_equals left right scope
    | Greater_Node (left, right) ->
        interpret_greater_than left right scope
    | Greq_Node (left, right) ->
        interpret_greater_than_or_equal_to left right scope
    | Less_Node (left, right) ->
        interpret_less_than left right scope
    | Leq_Node (left, right) ->
        interpret_less_than_or_equal_to left right scope
    | _ -> raise (Error.Interpreter_Error "Predicate type not yet implemented")


(* interpret if nodes *)
and interpret_if condition then_block else_block scope =
  let open Parse_node in
  match interpret_expression condition scope with
    | Bool_Node true -> interpret_block then_block scope
    | Bool_Node false -> interpret_block else_block scope
    | other_node ->
        let error_msg = "condition in if expression must evaluate to Bool, not " ^
        get_type other_node in
        raise (Error.Interpreter_Error error_msg)


and interpret_expression node scope =
  let open Parse_node in

  let add_nodes left_node right_node = match get_binary_result_type left_node right_node with
    | Integer_Node l, Integer_Node r -> Integer_Node (l+r)
    | Float_Node l, Float_Node r -> Float_Node (l+.r)
    | String_Node l, String_Node r -> String_Node (l^r)
    | _ -> raise (Error.Interpreter_Error "Invalid nodes to apply addition to")
  in

  let sub_nodes left_node right_node = match get_binary_result_type left_node right_node with
    | Integer_Node l, Integer_Node r -> Integer_Node (l-r)
    | Float_Node l, Float_Node r -> Float_Node (l-.r)
    | _ -> raise (Error.Interpreter_Error "Invalid nodes to apply subtraction to")
  in

  let mult_nodes left_node right_node = match get_binary_result_type left_node right_node with
    | Integer_Node l, Integer_Node r -> Integer_Node (l*r)
    | Float_Node l, Float_Node r -> Float_Node (l*.r)
    | _ -> raise (Error.Interpreter_Error "Invalid nodes to apply multiplication to")
  in

  let apply operation left right =
    let eval_left = interpret_expression left scope in
    let eval_right = interpret_expression right scope in
    operation eval_left eval_right
  in

  match node with 
    | Eq_Node _ | Not_Eq_Node _ | Less_Node _ | Leq_Node _ | Greater_Node _ | Greq_Node _ ->
        interpret_predicate node scope
    | Add_Node (l, r) -> apply add_nodes l r
    | Sub_Node (l, r) -> apply sub_nodes l r
    | Mult_Node (l, r) -> apply mult_nodes l r
    | Integer_Node _ | Float_Node _ | String_Node _ | Bool_Node _ -> node
    | Variable_Node _ -> interpret_function node scope
    | Div_Node _ -> raise (Error.Interpreter_Error "Division not implemented")
    | If_node {condition; then_block; else_block} -> interpret_if condition then_block else_block scope
    (* | _ -> raise (Error.Interpreter_Error "Node type not yet implemented") *)


let interpret user_input current_scope : Scope.scope option =
  try
    Some(interpret_program user_input current_scope)
  with e ->
    Error.print_error e;
    None
