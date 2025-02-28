let get_type = let open Parse_node in function
| Float_Node _ -> "Float"
| Integer_Node _ -> "Int"
| String_Node _ -> "String"
| Bool_Node _ -> "Bool"
| Tagged_Node {tag; _} -> tag
| _ -> raise (Error.Interpreter_Error "Invalid expression type")


let _get_list_length = let open Parse_node in function
  | List_Node list ->
      let rec impl counter = function
      | Nil -> counter | Node (_, r) -> impl (counter+1) r
      in
      impl 0 list
  | _ -> raise (Error.Interpreter_Error "not a list")


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
  | Print_Node e -> interpret_print e scope; scope
  | Println_Node e ->
      interpret_print e scope;
      print_newline ();
      scope
  | Let_Node { namespace = name; block = block} -> 
      (*  FIXME: let x = x - 1; currently breaks the scope, investigate *)
      Scope.bind (String_map.add name block scope.inner_scope) scope
  | Expr e -> interpret_print e scope; scope


(* interpreting a print statement, returns the scope given to it *)
and interpret_print node scope =
  let open Parse_node in
  let rec print_list = 
    function
    | Node (x, Nil) -> 
        interpret_print x scope;
        print_string "]";
    | Node(x, xs) ->
        interpret_print x scope;
        print_string "; ";
        print_list xs;
    | Nil ->
        print_string "]"
  in
  let eval_node = interpret_expression node scope in
  match eval_node with
  | Integer_Node n -> print_int n
  | String_Node s -> print_string s
  | Float_Node f -> print_float f
  | Bool_Node b -> if b then print_string "true" else print_string "false"
  | List_Node l -> print_string "["; print_list l
  | Tagged_Node {tag; data} -> print_string tag; print_string " ";
      (match data with
      | None -> ()
      | Some e -> interpret_print e scope
      )
  | _ -> print_string "cannot print expression type"


(* intepreting a variable node, which is basically a function xd *)
and interpret_function node scope = 
  (* function to set the parameters of the function call *)
  let rec set_params taken given params =
    match taken, given with
    | [], [] ->
        params
    | [] , _ :: _ -> raise (Error.Interpreter_Error "Too many params given")
    | _ :: _, [] -> print_endline "ERROR!";
        raise (Error.Interpreter_Error "Not enough params given")
    | taken_head :: taken_tail, given_head :: given_tail ->
        (*print_string " "; print_endline taken_head;*)
        let open Parse_node in
        (* FIXME:*)
        (*
           CURRENTLY I BELIEVE HIGHER ORDER FUNCTIONS DO NOT WORK
           CAUSE ITS TRYING TO INTERPRET AN EXPRESSION ON INPUT

           INVESTIGATE
        *)
        let block = match given_head with
        | Variable_Node {namespace; parameters = []} ->
            (match Scope.get namespace scope with
            | b when b.parameters != [] -> b
            | _ -> 
                Parse_node.{statements = []; expression = interpret_expression given_head scope; parameters = []}
            )
        | _ ->
            Parse_node.{statements = []; expression = interpret_expression given_head scope; parameters = []}
        in
        let open Scope in
        set_params
          taken_tail
          given_tail
          (bind (String_map.add taken_head block params.inner_scope) params)
  in
  (* getting the block assocaited with the function namespace and its parameters *)
  let func, given_params = match node with
  | Parse_node.Variable_Node {namespace; parameters = params} -> 
      (*print_string "NAME: "; print_endline namespace;*)
      Scope.get namespace scope, params
  | _ -> raise (Error.Interpreter_Error "non-namespace expression being interpreted as function")
  in
  (* List.iter (fun s -> print_string "INPUT PARAM: "; print_endline s) func.parameters; *)
  (* List.iter (fun n -> print_string "GIVEN PARAM: "; Parse_node.print_node n) given_params; *)
  let new_scope = Scope.{inner_scope = Scope.String_map.empty; outer_scope = Some scope} in
  let function_scope = set_params func.parameters given_params new_scope in
  let result = interpret_block func function_scope in
  let open Parse_node in
  match result with
  | Variable_Node {namespace; parameters} when parameters != [] ->
      print_endline "higher order";
      interpret_function (Variable_Node {namespace; parameters = given_params}) scope
  | _ -> result


and interpret_block node scope =
  (*
  print_newline (); print_newline ();
  Scope.print_scope scope;
  print_newline (); print_newline ();
  *)
  let rec interpret_block_impl statements block_scope = match statements with
  | [] -> block_scope
  | statement :: remainder ->
      let new_scope = interpret_statement statement block_scope in
      interpret_block_impl remainder new_scope
  in
  let new_scope = interpret_block_impl node.statements scope in
  let open Parse_node in
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


and interpret_if condition then_block else_block scope =
  let open Parse_node in
  match interpret_expression condition scope with
  | Bool_Node true -> interpret_block then_block scope
  | Bool_Node false -> interpret_block else_block scope
  | other_node ->
      let error_msg = "condition in if expression must evaluate to Bool, not " ^
      get_type other_node in
      raise (Error.Interpreter_Error error_msg)


and interpret_cons left right scope = 
  let open Parse_node in
  let left_eval = interpret_expression left scope in
  let right_eval = interpret_expression right scope in
  match right_eval with
  | List_Node l -> List_Node (Node (left_eval, l))
  | _ -> raise (Error.Interpreter_Error "right side of cons must be a list")


and match_cons list left right cons_scope =
  let open Parse_node in
  match left with
  | Variable_Node {namespace = name; _} ->
      (match list with
      | Node (cur, rem) -> 
          let open Scope in
          let new_item = {statements = []; expression = cur; parameters = []} in
          let new_scope = Scope.bind (Scope.String_map.add name new_item cons_scope.inner_scope) cons_scope in
          (match right with 
          | Cons_Node (l, r) -> match_cons rem l r new_scope
          | Variable_Node {namespace = "_"; _} ->
              Some new_scope
          | Variable_Node {namespace = rem_name; _} ->
              let end_item = {statements = []; expression = List_Node rem; parameters = []} in
              let end_scope = Scope.bind (Scope.String_map.add rem_name end_item new_scope.inner_scope) new_scope in
              Some end_scope
          | List_Node l -> 
              (match match_list l rem new_scope with
              | None -> None
              | Some s -> Some s
              )
          | _ -> raise (Error.Interpreter_Error "right side of cons must be cons, variable, list, or wildcard")
          )
      | Nil -> None
      )
  | _ ->
      (match list with
      | Node (e, rem) when left = e ->
          (match right with | Cons_Node (l, r) -> match_cons rem l r cons_scope
          | Variable_Node {namespace = "_"; _} ->
              Some cons_scope
          | Variable_Node {namespace = rem_name; _} ->
              let new_item = {statements = []; expression = List_Node rem; parameters = []} in
              let end_scope = Scope.bind (Scope.String_map.add rem_name new_item cons_scope.inner_scope) cons_scope in
              Some end_scope
          | List_Node l -> 
              match_list l list cons_scope
          | _ -> raise (Error.Interpreter_Error "right side of cons must be cons, variable, list, or wildcard")
          )
      | _ -> None
      )


and match_list pattern_list expr_list list_scope =
  let open Parse_node in
  match pattern_list, expr_list with
  | Nil, Nil -> Some list_scope
  | Node (Variable_Node {namespace = "_"; _}, pattern_rem), Node (_, expr_rem) ->
      match_list pattern_rem expr_rem list_scope
  | Node (Variable_Node {namespace = name; _}, pattern_rem), Node (e, expr_rem) ->
      let new_item = {statements = []; expression = e; parameters = []} in
      let new_scope = Scope.bind (Scope.String_map.add name new_item list_scope.inner_scope) list_scope in
      match_list pattern_rem expr_rem new_scope
  | Node (pattern_expr, pattern_rem), Node (expr_expr, expr_rem) when pattern_expr = expr_expr ->
      match_list pattern_rem expr_rem list_scope
  | Node (List_Node pl, pattern_rem), Node (List_Node el, expr_rem) ->
      (match match_list pl el list_scope with
      | None -> None
      | Some s -> match_list pattern_rem expr_rem s)
  | _ -> None


(*
  recursively iterate through cases of match expression
  when a pattern is found to match we will execute the block associated with it
  and return its value
*)
and interpret_match expr cases scope =
  let open Parse_node in
  let is_list = function
  | List_Node _ -> true
  | _ -> false
  in
  let is_tag = function
  | Tagged_Node _ -> true
  | _ -> false
  in
  match cases with
  (* no matching case *)
  | [] -> raise (Error.Interpreter_Error "no cases to match expression in match")
  (* wildcard cases *)
  | Case_Node {pattern = Variable_Node {namespace = "_"; _}; guard; block} :: cs ->
      (match guard with
      | None -> interpret_block block scope
      | Some g ->
          if interpret_expression g scope = Bool_Node true
          then interpret_block block scope
          else interpret_match expr cs scope
      )
  (* variable cases *)
  | Case_Node {pattern = Variable_Node {namespace = name; _}; guard; block} :: cs ->
      let open Scope in
      let new_item = {statements = []; expression = expr; parameters = []} in
      let case_scope = bind (String_map.add name new_item scope.inner_scope) scope in
      (match guard with
      | None -> interpret_block block case_scope
      | Some g ->
          if interpret_expression g case_scope = Bool_Node true
          then interpret_block block case_scope
          else interpret_match expr cs scope
      )
  | Case_Node {pattern = Tagged_Node {tag; data}; guard; block} :: cs when is_tag expr ->
      let unwrap_tag = function
      | Tagged_Node {tag; data} -> tag, data
      | _ -> raise (Error.Interpreter_Error "not a tag")
      in
      let expr_tag, expr_data = unwrap_tag expr in
      if not (String.equal expr_tag tag) then interpret_match expr cs scope else
      (match data, expr_data with
      | None, None -> interpret_block block scope
      | Some(Cons_Node (left, right)), Some (List_Node l) -> 
          (match match_cons l left right scope with
          | None -> interpret_match expr cs scope
          | Some s -> 
              (match guard with
              | None -> interpret_block block s
              | Some g ->
                  if interpret_expression g scope = Bool_Node true 
                  then interpret_block block scope
                  else interpret_match expr cs scope
              )
          )
      | Some (List_Node pl), Some (List_Node el) ->
          (match match_list pl el scope with
          | None -> interpret_match expr cs scope
          | Some s ->
              (match guard with
              | None -> interpret_block block s
              | Some g -> 
                  if interpret_expression g scope = Bool_Node true 
                  then interpret_block block scope
                  else interpret_match expr cs scope
              )
          )
      | Some (Variable_Node {namespace; _}), Some e -> 
          let new_item = {statements = []; expression = e; parameters = []} in
          let new_scope = Scope.bind (Scope.String_map.add namespace new_item scope.inner_scope) scope in
          (match guard with
          | None -> interpret_block block new_scope
          | Some g -> 
              if interpret_expression g new_scope = Bool_Node true 
              then interpret_block block new_scope
              else interpret_match expr cs scope
          )
      | Some ce, Some ee when ce = ee ->
          (match guard with
          | None -> interpret_block block scope
          | Some g -> 
              if interpret_expression g scope = Bool_Node true 
              then interpret_block block scope
              else interpret_match expr cs scope
          )
      | _ -> interpret_match expr cs scope
      )
  (* matching expression cases *)
  | Case_Node {pattern; guard; block} :: cs when expr = pattern ->
      (match guard with
      | None -> interpret_block block scope
      | Some g -> 
          if interpret_expression g scope = Bool_Node true 
          then interpret_block block scope
          else interpret_match expr cs scope
      )
  | Case_Node {pattern = Cons_Node (left, right); guard; block} :: cs when is_list expr ->
      let open Parse_node in
      let unwrap_list = function
        | List_Node l -> l
        | _ -> raise (Error.Interpreter_Error "not a list")
      in
      let inner_list = unwrap_list expr in
      (match match_cons inner_list left right scope with
      | None -> interpret_match expr cs scope
      | Some s -> 
          (match guard with
          | Some g -> if interpret_expression g s = Bool_Node true 
              then interpret_block block s
              else interpret_match expr cs scope
          | None ->
              interpret_block block s
          )
      )
  (* matching lists *) 
  | Case_Node {pattern = List_Node l; guard; block} :: cs when is_list expr ->
      let open Parse_node in
      let unwrap_list = function
        | List_Node l -> l
        | _ -> raise (Error.Interpreter_Error "not a list")
      in
      let inner_list = unwrap_list expr in
      (match match_list l inner_list scope with
      | None -> interpret_match expr cs scope
      | Some s -> 
          (match guard with
          | Some g -> if interpret_expression g s = Bool_Node true then
              interpret_block block s
              else interpret_match expr cs scope
          | None ->
              interpret_block block s
          )
      )
  (* current case didnt match, move to next *)
  | _ :: cs -> interpret_match expr cs scope


and interpret_expression node scope =
  let open Parse_node in

  let rec interpret_list = function
      | Node(e, r) -> Node (interpret_expression e scope, interpret_list r)
      | Nil -> Nil
  in let add_nodes left_node right_node = match get_binary_result_type left_node right_node with
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
  | Integer_Node _ | Float_Node _ | String_Node _ | Bool_Node _ | Tagged_Node _ ->
      node
  | Eq_Node _ | Not_Eq_Node _ | Less_Node _ | Leq_Node _ | Greater_Node _ | Greq_Node _ ->
      interpret_predicate node scope
  | List_Node l -> List_Node (interpret_list l)
  | Add_Node (l, r) ->
      apply add_nodes l r
  | Sub_Node (l, r) ->
      apply sub_nodes l r
  | Mult_Node (l, r) ->
      apply mult_nodes l r
  | Variable_Node _ ->
      interpret_function node scope
  | Div_Node _ ->
      raise (Error.Interpreter_Error "Division not implemented")
  | If_Node {condition; then_block; else_block} ->
      interpret_if condition then_block else_block scope
  | Cons_Node (l, r) ->
      interpret_cons l r scope
  | Match_Node {expr; cases} ->
      let evaluated_expr = interpret_expression expr scope in
      let new_scope = Scope.{inner_scope = Scope.String_map.empty; outer_scope = Some scope} in
      interpret_match evaluated_expr cases new_scope
  | _ -> raise (Error.Interpreter_Error "Node type not yet implemented")


let interpret user_input current_scope : Scope.scope option =
  try
    Some(interpret_program user_input current_scope)
  with e ->
    Error.print_error e;
    None
