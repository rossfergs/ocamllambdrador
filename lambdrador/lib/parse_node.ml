type expression_node =
  | Negative_Node of expression_node
  | Block_Node of {
      parameters: string list;
      statements: statement_node list;
      expression:expression_node;
      closure_scope: (string list * expression_node list) option
  }
  | Tagged_Node of {
      tag: string;
      data: expression_node option
  }
  | Match_Node of {
      expr: expression_node;
      cases: expression_node list
  }
  | Case_Node of {
      pattern: expression_node;
      guard: expression_node option;
      block: expression_node
  }
  | Cons_Node of expression_node * expression_node
  | List_Node of list_node
  | Bool_Node of bool
  | Integer_Node of int
  | Float_Node of float 
  | String_Node of string
  | If_Node of {
      condition: expression_node;
      then_block: expression_node;
      else_block: expression_node
  }
  | Greater_Node of (expression_node * expression_node)
  | Less_Node of (expression_node * expression_node)
  | Eq_Node of (expression_node * expression_node)
  | Greq_Node of (expression_node * expression_node)
  | Leq_Node of (expression_node * expression_node)
  | Not_Eq_Node of (expression_node * expression_node)
  | Add_Node of (expression_node * expression_node)
  | Sub_Node of (expression_node * expression_node)
  | Mult_Node of (expression_node * expression_node)
  | Div_Node of (expression_node * expression_node)
  | Variable_Node of {
      namespace: string; 
      parameters: expression_node list
  }

and list_node =
  | Nil
  | Node of expression_node * list_node

and statement_node = 
  | Import_Node of string
  | Let_Node of {
      namespace: string;
      block: expression_node
  }
  | Print_Node of expression_node
  | Println_Node of expression_node
  | Expr of expression_node

type program_node = statement_node list

let to_int_node = function
  | Integer_Node i -> Integer_Node i
  | Float_Node f -> Integer_Node (Float.to_int f)
  | String_Node s -> Integer_Node (int_of_string s)
  | Bool_Node true -> Integer_Node 1
  | Bool_Node false -> Integer_Node 0
  | _ -> raise (Error.Interpreter_Error "invalid type to convert to int")

let to_float_node = function
  | Float_Node f -> Float_Node f
  | Integer_Node i -> Float_Node (Float.of_int i)
  | String_Node s -> Float_Node (float_of_string s)
  | Bool_Node true -> Float_Node 1.0
  | Bool_Node false -> Float_Node 0.0
  | _ -> raise (Error.Interpreter_Error "invalid type to convert to float")

let to_string_node = function
  | String_Node s -> String_Node s
  | Integer_Node i -> String_Node (string_of_int i)
  | Float_Node f -> String_Node (string_of_float f)
  | Bool_Node true -> String_Node "true"
  | Bool_Node false -> String_Node "false"
  | _ -> raise (Error.Interpreter_Error "invalid type to convert to float")

let print_node (node: expression_node) : unit =
  match node with
  | Integer_Node n -> print_int n; print_newline ()
  | Float_Node f -> print_float f; print_newline ()
  | String_Node s -> print_endline s;
  | Variable_Node {namespace; _} -> print_endline namespace
  | Bool_Node b -> if b then print_endline "true" else print_endline "false"
  | List_Node _ -> print_endline "list"
  | _ -> print_endline "not printable type"


let rec string_of_expression = function
  | Negative_Node expr -> "(-" ^ string_of_expression expr ^ ")"
  | Block_Node { parameters; statements; expression; closure_scope } ->
      let params = String.concat ", " parameters in
      let stmts = String.concat "; " (List.map string_of_statement statements) in
      let closure = match closure_scope with
        | Some (vars, _exprs) -> "Closure(" ^ (String.concat ", " vars) ^ ": " ^ (String.concat "," (List.map (fun x -> string_of_expression x) _exprs)) ^ ")"
        | None -> "None"
      in
      "Block { params: [" ^ params ^ "]; stmts: [" ^ stmts ^ "]; expr: " ^ string_of_expression expression ^ "; closure: " ^ closure ^ " }"
  | Tagged_Node { tag; data } ->
      let data_str = match data with Some d -> string_of_expression d | None -> "None" in
      "Tagged(" ^ tag ^ ", " ^ data_str ^ ")"
  | Match_Node { expr; cases } ->
      "Match(" ^ string_of_expression expr ^ " with [" ^ (String.concat ", " (List.map string_of_expression cases)) ^ "])"
  | Case_Node { pattern; guard; block } ->
      let guard_str = match guard with Some g -> " | " ^ string_of_expression g | None -> "" in
      "Case(" ^ string_of_expression pattern ^ guard_str ^ " -> " ^ string_of_expression block ^ ")"
  | Cons_Node (hd, tl) -> "(" ^ string_of_expression hd ^ " :: " ^ string_of_expression tl ^ ")"
  | List_Node lst -> string_of_list lst
  | Bool_Node b -> string_of_bool b
  | Integer_Node i -> string_of_int i
  | Float_Node f -> string_of_float f
  | String_Node s -> "\"" ^ s ^ "\""
  | If_Node { condition; then_block; else_block } ->
      "If(" ^ string_of_expression condition ^ " then " ^ string_of_expression then_block ^ " else " ^ string_of_expression else_block ^ ")"
  | Greater_Node (a, b) -> "(" ^ string_of_expression a ^ " > " ^ string_of_expression b ^ ")"
  | Less_Node (a, b) -> "(" ^ string_of_expression a ^ " < " ^ string_of_expression b ^ ")"
  | Eq_Node (a, b) -> "(" ^ string_of_expression a ^ " == " ^ string_of_expression b ^ ")"
  | Greq_Node (a, b) -> "(" ^ string_of_expression a ^ " >= " ^ string_of_expression b ^ ")"
  | Leq_Node (a, b) -> "(" ^ string_of_expression a ^ " <= " ^ string_of_expression b ^ ")"
  | Not_Eq_Node (a, b) -> "(" ^ string_of_expression a ^ " != " ^ string_of_expression b ^ ")"
  | Add_Node (a, b) -> "(" ^ string_of_expression a ^ " + " ^ string_of_expression b ^ ")"
  | Sub_Node (a, b) -> "(" ^ string_of_expression a ^ " - " ^ string_of_expression b ^ ")"
  | Mult_Node (a, b) -> "(" ^ string_of_expression a ^ " * " ^ string_of_expression b ^ ")"
  | Div_Node (a, b) -> "(" ^ string_of_expression a ^ " / " ^ string_of_expression b ^ ")"
  | Variable_Node { namespace; parameters } ->
      let params_str = match parameters with
        | [] -> ""
        | ps -> "(" ^ String.concat ", " (List.map string_of_expression ps) ^ ")"
      in
      "Variable(" ^ namespace ^ params_str ^ ")"

and string_of_list = function
  | Nil -> "[]"
  | Node (hd, tl) -> "(" ^ string_of_expression hd ^ " :: " ^ string_of_list tl ^ ")"

and string_of_statement = function
  | Import_Node name -> "Import(" ^ name ^ ")"
  | Let_Node { namespace; block } -> "Let(" ^ namespace ^ " = " ^ string_of_expression block ^ ")"
  | Print_Node expr -> "Print(" ^ string_of_expression expr ^ ")"
  | Println_Node expr -> "Println(" ^ string_of_expression expr ^ ")"
  | Expr expr -> string_of_expression expr
