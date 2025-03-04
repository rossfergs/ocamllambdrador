type expression_node =
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
  | _ -> raise (Error.Interpreter_Error "invalid type to convert to int")

let to_float_node = function
  | Float_Node f -> Float_Node f
  | Integer_Node i -> Float_Node (Float.of_int i)
  | String_Node s -> Float_Node (float_of_string s)
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
