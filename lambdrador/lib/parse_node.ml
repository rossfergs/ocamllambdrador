type expression_node =
  | Bool_Node of bool
  | Integer_Node of int
  | Float_Node of float 
  | String_Node of string
  | If_node of {condition: expression_node; then_block: block_node; else_block: block_node}
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
  | Variable_Node of {namespace: string; parameters: expression_node list}

and statement_node = 
  | Let_Node of {namespace: string; block: block_node}
  | Print_Node of expression_node
  | Expr of expression_node

and block_node = 
  {parameters: string list; statements: statement_node list; expression:expression_node}

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

