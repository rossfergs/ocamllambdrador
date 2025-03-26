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
  | Let_Node of { namespace : string; block : expression_node; recursive: bool }
  | Print_Node of expression_node
  | Println_Node of expression_node
  | Expr of expression_node

type program_node = statement_node list

val to_int_node : expression_node -> expression_node
val to_float_node : expression_node -> expression_node
val to_string_node : expression_node -> expression_node
val print_node : expression_node -> unit
