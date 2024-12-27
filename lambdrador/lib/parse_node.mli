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
    Let_Node of { namespace : string; block : block_node; }
  | Print_Node of expression_node
  | Expr of expression_node
and block_node = {
  parameters : string list;
  statements : statement_node list;
  expression : expression_node;
}
type program_node = statement_node list
val to_int_node : expression_node -> expression_node
val to_float_node : expression_node -> expression_node
