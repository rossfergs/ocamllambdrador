type expression_node =
    Integer_Node of int
  | Float_Node of float
  | String_Node of string
  | Add_Node of (expression_node * expression_node)
  | Sub_Node of (expression_node * expression_node)
  | Mult_Node of (expression_node * expression_node)
  | Div_Node of (expression_node * expression_node)
  | Variable_Node of { namespace : string; parameters : expression_node list;
    }
type statement_node =
    Let_Node of { namespace : string; block : block_node; }
  | Print_Node of expression_node
  | Expr of expression_node
and block_node = {
  parameters : string list;
  statements : statement_node list;
  expression : expression_node;
}
type program_node = statement_node list
