type scope = {
  inner_scope : (string, Parse_node.block_node) Hashtbl.t;
  outer_scope : scope option;
}
