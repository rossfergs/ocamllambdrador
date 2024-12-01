
module String_map = Map.Make(String)

type scope = 
  {inner_scope: Parse_node.block_node String_map.t; outer_scope: scope option}

let bind nis cs = 
  { inner_scope = nis; outer_scope = cs.outer_scope }


