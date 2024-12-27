val parse : string -> Parse_node.program_node
val parse_block : string -> int -> input_params: string list -> statement_list: Parse_node.statement_node list -> Parse_node.block_node * int 
