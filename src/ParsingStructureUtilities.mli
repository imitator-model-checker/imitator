open ParsingStructure

(* General function to map leafs into anything and return the mapped leaf to a list *)
val map_parsed_arithmetic_expression_leafs : (parsed_discrete_factor -> 'a) -> parsed_discrete_arithmetic_expression -> 'a list