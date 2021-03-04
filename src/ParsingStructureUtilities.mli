open ParsingStructure

(* Map the leafs of an arithmetic expression according to map_function *)
(* Leafs are Parsed_DF_variable, Parsed_DF_constant *)
val map_parsed_arithmetic_expression_leafs : (parsed_discrete_factor -> 'a) -> parsed_discrete_arithmetic_expression -> 'a list

