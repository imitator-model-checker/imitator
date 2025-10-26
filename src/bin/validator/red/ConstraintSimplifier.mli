open Lib
open ParsingStructure

val simplify : predicate:(parsed_model -> bool) -> parsed_model ->  parsed_model