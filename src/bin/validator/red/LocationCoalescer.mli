open Lib
open ParsingStructure

val coalesce : predicate:(parsed_model -> bool) -> parsed_model -> parsed_model