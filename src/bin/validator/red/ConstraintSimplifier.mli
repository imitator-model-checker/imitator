open Lib
open ParsingStructure

val simplify : predicate:(parsed_model -> bool) -> parsed_model -> printer:Printer.t ->  parsed_model