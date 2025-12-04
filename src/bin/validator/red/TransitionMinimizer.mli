open Lib
open ParsingStructure

val minimize : parsed_model -> predicate:(parsed_model -> bool) -> printer:Printer.t -> parsed_model