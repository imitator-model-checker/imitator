open Lib
open ParsingStructure

val coalesce : predicate:(parsed_model -> bool) -> parsed_model -> printer:Printer.t -> parsed_model