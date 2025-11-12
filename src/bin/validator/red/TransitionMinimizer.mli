open Lib
open ParsingStructure

val minimize : parsed_model -> original_nb_transitions:int -> predicate:(parsed_model -> bool) -> printer:Printer.t -> parsed_model