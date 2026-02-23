open Lib

val reduce : ParsingStructure.parsed_model -> 
             predicate:(ParsingStructure.parsed_model -> bool) ->
             printer:Printer.t ->
             ParsingStructure.parsed_model