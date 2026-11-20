(* Raised by [reduce] if the model does not satisfy the predicate *)
exception Predicate_not_satisfied

val reduce : ParsingStructure.parsed_model -> 
             predicate:(ParsingStructure.parsed_model -> bool) ->
             printer:Printer.t ->
             ParsingStructure.parsed_model