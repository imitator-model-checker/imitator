open Lib

val reduce : ParsingStructure.parsed_model -> 
             printer:Printer.t ->
             options_and_properties:(Options.imitator_options * ParsingStructure.parsed_property option) list  ->
             ParsingStructure.parsed_model