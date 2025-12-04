open Lib

val reduce : ParsingStructure.parsed_model -> 
             printer:Printer.t ->
             options_a:Options.imitator_options ->
             options_b:Options.imitator_options ->
             parsed_property_option_a:ParsingStructure.parsed_property option ->
             parsed_property_option_b:ParsingStructure.parsed_property option ->
             ParsingStructure.parsed_model