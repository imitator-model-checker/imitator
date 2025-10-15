open Lib

val reduce : ParsingStructure.parsed_model -> 
             options_a:Options.imitator_options ->
             options_b:Options.imitator_options ->
             parsed_property_option_a:ParsingStructure.parsed_property option ->
             parsed_property_option_b:ParsingStructure.parsed_property option ->
             original_nb_transitions:int -> 
             ParsingStructure.parsed_model