open Lib
open ParsingStructure
open Options

val counter_example_predicate : options_a:imitator_options -> options_b:imitator_options -> 
    parsed_property_option_a:parsed_property option -> parsed_property_option_b:parsed_property option -> parsed_model ->
    bool