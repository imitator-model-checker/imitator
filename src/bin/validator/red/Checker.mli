open Lib
open ParsingStructure
open Options

val counter_example_predicate : options_and_properties:(Options.imitator_options * ParsingStructure.parsed_property option) list -> parsed_model ->
    bool