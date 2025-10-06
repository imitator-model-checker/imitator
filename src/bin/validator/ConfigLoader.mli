open Lib

module ImitatorOptions = Options

val build_imitator_options_and_property : imitator_args_file:string -> ImitatorOptions.imitator_options * ParsingStructure.parsed_property option
