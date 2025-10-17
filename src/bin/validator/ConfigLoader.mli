open Lib

module ImitatorOptions = Options

val build_imitator_options_and_property : ?model_file:string -> string -> validator_options:ValidatorOptions.t -> ImitatorOptions.imitator_options * ParsingStructure.parsed_property option
