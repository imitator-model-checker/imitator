open Lib

module ImitatorOptions = Options

val build_imitator_options_and_property_from_args : ?model_path:string -> string array -> validator_options:ValidatorOptions.t -> ImitatorOptions.imitator_options * ParsingStructure.parsed_property option
