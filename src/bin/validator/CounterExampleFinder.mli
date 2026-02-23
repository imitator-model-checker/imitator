open Lib
open Validator_config


type predicate_result = 
  | True
  | False
  | Time_out

type predicate = ParsingStructure.parsed_model -> predicate_result

type counter_example =
  | CounterExample of {full_parsed_model : ParsingStructure.parsed_model; reduced_parsed_model : ParsingStructure.parsed_model option}
  | No_CounterExample

val find : predicate:predicate -> 
            printer:Printer.t -> 
            reduce:bool ->
            Spec.t -> 
            ValidatorOptions.t -> 
            counter_example