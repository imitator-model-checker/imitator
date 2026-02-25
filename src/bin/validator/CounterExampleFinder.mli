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

type run_stats = {
  total_runs_completed : int;
  run_found : int option;
  time_to_find_seconds : float option;
  initial_locations : int option;
  initial_transitions : int option;
}

val nb_locations : ParsingStructure.parsed_model -> int
val nb_transitions : ParsingStructure.parsed_model -> int

val find : predicate:predicate ->
            printer:Printer.t ->
            reduce:bool ->
            Spec.t ->
            ValidatorOptions.t ->
            counter_example * run_stats
