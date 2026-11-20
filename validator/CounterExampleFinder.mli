type predicate_result =
  | True
  | False
  | Time_out

type predicate = ParsingStructure.parsed_model -> predicate_result

type counter_example =
  | CounterExample of {full_parsed_model : ParsingStructure.parsed_model; reduced_parsed_model : ParsingStructure.parsed_model option}
  | No_CounterExample
  (* The predicate raised an exception on this model *)
  | Crash of {parsed_model : ParsingStructure.parsed_model; exn : exn}

type run_stats = {
  total_runs_completed : int;
  run_found : int option;
  time_to_find_seconds : float option;
  initial_locations : int option;
  initial_transitions : int option;
}

val nb_locations : ParsingStructure.parsed_model -> int
val nb_transitions : ParsingStructure.parsed_model -> int

(* [on_found] is called on the counter example before it is reduced *)
val find : predicate:predicate ->
            printer:Printer.t ->
            ?on_found:(ParsingStructure.parsed_model -> unit) ->
            reduce:bool ->
            Spec.t ->
            ValidatorOptions.t ->
            counter_example * run_stats
