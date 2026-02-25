open Lib
open Validator_config
open Model_gen
open Red

let nb_transitions (parsed_model : ParsingStructure.parsed_model) =
  parsed_model.automata
  |> List.fold_left (fun acc (_, _, locations) ->
    acc + List.fold_left (fun acc_loc (location : ParsingStructure.parsed_location) ->
      acc_loc + List.length location.transitions
    ) 0 locations
  ) 0


let nb_locations (parsed_model : ParsingStructure.parsed_model) =
  parsed_model.automata
  |> List.fold_left (fun acc (_, _, locations) ->
    acc + List.length locations
  ) 0

type predicate_result =
  | True
  | False
  | Time_out

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

type predicate = ParsingStructure.parsed_model -> predicate_result

let find ~predicate ~printer ~reduce (spec : Spec.t) (options : ValidatorOptions.t) =
  let random =
    match options.seed with
    | None -> Random.State.make_self_init ()
    | Some n -> Random.State.make [|n|]
  in
  let i = ref 0 in
  let time_outs = ref 0 in
  let start_time = Unix.gettimeofday () in
  let found_at_run = ref None in
  let time_to_find = ref None in
  let initial_size = ref None in
  Printer.start_live printer;
  let cell = QCheck2.Test.make_cell ~count:options.repetitions (ModelGen.parsed_model spec) (fun parsed_model ->
    Printer.info printer "[%d | TO: %d]" (!i + 1) !time_outs;
    incr i;

    let result = predicate parsed_model in
    match result with
    | True -> true
    | False ->
      if !found_at_run = None then begin
        found_at_run := Some !i;
        time_to_find := Some (Unix.gettimeofday () -. start_time);
        initial_size := Some (nb_locations parsed_model, nb_transitions parsed_model)
      end;
      false
    | Time_out -> incr time_outs; true
  ) in
  let result = QCheck2.Test.check_cell ~rand:random cell in
  let state = QCheck2.TestResult.get_state result in
  let stats = {
    total_runs_completed = !i;
    run_found = !found_at_run;
    time_to_find_seconds = !time_to_find;
    initial_locations = Option.map fst !initial_size;
    initial_transitions = Option.map snd !initial_size;
  } in
  match state with
    | Failed {instances} -> (
      let counter_example = List.hd instances in
      let parsed_model = counter_example.instance in

      let full_nb_locations = nb_locations parsed_model in
      let full_nb_transitions = nb_transitions parsed_model in

      Printer.end_live printer;
      Printer.info printer "Found model with %d locations and %d transitions" full_nb_locations full_nb_transitions;


      if reduce then (
        let negated_predicate = fun m -> match predicate m with True -> false | False -> true | Time_out -> false in
        let reduced_parsed_model = ModelReducer.reduce parsed_model ~predicate:negated_predicate ~printer in

        let reduced_nb_locations = nb_locations reduced_parsed_model in
        let reduced_nb_transitions = nb_transitions reduced_parsed_model in
        Printer.info printer "Reduced model to %d locations and %d transitions" reduced_nb_locations reduced_nb_transitions;
        CounterExample {full_parsed_model = parsed_model; reduced_parsed_model = Some reduced_parsed_model}, stats
      )
      else
        CounterExample {full_parsed_model = parsed_model; reduced_parsed_model = None}, stats)
    | _ -> No_CounterExample, stats
