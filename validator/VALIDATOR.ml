open Lib

module ImitatorOptions = Options

let default_options () =
  let options = new ImitatorOptions.imitator_options in
    (* Models are saved with these options: keep the variables that they do not use *)
    options#parse ~skip_model:true ~argv:[|"";"-verbose=mute";"-no-var-autoremove"|] ();
  Input.set_options options;
  options
let uuid () = (Printf.sprintf "%.6f %d" (Unix.gettimeofday ()) (Unix.getpid ()) |> Digest.string |> Digest.to_hex |> String.sub) 0 8

let json_string s =
  let buffer = Buffer.create (String.length s + 2) in
  Buffer.add_char buffer '"';
  String.iter (function
    | '"' -> Buffer.add_string buffer "\\\""
    | '\\' -> Buffer.add_string buffer "\\\\"
    | '\n' -> Buffer.add_string buffer "\\n"
    | c when Char.code c < 0x20 -> Buffer.add_string buffer (Printf.sprintf "\\u%04x" (Char.code c))
    | c -> Buffer.add_char buffer c
  ) s;
  Buffer.add_char buffer '"';
  Buffer.contents buffer

let json_opt_int = function None -> "null" | Some n -> string_of_int n
let json_opt_float = function None -> "null" | Some f -> Printf.sprintf "%.6g" f
let json_opt_string = function None -> "null" | Some s -> json_string s
let json_bool b = if b then "true" else "false"

(* [stats] is not known when IMITATOR ends the process *)
let write_json path (validator_options : ValidatorOptions.t)
    (stats : CounterExampleFinder.run_stats option)
    ~found ~final_locations ~final_transitions ~counterexample_file ~error ~crash_file =
  let stat f = Option.bind stats f in
  let oc = open_out path in
  Printf.fprintf oc "{\n";
  Printf.fprintf oc "  \"seed\": %s,\n" (json_opt_int validator_options.seed);
  Printf.fprintf oc "  \"found\": %s,\n" (json_bool found);
  Printf.fprintf oc "  \"run_found\": %s,\n" (json_opt_int (stat (fun s -> s.run_found)));
  Printf.fprintf oc "  \"total_runs_completed\": %s,\n" (json_opt_int (stat (fun s -> Some s.total_runs_completed)));
  Printf.fprintf oc "  \"time_to_find_seconds\": %s,\n" (json_opt_float (stat (fun s -> s.time_to_find_seconds)));
  Printf.fprintf oc "  \"initial_locations\": %s,\n" (json_opt_int (stat (fun s -> s.initial_locations)));
  Printf.fprintf oc "  \"initial_transitions\": %s,\n" (json_opt_int (stat (fun s -> s.initial_transitions)));
  Printf.fprintf oc "  \"final_locations\": %s,\n" (json_opt_int final_locations);
  Printf.fprintf oc "  \"final_transitions\": %s,\n" (json_opt_int final_transitions);
  Printf.fprintf oc "  \"counterexample_file\": %s,\n" (json_opt_string counterexample_file);
  Printf.fprintf oc "  \"error\": %s,\n" (json_opt_string error);
  Printf.fprintf oc "  \"crash_file\": %s\n" (json_opt_string crash_file);
  Printf.fprintf oc "}\n";
  close_out oc

let write_results_json path validator_options stats ~found ~final_locations ~final_transitions ~counterexample_file =
  write_json path validator_options (Some stats)
    ~found ~final_locations ~final_transitions ~counterexample_file ~error:None ~crash_file:None

(* Results of a run that ended because of a failure on a model *)
let write_failure_json (validator_options : ValidatorOptions.t) stats ~error ~crash_file =
  match validator_options.results_file with
  | None -> ()
  | Some path ->
    write_json path validator_options stats
      ~found:false ~final_locations:None ~final_transitions:None ~counterexample_file:None ~error:(Some error) ~crash_file

let fail printer message =
  Printer.end_live printer;
  Printer.error printer "ERROR: %s" message;
  exit 1

let message_of_exception = function
  | ParsingDriver.ParsingFailure message | Failure message -> message
  (* The message of Arg.Bad is followed by the whole usage text *)
  | Arg.Bad message -> List.hd (String.split_on_char '\n' message)
  (* Raised once IMITATOR has printed the actual error *)
  | Exceptions.InternalError _ -> "see the error above"
  | e -> Printexc.to_string e

let fail_config printer config e =
  fail printer (Printf.sprintf "the config `%s` could not be loaded: %s" (String.concat " " (Array.to_list config)) (message_of_exception e))

(* Convert a generated model and save it as <output_folder>/<file_name>.imi *)
let save_model ~printer ~what ~output_folder ~file_name options parsed_model =
  let model, _ = ModelRunner.convert options parsed_model None in
  Printer.info printer "Saving %s as %s/%s.imi" what output_folder file_name;
  ModelOutput.output_model ~file_name ~output_folder options model

(* The model IMITATOR is currently running on *)
let running_model : ParsingStructure.parsed_model option ref = ref None

exception Imitator_error of string

let run options parsed_model parsed_property =
  running_model := Some parsed_model;
  let result, _ =
    Fun.protect ~finally:(fun () -> running_model := None)
      (fun () -> ModelRunner.run options parsed_model parsed_property)
  in
  match result with
  | Result.Error_result _ -> raise (Imitator_error "IMITATOR returned an error result")
  | _ -> result

(* A run stopped by a time, depth or states limit gives a partial result: the model is skipped *)
let is_partial (result : Result.imitator_result) =
  match result with
  | Single_synthesis_result {termination = Time_limit _ | Depth_limit _ | States_limit _; _} -> true
  | _ -> false

(* Results that cannot be compared are reported like a failure of IMITATOR *)
let compare_results results : Comparison.result =
  if List.exists is_partial results then Time_out else
  match Comparison.eq_results results with
  | Incomparable -> raise (Imitator_error "the results of the configs have incomparable constraint types")
  | Error -> raise (Imitator_error "IMITATOR returned an error result")
  | Not_supported -> raise (Imitator_error "the configs give a type of result that cannot be compared yet! You can implement it in `Comparison.ml`")
  | exception Invalid_argument _ -> raise (Imitator_error "the results of the configs are not over the same parameters (does only one of them use -no-var-autoremove?)")
  | result -> result

let save_crash ~printer ~(validator_options : ValidatorOptions.t) parsed_model =
  let output_folder = Printf.sprintf "%s/crashes" validator_options.output_folder_path in
  let file_name = Printf.sprintf "crash_%s" @@ uuid () in
  try
    save_model ~printer ~what:"the model" ~output_folder ~file_name (default_options ()) parsed_model;
    Some (Printf.sprintf "%s/%s.imi" output_folder file_name)
  with e -> Printer.error printer "The model could not be saved: %s" (Printexc.to_string e); None

(* Some errors make IMITATOR end the process instead of raising an exception: save the model it was running on *)
let save_running_model_at_exit ~printer ~validator_options =
  at_exit (fun () ->
    match !running_model with
    | None -> ()
    | Some parsed_model ->
      running_model := None;
      Printer.end_live printer;
      let error = "IMITATOR ended the process while running on a model" in
      Printer.error printer "ERROR: %s" error;
      let crash_file = save_crash ~printer ~validator_options parsed_model in
      write_failure_json validator_options None ~error ~crash_file)

let report_crash ~printer ~validator_options ~stats parsed_model exn =
  let message =
    match exn with
    | Imitator_error message -> message
    | ModelConverter.InvalidProperty -> "the property is not valid for this model (does it refer to a location that some generated models do not have?)"
    | exn -> "IMITATOR raised the exception " ^ Printexc.to_string exn
  in
  Printer.error printer "ERROR on a model: %s" message;
  let crash_file = save_crash ~printer ~validator_options parsed_model in
  write_failure_json validator_options (Some stats) ~error:message ~crash_file;
  exit 1

let main () =
  let validator_options = ValidatorOptions.parse () in

  let printer = Printer.create () in

  let {spec;mode} : Config.t = Parser.parse_file validator_options.validator_file  in

  (* Without -seed, choose a seed and print it so that the run can be reproduced *)
  let seed =
    match validator_options.seed, mode with
    | Some seed, _ -> seed
    | None, Reduce _ -> 0
    | None, _ ->
      Random.self_init ();
      let seed = Random.bits () in
      Printer.info printer "Seed: %d" seed;
      seed
  in
  let validator_options = {validator_options with seed = Some seed} in
  let random = Random.State.make [|seed|] in

  ModelOutput.create_folder validator_options.output_folder_path;

  save_running_model_at_exit ~printer ~validator_options;

  match mode with
  | SampleGenerator {pdf; samples} ->
    let options = default_options () in
    let sample_number = ref 1 in
    Printer.info printer "Sampling %d model(s) into %s/" samples validator_options.output_folder_path;
    let samples = QCheck2.Gen.generate ~rand:random ~n:samples (ModelGen.parsed_model spec) in
    List.iter
      (fun parsed_model ->
        let model, _ = ModelRunner.convert options parsed_model None in
        let output_folder = Printf.sprintf "%s/samples" validator_options.output_folder_path in
        let file_name = Printf.sprintf "sampled_model_%d" !sample_number in
          ModelOutput.output_model
          ~draw:pdf
          ~file_name
          ~output_folder
          options
          model;
        incr sample_number
      ) samples;

  | Compare {configs} -> (
    let default_options = default_options () in

    let options_and_properties =
      List.map
        (fun config ->
          try ConfigLoader.build_imitator_options_and_property_from_args config ~validator_options
          with e -> fail_config printer config e
        ) configs
    in
    if List.exists (fun (_, property) -> property = None) options_and_properties then
      fail printer "every config must contain a property file";

    Printer.start_section printer "Searching for counter example";

    let predicate parsed_model : CounterExampleFinder.predicate_result =
      let results =
        List.map
          (fun (options, parsed_property_option) ->
            run options parsed_model parsed_property_option
          ) options_and_properties
      in

      match compare_results results with
      | Equal -> True
      | Time_out -> Time_out
      | _ -> False
    in

    let output_folder = Printf.sprintf "%s/counter_examples" validator_options.output_folder_path in
    let base_file_name = Printf.sprintf "counter_example_%s" @@ uuid () in
    let on_found = save_model ~printer ~what:"full counter example" ~output_folder ~file_name:base_file_name default_options in

    let result, stats = CounterExampleFinder.find ~predicate ~printer ~on_found ~reduce:true spec validator_options in

    match result with
    | CounterExample {full_parsed_model; reduced_parsed_model} ->
      let reduced_parsed_model = match reduced_parsed_model with Some m -> m | None -> full_parsed_model in
      let red_file_name = Printf.sprintf "%s_reduced" base_file_name in
      save_model ~printer ~what:"reduced counter example" ~output_folder ~file_name:red_file_name default_options reduced_parsed_model;

      let final_locations = Some (CounterExampleFinder.nb_locations reduced_parsed_model) in
      let final_transitions = Some (CounterExampleFinder.nb_transitions reduced_parsed_model) in
      let counterexample_file = Some (Printf.sprintf "%s/%s.imi" output_folder red_file_name) in
      (match validator_options.results_file with
      | None -> ()
      | Some path ->
        write_results_json path validator_options stats
          ~found:true ~final_locations ~final_transitions ~counterexample_file)
    | No_CounterExample ->
      Printer.info printer "No counter example found in %d models" stats.total_runs_completed;
      (match validator_options.results_file with
      | None -> ()
      | Some path ->
        write_results_json path validator_options stats
          ~found:false ~final_locations:None ~final_transitions:None ~counterexample_file:None)
    | Crash {parsed_model; exn} -> report_crash ~printer ~validator_options ~stats parsed_model exn)
  | Coverage ->
    let gen_of_dist (d : Spec.dist) =
      let open QCheck2.Gen in
      match d with
      | Exact n -> pure n
      | Range (min, max) -> int_range ~origin:min min max
    in
    let guard_gen =
      let open QCheck2.Gen in
      let* nb_clocks = gen_of_dist spec.nb_clocks
      and* nb_parameters = gen_of_dist spec.nb_parameters in
      let seed = PZone.top ~nb_clocks ~nb_parameters in
      PZoneGen.gen ~nb_clocks ~nb_parameters ~max_constant:spec.max_constant ~seed
    in
    let state = CoverageAnalyzer.create () in
    for _ = 1 to validator_options.repetitions do
      let guard = QCheck2.Gen.generate1 ~rand:random guard_gen in
      CoverageAnalyzer.update state guard
    done;
    CoverageAnalyzer.report state ~results_file:validator_options.results_file

  | Reduce {model_path;configs} ->
    let options_and_properties =
      List.map
        (fun config ->
          try ConfigLoader.build_imitator_options_and_property_from_args config ~validator_options ~model_path
          with e -> fail_config printer config e
        ) configs
    in
    if List.exists (fun (_, property) -> property = None) options_and_properties then
      fail printer "every config must contain a property file";

    let (options_a, _) = List.hd options_and_properties in
    let default_options = default_options () in

    let parsed_model =
      try Templates.expand_model (ParsingUtility.compile_unexpanded_parsed_model options_a)
      with e -> fail printer (Printf.sprintf "the model `%s` could not be loaded: %s" model_path (message_of_exception e))
    in

    Printer.info printer "Attempting to reduce while preserving counter example";

    (* A candidate on which IMITATOR fails or the results cannot be compared is rejected *)
    let predicate parsed_model =
      match
        List.map
          (fun (options, parsed_property_option) ->
            run options parsed_model parsed_property_option
          ) options_and_properties
        |> compare_results
      with
      | Not_Equal -> true
      | _ | exception _ -> false
    in

    let reduced_parsed_model =
      try ModelReducer.reduce parsed_model ~printer ~predicate
      with ModelReducer.Predicate_not_satisfied ->
        fail printer "the configs give the same result on this model (or IMITATOR fails or times out on it): there is nothing to reduce"
    in

    let output_folder = Printf.sprintf "%s/reducer" validator_options.output_folder_path in
    let file_name = Printf.sprintf "%s_reduced" (Filename.basename options_a#files_prefix) in
    let reduced_model, _ = ModelRunner.convert default_options reduced_parsed_model None in
    Printer.info printer "Saving reduced counter example as %s/%s.imi" output_folder file_name;
    ModelOutput.output_model ~file_name ~output_folder options_a reduced_model

let () =
  let fail message = prerr_endline ("Error: " ^ message); exit 1 in
  try main () with
  | ParsingUtil.Parse_error message -> fail ("invalid specification file: " ^ message)
  | ParsingDriver.ParsingFailure message -> fail ("invalid property: " ^ message)
  | Sys_error message -> fail message
  | Unix.Unix_error (error, _, path) -> fail (path ^ ": " ^ Unix.error_message error)
  | Failure message -> fail message
