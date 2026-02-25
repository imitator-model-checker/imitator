open Lib
open Model_gen
open Comp
open Runner
open Red
open Validator_config

module ImitatorOptions = Options

let default_options () =
  let options = new ImitatorOptions.imitator_options in 
  Arg.current := 0;
  options#parse ~skip_model:true ~from_arg_list:(Some [|"";"-verbose=mute"|]) ();
  Input.set_options options;
  options
let uuid () = (Unix.gettimeofday () |> string_of_float |> Digest.string |> Digest.to_hex |> String.sub) 0 8


let () = 
  let validator_options = ValidatorOptions.parse () in 

  let printer = Printer.create () in 

  let {spec;mode} : Config.t = Parser.parse_file validator_options.validator_file  in

  let random = Random.State.make_self_init () in

  match mode with 
  | SampleGenerator {pdf; samples} -> 
    let options = default_options () in
    let sample_number = ref 1 in 
    Printer.info printer "Sampling %d model(s) into %s/" samples validator_options.output_folder_path;
    let samples = QCheck2.Gen.generate ~rand:random ~n:samples (ModelGen.parsed_model spec) in
    List.iter 
      (fun parsed_model -> 
        let model, _ = ModelConverter.abstract_structures_of_parsing_structures options parsed_model None in
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
          ConfigLoader.build_imitator_options_and_property_from_args config ~validator_options
        ) configs
    in

    Printer.start_section printer "Searching for counter example";

    let predicate parsed_model : CounterExampleFinder.predicate_result = 
      let results = 
        List.map 
          (fun (options, parsed_property_option) -> 
            let result, _ = ModelRunner.run options parsed_model parsed_property_option in 
            result
          ) options_and_properties
      in

      match Comparison.eq_results results with 
      | Equal -> True
      | Time_out -> Time_out
      | Not_Equal -> False
      | Incomparable -> Printer.end_live printer; 
        Printer.error printer "ERROR: Incomparable constraint types"; exit 1
      | Error -> Printer.end_live printer; 
        Printer.error printer "ERROR: One or more imitator runs resulted in an error"; exit 1
      | Not_supported -> Printer.end_live printer; 
        Printer.error printer "ERROR: This type of comparison is not supported yet! You can implement it in `Comparison.ml`"; exit 1
    in
    let result = CounterExampleFinder.find ~predicate ~printer ~reduce:true spec validator_options in

    match result with 
    | CounterExample {full_parsed_model; reduced_parsed_model} -> 
      let output_folder = Printf.sprintf "%s/counter_examples" validator_options.output_folder_path in
      let base_file_name = Printf.sprintf "counter_example_%s" @@ uuid () in 

      let abstract_model, _ = ModelConverter.abstract_structures_of_parsing_structures default_options full_parsed_model None in 
      Printer.info printer "Saving full counter example as %s/%s.imi" output_folder base_file_name; 
      ModelOutput.output_model ~file_name:base_file_name ~output_folder default_options abstract_model;

      let reduced_parsed_model = match reduced_parsed_model with Some m -> m | None -> full_parsed_model in
      let reduced_model, _ = ModelConverter.abstract_structures_of_parsing_structures default_options reduced_parsed_model None in 
      let red_file_name = Printf.sprintf "%s_reduced" base_file_name in
      Printer.info printer "Saving reduced counter example as %s/%s.imi" output_folder red_file_name; 
      ModelOutput.output_model ~file_name:red_file_name ~output_folder default_options reduced_model
    | No_CounterExample -> ())
  | PropertyGuided properties -> (

    let default_options = default_options () in

    let property_string_sanitized property negate = 
      (if negate then "not_" else "") ^
      (property 
      |> String.trim
      |> (fun s -> if String.starts_with ~prefix:"property :=" s then String.sub s 11 (String.length s - 11) else s)
      |> (fun s -> if String.contains s '#' then String.sub s (String.index s '#'+1) (String.length s - String.index s '#' - 1) else s)
      |> String.map (fun c -> if c = ' ' then '_' else c)
      |> (fun s -> if String.ends_with ~suffix:";" s then String.sub s 0 (String.length s - 1) else s))
    in

    let properties_as_string = 
      properties 
      |> List.map (fun (p : Mode.property) -> property_string_sanitized p.property p.negated) 
      |> String.concat "__and__"
    in



    Printer.start_section printer "Searching for a PTA with non-empty witness set for the property";

    let predicate parsed_model : CounterExampleFinder.predicate_result = 
      properties
      |> List.map (fun (p : Mode.property) -> 
        let options, property = ConfigLoader.build_imitator_options_and_property_from_property_string p.property ~validator_options in
        let result, _ = ModelRunner.run options parsed_model property in 
        result, p.negated
      )
      |> Predicates.results_intersect 
      |> Predicates.negate
    in
    let result = CounterExampleFinder.find ~predicate ~printer ~reduce:true spec validator_options in

    match result with 
    | CounterExample {full_parsed_model; reduced_parsed_model} ->
      let output_folder = Printf.sprintf "%s/property_guided" validator_options.output_folder_path in
      let base_file_name = Printf.sprintf "%s" @@ properties_as_string in 

      let abstract_model, _ = ModelConverter.abstract_structures_of_parsing_structures default_options full_parsed_model None in 
      Printer.info printer "Saving full property guided PTA as %s/%s.imi" output_folder base_file_name; 
      ModelOutput.output_model ~file_name:base_file_name ~output_folder default_options abstract_model;

      let reduced_parsed_model = match reduced_parsed_model with Some m -> m | None -> full_parsed_model in
      let reduced_model, _ = ModelConverter.abstract_structures_of_parsing_structures default_options reduced_parsed_model None in 
      let red_file_name = Printf.sprintf "%s_REDUCED" base_file_name in
      Printer.info printer "Saving reduced property guided PTA as %s/%s.imi" output_folder red_file_name; 
      ModelOutput.output_model ~file_name:red_file_name ~output_folder default_options reduced_model
    | No_CounterExample -> ())
  | Reduce {model_path;configs} ->
    let options_and_properties = 
      List.map 
        (fun config -> 
          ConfigLoader.build_imitator_options_and_property_from_args config ~validator_options ~model_path
        ) configs
    in

    let (options_a, _) = List.hd options_and_properties in
    let default_options = default_options () in

    let unexpanded_parsed_model : ParsingStructure.unexpanded_parsed_model = ParsingUtility.compile_unexpanded_parsed_model options_a in
    let parsed_model = Templates.expand_model unexpanded_parsed_model in 

    Printer.info printer "Attempting to reduce while preserving counter example";

    let predicate parsed_model = 
      let results = 
        List.map 
          (fun (options, parsed_property_option) -> 
            let result, _ = ModelRunner.run options parsed_model parsed_property_option in 
            result
          ) options_and_properties
      in

      match Comparison.eq_results results with 
      | Equal -> true
      | Time_out -> true
      | Not_Equal -> false
      | Incomparable -> Printer.error printer "ERROR: Incomparable constraint types"; exit 1
      | Error -> Printer.error printer "ERROR: One or more imitator runs resulted in an error"; exit 1
      | Not_supported -> Printer.error printer "ERROR: This type of comparison is not supported yet! You can implement it in `Comparison.ml`"; exit 1
    in

    let reduced_parsed_model = ModelReducer.reduce parsed_model ~printer ~predicate in 
    let reduced_model, _ = ModelConverter.abstract_structures_of_parsing_structures default_options reduced_parsed_model None in 

    let output_folder = Printf.sprintf "%s/reducer" validator_options.output_folder_path in
    let file_name = Printf.sprintf "%s_reduced" options_a#files_prefix in 
    Printer.info printer "Saving reduced counter example as %s/%s.imi" output_folder file_name;
    ModelOutput.output_model ~file_name ~output_folder options_a reduced_model
