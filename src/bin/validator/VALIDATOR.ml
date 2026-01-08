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
  options

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
    let i = ref 0 in 
    let time_outs = ref 0 in 
    let default_options = default_options () in

    let options_and_properties = 
      List.map 
        (fun config -> 
          ConfigLoader.build_imitator_options_and_property config ~validator_options
        ) configs
    in

    Printer.start_section printer "Searching for counter example";
    Printer.start_live printer;
    let cell = QCheck2.Test.make_cell ~count:validator_options.repetitions (ModelGen.parsed_model spec) (fun parsed_model ->

      Printer.info printer "[%d | TO: %d]" (!i + 1) !time_outs;
      incr i;

      let results = 
        List.map 
          (fun (options, parsed_property_option) -> 
            let result, _ = ModelRunner.run options parsed_model parsed_property_option in 
            result
          ) options_and_properties
      in

      match Comparison.eq_results results with 
      | Equal -> true
      | Time_out -> incr time_outs; true
      | Not_Equal -> false
      | Incomparable -> Printer.end_live printer; 
        Printer.error printer "ERROR: Incomparable constraint types"; exit 1
      | Error -> Printer.end_live printer; 
        Printer.error printer "ERROR: One or more imitator runs resulted in an error"; exit 1
      | Not_supported -> Printer.end_live printer; 
        Printer.error printer "ERROR: This type of comparison is not supported yet! You can implement it in `Comparison.ml`"; exit 1
    ) in 

    let result = QCheck2.Test.check_cell ~rand:random cell in 
    let state = QCheck2.TestResult.get_state result in 
    match state with 
    | Success -> ()
    | Failed {instances} -> 
      let counter_example = List.hd instances in 
      let parsed_model = counter_example.instance in 
      let nb_locations = nb_locations parsed_model in
      let nb_transitions = nb_transitions parsed_model in
      Printer.end_live printer;
      Printer.info printer "Found counter example with %d locations and %d transitions" nb_locations nb_transitions;
      
      Printer.info printer "Attempting to reduce ... ";
      let reduced_parsed_model = ModelReducer.reduce parsed_model ~printer ~options_and_properties in 
      let reduced_model, _ = ModelConverter.abstract_structures_of_parsing_structures default_options reduced_parsed_model None in 
      Printer.info printer "Reduced model to %d locations and %d transitions" reduced_model.nb_locations reduced_model.nb_transitions;

      let output_folder = Printf.sprintf "%s/counter_examples" validator_options.output_folder_path in
      let file_name = Printf.sprintf "counter_example_%d" !i in 
      Printer.info printer "Saving reduced counter example as %s/%s.imi" output_folder file_name; 
      ModelOutput.output_model ~file_name ~output_folder default_options reduced_model
    | _ -> ())
  | Reduce {model_path;configs} ->
    let options_and_properties = 
      List.map 
        (fun config -> 
          ConfigLoader.build_imitator_options_and_property config ~validator_options ~model_path
        ) configs
    in

    let (options_a, _) = List.hd options_and_properties in
    let default_options = default_options () in

    let unexpanded_parsed_model : ParsingStructure.unexpanded_parsed_model = ParsingUtility.compile_unexpanded_parsed_model options_a in
    let parsed_model = Templates.expand_model unexpanded_parsed_model in 

    let nb_locations = nb_locations parsed_model in
    let nb_transitions = nb_transitions parsed_model in
    Printer.info printer "Reducer provided with a counter example with %d locations and %d transitions" nb_locations nb_transitions;
    Printer.info printer "Attempting to reduce while preserving counter example";
    let reduced_parsed_model = ModelReducer.reduce parsed_model ~printer ~options_and_properties in 
    let reduced_model, _ = ModelConverter.abstract_structures_of_parsing_structures default_options reduced_parsed_model None in 
    Printer.info printer "Reduced model to %d locations and %d transitions" reduced_model.nb_locations reduced_model.nb_transitions;

    let output_folder = Printf.sprintf "%s/reducer" validator_options.output_folder_path in
    let file_name = Printf.sprintf "%s_reduced" options_a#files_prefix in 
    Printer.info printer "Saving reduced counter example as %s/%s.imi" output_folder file_name;
    ModelOutput.output_model ~file_name ~output_folder options_a reduced_model
