open Lib
open Model_gen
open Comp
open Runner
open Red
open Validator_spec

module ImitatorOptions = Options


let () = 
  let validator_args = ArgStash.stash_and_retrieve ValidatorOptions.arg_list in
  let validator_options = ValidatorOptions.parse validator_args in 
  Arg.current := 0;
  
  let mode = validator_options.mode in 

  let printer = Printer.create () in 

  let spec = Parse.parse_file validator_options.validator_file  in

  match mode with 
  | SampleModelGenerator {draw_pdf} -> 
    
    (* default options *)
    let options = new ImitatorOptions.imitator_options in 
    options#parse ~skip_model:true ~from_arg_list:(Some [|"";"-verbose=mute"|]) ();
    Arg.current := 0;
    Input.set_options options;

    let sample_number = ref 1 in 
    Printer.info printer "Sampling 10 models into %s/" validator_options.output_folder_path;
    let samples = QCheck2.Gen.generate ~n:10 (ModelGen.parsed_model spec) in
    List.iter 
      (fun parsed_model -> 
        let model, _ = ModelConverter.abstract_structures_of_parsing_structures options parsed_model None in
        let output_folder = Printf.sprintf "%s/samples" validator_options.output_folder_path in
        let file_name = Printf.sprintf "sampled_model_%d" !sample_number in 
        if !sample_number <= 10 then 
          ModelOutput.output_model 
          ~draw:draw_pdf 
          ~file_name 
          ~output_folder
          options
          model;
        incr sample_number
      ) samples;

  | CompareOutput {config_file_a;config_file_b} ->
  begin
    let i = ref 0 in 
    let time_outs = ref 0 in 
    let options_a, parsed_property_option_a = ConfigLoader.build_imitator_options_and_property config_file_a ~validator_options in 
    let options_b, parsed_property_option_b = ConfigLoader.build_imitator_options_and_property config_file_b ~validator_options in 
    
    Printer.start_section printer "Searching for counter example";
    Printer.start_live printer;
    let cell = QCheck2.Test.make_cell ~count:1000 (ModelGen.parsed_model spec) (fun parsed_model ->

      Printer.info printer "[%d | TO: %d]" (!i + 1) !time_outs;
      incr i;
      let result_a, _ = ModelRunner.run options_a parsed_model parsed_property_option_a in 
      let result_b, _ = ModelRunner.run options_b parsed_model parsed_property_option_b in 

      match Comparison.eq_result result_a result_b with 
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

    let result = QCheck2.Test.check_cell cell in 
    let state = QCheck2.TestResult.get_state result in 
    match state with 
    | Success -> ()
    | Failed {instances} -> 
      let counter_example = List.hd instances in 
      let parsed_model = counter_example.instance in 
      
      let nb_transitions = 
        parsed_model.automata
        |> List.fold_left (fun acc (_, _, locations) ->
          acc + List.fold_left (fun acc_loc (location : ParsingStructure.parsed_location) ->
            acc_loc + List.length location.transitions
          ) 0 locations
        ) 0
      in

      let nb_locations = 
        parsed_model.automata
        |> List.fold_left (fun acc (_, _, locations) ->
          acc + List.length locations
        ) 0
      in
      Printer.end_live printer;
      Printer.info printer "Found counter example with %d locations and %d transitions" nb_locations nb_transitions;
      
      Printer.info printer "Attempting to reduce ... ";
      let reduced_parsed_model = ModelReducer.reduce parsed_model ~printer ~options_a ~parsed_property_option_a ~options_b ~parsed_property_option_b in 
      let reduced_model, _ = ModelConverter.abstract_structures_of_parsing_structures options_a reduced_parsed_model None in 
      Printer.info printer "Reduced model to %d locations and %d transitions" reduced_model.nb_locations reduced_model.nb_transitions;

      let output_folder = Printf.sprintf "%s/counter_examples" validator_options.output_folder_path in
      let file_name = Printf.sprintf "counter_example_%d" !i in 
      Printer.info printer "Saving reduced counter example as %s/%s.imi" output_folder file_name; 
      ModelOutput.output_model ~file_name ~output_folder options_b reduced_model
    | _ -> ()
    

  end
  | Reduce {model_file;config_file_a;config_file_b} -> 
    let options_a, parsed_property_option_a = ConfigLoader.build_imitator_options_and_property config_file_a ~model_file ~validator_options in 
    let options_b, parsed_property_option_b = ConfigLoader.build_imitator_options_and_property config_file_b ~model_file ~validator_options in 

	  let unexpanded_parsed_model : ParsingStructure.unexpanded_parsed_model = ParsingUtility.compile_unexpanded_parsed_model options_a in
    let parsed_model = Templates.expand_model unexpanded_parsed_model in 

    let model, _ = ParsingUtility.compile_model_and_property options_a in 
    Printer.info printer "Reducer provided with a counter example with %d locations and %d transitions" model.nb_locations model.nb_transitions;
    Printer.info printer "Attempting to reduce while preserving counter example";
    let reduced_parsed_model = ModelReducer.reduce parsed_model ~printer ~options_a ~parsed_property_option_a ~options_b ~parsed_property_option_b in 
    let reduced_model, _ = ModelConverter.abstract_structures_of_parsing_structures options_a reduced_parsed_model None in 
    Printer.info printer "Reduced model to %d locations and %d transitions" reduced_model.nb_locations reduced_model.nb_transitions;

    let output_folder = Printf.sprintf "%s/reducer" validator_options.output_folder_path in
    let file_name = Printf.sprintf "%s_reduced" options_a#files_prefix in 
    Printer.info printer "Saving reduced counter example as %s/%s.imi" output_folder file_name;
    ModelOutput.output_model ~file_name ~output_folder options_b reduced_model