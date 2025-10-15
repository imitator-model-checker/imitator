open Crowbar
open Lib

module ImitatorOptions = Options

let () =
  let validator_args = ArgStash.stash_and_retrieve ValidatorOptions.arg_list in
  let validator_options = ValidatorOptions.parse validator_args in 
  Arg.current := 0;
  
  let mode = validator_options.mode in 

  match mode with 
  | SampleModelGenerator {draw_pdf} -> 
    
    (* default options *)
    let options = new ImitatorOptions.imitator_options in 
    options#parse ~skip_model:true ~from_arg_list:(Some [|"";"-verbose=mute"|]) ();
    Arg.current := 0;
    Input.set_options options;

    let sample_number = ref 1 in 
    add_test ~name:(Printf.sprintf "Sampling 10 models into %s/" validator_options.output_folder_path)[ModelGen.parsed_model] (fun parsed_model -> 
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
    ) 
  | CompareOutput {config_file_a;config_file_b} ->
  begin
    let i = ref 0 in 
    let options_a, parsed_property_option_a = ConfigLoader.build_imitator_options_and_property config_file_a in 
    let options_b, parsed_property_option_b = ConfigLoader.build_imitator_options_and_property config_file_b in 

    add_test ~name:"The two configurations give the same result" [ModelGen.parsed_model] (fun parsed_model ->
      print_endline (Printf.sprintf "\x1b[2K[%d]" !i);
      Input.set_options options_a;
      let model, property_a = ModelConverter.abstract_structures_of_parsing_structures options_a parsed_model parsed_property_option_a in 
      let result_a = ImitatorRunner.run options_a model property_a in 

      Input.set_options options_b;
      let model, property_b = ModelConverter.abstract_structures_of_parsing_structures options_b parsed_model parsed_property_option_b in 
      let result_b = ImitatorRunner.run options_b model property_b in

      (try 
        Comparison.check_eq_result model result_a result_b
      with exn -> 
        Printf.printf "Found counter example with %d locations and %d transitions\n" model.nb_locations model.nb_transitions;
        
        Printf.printf "Attempting to reduce ... \n";
        let reduced_parsed_model = ModelReducer.reduce parsed_model ~options_a ~parsed_property_option_a ~options_b ~parsed_property_option_b ~original_nb_transitions:model.nb_transitions in 
        let reduced_model, _ = ModelConverter.abstract_structures_of_parsing_structures options_a reduced_parsed_model None in 
        Printf.printf "Reduced model to %d locations and %d transitions\n" reduced_model.nb_locations reduced_model.nb_transitions;

        let output_folder = Printf.sprintf "%s/counter_examples" validator_options.output_folder_path in
        let file_name = Printf.sprintf "counter_example_%d" !i in 
        Printf.printf "Saving reduced counter example in %s/%s\n" output_folder file_name; 
        ModelOutput.output_model ~file_name ~output_folder options_b reduced_model;
        raise exn : unit);

      State.flush_invariant_cache ();
      print_string "\x1b[1F";
      incr i
    )
  end
  | Reduce {model_file;config_file_a;config_file_b} -> 
    let options_a, parsed_property_option_a = ConfigLoader.build_imitator_options_and_property config_file_a ~model_file in 
    let options_b, parsed_property_option_b = ConfigLoader.build_imitator_options_and_property config_file_b ~model_file in 

	  let unexpanded_parsed_model : ParsingStructure.unexpanded_parsed_model = ParsingUtility.compile_unexpanded_parsed_model options_a in
    let parsed_model = Templates.expand_model unexpanded_parsed_model in 

    let model, _ = ParsingUtility.compile_model_and_property options_a in 
    Printf.printf "Reducer provided with a counter example with %d locations and %d transitions\n" model.nb_locations model.nb_transitions;
    
    Printf.printf "Attempting to reduce while preserving counter example\n";
    let reduced_parsed_model = ModelReducer.reduce parsed_model ~options_a ~parsed_property_option_a ~options_b ~parsed_property_option_b ~original_nb_transitions:model.nb_transitions in 
    let reduced_model, _ = ModelConverter.abstract_structures_of_parsing_structures options_a reduced_parsed_model None in 
    Printf.printf "Reduced model to %d locations and %d transitions\n" reduced_model.nb_locations reduced_model.nb_transitions;

    let output_folder = Printf.sprintf "%s/reducer" validator_options.output_folder_path in
    let file_name = Printf.sprintf "%s_reduced" options_a#files_prefix in 
    Printf.printf "Saving reduced counter example in %s/%s\n" output_folder file_name;
    ModelOutput.output_model ~file_name ~output_folder options_b reduced_model; 
    ()