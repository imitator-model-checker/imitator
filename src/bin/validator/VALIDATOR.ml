open Lib
open Gen
open Comp
open Runner
open Red

module ImitatorOptions = Options


let validator_main () = 
  let validator_args = ArgStash.stash_and_retrieve ValidatorOptions.arg_list in
  let validator_options = ValidatorOptions.parse validator_args in 
  Arg.current := 0;
  
  let mode = validator_options.mode in 

  let printer = Printer.create () in 

  match mode with 
  | SampleModelGenerator {draw_pdf} -> 
    
    (* default options *)
    let options = new ImitatorOptions.imitator_options in 
    options#parse ~skip_model:true ~from_arg_list:(Some [|"";"-verbose=mute"|]) ();
    Arg.current := 0;
    Input.set_options options;

    let sample_number = ref 1 in 
    Printer.info printer "Sampling 10 models into %s/\n" validator_options.output_folder_path;
    ValidatorCrowbar.add_test [ModelGen.parsed_model] (fun parsed_model -> 
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
    );

  | CompareOutput {config_file_a;config_file_b} ->
  begin
    let i = ref 0 in 
    let time_outs = ref 0 in 
    let options_a, parsed_property_option_a = ConfigLoader.build_imitator_options_and_property config_file_a ~validator_options in 
    let options_b, parsed_property_option_b = ConfigLoader.build_imitator_options_and_property config_file_b ~validator_options in 
    
    Printer.start_section printer "Searching for counter example";
    Printer.start_live printer;
    ValidatorCrowbar.add_test [ModelGen.parsed_model] (fun parsed_model ->

      Printer.info printer "[%d | TO: %d]" (!i + 1) !time_outs;
      
      let result_a, _ = ModelRunner.run options_a parsed_model parsed_property_option_a in 
      let result_b, model = ModelRunner.run options_b parsed_model parsed_property_option_b in 

      (try 
        ComparisonCrowbar.check_eq_result model result_a result_b
      with exn -> 
        match exn with 
        | Comparison.TimeOutResult -> incr time_outs
        | _ ->
          Printer.info printer "Found counter example with %d locations and %d transitions" model.nb_locations model.nb_transitions;
          
          Printer.info printer "Attempting to reduce ... \n";
          let reduced_parsed_model = ModelReducer.reduce parsed_model ~printer ~options_a ~parsed_property_option_a ~options_b ~parsed_property_option_b in 
          let reduced_model, _ = ModelConverter.abstract_structures_of_parsing_structures options_a reduced_parsed_model None in 
          Printer.info printer "Reduced model to %d locations and %d transitions" reduced_model.nb_locations reduced_model.nb_transitions;

          let output_folder = Printf.sprintf "%s/counter_examples" validator_options.output_folder_path in
          let file_name = Printf.sprintf "counter_example_%d" !i in 
          Printer.info printer "Saving reduced counter example as %s/%s.imi" output_folder file_name; 
          ModelOutput.output_model ~file_name ~output_folder options_b reduced_model;
          raise exn : unit);
      incr i;
    );
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

let () =
  at_exit (fun () ->
    flush_all ();
    (* redirect stdout and stderr to /dev/null before Crowbar's own at_exit runs *)
    let devnull = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0o666 in
    Unix.dup2 devnull Unix.stdout;
    Unix.dup2 devnull Unix.stderr;
    Unix.close devnull
  );
  validator_main ()

  