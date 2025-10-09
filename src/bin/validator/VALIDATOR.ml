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
      if !sample_number <= 10 then 
        ModelOutput.output_model 
        ~draw:draw_pdf 
        ~sample_number:!sample_number 
        ~output_folder:validator_options.output_folder_path
        options
        model;
      incr sample_number
    ) 
  | CompareOutput {config_file_a;config_file_b} ->
  begin
    let i = ref 0 in 
    let options_a, parsed_property_option_a = ConfigLoader.build_imitator_options_and_property ~imitator_args_file:config_file_a in 
    let options_b, parsed_property_option_b = ConfigLoader.build_imitator_options_and_property ~imitator_args_file:config_file_b in 

    add_test ~name:"The two configurations give the same result" [ModelGen.parsed_model] (fun parsed_model ->
      print_endline (Printf.sprintf "%d" !i);
      Input.set_options options_a;
      let model, property_a = ModelConverter.abstract_structures_of_parsing_structures options_a parsed_model parsed_property_option_a in 
      let result_a = ImitatorRunner.run options_a model property_a in 

      Input.set_options options_b;
      let model, property_b = ModelConverter.abstract_structures_of_parsing_structures options_b parsed_model parsed_property_option_b in 
      let result_b = ImitatorRunner.run options_b model property_b in

      Comparison.check_eq_result model result_a result_b;
      State.flush_invariant_cache ();
      incr i
    )
  end
