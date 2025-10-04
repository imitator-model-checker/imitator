open Crowbar
open Lib

module ImitatorOptions = Options

exception ValidatorError of string 

let check_eq_p_nnconvex_constraint (model : AbstractModel.abstract_model) = 
  check_eq 
  ~eq:LinearConstraint.p_nnconvex_constraint_is_equal 
  ~pp:(fun f k -> Format.pp_print_string f (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names k))

let check_eq_result model (a : Result.imitator_result) (b : Result.imitator_result) = 
  match a, b with 
    | Single_synthesis_result ra, Single_synthesis_result rb -> 
      (match ra.result,rb.result with 
      | Good_constraint (constr_a, _), Good_constraint (constr_b, _)
      | Bad_constraint (constr_a, _), Bad_constraint (constr_b, _) ->
        check_eq_p_nnconvex_constraint model constr_a constr_b
      | Good_bad_constraint {good=(good_a, _) ;bad=(bad_a, _)}, 
        Good_bad_constraint {good=(good_b, _) ;bad=(bad_b, _)} -> 
        check_eq_p_nnconvex_constraint model good_a good_b; 
        check_eq_p_nnconvex_constraint model bad_a bad_b
      | _ -> fail "")
    | _ -> raise @@ ValidatorError "Validator can only compare Single Synthesis Results"

let build_imitator_options_and_property ~imitator_args_file = 
  let default_args = [|" "; "-verbose=mute"|] in 
                    (* append empty string in beginning to please argument parser *)
  let arg_array = Array.append default_args @@ Arg.read_arg imitator_args_file in
  let options = new ImitatorOptions.imitator_options in
  options#parse ~from_arg_list:(Some arg_array) ~skip_model:true ();
  (* Reset for arg parsing again *)
  Arg.current := 0;

  let unexpanded_parsed_property_option = ParsingUtility.compile_unexpanded_parsed_property options in 
    (* Assumption: The property is simple and doesn't include variables. 
     Variables in properties do not make sense when models are randomly generated
     TODO: Throw error if a parsed property has variables *)
  let parsed_property_option = Option.map (fun property -> Templates.expand_property [] property) unexpanded_parsed_property_option in
  options, parsed_property_option

let output_model ?(draw=false) ~sample_number ~output_folder options parsed_model parsed_property_option  = 
  let model, _ = ModelConverter.abstract_structures_of_parsing_structures options parsed_model parsed_property_option in 
  let path = Printf.sprintf "%s/sampled_model_%d" output_folder sample_number in 
  let imi_file_name = path ^ ".imi" in
  let imi_file = open_out imi_file_name in
  output_string imi_file @@ ModelPrinter.string_of_model model; 
  close_out imi_file;
  if draw then 
    let translated_model = PTA2dot.string_of_model options model in
    let dot_created_file_option = Graphics.dot "pdf" path translated_model in
    begin
    match dot_created_file_option with
    | None -> print_endline "Oops…! Something went wrong with dot."
    | Some _ -> ()
    end
  


let () =
  let validator_args = ArgStash.stash_and_retrieve ValidatorOptions.arg_list in
  let validator_options = ValidatorOptions.parse validator_args in 
  
  let mode = validator_options.mode in 

  match mode with 
  | SampleModelGenerator {draw_pdf} -> 
    
    (* default options *)
    let options = new ImitatorOptions.imitator_options in 
    options#parse ~skip_model:true ~from_arg_list:(Some [|"";"-verbose=mute"|]) ();
    Arg.current := 0;
    Input.set_options options;

    let sample_number = ref 1 in 
    add_test ~name:(Printf.sprintf "Sampling 10 models into %s/" validator_options.output_folder_path)[Generators.parsed_model] (fun parsed_model -> 
      if !sample_number <= 10 then 
        output_model 
        ~draw:draw_pdf 
        ~sample_number:!sample_number 
        ~output_folder:validator_options.output_folder_path
        options
        parsed_model
        None;
      incr sample_number
    ) 
  | CompareOutput {config_file_a;config_file_b} ->
  begin
    let options_a, parsed_property_option_a = build_imitator_options_and_property ~imitator_args_file:config_file_a in 
    let options_b, parsed_property_option_b = build_imitator_options_and_property ~imitator_args_file:config_file_b in 

    add_test ~name:"The two configurations give the same result" [Generators.parsed_model] (fun parsed_model ->
      Input.set_options options_a;
      let model, property_a = ModelConverter.abstract_structures_of_parsing_structures options_a parsed_model parsed_property_option_a in 
      let result_a = ImitatorRunner.run options_a model property_a in 

      Input.set_options options_b;
      let model, property_b = ModelConverter.abstract_structures_of_parsing_structures options_b parsed_model parsed_property_option_b in 
      let result_b = ImitatorRunner.run options_b model property_b in

      check_eq_result model result_a result_b
    )
  end
