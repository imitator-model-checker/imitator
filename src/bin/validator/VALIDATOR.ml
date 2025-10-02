open Crowbar
open Lib
open Generators

exception ValidatorError of string 
let imitator_args_label_a = "--imitator-args-file-a"
let imitator_args_label_b = "--imitator-args-file-b"

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

let build_options_and_property ~args_label = 
  let imitator_args_file = ArgStash.get_exn args_label in 

                    (* append empty string in beginning to please argument parser *)
  let arg_array = Array.append [|" ";"-verbose=mute"|] @@ Arg.read_arg imitator_args_file in
  let options = new Options.imitator_options in
  options#parse ~from_arg_list:(Some arg_array) ~skip_model:true ();
  (* Reset for arg parsing again *)
  Arg.current := 0;

  let unexpanded_parsed_property_option = ParsingUtility.compile_unexpanded_parsed_property options in 
    (* Assumption: The property is simple and doesn't include variables. 
     Variables in properties do not make sense when models are randomly generated
     TODO: Throw error if a parsed property has variables *)
  let parsed_property_option = Option.map (fun property -> Templates.expand_property [] property) unexpanded_parsed_property_option in
  options, parsed_property_option

let () =
  ArgStash.stash ~names:[imitator_args_label_a; imitator_args_label_b];
  let options_a, parsed_property_option_a = build_options_and_property ~args_label:imitator_args_label_a in 
  let options_b, parsed_property_option_b = build_options_and_property ~args_label:imitator_args_label_b in 

  add_test ~name:"The two configurations give the same result" [parsed_model] (fun parsed_model ->
    Input.set_options options_a;
    let model, property_a = ModelConverter.abstract_structures_of_parsing_structures options_a parsed_model parsed_property_option_a in 
    let result_a = ImitatorRunner.run options_a model property_a in 

    Input.set_options options_b;
    let model, property_b = ModelConverter.abstract_structures_of_parsing_structures options_b parsed_model parsed_property_option_b in 
    let result_b = ImitatorRunner.run options_b model property_b in

    check_eq_result model result_a result_b
  )
