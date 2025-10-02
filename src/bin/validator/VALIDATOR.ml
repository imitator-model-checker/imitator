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


let () =
  ArgStash.stash ~names:[imitator_args_label_a; imitator_args_label_b];
  let imitator_args_file_a = ArgStash.get_exn imitator_args_label_a in 
  let imitator_args_file_b = ArgStash.get_exn imitator_args_label_b in 

                    (* append empty string in beginning to please argument parser *)
  let arg_array_a = Array.append [|" ";"-verbose=mute"|] @@ Arg.read_arg imitator_args_file_a in
  let arg_array_b = Array.append [|" ";"-verbose=mute"|] @@ Arg.read_arg imitator_args_file_b in 

  let options_a = new Options.imitator_options in
  let options_b = new Options.imitator_options in

  options_a#parse ~from_arg_list:(Some arg_array_a) ~skip_model:true ();
  (* Reset for arg parsing again *)
  Arg.current := 0;
  options_b#parse ~from_arg_list:(Some arg_array_b) ~skip_model:true ();
  Arg.current := 0;
  let unexpanded_parsed_property_a = ParsingUtility.compile_unexpanded_parsed_property options_a in 
  let unexpanded_parsed_property_b = ParsingUtility.compile_unexpanded_parsed_property options_b in 

  (* Assumption: The property is simple and doesn't include variables. 
     Variables in properties do not make sense when models are randomly generated
     TODO: Throw error if a parsed property has variables *)
  let parsed_property_a = Option.map (fun property -> Templates.expand_property [] property) unexpanded_parsed_property_a in
  let parsed_property_b = Option.map (fun property -> Templates.expand_property [] property) unexpanded_parsed_property_b in
  

  add_test ~name:"The two configurations give the same result" [parsed_model] (fun parsed_model ->
    Input.set_options options_a;
    let model, property_a = ModelConverter.abstract_structures_of_parsing_structures options_a parsed_model parsed_property_a in 
    let result_a = ImitatorRunner.run options_a model property_a in 

    Input.set_options options_b;
    let model, property_b = ModelConverter.abstract_structures_of_parsing_structures options_b parsed_model parsed_property_b in 
    let result_b = ImitatorRunner.run options_b model property_b in

    check_eq_result model result_a result_b
  )
