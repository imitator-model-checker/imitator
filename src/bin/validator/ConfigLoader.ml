open Lib
module ImitatorOptions = Options

let build_imitator_options_and_property ?model_path imitator_args_array ~(validator_options : ValidatorOptions.t)   = 
(* append empty string in beginning to please argument parser *)
  let time_limit_arg = match validator_options.time_limit with 
    | Some t -> [|Printf.sprintf "-time-limit=%f" t|] 
    | None -> [||] 
  in 
  let model_file_arg = match model_path with 
    | Some model_file -> [|model_file|]
    | None -> [||]
  in

  let default_args = Array.concat [[|" "; "-verbose=mute"|]; time_limit_arg; model_file_arg] in
                    
  let arg_array = Array.append default_args imitator_args_array in
  let options = new ImitatorOptions.imitator_options in
  let skip_model = Option.is_none model_path in 

  Arg.current := 0;
  options#parse ~from_arg_list:(Some arg_array) ~skip_model ();

  let unexpanded_parsed_property_option = ParsingUtility.compile_unexpanded_parsed_property options in 
    (* Assumption: The property is simple and doesn't include variables. 
     Variables in properties do not make sense when models are randomly generated
     TODO: Throw error if a parsed property has variables *)
  let parsed_property_option = Option.map (fun property -> Templates.expand_property [] property) unexpanded_parsed_property_option in
  options, parsed_property_option
