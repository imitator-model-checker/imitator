open Lib
module ImitatorOptions = Options

let build_imitator_options_and_property ?model_file imitator_args_file   = 
  let default_args = Array.append [|" "; "-verbose=mute"; "-time-limit=1"|] 
    (match model_file with 
    | Some model_file -> [|model_file|]
    | None -> [||]) 
  in 
                    (* append empty string in beginning to please argument parser *)
  let arg_array = Array.append default_args @@ Arg.read_arg imitator_args_file in
  let options = new ImitatorOptions.imitator_options in
  let skip_model = Option.is_none model_file in 

  options#parse ~from_arg_list:(Some arg_array) ~skip_model ();
  (* Reset for arg parsing again *)
  Arg.current := 0;

  let unexpanded_parsed_property_option = ParsingUtility.compile_unexpanded_parsed_property options in 
    (* Assumption: The property is simple and doesn't include variables. 
     Variables in properties do not make sense when models are randomly generated
     TODO: Throw error if a parsed property has variables *)
  let parsed_property_option = Option.map (fun property -> Templates.expand_property [] property) unexpanded_parsed_property_option in
  options, parsed_property_option
