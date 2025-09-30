open Crowbar
open Lib
open Generators

let imitator_args_label_a = "--imitator-args-file-a"
let imitator_args_label_b = "--imitator-args-file-b"

let () =
  ArgStash.stash ~names:[imitator_args_label_a; imitator_args_label_b];
  let imitator_args_file_a = ArgStash.get_exn imitator_args_label_a in 
  let imitator_args_file_b = ArgStash.get_exn imitator_args_label_b in 

  let arg_array_a = Arg.read_arg imitator_args_file_a in
  let arg_array_b = Arg.read_arg imitator_args_file_b in 

  let options_a = new Options.imitator_options in
  let options_b = new Options.imitator_options in

  options_a#parse ~from_arg_list:(Some arg_array_a) ~skip_model:true ();
  options_b#parse ~from_arg_list:(Some arg_array_b) ~skip_model:true ();
  let unexpanded_parsed_property_a = ParsingUtility.compile_unexpanded_parsed_property options_a in 
  let unexpanded_parsed_property_b = ParsingUtility.compile_unexpanded_parsed_property options_b in 

  (* Assumption: The property is simple and doesn't include variables. 
     Variables in properties do not make sense when models are randomly generated
     TODO: Throw error if a parsed property has variables *)
  let parsed_property_a = Option.map (fun property -> Templates.expand_property [] property) unexpanded_parsed_property_a in
  let parsed_property_b = Option.map (fun property -> Templates.expand_property [] property) unexpanded_parsed_property_b in



  add_test ~name:"model generator makes valid model" [parsed_model] (fun parsed_model ->
    Input.set_options options_a;
    let model, property = ModelConverter.abstract_structures_of_parsing_structures options_a parsed_model parsed_property_a in 
    ()
  )
