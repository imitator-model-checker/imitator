open Crowbar
open Lib
open Generators
open AbstractModel

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

  add_test ~name:"model generator makes valid model" [abstract_model] (fun model ->
    Input.set_options options_a;
    () 
  )
