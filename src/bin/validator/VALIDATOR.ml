open Crowbar
open Lib
open Generators
open AbstractModel

let pp_abstract_model = fun (f : Format.formatter) (model : abstract_model) -> Format.pp_print_string f (ModelPrinter.string_of_model model)

let () =
  let options = new Options.imitator_options in

  (* Set the options (for other modules) *)
  Input.set_options options;

  add_test ~name:"model generator makes valid model" [abstract_model] (fun model ->
    ()
  )
