open Lib
open Driver

let convert options parsed_model parsed_property =
  PVal.reset_dimensions ();
  HyperRectangle.reset_dimensions ();
  ModelConverter.abstract_structures_of_parsing_structures options parsed_model parsed_property

let run options parsed_model parsed_property =
  Input.set_options options;
  let model, property = convert options parsed_model parsed_property in
  let property = Option.get property in
  AlgorithmDriver.set_default_options options model (Some property);
  AlgorithmDriver.prepare_model options model;
  let algorithm = AlgorithmDriver.algorithm_of_property options model property (fun _ -> ()) in
  (* Reset what a run leaves behind in IMITATOR, also if it fails: the invariant cache,
     and the dimensions (changed by the PTG strategy generation) *)
  let result =
    Fun.protect (fun () -> algorithm#run) ~finally:(fun () ->
      State.flush_invariant_cache ();
      LinearConstraint.set_dimensions model.nb_parameters model.nb_clocks model.nb_discrete)
  in
  result, model
