
open Lib

let run options parsed_model parsed_property =
  Input.set_options options;
  let model, property =
    ModelConverter.abstract_structures_of_parsing_structures
      options parsed_model parsed_property
  in
  let result = ImitatorRunner.run options model property in 
  State.flush_invariant_cache ();
  result, model
  