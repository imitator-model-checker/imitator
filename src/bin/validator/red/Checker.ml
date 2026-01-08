open Runner
open Comp

let counter_example_predicate ~options_and_properties model =
  let results = 
    List.map 
      (fun (options, parsed_property_option) -> 
        let result, _ = ModelRunner.run options model parsed_property_option in 
        result
      ) options_and_properties
  in
  Comparison.eq_results results = Not_Equal