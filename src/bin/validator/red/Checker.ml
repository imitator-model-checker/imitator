open Runner
open Comp

let counter_example_predicate ~options_a ~options_b ~parsed_property_option_a ~parsed_property_option_b model =
  let result_a, _ = ModelRunner.run options_a model parsed_property_option_a in
  let result_b, _ = ModelRunner.run options_b model parsed_property_option_b in
  not (Comparison.eq_result result_a result_b)
