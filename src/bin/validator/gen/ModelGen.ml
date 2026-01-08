open Validator_config
open QCheck2.Gen

let parsed_model (spec : Spec.t) =
  SimpleModelGen.gen spec
  >|= Convert.parsed_model_of_simple_model 
  |>  no_shrink