open Crowbar
open Lib
open Validator_spec

let parsed_model (spec : Spec.t) : ParsingStructure.parsed_model gen =
  map [SimpleModelGen.gen spec] Convert.parsed_model_of_simple_model