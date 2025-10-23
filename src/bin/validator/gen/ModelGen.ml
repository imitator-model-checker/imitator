open Crowbar
open Lib

let parsed_model : ParsingStructure.parsed_model gen =
  map [SimpleModel.gen] Convert.parsed_model_of_simple_model