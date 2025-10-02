open Lib

open Exceptions
open OCamlUtilities

open ImitatorUtilities
open AbstractModel
open AbstractAlgorithm
open AbstractProperty
open Result
open Options
open Statistics

val run : imitator_options -> abstract_model -> abstract_property option -> imitator_result