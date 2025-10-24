open Lib

exception TimeOutResult
exception ComparisonError of string

val eq_result : Result.imitator_result -> Result.imitator_result -> bool