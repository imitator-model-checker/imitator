open Lib

type result =
  | Equal
  | Not_Equal
  | Time_out
  | Not_supported
  | Incomparable
  | Error

val eq_result : Result.imitator_result -> Result.imitator_result -> result