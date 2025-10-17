open Lib

exception TimeOutResult

val check_eq_result : AbstractModel.abstract_model -> Result.imitator_result -> Result.imitator_result -> unit

val eq_result : Result.imitator_result -> Result.imitator_result -> bool