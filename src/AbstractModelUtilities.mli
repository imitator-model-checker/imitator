(************************************************************
 *
 *                       IMITATOR
 *
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: utilities for abstract models
 *
 * File contributors : Étienne André
 * Created           : 2022/10/17
 *
 ************************************************************)

(*------------------------------------------------------------*)
(* Check whether at least one local location is urgent *)
(*------------------------------------------------------------*)

val is_global_location_urgent : AbstractModel.abstract_model -> Location.global_location -> bool
