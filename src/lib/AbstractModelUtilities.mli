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

val is_global_location_urgent : AbstractModel.abstract_model -> DiscreteState.global_location -> bool

(*------------------------------------------------------------*)
(* Get all invariants of model's automatas *)
(* Note : use of Input.get_model *)
(*------------------------------------------------------------*)
val get_model_invariants : AbstractModel.abstract_model -> DiscreteState.global_location -> AbstractModel.invariant list


(** Split guards into two list, one of discrete guards and other of continuous guards *)
val split_guards_into_discrete_and_continuous : AbstractModel.invariant list -> (AbstractModel.discrete_guard list * AbstractModel.continuous_guard list)
