(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Convert an abstract model to the input syntax of IMITATOR
 * 
 * File contributors : Étienne André
 * Created           : 2009/12/02
 * Last modified     : 2017/06/25
 *
 ************************************************************)
 

open AbstractModel

(****************************************************************)
(** Local printings *)
(****************************************************************)
(* Convert a var_type into a string *)
val string_of_var_type : var_type -> string


(************************************************************)
(** Arithmetic expression *)
(************************************************************)
(** Convert a AbstractModel.discrete_arithmetic_expression into a string *)
val string_of_arithmetic_expression : (Automaton.variable_index -> Automaton.variable_name) -> discrete_arithmetic_expression -> string


(************************************************************)
(** State *)
(************************************************************)
(*** TODO/BADPROG : Move elsewhere? ***)
(** Convert a state into a string *)
val string_of_state : abstract_model -> (Location.global_location * LinearConstraint.px_linear_constraint) -> string

(** Convert a state into a string *)
val string_of_location : abstract_model -> Location.global_location -> string


(************************************************************)
(** Guard *)
(************************************************************)
(** Convert a guard into a string *)
val string_of_guard : (Automaton.variable_index -> Automaton.variable_name) -> AbstractModel.guard -> string



(************************************************************)
(** model *)
(************************************************************)
(* Convert a pi0 into a string *)
val string_of_pi0 : abstract_model -> pi0 -> string

(* Convert a v0 into a string *)
val string_of_v0 : abstract_model -> v0 -> string

(* Convert a model into a string *)
val string_of_model : abstract_model -> string

(** Convert the correctness property to a string *)
val string_of_property : abstract_model -> property_definition -> string
