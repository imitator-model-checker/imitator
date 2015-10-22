(*****************************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2009/12/02
 * Last modified: 2015/10/22
 *
 ****************************************************************)


open AbstractModel

(****************************************************************)
(** Local printings *)
(****************************************************************)
(* Convert a var_type into a string *)
val string_of_var_type : var_type -> string

(**************************************************)
(** State *)
(**************************************************)
(* Convert a state into a string *)
val string_of_state : abstract_model -> (Location.global_location * LinearConstraint.px_linear_constraint) -> string

(**************************************************)
(** Result *)
(**************************************************)
(*** TODO/BADPROG : Move elsewhere? ***)
val string_of_returned_constraint : (int -> string) -> returned_constraint -> string



(**************************************************)
(** model *)
(**************************************************)
(* Convert a pi0 into a string *)
val string_of_pi0 : abstract_model -> pi0 -> string

(* Convert a v0 into a string *)
val string_of_v0 : abstract_model -> v0 -> string

(* Convert a model into a string *)
val string_of_model : abstract_model -> string

(** Convert the correctness property to a string *)
val string_of_property : abstract_model -> property_definition -> string
