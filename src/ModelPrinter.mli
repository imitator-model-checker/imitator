(*****************************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2009/12/02
 * Last modified: 2012/06/15
 *
 ****************************************************************)


open AbstractModel
open Graph

(****************************************************************)
(** Local printings *)
(****************************************************************)
(* Convert a var_type into a string *)
val string_of_var_type : var_type -> string

(**************************************************)
(** State *)
(**************************************************)
(* Convert a state into a string *)
val string_of_state : abstract_program -> (Automaton.global_location * LinearConstraint.linear_constraint) -> string

(**************************************************)
(** Result *)
(**************************************************)
val string_of_returned_constraint : (int -> string) -> returned_constraint -> string



(**************************************************)
(** Program *)
(**************************************************)
(* Convert a pi0 into a string *)
val string_of_pi0 : abstract_program -> pi0 -> string

(* Convert a program into a string *)
val string_of_program : abstract_program -> string

