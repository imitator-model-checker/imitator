(*****************************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2009/12/02
 * Last modified: 2010/01/04
 *
 ****************************************************************)


open AbstractImitatorFile

(****************************************************************)
(** Local printings *)
(****************************************************************)
(* Convert a var_type into a string *)
val string_of_var_type : var_type -> string

(**************************************************)
(** State *)
(**************************************************)
(* Convert a state into a string *)
val string_of_state : abstract_program -> state -> string

(**************************************************)
(** Program *)
(**************************************************)
(* Convert a pi0 into a string *)
val string_of_pi0 : abstract_program -> pi0 -> string

(* Convert a program into a string *)
val string_of_program : abstract_program -> string

(* Convert discrete updates into a string *)
val string_of_discrete_updates : abstract_program -> discrete_update list -> string