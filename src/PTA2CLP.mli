(*****************************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2011/11/03 (after first draft created in 10/2010)
 * Last modified: 2011/11/03
 *
 ****************************************************************)


open AbstractImitatorFile

(****************************************************************)
(** Local printings *)
(****************************************************************)
(* Convert a var_type into a string *)
(* val string_of_var_type : var_type -> string *)

(**************************************************)
(** Program *)
(**************************************************)

(* Convert a program into a string *)
val string_of_program : abstract_program -> string

