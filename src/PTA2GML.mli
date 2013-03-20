(*****************************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2011/11/22
 * Last modified: 2011/11/22
 *
 ****************************************************************)


open AbstractModel
open Graph


(**************************************************)
(** Program *)
(**************************************************)
(* Convert a pi0 into a string *)
val string_of_pi0 : abstract_model -> pi0 -> string

(* Convert a program into a string *)
val string_of_model : abstract_model -> string

