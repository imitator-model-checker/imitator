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

  !!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!
 WARNING !!! THIS FILE IS NOW UNPLUGGED FROM THE IMITATOR SOURCE CODE (as for 21st March 2017)
 This paragraph should raise a compiling error (syntax error) if by any chance this file was linked from another file.
  !!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!

open AbstractModel


(**************************************************)
(** Program *)
(**************************************************)
(* Convert a pi0 into a string *)
val string_of_pi0 : abstract_model -> pi0 -> string

(* Convert a program into a string *)
val string_of_model : abstract_model -> string

