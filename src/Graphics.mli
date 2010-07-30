(*****************************************************************
 *
 *                     IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre, Ulrich Kuehne
 * Created:       2010/07/05
 * Last modified: 2010/07/13
 *
 ****************************************************************)

(**************************************************)
(* Functions *)
(**************************************************)
open LinearConstraint
open AbstractImitatorFile

val strict_to_not_strict_inequality : linear_inequality -> linear_inequality

val cartography : abstract_program ->  (int*int) array -> linear_constraint list ->  int -> string -> unit
