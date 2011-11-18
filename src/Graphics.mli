(*****************************************************************
 *
 *                     IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre, Ulrich Kuehne
 * Created:       2010/07/05
 * Last modified: 2010/11/22
 *
 ****************************************************************)

(**************************************************)
(* Functions *)
(**************************************************)
open Global
open LinearConstraint
open Reachability
open AbstractImitatorFile

val strict_to_not_strict_inequality : linear_inequality -> linear_inequality

val cartography : abstract_program ->  (int*int) array -> returned_constraint list ->  string -> unit
