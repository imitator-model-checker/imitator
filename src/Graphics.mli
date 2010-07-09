(*****************************************************************
 *
 *                     IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre, Ulrich Kuehne
 * Created:       2010/07/05
 * Last modified: 2010/07/05
 *
 ****************************************************************)

(**************************************************)
(* Functions *)
(**************************************************)
open LinearConstraint

val hello_world : unit -> unit

val cartography : linear_constraint list -> (int*int) array -> int -> string -> unit
