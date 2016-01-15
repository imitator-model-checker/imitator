(************************************************************
 *
 *                     IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre, Ulrich Kuehne
 * Created:       2010/07/05
 * Last modified: 2016/01/15
 *
 ************************************************************)

(************************************************************)
(* Modules *)
(************************************************************)

open Global
open LinearConstraint
open AbstractModel



(************************************************************)
(* Constants *)
(************************************************************)
val dot_colors : string list


(************************************************************)
(* Functions *)
(************************************************************)

val cartography : Result.returned_constraint list ->  string -> unit

val dot : string -> string -> unit

val generate_graph : StateSpace.state_space -> string -> unit

