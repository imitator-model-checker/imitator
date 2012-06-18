(************************************************************
 *
 *                     IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre, Ulrich Kuehne
 * Created:       2010/07/05
 * Last modified: 2012/06/18
 *
 ************************************************************)

(************************************************************)
(* Modules *)
(************************************************************)

open Global
open LinearConstraint
open Reachability
open AbstractModel



(************************************************************)
(* Constants *)
(************************************************************)
val dot_image_extension : string


(************************************************************)
(* Functions *)
(************************************************************)



val strict_to_not_strict_inequality : linear_inequality -> linear_inequality

val cartography : abstract_program ->  (int*int) array -> returned_constraint list ->  string -> unit

val generate_graph : abstract_program -> pi0 -> Graph.reachability_graph -> string -> unit
