(************************************************************
 *
 *                     IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre, Ulrich Kuehne
 * Created:       2010/07/05
 * Last modified: 2013/01/31
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
val dot_image_extension : string

val dot_colors : string list


(************************************************************)
(* Functions *)
(************************************************************)


val cartography : abstract_program ->  v0 -> returned_constraint list ->  string -> unit

val dot : abstract_program -> string -> string -> unit

val generate_graph : abstract_program -> Graph.reachability_graph -> string -> unit

(* Convert a graph to a dot file *)
(* val dot_of_graph : AbstractModel.abstract_program -> reachability_graph -> fancy:bool -> (string * string) *)
