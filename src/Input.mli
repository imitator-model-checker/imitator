(*****************************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2012/06/15
 * Last modified: 2014/03/15
 *
 ****************************************************************)


open AbstractModel
open Options

(** Compile the different files and set the models *)
(* val compile: unit -> abstract_model * pi0 * v0 *)

val get_model: unit -> abstract_model
val set_model: abstract_model -> unit

val get_pi0: unit -> pi0
val set_pi0: pi0 -> unit

val get_v0: unit -> v0
val set_v0: v0 -> unit

val get_options: unit -> imitator_options
val set_options: imitator_options -> unit

(*val get_reachability_graph: unit -> reachability_graph
val set_reachability_graph: reachability_graph -> unit

val get_abstract_reachability_graph: unit -> abstract_reachability_graph
val set_abstract_reachability_graph: abstract_reachability_graph -> unit*)

