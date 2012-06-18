(***************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre, Ulrich Kuehne, Romain Soulat
 * Created:       2010/07/22
 * Last modified: 2012/06/18
 *
 **************************************************)

open Global
open AbstractModel
open Options
open Graph
open LinearConstraint


val create_initial_state : abstract_program -> state

(* val post : abstract_program -> pi0 -> reachability_graph -> state_index -> state_index list *)

(*val post_star :
	abstract_program -> pi0 -> state ->
	(* K0 * reachability_graph * nb_iterations * counter_value *)
	returned_constraint * Graph.reachability_graph * int * float*)



val branch_and_bound :
	abstract_program -> pi0 -> state -> unit

val print_stats: unit -> unit



(************************************************************)
(* Full reachability analysis *)
(************************************************************)
val full_reachability : abstract_program -> state -> unit


(************************************************************)
(* Main inverse method function *)
(************************************************************)
val inverse_method_gen : abstract_program -> state -> returned_constraint * Graph.reachability_graph * int * float
val inverse_method : abstract_program -> state -> unit
