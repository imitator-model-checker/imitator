(***************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre, Ulrich Kuehne, Romain Soulat
 * Created:       2010/07/22
 * Last modified: 2014/04/16
 *
 **************************************************)

open Global
open AbstractModel
open Options
open StateSpace
open LinearConstraint


(****************************************************************)
(** The result output by IM *)
(****************************************************************)
type im_result = {
	(* Returned constraint *)
	result : returned_constraint;
(*	(* Reachability graph *)
	reachability_graph : StateSpace.reachability_graph;*)
	(* Tile nature *)
	tile_nature : tile_nature;
	(* Deterministic analysis? *)
	deterministic : bool;
	(* Number of states *)
	nb_states : int;
	(* Number of transitions *)
	nb_transitions : int;
	(* Number of iterations *)
	nb_iterations : int;
	(* Computation time *)
	total_time : float;
}



val get_initial_state_or_abort : abstract_model -> state

val print_stats: unit -> unit


(************************************************************)
(* Clock elimination *)
(************************************************************)
(* Create data structures for detecting useless clocks (to be called once per model) *)
(** WARNING: should maybe be somewhere else? *)
val prepare_clocks_elimination : abstract_model -> unit


(************************************************************)
(* Algorithms *)
(************************************************************)
val full_state_space_exploration : abstract_model -> unit

val ef_synthesis : abstract_model -> returned_constraint

val inverse_method_gen : abstract_model -> state -> (im_result * StateSpace.reachability_graph)
	(*returned_constraint * StateSpace.reachability_graph * tile_nature * bool * int * float*)

val efim : abstract_model -> unit

val inverse_method : abstract_model -> unit
