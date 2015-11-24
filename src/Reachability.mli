(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Defines algorithms based on state space exploration
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        Ulrich Kuehne, Etienne Andre
 * 
 * Created:       2010/07/22
 * Last modified: 2015/11/23
 *
 ****************************************************************)

open Global
open AbstractModel
open Options
open StateSpace
open LinearConstraint



val get_initial_state_or_abort : abstract_model -> state

val print_stats: unit -> unit


(************************************************************)
(* Clock elimination *)
(************************************************************)
(* Create data structures for detecting useless clocks (to be called once per model) *)
(** WARNING: should maybe be somewhere else? *)
val prepare_clocks_elimination : abstract_model -> unit


(************************************************************)
(* Function for PaTATOR *)
(************************************************************)
val set_patator_termination_function : (unit -> unit) -> unit


(************************************************************)
(* SUCC functions *)
(************************************************************)
(* Compute the list of successor states of a given state, and update the state space; returns the list of new states' indexes actually added *)
val post_from_one_state : abstract_model ->  StateSpace.reachability_graph -> StateSpace.state_index -> StateSpace.state_index list


(************************************************************)
(* Algorithms *)
(************************************************************)
val full_state_space_exploration : abstract_model -> unit

val ef_synthesis : abstract_model -> (*returned_constraint*)unit

val inverse_method_gen : abstract_model -> state -> (Result.im_result * StateSpace.reachability_graph)
	(*returned_constraint * StateSpace.reachability_graph * tile_nature * bool * int * float*)

val efim : abstract_model -> unit

val inverse_method : abstract_model -> unit
