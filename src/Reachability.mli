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
 * Last modified: 2015/11/27
 *
 ****************************************************************)

open Global
open AbstractModel
open Options
open StateSpace
open LinearConstraint



type limit_reached =
	(* No limit *)
	| Keep_going

	(* Termination due to time limit reached *)
	| Time_limit_reached
	
	(* Termination due to state space depth limit reached *)
	| Depth_limit_reached
	
	(* Termination due to a number of explored states reached *)
	| States_limit_reached


val get_initial_state_or_abort : abstract_model -> state

val print_stats: unit -> unit



(************************************************************)
(* Function for PaTATOR *)
(************************************************************)
val set_patator_termination_function : (unit -> unit) -> unit


(************************************************************)
(* SUCC functions *)
(************************************************************)
(* Compute the list of successor states of a given state, and update the state space; returns the list of new states' indexes actually added *)
val post_from_one_state : abstract_model ->  StateSpace.state_space -> StateSpace.state_index -> StateSpace.state_index list

(*------------------------------------------------------------*)
(* Check whether the limit of an BFS exploration has been reached, according to the analysis options *)
(*** NOTE: May raise an exception when used in PaTATOR mode (the exception will be caught by PaTATOR) ***)
(*------------------------------------------------------------*)
val check_bfs_limit : int -> int -> float -> limit_reached


(************************************************************)
(* Algorithms *)
(************************************************************)
val full_state_space_exploration : abstract_model -> unit

val ef_synthesis : abstract_model -> (*returned_constraint*)unit

val inverse_method_gen : abstract_model -> state -> (Result.im_result * StateSpace.state_space)

val efim : abstract_model -> unit

val inverse_method : abstract_model -> unit
