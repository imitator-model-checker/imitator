(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: result output by IMITATOR
 * 
 * File contributors : Étienne André
 * Created           : 2015/11/23
 * Last modified     : 2015/11/23
 *
 ************************************************************)


(**************************************************************)
(* Modules *)
(**************************************************************)


(************************************************************)
(** General result for the IMITATOR algorithms *)
(************************************************************)

type imitator_result =
	(*** HACK: just to test the algorithms ***)
	| Noresultbecausethatsatest































(************************************************************)
(** Result *)
(************************************************************)

(** Result returned by IMITATOR *)
type returned_constraint =
	(*** TODO: merge these 2 objects (Convex_constraint and Union_of_constraints) ***)
	(** Constraint under convex form *)
	| Convex_constraint of LinearConstraint.p_linear_constraint * AbstractModel.tile_nature
	
	(** Disjunction of constraints *)
	| Union_of_constraints of LinearConstraint.p_linear_constraint list * AbstractModel.tile_nature

	(*** BADPROG: NNCC should NOT be here! but rather in LinearConstraint ***)
	(** Non-necessarily convex constraint: set of constraints MINUS a set of negations of constraints *)
	| NNCConstraint of (LinearConstraint.p_linear_constraint list) * (LinearConstraint.p_linear_constraint list) * AbstractModel.tile_nature
 


(****************************************************************)
(** The result output by IM *)
(****************************************************************)
(*** TODO: convert to a separate class ***)
type im_result = {
	(* Returned constraint *)
	result : returned_constraint;
(*	(* Reachability graph *)
	reachability_graph : StateSpace.reachability_graph;*)
	(* Tile nature *)
	tile_nature : AbstractModel.tile_nature;
	(* Premature stop? (i.e., states / depth / time limit reached) *)
	premature_stop : bool;
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

