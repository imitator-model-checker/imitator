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
 * Last modified     : 2016/01/11
 *
 ************************************************************)


(**************************************************************)
(* Modules *)
(**************************************************************)


(************************************************************)
(** Termination of the algorithm *)
(************************************************************)
type nb_unexplored_successors = int

type algorithm_termination =
	(* Fixpoint-like termination *)
	| Regular_termination

	(* Termination due to time limit reached *)
	(*** TODO: list of state indexes instead of nb ??? ***)
	| Time_limit of nb_unexplored_successors
	
	(* Termination due to state space depth limit reached *)
	| Depth_limit of nb_unexplored_successors
	
	(* Termination due to a number of explored states reached *)
	| States_limit of nb_unexplored_successors


(************************************************************)
(** General result for the IMITATOR algorithms *)
(************************************************************)

type poststar_result = {
	(* Explored state space *)
	state_space			: StateSpace.state_space;
	
	(* Total computation time of the algorithm *)
	computation_time	: float;
	
	(* Termination *)
	termination			: algorithm_termination;
}


type efsynth_result = {
	(* List of constraints ensuring EF location *)
	constraints			: LinearConstraint.p_linear_constraint list;
	
	(* Explored state space *)
	state_space			: StateSpace.state_space;
	
	(* Nature of the state space (needed??) *)
(* 	tile_nature			: AbstractModel.tile_nature; *)
	
	(* Total computation time of the algorithm *)
	computation_time	: float;
	
	(* Termination *)
	termination			: algorithm_termination;
}


(* Variants of IM with a convex constraint as result *)
type imconvex_result = {
	(* Convex constraint *)
	convex_constraint	: LinearConstraint.p_linear_constraint;
	
	(* Explored state space *)
	state_space			: StateSpace.state_space;
	
	(* Nature of the state space (needed??) *)
(* 	tile_nature			: AbstractModel.tile_nature; *)
	
	(* Number of random selections of pi-incompatible inequalities performed *)
	nb_random_selections: int;
	
	(* Total computation time of the algorithm *)
	computation_time	: float;
	
	(* Termination *)
	termination			: algorithm_termination;
}

(* Variants of IM with a non-convex constraint as result *)
type imnonconvex_result = {
	(* Convex constraint *)
	nonconvex_constraint: LinearConstraint.p_nnconvex_constraint;
	
	(* Explored state space *)
	state_space			: StateSpace.state_space;
	
	(* Nature of the state space (needed??) *)
(* 	tile_nature			: AbstractModel.tile_nature; *)
	
	(* Number of random selections of pi-incompatible inequalities performed *)
	nb_random_selections: int;
	
	(* Total computation time of the algorithm *)
	computation_time	: float;
	
	(* Termination *)
	termination			: algorithm_termination;
}



type imitator_result =
	(* Result for Post* *)
	| PostStar_result of poststar_result

	(* Result for EFsynth *)
	| EFsynth_result of efsynth_result

	(* Result for IM, IMK *)
	| IMConvex_result of imconvex_result

	(* Result for IMunion *)
	| IMNonconvex_result of imnonconvex_result






















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

