(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: description of the result output by IMITATOR
 * 
 * File contributors : Étienne André
 * Created           : 2015/11/23
 * Last modified     : 2016/03/18
 *
 ************************************************************)



(************************************************************)
(** Termination of the algorithm *)
(************************************************************)
type nb_unexplored_successors = int

(* Termination for state-space based algorithms *)
type bfs_algorithm_termination =
	(* Fixpoint-like termination *)
	| Regular_termination

	(* Termination due to time limit reached *)
	(*** TODO: list of state indexes instead of nb ??? ***)
	| Time_limit of nb_unexplored_successors
	
	(* Termination due to state space depth limit reached *)
	| Depth_limit of nb_unexplored_successors
	
	(* Termination due to a number of explored states reached *)
	| States_limit of nb_unexplored_successors


(* Termination for cartography algorithms *)
type bc_algorithm_termination =
	(* Fixpoint-like termination *)
	| BC_Regular_termination

	(* Termination due to a maximum number of tiles computed *)
	| BC_Tiles_limit
	
	(* Termination due to time limit reached *)
	| BC_Time_limit
	
	(* Termination due to several limits (only possible in distributed setting) *)
	| BC_Mixed_limit


(************************************************************)
(** Soudness/completeness of the constraints *)
(************************************************************)
type constraint_soundness =
	(* Constraint strictly included in the real result *)
(* 	| Constraint_under *)
	
	(* Constraint included in or equal to the real result *)
	| Constraint_maybe_under
	
	(* Exact result *)
	| Constraint_exact
	
	(* Constraint equal to or larger than the real result *)
	| Constraint_maybe_over
	
	(* Constraint strictly larger than the real result *)
(* 	| Constraint_over *)

	(* Impossible to compare the constraint with the original result *)
	(*** NOTE: technically it used by variants of IM where the intersection with the real result is not null ***)
	| Constraint_maybe_invalid


(************************************************************)
(** Coverage of the cartography *)
(************************************************************)
type bc_coverage =
	(* Full coverage in all dimensions, including rational points *)
	| Coverage_full

	(* At least all integers are covered, rationals perhaps not *)
	| Coverage_integer_complete

	(* No indication of coverage *)
	| Coverage_unknown


(************************************************************)
(** General result for the IMITATOR algorithms *)
(************************************************************)

(*------------------------------------------------------------*)
(* BFS algorithms *)
(*------------------------------------------------------------*)

type poststar_result = {
	(* Explored state space *)
	state_space			: StateSpace.state_space;
	
	(* Nature of the state space *)
	statespace_nature	: StateSpace.statespace_nature;

	(* Total computation time of the algorithm *)
	computation_time	: float;
	
	(* Termination *)
	termination			: bfs_algorithm_termination;
}


(*** TODO: merge with pdfc_result when EFsynth will allow for non-convex constraints ***)
type efsynth_result = {
	(* List of constraints ensuring EF location *)
	constraints			: LinearConstraint.p_linear_constraint list;
	
	(* Explored state space *)
	state_space			: StateSpace.state_space;
	
	(* Nature of the state space *)
	statespace_nature	: StateSpace.statespace_nature;
	
	(* Total computation time of the algorithm *)
	computation_time	: float;
	
	(* Soundness of the result *)
	soundness			: constraint_soundness;
	
	(* Termination *)
	termination			: bfs_algorithm_termination;
}


type pdfc_result = {
	(* List of constraints *)
	result				: LinearConstraint.p_nnconvex_constraint;
	
	(* Explored state space *)
	state_space			: StateSpace.state_space;
	
	(* Nature of the state space *)
	statespace_nature	: StateSpace.statespace_nature;
	
	(* Total computation time of the algorithm *)
	computation_time	: float;
	
	(* Soundness of the result *)
	soundness			: constraint_soundness;
	
	(* Termination *)
	termination			: bfs_algorithm_termination;
}


(* Result of IM and variants *)
type im_result = {
	(* Convex constraint *)
	result				: LinearConstraint.p_convex_or_nonconvex_constraint;
	
	(* Explored state space *)
	state_space			: StateSpace.state_space;
	
	(* Nature of the state space *)
	statespace_nature	: StateSpace.statespace_nature;
	
	(* Number of random selections of pi-incompatible inequalities performed *)
	nb_random_selections: int;
	
	(* Total computation time of the algorithm *)
	computation_time	: float;
	
	(* Soundness of the result *)
	soundness			: constraint_soundness;
	
	(* Termination *)
	termination			: bfs_algorithm_termination;
}


(*------------------------------------------------------------*)
(* Cartography algorithms *)
(*------------------------------------------------------------*)

(* Abstract state space of IM for BC (to save memory) *)
type abstract_state_space = {
	nb_states			: int;
	nb_transitions		: int;
(* 	depth				: int; *)
}

(* Abstract result of IM for BC (to save memory) *)
type abstract_im_result = {
	(* Reference valuation *)
	reference_val		: PVal.pval;
	
	(* Convex constraint *)
	result				: LinearConstraint.p_convex_or_nonconvex_constraint;
	
	(* Abstracted version of the explored state space *)
	abstract_state_space			: abstract_state_space;
	
	(* Nature of the state space *)
	statespace_nature	: StateSpace.statespace_nature;

	(*** TODO: add depth (?) ***)
	
	(* Number of random selections of pi-incompatible inequalities performed *)
	nb_random_selections: int;
	
	(* Total computation time of the algorithm *)
	computation_time	: float;
	
	(* Soundness of the result *)
	soundness			: constraint_soundness;
	
	(* Termination *)
	termination			: bfs_algorithm_termination;
}

(* Result for BC and variants *)
type bc_result = {
	(* Number of points in V0 *)
	(*** NOTE: not technically part of the result, but useful to have it here *)
	size_v0				: NumConst.t;
	
	(* List of tiles *)
	tiles				: abstract_im_result list;
	
	(* Total computation time of the algorithm *)
	computation_time	: float;
	
	(* Computation time to look for points *)
	find_point_time		: float;
	
	(* Number of points on which IM could not be called because already covered *)
	nb_unsuccessful_points: int;
	
	(* Evaluation of the coverage of V0 by tiles computed by the cartography *)
	coverage			: bc_coverage;
	
	(* Termination *)
	termination			: bc_algorithm_termination;

	(*** TODO: compute a percentage of the points explored ?? ***)
}


(************************************************************)
(** A unified type for all results *)
(************************************************************)

type imitator_result =
	(* Result for Post* *)
	| PostStar_result of poststar_result

	(* Result for EFsynth *)
	| EFsynth_result of efsynth_result
	
	(* Result for Parametric_deadlock_checking *)
	| PDFC_result of pdfc_result

	(* Result for IM, IMK, IMunion *)
	| IM_result of im_result

(*	(* Result for IMunion *)
	| IMNonconvex_result of imnonconvex_result*)
	
	(* Result for cartography *)
	| BC_result of bc_result
	
	(* No result for workers in distributed mode *)
	| Distributed_worker_result

