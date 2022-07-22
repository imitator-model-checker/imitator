(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: description of the result output by IMITATOR
 * 
 * File contributors : Étienne André
 * Created           : 2015/11/23
 *
 ************************************************************)


(************************************************************)
(** Error types *)
(************************************************************)
type error_type =
	| Division_by_zero				of string
	| Out_of_bound
	| Empty_collection

	| ModelFileNotFound_error
	| PropertyFileNotFound_error

	| InvalidModel_error
	| InvalidProperty_error
	
	| Lexing_error					of string
	
	| ModelParsing_error			of string
	
	| PropertyParsing_error			of string
	
	| Unsatisfiable_initial_state


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
	
	(* Termination due to a target state found *)
	(*** NOTE/HACK: the number of unexplored states is not known, therefore we do not add it… ***)
	| Target_found


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

	(* Pair of constraints: one under-approximation and one over-approximation *)
(* 	| Constraint_under_over *)
	
	(* Impossible to compare the constraint with the original result *)
	(*** NOTE: technically it used by variants of IM where the intersection with the real result is not null ***)
	| Constraint_maybe_invalid


(************************************************************)
(** Constraint with its soundness *)
(************************************************************)
type constraint_and_soundness = LinearConstraint.p_nnconvex_constraint * constraint_soundness

(** A pair good valuations / bad valuations *)
type good_and_bad_constraint = {
	good	: constraint_and_soundness;
	bad		: constraint_and_soundness;
}

(** A good, a bad, or a pair *)
type good_or_bad_constraint =
	(* Only good valuations *)
	| Good_constraint of constraint_and_soundness
	(* Only bad valuations *)
	| Bad_constraint of constraint_and_soundness
	(* Both good and bad valuations *)
	| Good_bad_constraint of good_and_bad_constraint
(*	(* result of NDFS *)
	| Accepting_cycle_constraint of constraint_and_soundness*)


(************************************************************)
(** Coverage of the cartography *)
(************************************************************)
type bc_coverage =
	(* Full coverage in all dimensions, including rational points *)
	| Coverage_full

	(* No constraint computed at all *)
	| Coverage_empty

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

type state_space_computation_result = {
	(* Explored state space *)
	state_space			: StateSpace.state_space;
	
	(* Nature of the state space *)
	statespace_nature	: StateSpace.statespace_nature;

	(* Total computation time of the algorithm *)
	computation_time	: float;
	
	(* Termination *)
	termination			: bfs_algorithm_termination;
}



(** Result for single synthesis: EF, PDFC *)
type single_synthesis_result = {
	(* Good and/or bad valuations *)
	result				: good_or_bad_constraint;
	
	(* English description of the constraint *)
	constraint_description: string;
	
	(* Explored state space *)
	state_space			: StateSpace.state_space;
	
	(* Total computation time of the algorithm *)
	computation_time	: float;
	
	(* Termination *)
	termination			: bfs_algorithm_termination;
}


(** Result for single synthesis based on a reference valuation: IM and its variants, PRP *)
type point_based_result = {
	(* Reference valuation *)
	reference_val		: PVal.pval;
	
	(* Good and/or bad valuations *)
	result				: good_or_bad_constraint;
	
	(* Explored state space *)
	state_space			: StateSpace.state_space;
	
	(* Number of random selections of pi-incompatible inequalities performed *)
(* 	nb_random_selections: int; *)
	
	(* Total computation time of the algorithm *)
	computation_time	: float;
	
	(* Termination *)
	termination			: bfs_algorithm_termination;
}


(*------------------------------------------------------------*)
(* Cartography algorithms *)
(*------------------------------------------------------------*)

(** Abstract state space of IM for BC (to save memory) *)
type abstract_state_space = {
	nb_states			: int;
	nb_transitions		: int;
(* 	depth				: int; *)
}

(** Abstract result of point-based algorithms for BC (to save memory) *)
type abstract_point_based_result = {
	(* Reference valuation *)
	reference_val		: PVal.pval;
	
	(* Good and/or bad valuations *)
	result				: good_or_bad_constraint;
	
	(* Abstracted version of the explored state space *)
	abstract_state_space	: abstract_state_space;
	
(*	(* Number of random selections of pi-incompatible inequalities performed *)
	nb_random_selections: int;*)
	
	(* Total computation time of the algorithm *)
	computation_time	: float;

	(* Termination *)
	termination			: bfs_algorithm_termination;
}

(** Result for the original behavioral cartography and its variants: a list of tiles *)
type cartography_result = {
	(* Parameter domain *)
	parameter_domain	: HyperRectangle.hyper_rectangle;
	
	(* Number of points in V0 *)
	(*** NOTE: not technically part of the result, but useful to have it here *)
	size_v0				: NumConst.t;
	
	(* List of tiles *)
	tiles				: abstract_point_based_result list;
	
	(* Total computation time of the algorithm *)
	computation_time	: float;
	
	(* Computation time to look for points *)
(* 	find_point_time		: float; *)
	
	(* Number of points on which IM could not be called because already covered *)
	nb_unsuccessful_points: int;
	
	(* Evaluation of the coverage of V0 by tiles computed by the cartography *)
	coverage			: bc_coverage;
	
	(* Termination *)
	termination			: bc_algorithm_termination;

	(*** TODO: compute a percentage of the points explored ?? ***)
}


(** Result for the cartography-based algorithms for which mainly the resulting constraint (and not the list of tiles) is important: PRPC *)
type multiple_synthesis_result = {
	(* Parameter domain *)
	parameter_domain	: HyperRectangle.hyper_rectangle;
	
	(* Number of points in V0 *)
	(*** NOTE: not technically part of the result, but useful to have it here *)
	size_v0				: NumConst.t;
	
	(* Good and/or bad valuations *)
	result				: good_or_bad_constraint;

	(* List of tiles *)
	(*** NOTE: so far we do NOT keep tiles ***)
	(*** TODO: compact to save memory? ***)
(* 	tiles				: abstract_point_based_result list; *)
	
	(* Total computation time of the algorithm *)
	computation_time	: float;
	
	(* Computation time to look for points *)
(* 	find_point_time		: float; *)
	
	(* Number of points on which IM could not be called because already covered *)
	nb_unsuccessful_points: int;
	
	(* Evaluation of the coverage of V0 by tiles computed by the cartography *)
	coverage			: bc_coverage;
	
	(* Termination *)
	termination			: bc_algorithm_termination;

	(*** TODO: compute a percentage of the points explored ?? ***)
}

(*------------------------------------------------------------*)
(* Algorithms synthesizing runs *)
(*------------------------------------------------------------*)

type impossible_or_concrete_run =
	| Impossible_concrete_run of StateSpace.impossible_concrete_run
	| Concrete_run of StateSpace.concrete_run

type valuation_and_concrete_run = {
	(* The parameter valuation for which this run exists *)
	valuation		: PVal.pval;
	
	(* Sometimes, we can even infer more valuations for which an equivalent DISCRETE run exist (note that the exact timings of the run might differ!!!) *)
	valuations		: LinearConstraint.p_convex_or_nonconvex_constraint;
	
	(* The possibly impossible concrete run *)
	concrete_run	: impossible_or_concrete_run;
}

(** Result for runs exhibition *)
type runs_exhibition_result = {
	(* Set of runs *)
	runs				: valuation_and_concrete_run list;
	
	(* Explored state space *)
	state_space			: StateSpace.state_space;
	
	(* Total computation time of the algorithm *)
	computation_time	: float;
	
	(* Termination *)
	termination			: bfs_algorithm_termination;
}

(*------------------------------------------------------------*)
(* PETS creation for opacity (work by Johan Arcile) *)
(*------------------------------------------------------------*)

(** Result for PETS computation (work by Johan Arcile) *)
type pets_result = {
	(* So far, string representation *)
	pets				: string;
	
	(* Total computation time of the algorithm *)
	computation_time	: float;
}

(************************************************************)
(** A unified type for all results *)
(************************************************************)

type imitator_result =
	(* Error result (parsing, etc.) *)
	| Error_result of error_type
	
	(* No analysis: syntactic check only (+ generation of the result file with syntactic information if requested) *)
	| Syntax_check_result

	(* No analysis: syntactic check only (+ generation of the result file with syntactic information if requested) *)
	| Translation_result
	
	(* Result for Post* *)
	| State_space_computation_result of state_space_computation_result

	(* Result for EFsynth, PDFC PRP *)
	| Single_synthesis_result of single_synthesis_result
	
	(* Result for IM, PRP *)
	| Point_based_result of point_based_result
	
	(* Result for original cartography *)
	| Cartography_result of cartography_result
	
	(* Result for PRPC *)
	| Multiple_synthesis_result of multiple_synthesis_result
	
	(* No result for workers in distributed mode *)
	| Distributed_worker_result

	(* Result for runs exhibition *)
	| Runs_exhibition_result of runs_exhibition_result

	(* Result for PETS computation (work by Johan Arcile) *)
	| PETS_result of pets_result
