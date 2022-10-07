(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: IMK algorithm [AS11]
 * 
 * File contributors : Étienne André
 * Created           : 2015/12/04
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoStateBased
open State

(************************************************************)
(* Class definition *)
(************************************************************)
class algoIMK : PVal.pval ->
	object inherit algoStateBased
		(************************************************************)
		(* Class variables *)
		(************************************************************)
		(* Start time for the algorithm *)
		val mutable nb_random_selections : int


		(************************************************************)
		(* Class methods *)
		(************************************************************)
		method algorithm_name : string
		
		
		method initialize_variables : unit
		
		
		method run : unit -> Result.imitator_result
		
		
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Get the reference valuation *)
		(*** HACK: for now, it is obtained from the property, stored in the Input module ***)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method get_reference_pval : PVal.pval

	
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Checks a new state for pi0-compatibility .*)
		(* constr            : new state constraint            *)
		(*------------------------------------------------------------*)
		(* returns true if the state is pi0-compatible, and false otherwise *)
		(*------------------------------------------------------------*)
		(* side effect: add the negation of the p_constraint to all computed states *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method check_pi0compatibility : LinearConstraint.px_linear_constraint -> bool

		
		(*------------------------------------------------------------*)
		(* Add a new state to the state space (if indeed needed) *)
		(* Return true if the state is not discarded by the algorithm, i.e., if it is either added OR was already present before *)
		(* Can raise an exception TerminateAnalysis to lead to an immediate termination *)
		(*------------------------------------------------------------*)
		(*** TODO: return the list of actually added states ***)
		method add_a_new_state : state_index -> StateSpace.combined_transition -> State.state -> bool

		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Actions to perform with the initial state; returns None unless the initial state cannot be kept, in which case the algorithm returns an imitator_result *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method try_termination_at_initial_state : Result.imitator_result option


		(* Actions to perform when meeting a state with no successors: nothing to do for this algorithm *)
		method process_deadlock_state : state_index -> unit
		
		
		(* Actions to perform when meeting a state that is on a loop: nothing to do for this algorithm, but can be defined in subclasses *)
		method process_looping_state : state_index -> unit
		
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Should we process a pi-incompatible inequality? By default yes *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method process_pi_incompatible_states : unit -> bool

		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Actions to perform when a pi-incompatible inequality is found. By default: add its negation to all previous states *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method process_negated_incompatible_inequality : LinearConstraint.p_linear_inequality -> unit

		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Actions to perform at the end of the computation of the *successors* of post^n (i.e., when this method is called, the successors were just computed). Nothing to do for this algorithm. *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method process_post_n : state_index list -> unit

		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Check whether the algorithm should terminate at the end of some post, independently of the number of states to be processed (e.g., if the constraint is already true or false) *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method check_termination_at_post_n : bool
		
		
		method compute_result : Result.imitator_result
		
		
end