(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: IMK algorithm [AS11]
 * 
 * File contributors : Étienne André
 * Created           : 2015/12/04
 * Last modified     : 2016/05/04
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoBFS
open State

(************************************************************)
(* Class definition *)
(************************************************************)
class algoIMK :
	object inherit algoBFS
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
		(* Checks a new state for pi0-compatibility .*)
		(* constr            : new state constraint            *)
		(*------------------------------------------------------------*)
		(* returns true if the state is pi0-compatible, and false otherwise *)
		(*------------------------------------------------------------*)
		(* side effect: add the negation of the p_constraint to all computed states *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method check_pi0compatibility : LinearConstraint.px_linear_constraint -> bool

		
		(*------------------------------------------------------------*)
		(* Add a new state to the reachability_graph (if indeed needed) *)
		(* Also update tile_nature and slast (*** TODO: remove these operations, and move them back to their algorithms ***) *)
		(* Return true if the state is not discarded by the algorithm, i.e., if it is either added OR was already present before *)
		(*------------------------------------------------------------*)
		(*** TODO: simplify signature by removing the orig_state_index and returning the list of actually added states ***)
		method add_a_new_state : StateSpace.state_space -> state_index -> state_index list ref -> Automaton.action_index -> Location.global_location -> LinearConstraint.px_linear_constraint -> bool

		
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