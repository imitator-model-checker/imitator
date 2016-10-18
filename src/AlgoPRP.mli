(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: PRP algorithm [ALNS15]
 * 
 * File contributors : Étienne André
 * Created           : 2016/01/11
 * Last modified     : 2016/10/18
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoIMK
open State


(************************************************************)
(* Class definition *)
(************************************************************)
class algoPRP :
	object inherit algoIMK
		(************************************************************)
		(* Class variables *)
		(************************************************************)
		(* Determines the mode of the algorithm: was a bad state already found? *)
		val mutable bad_state_found: bool
		
		(* Convex constraint ensuring unreachability of the bad states *)
		val mutable good_constraint : LinearConstraint.p_linear_constraint
		
		(* Non-necessarily convex constraint ensuring reachability of at least one bad state *)
		val mutable bad_constraint : LinearConstraint.p_nnconvex_constraint


		(************************************************************)
		(* Class methods *)
		(************************************************************)
		method algorithm_name : string
		
		method run : unit -> Result.imitator_result
		
		method initialize_variables : unit
		
		(*------------------------------------------------------------*)
		(* Add a new state to the reachability_graph (if indeed needed) *)
		(* Also update tile_nature and slast (*** TODO: remove these operations, and move them back to their algorithms ***) *)
		(* Return true if the state is not discarded by the algorithm, i.e., if it is either added OR was already present before *)
		(*------------------------------------------------------------*)
		(*** TODO: simplify signature by removing the orig_state_index and returning the list of actually added states ***)
		method add_a_new_state : state_index -> state_index list ref -> Automaton.action_index -> Location.global_location -> LinearConstraint.px_linear_constraint -> bool

		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Actions to perform with the initial state; returns true unless the initial state cannot be kept (in which case the algorithm will stop immediately) *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method process_initial_state : State.state -> bool
		

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Actions to perform when meeting a state with no successors: nothing to do for this algorithm *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method process_deadlock_state : state_index -> unit
		
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Actions to perform at the end of the computation of the *successors* of post^n (i.e., when this method is called, the successors were just computed). Nothing to do for this algorithm. *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method process_post_n : state_index list -> unit

		
		method compute_result : Result.imitator_result
end