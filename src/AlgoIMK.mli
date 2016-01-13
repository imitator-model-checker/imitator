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
 * Last modified     : 2016/01/13
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoBFS

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
		
		
		(*------------------------------------------------------------*)
		(* Add a new state to the reachability_graph (if indeed needed) *)
		(* Also update tile_nature and slast (*** TODO: remove these operations, and move them back to their algorithms ***) *)
		(*------------------------------------------------------------*)
		(*** TODO: simplify signature by removing the orig_state_index and returning the list of actually added states ***)
		method add_a_new_state : StateSpace.state_space -> StateSpace.state_index -> StateSpace.state_index list ref -> Automaton.action_index -> Location.global_location -> LinearConstraint.px_linear_constraint -> unit

		
		(* Actions to perform when meeting a state with no successors: nothing to do for this algorithm *)
		method process_deadlock_state : StateSpace.state_index -> unit
		
		
		(* Actions to perform when meeting a state that is on a loop: nothing to do for this algorithm, but can be defined in subclasses *)
		method process_looping_state : StateSpace.state_index -> unit
		
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Should we explore a pi-incompatible inequality? By default yes *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method explore_pi_incompatible_states : unit -> bool

		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Actions to perform when a pi-incompatible inequality is found. By default: add its negation to all previous states *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method process_negated_incompatible_inequality : LinearConstraint.p_linear_inequality -> unit

		
		method compute_result : Result.imitator_result
		
end