(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: IM algorithm [ACEF09]
 * 
 * File contributors : Étienne André
 * Created           : 2016/01/06
 * Last modified     : 2016/01/08
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoIMK


(************************************************************)
(* Class definition *)
(************************************************************)
class algoIM :
	object inherit algoIMK
		(************************************************************)
		(* Class variables *)
		(************************************************************)


		(************************************************************)
		(* Class methods *)
		(************************************************************)
		method algorithm_name : string
		
		method run : unit -> Result.imitator_result
		
		method initialize_variables : unit
		
			(*------------------------------------------------------------*)
		(* Add a new state to the reachability_graph (if indeed needed) *)
		(* Also update tile_nature and slast (*** TODO: remove these operations, and move them back to their algorithms ***) *)
		(*------------------------------------------------------------*)
		(*** TODO: simplify signature by removing the orig_state_index and returning the list of actually added states ***)
(* 		method add_a_new_state : StateSpace.state_space -> StateSpace.state_index -> StateSpace.state_index list ref -> Automaton.action_index -> Location.global_location -> LinearConstraint.px_linear_constraint -> unit *)

		(* Actions to perform when meeting a state with no successors: nothing to do for this algorithm *)
		method process_deadlock_state : StateSpace.state_index -> unit
		
		method compute_result : Result.imitator_result
end