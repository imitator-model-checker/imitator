(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: main virtual class to explore the state space: only defines post-related function, i.e., to compute the successor states of ONE state
 * 
 * File contributors : Étienne André
 * Created           : 2015/12/02
 * Last modified     : 2015/12/02
 *
 ************************************************************)


(**************************************************************)
(* Modules *)
(**************************************************************)


(**************************************************************)
(* Class definition *)
(**************************************************************)
class virtual algoGeneric :
	object
		(* Start time for the algorithm *)
		val mutable start_time : float

		
		(* Name of the algorithm (to be defined in subclasses) *)
		method virtual algorithm_name : string
		
		(* Variable initialization (to be defined in subclasses) *)
		method virtual initialize_variables : unit
		
		(*------------------------------------------------------------*)
		(* Add a new state to the reachability_graph (if indeed needed) *)
		(* Also update tile_nature and slast (*** TODO: remove these operations, and move them back to their algorithms ***) *)
		(*------------------------------------------------------------*)
		(*** TODO: simplify signature by removing the orig_state_index and returning the list of actually added states ***)
		method virtual add_a_new_state : StateSpace.state_space -> StateSpace.state_index -> StateSpace.state_index list ref -> Automaton.action_index -> Location.global_location -> LinearConstraint.px_linear_constraint -> unit
		
		
		(* Compute the list of successor states of a given state, and update the state space; returns the list of new states' indexes actually added *)
		(** TODO: to get a more abstract method, should get rid of the state space, and update the state space from another function ***)
		method post_from_one_state : StateSpace.state_space -> StateSpace.state_index -> StateSpace.state_index list

	(* Main method to run the algorithm: virtual method to be defined in subclasses *)
		method virtual run : StateSpace.state -> Result.imitator_result
		
		(* Packaging the result at the end of the exploration (to be defined in subclasses) *)
		method virtual compute_result : Result.imitator_result
end
