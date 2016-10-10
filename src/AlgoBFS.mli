(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: main class to explore the state space in breadth-first search manner; virual class (no concrete instance of this class should be created)
 * 
 * File contributors : Étienne André
 * Created           : 2015/11/23
 * Last modified     : 2016/10/10
 *
 ************************************************************)


(**************************************************************)
(**************************************************************)
(* Modules *)
(**************************************************************)
(**************************************************************)
open AlgoStateBased
open State


(************************************************************)
(************************************************************)
(* Types *)
(************************************************************)
(************************************************************)

(* Type to define the state_index that have unexplored successors in case of premature termination *)
type unexplored_successors =
	(* Not defined (i.e., not yet defined, or no premature termination) *)
	| UnexSucc_undef
	(* A list of states with unexplored successors *)
	| UnexSucc_some of state_index list
	

(**************************************************************)
(**************************************************************)
(* Class definition *)
(**************************************************************)
(**************************************************************)
class virtual algoBFS :
	object inherit algoStateBased
		(************************************************************)
		(* Class variables *)
		(************************************************************)
		(* Status of the analysis *)
		(*** TODO: make private (while accessible to subclasses ***)
		val mutable termination_status : Result.bfs_algorithm_termination option

		(* Constraint of the initial state (used by some algorithms to initialize their variables) *)
		val mutable initial_constraint : LinearConstraint.px_linear_constraint option
		
		(* List of state_index that have unexplored successors in case of premature termination *)
		val mutable unexplored_successors : unexplored_successors
		
		
		(************************************************************)
		(* Class methods *)
		(************************************************************)

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Variable initialization *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method initialize_variables : unit


		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Set the PaTATOR termination function *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method set_patator_termination_function : (unit -> unit) -> unit
	
	
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Actions to perform at the end of the computation of the *successors* of post^n (i.e., when this method is called, the successors were just computed) *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method virtual process_post_n : state_index list -> unit

		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Check whether the algorithm should terminate at the end of some post, independently of the number of states to be processed (e.g., if the constraint is already true or false) *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method virtual check_termination_at_post_n : bool
		
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Main method to run the algorithm; implements here a BFS search, and call other functions that may be modified in subclasses *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method run : unit -> Result.imitator_result
		
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Packaging the result at the end of the exploration (to be defined in subclasses) *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method virtual compute_result : Result.imitator_result
		
end
