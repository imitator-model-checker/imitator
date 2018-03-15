(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Parametric deadlock-freeness
 * 
 * File contributors : Étienne André
 * Created           : 2016/02/08
 * Last modified     : 2016/10/18
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoPostStar

(************************************************************)
(* Class definition *)
(************************************************************)
class algoDeadlockFree :
	object inherit algoPostStar
		(************************************************************)
		(* Class variables *)
		(************************************************************)

		method algorithm_name : string


		(************************************************************)
		(* Class methods *)
		(************************************************************)
		
(* 		method run : unit -> Result.imitator_result *)
		
		method initialize_variables : unit
		
		(* Actions to perform when meeting a state with no successors: nothing to do for this algorithm *)
(* 		method process_deadlock_state : StateSpace.state_index -> unit *)
		
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Actions to perform with the initial state; returns true unless the initial state cannot be kept (in which case the algorithm will stop immediately) *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method process_initial_state : State.state -> bool
		

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Check whether the algorithm should terminate at the end of some post, independently of the number of states to be processed (e.g., if the constraint is already true or false) *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method check_termination_at_post_n : bool

		
		method compute_result : Result.imitator_result
end
