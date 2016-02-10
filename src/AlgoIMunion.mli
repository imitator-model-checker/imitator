(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: IMKunion algorithm [AS11]
 * 
 * File contributors : Étienne André
 * Created           : 2016/01/08
 * Last modified     : 2016/02/10
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoIMK


(************************************************************)
(* Class definition *)
(************************************************************)
class algoIMunion :
	object inherit algoIMK
		(************************************************************)
		(* Class variables *)
		(************************************************************)

		(* Non-necessarily convex parameter constraint *)
		val mutable result: LinearConstraint.p_nnconvex_constraint 


		(************************************************************)
		(* Class methods *)
		(************************************************************)
		method algorithm_name : string
		
		method run : unit -> Result.imitator_result
		
		method initialize_variables : unit
		
		(* Actions to perform when meeting a state with no successors: add the state to the list of "last" states *)
		method process_deadlock_state : StateSpace.state_index -> unit
		
		(* Actions to perform when meeting a state that is on a loop: add the state to the list of "last" states *)
		method process_looping_state : StateSpace.state_index -> unit
		
		method compute_result : Result.imitator_result
end