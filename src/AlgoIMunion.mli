(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: IMKunion algorithm [AS11]
 * 
 * File contributors : Étienne André
 * Created           : 2016/01/08
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoIMK


(************************************************************)
(* Class definition *)
(************************************************************)
class algoIMunion : PVal.pval ->
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
		method process_deadlock_state : State.state_index -> unit
		
		(* Actions to perform when meeting a state that is on a loop: add the state to the list of "last" states *)
		method process_looping_state : State.state_index -> unit
		
		method compute_result : Result.imitator_result
end