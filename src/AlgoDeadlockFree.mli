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
 * Last modified     : 2016/02/08
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
		
		method compute_result : Result.imitator_result
end
