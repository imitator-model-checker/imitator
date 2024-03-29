(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
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
class algoIMunion : AbstractModel.abstract_model -> AbstractProperty.abstract_property -> Options.imitator_options -> PVal.pval ->
	object inherit algoIMK
		(************************************************************)
		(* Class variables *)
		(************************************************************)

		(************************************************************)
		(* Class methods *)
		(************************************************************)
		method algorithm_name : string
		
		(* Actions to perform when meeting a state with no successors: add the state to the list of "last" states *)
		method process_deadlock_state : State.state_index -> unit
		
		(* Actions to perform when meeting a state that is on a loop: add the state to the list of "last" states *)
		method process_looping_state : State.state_index -> unit
		
		method compute_result : Result.imitator_result
end