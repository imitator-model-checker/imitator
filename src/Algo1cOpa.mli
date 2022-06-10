(************************************************************
 *
 *                       IMITATOR
 * 
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: algorithm for 1-clock opacity (EXPERIMENTAL)
 * 
 * File contributors : Étienne André, Johan Arcile
 * Created           : 2022/06/10
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoGeneric
open State


(************************************************************)
(* Class definition *)
(************************************************************)
class algo1cOpa : AbstractProperty.state_predicate ->
	object inherit algoGeneric
		(************************************************************)
		(* Class variables *)
		(************************************************************)

		method algorithm_name : string
		
		
		(************************************************************)
		(* Class methods *)
		(************************************************************)
		
		method run : unit -> Result.imitator_result
		
		method initialize_variables : unit
		
end
