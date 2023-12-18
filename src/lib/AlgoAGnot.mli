(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: "AG not" algorithm (safety from a set of bad states) [JLR15]
 * 
 * File contributors : Étienne André
 * Created           : 2017/02/03
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoEUgen


(************************************************************)
(* Class definition *)
(************************************************************)
class algoAGnot : AbstractModel.abstract_model -> Options.imitator_options -> AbstractProperty.state_predicate ->
	object inherit algoEUgen
		(************************************************************)
		(* Class variables *)
		(************************************************************)

		method algorithm_name : string


		(************************************************************)
		(* Class methods *)
		(************************************************************)
		
		method compute_result : Result.imitator_result
end