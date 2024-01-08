(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: IM algorithm [ACEF09]
 * 
 * File contributors : Étienne André
 * Created           : 2016/01/06
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoIMK


(************************************************************)
(* Class definition *)
(************************************************************)
class algoIM : AbstractModel.abstract_model -> Options.imitator_options -> PVal.pval ->
	object inherit algoIMK
		(************************************************************)
		(* Class variables *)
		(************************************************************)


		(************************************************************)
		(* Class methods *)
		(************************************************************)
		method algorithm_name : string
		
		method compute_result : Result.imitator_result
end