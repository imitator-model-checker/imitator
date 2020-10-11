(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: "EF" algorithm (unsafe w.r.t. a set of bad states) [JLR15]
 * 
 * File contributors : Étienne André
 * Created           : 2017/02/03
 * Last modified     : 2020/04/16
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoEFsynth


(************************************************************)
(* Class definition *)
(************************************************************)
class algoEFunsafeSynth : AbstractProperty.state_predicate ->
	object inherit algoEFsynth
		(************************************************************)
		(* Class variables *)
		(************************************************************)

		method algorithm_name : string


		(************************************************************)
		(* Class methods *)
		(************************************************************)
		
		method compute_result : Result.imitator_result
end