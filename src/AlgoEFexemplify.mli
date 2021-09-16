(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: EFexemplify algorithm [work in progress]
 * 
 * File contributors : Étienne André
 * Created           : 2019/07/08
 * Last modified     : 2021/09/16
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoEFsynth
open State


(************************************************************)
(* Class definition *)
(************************************************************)
class algoEFexemplify : AbstractProperty.state_predicate ->
	object inherit algoEFsynth
		(************************************************************)
		(* Class variables *)
		(************************************************************)

		method algorithm_name : string
		

		(************************************************************)
		(* Class methods *)
		(************************************************************)
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Generate counter-example(s) if required by the algorithm *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method construct_counterexamples : state_index -> unit

		
		method compute_result : Result.imitator_result
end