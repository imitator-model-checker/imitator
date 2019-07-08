(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * 
 * Module description: EFexemplify algorithm [work in progress]
 * 
 * File contributors : Étienne André
 * Created           : 2019/07/08
 * Last modified     : 2019/07/08
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
class algoEFexemplify :
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
		method process_counterexample : state_index -> unit

		
		method compute_result : Result.imitator_result
end