(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: "EF" algorithm (unsafe w.r.t. a set of bad states) [JLR15]
 * 
 * File contributors : Étienne André
 * Created           : 2017/02/03
 * Last modified     : 2017/02/03
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoEFsynth


(************************************************************)
(* Class definition *)
(************************************************************)
class algoEFunsafeSynth :
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