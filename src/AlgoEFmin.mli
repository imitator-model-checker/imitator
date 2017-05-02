(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: "EF min" algorithm: minimization of a parameter valuation for which there exists a run leading to some states
 * 
 * File contributors : Étienne André
 * Created           : 2017/05/02
 * Last modified     : 2017/05/02
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoEFsynth


(************************************************************)
(* Class definition *)
(************************************************************)
class algoEFmin :
	object inherit algoEFsynth
		(************************************************************)
		(* Class variables *)
		(************************************************************)

		method algorithm_name : string


		(************************************************************)
		(* Class methods *)
		(************************************************************)
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Actions to perform when trying to minimize/maximize a parameter. Returns true if the same should be kept, false if discarded. *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method optimize_parameter : State.state -> bool


		method compute_result : Result.imitator_result
end