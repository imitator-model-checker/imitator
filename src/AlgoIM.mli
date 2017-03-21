(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: IM algorithm [ACEF09]
 * 
 * File contributors : Étienne André
 * Created           : 2016/01/06
 * Last modified     : 2016/05/04
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoIMK


(************************************************************)
(* Class definition *)
(************************************************************)
class algoIM :
	object inherit algoIMK
		(************************************************************)
		(* Class variables *)
		(************************************************************)


		(************************************************************)
		(* Class methods *)
		(************************************************************)
		method algorithm_name : string
		
		method run : unit -> Result.imitator_result
		
		method initialize_variables : unit
		
		method compute_result : Result.imitator_result
end