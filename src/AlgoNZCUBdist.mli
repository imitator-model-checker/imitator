(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13 (France)
 * 
 * Module description: Non-zenoness emptiness check using CUB transformation (synthesizes valuations for which there exists a non-zeno loop in the PTA). Distributed version.
 * 
 * File contributors : Étienne André
 * Created           : 2017/10/03
 * Last modified     : 2017/10/03
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoNZCUB
open State


(************************************************************)
(* Class definition *)
(************************************************************)
class algoNZCUBdist :
	object inherit algoNZCUB
		(************************************************************)
		(* Class variables *)
		(************************************************************)

		method algorithm_name : string


		(************************************************************)
		(* Class methods *)
		(************************************************************)
		

end
