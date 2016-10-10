(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Non-zenoness emptiness check using CUB transformation (synthesizes valuations for which there exists a non-zeno loop in the PTA)
 * 
 * File contributors : Étienne André
 * Created           : 2016/10/10
 * Last modified     : 2016/10/10
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoLoopSynth
open State


(************************************************************)
(* Class definition *)
(************************************************************)
class algoNZCUB :
	object inherit algoLoopSynth
		(************************************************************)
		(* Class variables *)
		(************************************************************)

		method algorithm_name : string


		(************************************************************)
		(* Class methods *)
		(************************************************************)
		
		method initialize_variables : unit
		

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Actions to perform when found a loop, after updating the state space *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method process_loop_after_state_space_update : state_index -> unit
		

		method compute_result : Result.imitator_result
end
