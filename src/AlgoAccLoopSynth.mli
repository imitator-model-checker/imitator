(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: AccLoopSynth algorithm (synthesizes valuations for which there exists an accepting cycle in the PTA)
 * 
 * File contributors : Étienne André
 * Created           : 2019/07/17
 * Last modified     : 2020/04/21
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
class algoAccLoopSynth : AbstractProperty.state_predicate ->
	object inherit algoLoopSynth
		(************************************************************)
		(* Class variables *)
		(************************************************************)

		method algorithm_name : string
		

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Detect whether a cycle is accepting *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method is_accepting : StateSpace.scc ->  bool


end