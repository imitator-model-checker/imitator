(************************************************************
 *
 *                       IMITATOR
 * 
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: GeneralizedAccLoopSynth algorithm (synthesizes valuations for which there exists an accepting cycle in the PTA verifying a generalized condition given in the form of a *set* of state predicates)
 * 
 * File contributors : Étienne André
 * Created           : 2021/09/01
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
class algoGeneralizedAccLoopSynth : AbstractProperty.state_predicate list ->
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
