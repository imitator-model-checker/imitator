(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: AccLoopSynth algorithm (synthesizes valuations for which there exists an accepting loop in the PTA)
 * 
 * File contributors : Étienne André
 * Created           : 2019/07/17
 * Last modified     : 2020/08/24
 *
 ************************************************************)


(************************************************************)
(************************************************************)
(* Modules *)
(************************************************************)
(************************************************************)
open OCamlUtilities
open ImitatorUtilities
open Exceptions
open AbstractModel
open AbstractProperty
open AlgoLoopSynth


(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoAccLoopSynth (state_predicate : AbstractProperty.state_predicate) =
	object (self) inherit algoLoopSynth as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "AcceptingCycle"
	
			
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Detect whether a loop is accepting *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method is_accepting scc =
		(* Accepting if at least one state in the SCC matches the state predicate *)
		List.exists (fun state_index -> 
			State.match_state_predicate state_predicate (StateSpace.get_state state_space state_index)
		) scc



(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
