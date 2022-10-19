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
class algoAccLoopSynth (model : AbstractModel.abstract_model) (state_predicate : AbstractProperty.state_predicate) =
	object (self) inherit algoLoopSynth model as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "Cycle"
	
			
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Detect whether a cycle is accepting *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method is_accepting scc =
		(* Accepting if at least one state in the SCC matches the state predicate *)
		List.exists (fun state_index -> 
			State.match_state_predicate model.is_accepting state_predicate (state_space#get_state state_index)
		) scc



(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
