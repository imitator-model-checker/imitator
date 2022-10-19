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
class algoGeneralizedAccLoopSynth (model : AbstractModel.abstract_model) (state_predicate_list : AbstractProperty.state_predicate list) =
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
		(* Accepting if each state predicate is matched along the SCC *)
		List.for_all (fun state_predicate ->
			(* Accepting if at least one state in the SCC matches the state predicate *)
			List.exists (fun state_index -> 
				State.match_state_predicate model.is_accepting state_predicate (state_space#get_state state_index)
			) scc
		) state_predicate_list



(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
