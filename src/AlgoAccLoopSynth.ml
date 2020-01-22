(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * 
 * Module description: AccLoopSynth algorithm (synthesizes valuations for which there exists an accepting loop in the PTA)
 * 
 * File contributors : Étienne André
 * Created           : 2019/07/17
 * Last modified     : 2020/01/22
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
open AlgoLoopSynth


(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoAccLoopSynth =
	object (self) inherit algoLoopSynth as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "AcceptingCycle"
	
	
	(*** NOTE: dummy initialization ***)
	val state_predicate : AbstractProperty.state_predicate =
		(*** TODO: pass as a PARAMETER of the algorithm ***)
		(*** UGLY!!! ***)
		match (Input.get_property()).property with
			| Accepting_cycle state_predicate
				-> state_predicate
			
			(*** TODO ***)
			
			| _ -> raise (InternalError ("Problem when getting the state predicate when initializing AcceptingCycle"))
		
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Detect whether a loop is accepting *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method is_accepting scc =
		(* Retrieve the model *)
		let model = Input.get_model() in

		(* Accepting if at least one state in the SCC matches the state predicate *)
		List.exists (fun state_index -> 
			State.match_state_predicate state_predicate (StateSpace.get_state state_space state_index)
		) scc



(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
