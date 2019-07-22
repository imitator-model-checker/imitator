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
 * Last modified     : 2019/07/22
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
	method algorithm_name = "AccLoopSynth"
	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Detect whether a loop is accepting *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method is_accepting scc =
		(* Retrieve the model *)
		let model = Input.get_model() in

		(* Get unreachable locations *)
		let unreachable_locations = match model.correctness_condition with
			| Some (Unreachable unreachable_global_locations)
			| Some (Reachable unreachable_global_locations)
				-> unreachable_global_locations
			| _ -> raise (InternalError "Reachability property expected in AlgoLoopActSynth.is_accepting scc")
		in

		(* Accepting if at least one state matches the reachability condition *)
		List.exists (fun state_index -> 
			State.match_unreachable_global_locations unreachable_locations (StateSpace.get_location state_space (StateSpace.get_global_location_index state_space state_index))
		) scc



(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
