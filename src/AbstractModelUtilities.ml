(************************************************************
 *
 *                       IMITATOR
 *
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: utilities for abstract models
 *
 * File contributors : Étienne André
 * Created           : 2022/10/17
 *
 ************************************************************)

open DiscreteState


(************************************************************)
(* Functions related to locations *)
(************************************************************)

(*------------------------------------------------------------*)
(* Check whether at least one local location is urgent *)
(*------------------------------------------------------------*)

let is_global_location_urgent (model : AbstractModel.abstract_model) (location : DiscreteState.global_location) : bool =
	(* Subfunction checking that one location is urgent in a given automaton *)
	let is_local_location_urgent automaton_index =
		(* Retrieve location *)
		let location_index = DiscreteState.get_location location automaton_index in
		(* Check if urgent *)
		model.is_urgent automaton_index location_index
	in
	List.exists is_local_location_urgent model.automata

(*------------------------------------------------------------*)
(* Get all invariants of model's automatas *)
(* Note : use of Input.get_model *)
(*------------------------------------------------------------*)
let get_model_invariants (model : AbstractModel.abstract_model) (location : DiscreteState.global_location) : AbstractModel.invariant list =
	List.map (fun automaton_index ->
		(* Get the current location *)
		let location_index = DiscreteState.get_location location automaton_index in
		(* Compute the invariant *)
		model.invariants automaton_index location_index
	) model.automata
