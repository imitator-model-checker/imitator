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

open Location


(************************************************************)
(* Functions related to locations *)
(************************************************************)

(*------------------------------------------------------------*)
(* Check whether at least one local location is urgent *)
(*------------------------------------------------------------*)

let is_global_location_urgent (model : AbstractModel.abstract_model) (location : Location.global_location) =
	(* Subfunction checking that one location is urgent in a given automaton *)
	let is_local_location_urgent automaton_index =
		(* Retrieve location *)
		let location_index = Location.get_location location automaton_index in
		(* Check if urgent *)
		model.is_urgent automaton_index location_index
	in
	List.exists is_local_location_urgent model.automata
