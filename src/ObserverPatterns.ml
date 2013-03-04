(*****************************************************************
 *
 *                       IMITATOR
 * 
 * File containing the operations linked to the observer patterns
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        Etienne Andre
 * 
 * Created:       2013/02/04
 * Last modified: 2013/03/04
 *
 ****************************************************************)


(****************************************************************)
(** Modules *)
(****************************************************************)
open Global
open AbstractModel


(****************************************************************)
(** Constants *)
(****************************************************************)
let automaton_name = "observer"
let clock_name = "x_obs"
let location_prefix = "locobs_"


let location_name location_index =
	location_prefix ^ (string_of_int location_index)

let truec = LinearConstraint.true_constraint


(****************************************************************)
(** Functions *)
(****************************************************************)

(* Create the new automata and new clocks necessary for the observer *)
let new_elements = function
	| None -> (None , None)
	| Some property ->
	begin
		match property with
		(* Not a real observer: does not build anything *)
		| ParsingStructure.Unreachable_location _ -> (None , None)
		
		(* Untimed observers: add automaton, does not add clock *)
		| ParsingStructure.Action_precedence_acyclic _
		| ParsingStructure.Action_precedence_cyclic _
		| ParsingStructure.Action_precedence_cyclicstrict _
		| ParsingStructure.Eventual_response_acyclic _
		| ParsingStructure.Eventual_response_cyclic _
		| ParsingStructure.Eventual_response_cyclicstrict _
		| ParsingStructure.Sequence_acyclic _
		| ParsingStructure.Sequence_cyclic _
			-> (Some automaton_name, None)
		
		(* Timed observers: add automaton, add clock *)
		| ParsingStructure.Action_deadline _
		| ParsingStructure.TB_Action_precedence_acyclic _
		| ParsingStructure.TB_Action_precedence_cyclic _
		| ParsingStructure.TB_Action_precedence_cyclicstrict _
		| ParsingStructure.TB_response_acyclic _
		| ParsingStructure.TB_response_cyclic _
		| ParsingStructure.TB_response_cyclicstrict _
			-> (Some automaton_name, Some clock_name)
	
	end

(* Get the number of locations for this observer *)
let get_nb_locations = function
	| None -> 0
	| Some property ->
	begin
		match property with
		(* Not a real observer: does not build anything *)
		| ParsingStructure.Unreachable_location _ -> 0
		
		| ParsingStructure.Action_precedence_acyclic _
		| ParsingStructure.Action_precedence_cyclic _
		| ParsingStructure.Action_precedence_cyclicstrict _
			-> 3
		| ParsingStructure.Eventual_response_acyclic _ -> 3
		| ParsingStructure.Eventual_response_cyclic _ -> 2
		| ParsingStructure.Eventual_response_cyclicstrict _ -> 3
		| ParsingStructure.Action_deadline _ -> 3
		| ParsingStructure.TB_Action_precedence_acyclic _ -> 4
		| ParsingStructure.TB_Action_precedence_cyclic _ -> 3
		| ParsingStructure.TB_Action_precedence_cyclicstrict _ -> 3
		| ParsingStructure.TB_response_acyclic _ -> 4
		| ParsingStructure.TB_response_cyclic _ -> 3
		| ParsingStructure.TB_response_cyclicstrict _ -> 3
		| ParsingStructure.Sequence_acyclic list_of_actions -> (List.length list_of_actions) + 2
		| ParsingStructure.Sequence_cyclic list_of_actions -> (List.length list_of_actions) + 1
	end


(* Create the list of location indexes for this observer *)
let get_locations property =
	let nb = get_nb_locations property in
	Array.of_list(
		List.map location_name(
			if nb = 0 then [] else
				list_of_interval 0 (nb-1)
		)
	)

(*------------------------------------------------------------*)
(* Create the observer; returns:
	- Actions per automaton
	- Actions per location
	- Transitions
	- Invariants
*)
(*------------------------------------------------------------*)
let get_automaton property = 
	(* Create the common structures *)
	let initialize_structures nb_locations all_actions =
		let nb_actions = List.length all_actions in
		(*Array for actions for location *)
		let actions_per_location = 	Array.make nb_locations [] in
		(* All observers are complete: fill *)
		for i = 0 to nb_locations-1 do
			actions_per_location.(i) <- all_actions;
		done;
		(* Return : *)
		actions_per_location,
		(* Array for invariants *)
		Array.make nb_locations (truec ()),
		(* Array for transitions *)
		Array.make nb_locations (Array.make nb_actions []),
		(* The array of transitions for locations who allow all actions *)
		function location_index -> Array.make nb_actions [truec (), No_update , [], location_index]
	in
	match property with
	| Noproperty -> raise (InternalError("The function 'get_automaton property' should not be called in case of no observer."))
	(* Not a real observer: does not build anything *)
	| Unreachable_location _ -> raise (InternalError("The function 'get_automaton property' should not be called in case of a degenerate observer."))
	
	
	| Action_precedence_acyclic (a1, a2) -> 
		let all_actions = [a1;a2] in
		(* Initialize *)
		let actions_per_location, invariants, transitions, allow_all = initialize_structures 3 all_actions in
		(* Compute transitions *)
		transitions.(0).(a1) <- [truec (), No_update, [], 1];
(* 		transitions.(1) <- allow_all 1; *)
		(* Actions *)
		all_actions
		,
		actions_per_location
		,
		invariants
		,
		transitions
		
	
	
(*		| Action_precedence_acyclic _
	| Action_precedence_cyclic _
	| Action_precedence_cyclicstrict _
		-> 3
	| Eventual_response_acyclic _ -> 3
	| Eventual_response_cyclic _ -> 2
	| Eventual_response_cyclicstrict _ -> 3
	| Action_deadline _ -> 3
	| TB_Action_precedence_acyclic _ -> 4
	| TB_Action_precedence_cyclic _ -> 3
	| TB_Action_precedence_cyclicstrict _ -> 3
	| TB_response_acyclic _ -> 4
	| TB_response_cyclic _ -> 3
	| TB_response_cyclicstrict _ -> 3
	| Sequence_acyclic list_of_actions -> (List.length list_of_actions) + 2
	| Sequence_cyclic list_of_actions -> (List.length list_of_actions) + 1*)
	| _ -> raise (InternalError("Not implemented"))


