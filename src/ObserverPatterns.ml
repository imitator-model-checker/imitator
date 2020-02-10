(************************************************************
 *
 *                       IMITATOR
 *
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: operations linked to the observer patterns
 *
 * File contributors : Étienne André, Jaime Arias
 * Created:       2013/02/04
 * Last modified: 2020/02/10
 *
 ************************************************************)
 
(************************************************************)
(** Modules *)
(************************************************************)
open Exceptions
open OCamlUtilities
open ImitatorUtilities
open ParsingStructure
open AbstractModel


(************************************************************)
(** Constants *)
(************************************************************)
let observer_automaton_name	= "automatically_generated_observer"
let observer_clock_name		= "automatically_generated_x_obs"
let location_prefix			= "loc_AutoGen_obs_"


let location_name location_index =
	location_prefix ^ (string_of_int location_index)

(** Shortcuts *)
(* let truec = LinearConstraint.pxd_true_constraint *)
(*** NOTE: "()" to force execution of the constructor at each call ***)
let true_discrete_continuous_guard () = {
	discrete_guard   			= None;
	prebuilt_continuous_guard	= Some (LinearConstraint.px_true_constraint ());
	continuous_guard			= None;
}



(************************************************************)
(** Useful (parameterized) constants *)
(************************************************************)

(** Creates a new update *)
let create_update clock_updates discrete_updates conditional_updates =
	{clock = clock_updates;
	 discrete = discrete_updates;
	 conditional = conditional_updates}

(** Creates a transition without guards and updates *)
let untimedt action_index target_index =
	[{
		guard		= True_guard;
		action		= action_index;
		updates		= create_update [] [] [];
		target		= target_index;
	}]

(* Constraint x <= d, with 'd' a LinearConstraint.p_linear_term : d - x >= 0 *)
let ct_x_leq_d x d = {
	discrete_guard   			= None;
	prebuilt_continuous_guard	= Some (LinearConstraint.px_linear_constraint_of_clock_and_parameters x LinearConstraint.Op_ge d true);
	continuous_guard			= None;
}


(* Constraint x >= d, with 'd' a LinearConstraint.p_linear_term : x - d >= 0 *)
let ct_x_geq_d x d = {
	discrete_guard   			= None;
	prebuilt_continuous_guard	= Some (LinearConstraint.px_linear_constraint_of_clock_and_parameters x LinearConstraint.Op_ge d false);
	continuous_guard			= None;
}
	


(*(* Linear inequality x = d, with d LinearConstraint.p_linear_term : d - x = 0 *)
let lt_x_eq_d x d =
	(* Build the linear term *)
	let lt = LinearConstraint.add_linear_terms d (LinearConstraint.make_linear_term [NumConst.minus_one, x] NumConst.zero) in
	(* Build linear inequality *)
	LinearConstraint.make_linear_inequality lt LinearConstraint.Op_eq*)


(* Constraint x = d, with d LinearConstraint.p_linear_term : d - x = 0 *)
let ct_x_eq_d x d = {
	discrete_guard   			= None;
	prebuilt_continuous_guard	= Some (LinearConstraint.px_linear_constraint_of_clock_and_parameters x LinearConstraint.Op_eq d false);
	continuous_guard			= None;
}
	


(* Linear constraint x = 0 *)
let lc_x_eq_0 x =
	let d = LinearConstraint.make_p_linear_term [] NumConst.zero in
	{
	discrete_guard   			= None;
	prebuilt_continuous_guard	= Some (LinearConstraint.px_linear_constraint_of_clock_and_parameters x LinearConstraint.Op_ge d false);
	continuous_guard			= None;
}



(************************************************************)
(** Functions *)
(************************************************************)

(** Returns true whether the observer requires one clock *)
let needs_clock (parsed_property : ParsingStructure.parsed_property) =
	match parsed_property.property with
	| Parsed_EF _
	| Parsed_AGnot _

	| Parsed_EFpmin _
	| Parsed_EFtmin _

	| Parsed_Cycle
	| Parsed_Acc_Cycle _

	| Parsed_IM _

		->
	false
(* 	| Parsed_Action_deadline _ -> true *)
	(*** TODO: finish later ***)
(* 	| _ -> raise (NotImplemented "ObserverPatterns.needs_clock") *)


(*(* Create a list of unreachable_global_location from a single bad location *)
let single_unreachable_location automaton_index location_index =
	(* Create the (single) global location *)
	let unreachable_global_location = {
		unreachable_locations= [(automaton_index, location_index)];
		discrete_constraints =  [];
	}
	in
	Unreachable [unreachable_global_location]*)


(* Create the new automata and new clocks necessary for the observer *)
let new_elements (parsed_property : ParsingStructure.parsed_property) =
	match parsed_property.property with
	(* No observer required: does not build anything *)
	| Parsed_EF _
	| Parsed_AGnot _
	
	| Parsed_EFpmin _
	| Parsed_EFtmin _

	| Parsed_Cycle
	| Parsed_Acc_Cycle _

	| Parsed_IM _
		-> (None , None)
	
(*	| Parsed_Action_deadline _
		-> (Some observer_automaton_name, Some observer_clock_name)
	(*** TODO: finish later ***)
	| _ -> raise (NotImplemented "ObserverPatterns.new_elements")*)

(*	(* Untimed observers: add automaton, does not add clock *)
	| ParsingStructure.Action_precedence_acyclic _
	| ParsingStructure.Action_precedence_cyclic _
	| ParsingStructure.Action_precedence_cyclicstrict _
	(*** NOT IMPLEMENTED ***)
(*		| ParsingStructure.Eventual_response_acyclic _
	| ParsingStructure.Eventual_response_cyclic _
	| ParsingStructure.Eventual_response_cyclicstrict _*)
	| ParsingStructure.Sequence_acyclic _
	| ParsingStructure.Sequence_cyclic _
		-> (Some observer_automaton_name, None)

	(* Timed observers: add automaton, add clock *)
	| ParsingStructure.Action_deadline _
	| ParsingStructure.TB_Action_precedence_acyclic _
	| ParsingStructure.TB_Action_precedence_cyclic _
	| ParsingStructure.TB_Action_precedence_cyclicstrict _
	| ParsingStructure.TB_response_acyclic _
	| ParsingStructure.TB_response_cyclic _
	| ParsingStructure.TB_response_cyclicstrict _
		-> (Some observer_automaton_name, Some observer_clock_name)*)

(* Get the number of locations for this observer *)
let get_nb_locations (parsed_property : ParsingStructure.parsed_property) =
	match parsed_property.property with
	(* Not a real observer: does not build anything *)
	| Parsed_EF _
	| Parsed_AGnot _
	
	| Parsed_EFpmin _
	| Parsed_EFtmin _
	
	| Parsed_Cycle
	| Parsed_Acc_Cycle _

	| Parsed_IM _
	
		-> 0
(* 	| Parsed_Action_deadline _ -> 3 *)
	(*** TODO: finish later ***)
(* 	| _ -> raise (NotImplemented "ObserverPatterns.get_nb_locations") *)
	(*
		(* Not a real observer: does not build anything *)
		| ParsingStructure.Parsed_unreachable_locations _ -> 0

		| ParsingStructure.Action_precedence_acyclic _
		| ParsingStructure.Action_precedence_cyclic _
		| ParsingStructure.Action_precedence_cyclicstrict _
			-> 3
		(*** NOT IMPLEMENTED ***)
(*		| ParsingStructure.Eventual_response_acyclic _ -> 3
		| ParsingStructure.Eventual_response_cyclic _ -> 2
		| ParsingStructure.Eventual_response_cyclicstrict _ -> 3*)
		| ParsingStructure.Action_deadline _ -> 3
		| ParsingStructure.TB_Action_precedence_acyclic _ -> 4
		| ParsingStructure.TB_Action_precedence_cyclic _ -> 3
		| ParsingStructure.TB_Action_precedence_cyclicstrict _ -> 3
		| ParsingStructure.TB_response_acyclic _ -> 4
		| ParsingStructure.TB_response_cyclic _ -> 3
		| ParsingStructure.TB_response_cyclicstrict _ -> 3
		| ParsingStructure.Sequence_acyclic list_of_actions -> (List.length list_of_actions) + 2
		| ParsingStructure.Sequence_cyclic list_of_actions -> (List.length list_of_actions) + 1
			*)


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
(* Create the observer;
	Takes as parameters the number of actions, the automaton_index, the nosync index for the observer, the local clock id for the observer
	Returns:
	- Actions per automaton
	- Actions per location
	- Transitions
	- Invariants
*)
(*------------------------------------------------------------*)
let get_automaton nb_actions automaton_index nosync_index x_obs (parsed_property : ParsingStructure.parsed_property) =
	(* Create the common structures *)
	let initialize_structures nb_locations all_actions =
		(* Array for actions for location *)
		let actions_per_location = 	Array.make nb_locations [] in
		(* Array for urgency of locations (not used: all locations non urgent) *)
		let observer_location_urgency = Array.make nb_locations Location_nonurgent in
		(* All observers are complete: fill *)
		for i = 0 to nb_locations-1 do
			actions_per_location.(i) <- all_actions;
		done;
		(* Return : *)
		actions_per_location,
		(* Urgency *)
		observer_location_urgency,
		(* Array for invariants *)
		Array.make nb_locations (true_discrete_continuous_guard ()),
		(* Array for transitions *)
		(let transitions = Array.make nb_locations (Array.make nb_actions []) in
			(* Initialize transitions ! Otherwise pointers problems *)
			for location_index = 0 to nb_locations - 1 do
				transitions.(location_index) <- Array.make nb_actions [];
			done;
		transitions
		)
		,
		(* The array of transitions for locations who allow all declared observer actions for a location, as self-loops *)
		function location_index ->
			let allow_all = Array.make nb_actions [] in
			List.iter (fun action_index -> allow_all.(action_index) <- untimedt action_index location_index) all_actions;
			allow_all
	in

	match parsed_property with
		
	(*** TODO: finish later ***)
	| _ -> raise (NotImplemented "ObserverPatterns.get_automaton")

(*	| Action_precedence_acyclic (a1, a2) ->
		let nb_locations = 3 in
		let all_actions = [a1;a2] in
		(* Initialize *)
		let actions_per_location, observer_location_urgency, invariants, transitions, allow_all = initialize_structures nb_locations all_actions in
		(* No need to update actions per location (no nosync action here) *)

		(* Compute transitions *)
		transitions.(0).(a1) <- untimedt a1 1;
		transitions.(0).(a2) <- untimedt a2 2;
		transitions.(1) <- allow_all 1;
		transitions.(2) <- allow_all 2;
		(* Return structure *)
		all_actions, actions_per_location, observer_location_urgency, invariants, transitions,
		(* No init inequality *)
		None,
		(* Reduce to reachability property *)
		single_unreachable_location automaton_index 2


	| Action_precedence_cyclic (a1, a2) ->
		let nb_locations = 3 in
		let all_actions = [a1;a2] in
		(* Initialize *)
		let actions_per_location, observer_location_urgency, invariants, transitions, allow_all = initialize_structures nb_locations all_actions in
		(* No need to update actions per location (no nosync action here) *)

		(* Compute transitions *)
		transitions.(0).(a1) <- untimedt a1 1;
		transitions.(0).(a2) <- untimedt a2 2;
		transitions.(1).(a1) <- untimedt a1 1;
		transitions.(1).(a2) <- untimedt a2 0;
		transitions.(2) <- allow_all 2;
		(*print_message Debug_standard ("Index of a1: " ^ (string_of_int a1) ^ " ; index of a2: " ^ (string_of_int a2) ^ "");
		for location_index = 0 to nb_locations - 1 do
			for action_index = 0 to nb_actions - 1 do
				let t = transitions.(location_index).(action_index) in
				match t with
				| [] -> print_message Debug_standard ("Location " ^ (string_of_int location_index) ^ "  -> action " ^ (string_of_int action_index) ^ " : nothing")
				| [(_, _, _, target_index)] -> print_message Debug_standard ("Location " ^ (string_of_int location_index) ^ "  -> action " ^ (string_of_int action_index) ^ " : " ^ (string_of_int target_index) ^ "")
				| _ -> print_message Debug_standard ("Location " ^ (string_of_int location_index) ^ "  -> action " ^ (string_of_int action_index) ^ " : something else")
			done;
		done;*)
		(* Return structure *)
		all_actions, actions_per_location, observer_location_urgency, invariants, transitions,
		(* No init inequality *)
		None,
		(* Reduce to reachability property *)
		single_unreachable_location automaton_index 2


	| Action_precedence_cyclicstrict (a1, a2) ->
		let nb_locations = 3 in
		let all_actions = [a1;a2] in
		(* Initialize *)
		let actions_per_location, observer_location_urgency, invariants, transitions, allow_all = initialize_structures nb_locations all_actions in
		(* No need to update actions per location (no nosync action here) *)

		(* Compute transitions *)
		transitions.(0).(a1) <- untimedt a1 1;
		transitions.(0).(a2) <- untimedt a2 2;
		transitions.(1).(a1) <- untimedt a1 2;
		transitions.(1).(a2) <- untimedt a2 0;
		transitions.(2) <- allow_all 2;
		(* Return structure *)
		all_actions, actions_per_location, observer_location_urgency, invariants, transitions,
		(* No init inequality *)
		None,
		(* Reduce to reachability property *)
		single_unreachable_location automaton_index 2


		(*** NOT IMPLEMENTED ***)
(*	| Eventual_response_acyclic (a1, a2)
	| Eventual_response_cyclic (a1, a2)
	| Eventual_response_cyclicstrict (a1, a2)
		-> raise (InternalError("Observer not implemented."))*)


	| Action_deadline (a, d) ->
		let nb_locations = 3 in
		let all_actions = [a] in
		(* Initialize *)
		let actions_per_location, observer_location_urgency, invariants, transitions, allow_all = initialize_structures nb_locations all_actions in
		(* Update actions per location for the silent action *)
		actions_per_location.(0) <- nosync_index :: all_actions;
		(* Update invariants *)
		invariants.(0) <- ct_x_leq_d x_obs d ;
		(* Compute transitions *)
		transitions.(0).(a) <- untimedt a 1;
		transitions.(0).(nosync_index) <-
			[{
				guard		= Continuous_guard (ct_x_eq_d x_obs d);
				action		= nosync_index;
				updates		= create_update No_update [] [];
				target		= 2;
			}];
		transitions.(1) <- allow_all 1;
		transitions.(2) <- allow_all 2;
		(* Return structure (and add silent action) *)
		nosync_index :: all_actions, actions_per_location, observer_location_urgency, invariants, transitions,
		(* Return x_obs = 0 *)
		Some (lc_x_eq_0 x_obs),
		(* Reduce to reachability property *)
		single_unreachable_location automaton_index 2


	| TB_Action_precedence_acyclic (a1, a2, d) ->
		let nb_locations = 4 in
		let all_actions = [a1; a2] in
		(* Initialize *)
		let actions_per_location, observer_location_urgency, invariants, transitions, allow_all = initialize_structures nb_locations all_actions in
		(* No need to update actions per location (no silent action here) *)
		(* Compute transitions *)
		transitions.(0).(a1) <- 
			[{
				guard		= True_guard;
				action		= a1;
				updates		= create_update (Resets [x_obs]) [] [];
				target		= 1;
			}];
		transitions.(0).(a2) <- untimedt a2 3;
		transitions.(1).(a1) <-
			[{
				guard		= True_guard;
				action		= a1;
				updates		= create_update (Resets [x_obs]) [] [];
				target		= 1;
			}];
		transitions.(1).(a2) <-
			[
			{
				guard		= Continuous_guard (ct_x_leq_d x_obs d);
				action		= a2;
				updates		= create_update No_update [] [];
				target		= 2;
			}
			;
			{
				guard		= Continuous_guard (ct_x_geq_d x_obs d);
				action		= a2;
				updates		= create_update No_update [] [];
				target		= 3;
			}
			];
		transitions.(2) <- allow_all 2;
		transitions.(3) <- allow_all 3;
		(* Return structure *)
		all_actions, actions_per_location, observer_location_urgency, invariants, transitions,
		(* No init constraint *)
		None,
		(* Reduce to reachability property *)
		single_unreachable_location automaton_index 3


	| TB_Action_precedence_cyclic (a1, a2, d) ->
		let nb_locations = 3 in
		let all_actions = [a1; a2] in
		(* Initialize *)
		let actions_per_location, observer_location_urgency, invariants, transitions, allow_all = initialize_structures nb_locations all_actions in
		(* No need to update actions per location (no silent action here) *)
		(* Compute transitions *)
		transitions.(0).(a1) <-
			[{
				guard		= True_guard;
				action		= a1;
				updates		= create_update (Resets [x_obs]) [] [];
				target		= 1;
			}];
		transitions.(0).(a2) <- untimedt a2 2;
		transitions.(1).(a1) <-
			[{
				guard		= True_guard;
				action		= a1;
				updates		= create_update (Resets [x_obs]) [] [];
				target		= 1;
			}];
		transitions.(1).(a2) <-
			[
			{
				guard		= Continuous_guard (ct_x_leq_d x_obs d);
				action		= a2;
				updates		= create_update No_update [] [];
				target		= 0;
			}
			;
			{
				guard		= Continuous_guard (ct_x_geq_d x_obs d);
				action		= a2;
				updates		= create_update No_update [] [];
				target		= 2;
			}
			];
		transitions.(2) <- allow_all 2;
		(* Return structure *)
		all_actions, actions_per_location, observer_location_urgency, invariants, transitions,
		(* No init constraint *)
		None,
		(* Reduce to reachability property *)
		single_unreachable_location automaton_index 2


	| TB_Action_precedence_cyclicstrict (a1, a2, d) ->
		let nb_locations = 3 in
		let all_actions = [a1; a2] in
		(* Initialize *)
		let actions_per_location, observer_location_urgency, invariants, transitions, allow_all = initialize_structures nb_locations all_actions in
		(* No need to update actions per location (no silent action here) *)
		(* Compute transitions *)
		transitions.(0).(a1) <-
			[{
				guard		= True_guard;
				action		= a1;
				updates		= create_update (Resets [x_obs]) [] [];
				target		= 1;
			}];
		transitions.(0).(a2) <- untimedt a2 2;
		transitions.(1).(a1) <- untimedt a1 2;
		transitions.(1).(a2) <-
			[
			{
				guard		= Continuous_guard (ct_x_leq_d x_obs d);
				action		= a2;
				updates		= create_update No_update [] [];
				target		= 0;
			}
			;
			{
				guard		= Continuous_guard (ct_x_geq_d x_obs d);
				action		= a2;
				updates		= create_update No_update [] [];
				target		= 2;
			}
			];
		transitions.(2) <- allow_all 2;
		(* Return structure *)
		all_actions, actions_per_location, observer_location_urgency, invariants, transitions,
		(* No init constraint *)
		None,
		(* Reduce to reachability property *)
		single_unreachable_location automaton_index 2


	| TB_response_acyclic (a1, a2, d) ->
		let nb_locations = 4 in
		let all_actions = [a1; a2] in
		(* Initialize *)
		let actions_per_location, observer_location_urgency, invariants, transitions, allow_all = initialize_structures nb_locations all_actions in
		(* Update actions per location for the silent action *)
		actions_per_location.(1) <- nosync_index :: all_actions;
		(* Update invariants *)
		invariants.(1) <- ct_x_leq_d x_obs d;
		(* Compute transitions *)
		transitions.(0).(a1) <-
			[{
				guard		= True_guard;
				action		= a1;
				updates		= create_update (Resets [x_obs]) [] [];
				target		= 1;
			}];
		transitions.(0).(a2) <- untimedt a2 0;
		transitions.(1).(a1) <- untimedt a1 1;
		transitions.(1).(a2) <- untimedt a2 2;
		transitions.(1).(nosync_index) <-
			[{
				guard		= Continuous_guard (ct_x_eq_d x_obs d);
				action		= nosync_index;
				updates		= create_update No_update [] [];
				target		= 3;
			}];
		transitions.(2) <- allow_all 2;
		transitions.(3) <- allow_all 3;
		(* Return structure (and add silent action) *)
		nosync_index :: all_actions, actions_per_location, observer_location_urgency, invariants, transitions,
		(* No init constraint *)
		None,
		(* Reduce to reachability property *)
		single_unreachable_location automaton_index 3

	| TB_response_cyclic (a1, a2, d) ->
		let nb_locations = 3 in
		let all_actions = [a1; a2] in
		(* Initialize *)
		let actions_per_location, observer_location_urgency, invariants, transitions, allow_all = initialize_structures nb_locations all_actions in
		(* Update actions per location for the silent action *)
		actions_per_location.(1) <- nosync_index :: all_actions;
		(* Update invariants *)
		invariants.(1) <- ct_x_leq_d x_obs d;
		(* Compute transitions *)
		transitions.(0).(a1) <-
			[{
				guard		= True_guard;
				action		= a1;
				updates		= create_update (Resets [x_obs]) [] [];
				target		= 1;
			}];
		transitions.(0).(a2) <- untimedt a2 0;
		transitions.(1).(a1) <- untimedt a1 1;
		transitions.(1).(a2) <- untimedt a2 0;
		transitions.(1).(nosync_index) <-
			[{
				guard		= Continuous_guard (ct_x_eq_d x_obs d);
				action		= nosync_index;
				updates		= create_update No_update [] [];
				target		= 2;
			}];
		transitions.(2) <- allow_all 2;
		(* Return structure (and add silent action) *)
		nosync_index :: all_actions, actions_per_location, observer_location_urgency, invariants, transitions,
		(* No init constraint *)
		None,
		(* Reduce to reachability property *)
		single_unreachable_location automaton_index 2


	| TB_response_cyclicstrict (a1, a2, d) ->
		let nb_locations = 3 in
		let all_actions = [a1; a2] in
		(* Initialize *)
		let actions_per_location, observer_location_urgency, invariants, transitions, allow_all = initialize_structures nb_locations all_actions in
		(* Update actions per location for the silent action *)
		actions_per_location.(1) <- nosync_index :: all_actions;
		(* Update invariants *)
		invariants.(1) <- ct_x_leq_d x_obs d;
		(* Compute transitions *)
		transitions.(0).(a1) <-
			[{
				guard		= True_guard;
				action		= a1;
				updates		= create_update (Resets [x_obs]) [] [];
				target		= 1;
			}];
		transitions.(0).(a2) <- untimedt a2 2;
		transitions.(1).(a1) <- untimedt a1 2;
		transitions.(1).(a2) <- untimedt a2 0;
		transitions.(1).(nosync_index) <-
			[{
				guard		= Continuous_guard (ct_x_eq_d x_obs d);
				action		= nosync_index;
				updates		= create_update No_update [] [];
				target		= 2;
			}];
		transitions.(2) <- allow_all 2;
		(* Return structure (and add silent action) *)
		nosync_index :: all_actions, actions_per_location, observer_location_urgency, invariants, transitions,
		(* No init constraint *)
		None,
		(* Reduce to reachability property *)
		single_unreachable_location automaton_index 2


	| Sequence_acyclic list_of_actions ->
		let nb_locations = (List.length list_of_actions) + 2 in
		let all_actions = list_of_actions in
		(* Initialize *)
		let actions_per_location, observer_location_urgency, invariants, transitions, allow_all = initialize_structures nb_locations all_actions in

		(* Define 2 useful location indexes *)
		let lf = nb_locations - 2 in
		let lbad = nb_locations - 1 in

		(* No need to update actions per location (no silent action here) *)

		(* No need to update invariants *)

		(* Compute transitions *)
		for i = 0 to lf-1 do
			(* Add the action l_i --{a_i}--> l_i+1 *)
			let a_i = List.nth list_of_actions i in
			transitions.(i).(a_i) <- untimedt a_i (i+1);
			(* Add the transitions to bad *)
			List.iter(fun action_index ->
				if action_index <> a_i then (
					transitions.(i).(action_index) <- untimedt action_index lbad;
				);
			) list_of_actions;
		done;
		(* Add self-loops *)
		transitions.(lf) <- allow_all lf;
		transitions.(lbad) <- allow_all lbad;

		(* Return structure *)
		all_actions, actions_per_location, observer_location_urgency, invariants, transitions,
		(* No init constraint *)
		None,
		(* Reduce to reachability property *)
		single_unreachable_location automaton_index lbad


	| Sequence_cyclic list_of_actions ->
		let nb_locations = (List.length list_of_actions) + 1 in
		let all_actions = list_of_actions in
		(* Initialize *)
		let actions_per_location, observer_location_urgency, invariants, transitions, allow_all = initialize_structures nb_locations all_actions in

		(* Define 2 useful location indexes *)
		let lbad = nb_locations - 1 in

		(* No need to update actions per location (no silent action here) *)

		(* No need to update invariants *)

		(* Compute transitions *)
		(* Locations 0 to n-2 *)
		for i = 0 to lbad-1 do
			let a_i = List.nth list_of_actions i in
			(* Add the action l_i --{a_i}--> l_i+1, except for location n-1 *)
			if i <> lbad-1 then(
				transitions.(i).(a_i) <- untimedt a_i (i+1);
			);
			(* Add the transitions to bad *)
			List.iter(fun action_index ->
				if action_index <> a_i then (
					transitions.(i).(action_index) <- untimedt action_index lbad;
				);
			) list_of_actions;
		done;
		(* Location n-1: add the loop back to loc_0 *)
		let a_n = List.nth list_of_actions (List.length list_of_actions - 1) in
		transitions.(lbad-1).(a_n) <- untimedt a_n 0;
		(* Add self-loops *)
		transitions.(lbad) <- allow_all lbad;

		(* Return structure *)
		all_actions, actions_per_location, observer_location_urgency, invariants, transitions,
		(* No init constraint *)
		None,
		(* Reduce to reachability property *)
		single_unreachable_location automaton_index lbad*)

