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
 * Last modified: 2021/09/16
 *
 ************************************************************)
 
(************************************************************)
(** Modules *)
(************************************************************)
open Exceptions
open OCamlUtilities
open Constants
open ImitatorUtilities
open Automaton
open ParsingStructure
open AbstractModel
open AbstractProperty


(************************************************************)
(** Building location names *)
(************************************************************)
let location_prefix			= "loc_AutoGen_obs_"

let location_name location_index =
	location_prefix ^ (string_of_int location_index)

(************************************************************)
(** Shortcuts *)
(************************************************************)
let truec = LinearConstraint.pxd_true_constraint


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
		updates		= create_update No_update [] [];
		target		= target_index;
	}]

(* Constraint x <= d, with `d` a LinearConstraint.p_linear_term : d - x >= 0 *)
let ct_x_leq_d (x : clock_index) (d : LinearConstraint.p_linear_term) =
	LinearConstraint.pxd_linear_constraint_of_clock_and_parameters x LinearConstraint.Op_ge d true


(* Constraint x >= d, with `d` a LinearConstraint.p_linear_term : x - d >= 0 *)
let ct_x_geq_d (x : clock_index) (d : LinearConstraint.p_linear_term) =
	LinearConstraint.pxd_linear_constraint_of_clock_and_parameters x LinearConstraint.Op_ge d false


(* Constraint x > d, with `d` LinearConstraint.p_linear_term : d - x = 0 *)
let ct_x_g_d (x : clock_index) (d : LinearConstraint.p_linear_term) =
	LinearConstraint.pxd_linear_constraint_of_clock_and_parameters x LinearConstraint.Op_g d false


(* Linear constraint x = 0 *)
let lc_x_eq_0 (x : clock_index) =
	let d = LinearConstraint.make_p_linear_term [] NumConst.zero in
	LinearConstraint.px_linear_constraint_of_clock_and_parameters x LinearConstraint.Op_eq d false



(************************************************************)
(** Functions *)
(************************************************************)

(** Returns true whether the observer requires one clock *)
let needs_clock (parsed_property : ParsingStructure.parsed_property) =
	match parsed_property.property with
	
	(* Non-observer properties *)
	
	| Parsed_EF _
	| Parsed_AGnot _

	| Parsed_EFpmax _
	| Parsed_EFpmin _
	| Parsed_EFtmin _

	| Parsed_Cycle_Through _
	| Parsed_Cycle_Through_generalized _
	| Parsed_NZ_Cycle

	| Parsed_Deadlock_Freeness

	| Parsed_IM _
	| Parsed_ConvexIM _
	| Parsed_PRP _
	| Parsed_IMK _
	| Parsed_IMunion _

	| Parsed_Cover_cartography _
	| Parsed_Learning_cartography _
	| Parsed_Shuffle_cartography _
	| Parsed_Border_cartography _
	| Parsed_Random_cartography _
	| Parsed_RandomSeq_cartography _
	| Parsed_PRPC _

	(* Untimed observers *)
	
	| Parsed_action_precedence_acyclic _
	| Parsed_action_precedence_cyclic _
	| Parsed_action_precedence_cyclicstrict _
	
	| ParsingStructure.Parsed_Sequence_acyclic _
	| ParsingStructure.Parsed_Sequence_cyclic _

		-> false
	
	
	(* Timed observers *)
	
	| Parsed_action_deadline _
	
	| Parsed_TB_Action_precedence_acyclic _
	| Parsed_TB_Action_precedence_cyclic _
	| Parsed_TB_Action_precedence_cyclicstrict _

	| ParsingStructure.Parsed_TB_response_acyclic _
	| ParsingStructure.Parsed_TB_response_cyclic _
	| ParsingStructure.Parsed_TB_response_cyclicstrict _

		-> true
	


(* Create a property of the form AGnot from a single bad location *)
let make_AGnot_single_location automaton_index location_index =
	(* Print some information *)
	print_message Verbose_high ("Creating an `AGnot` property: `loc[" ^ (string_of_int automaton_index) ^ "] = " ^ (string_of_int location_index) ^ "`");
	
	(* Return actual property *)
	AGnot (State_predicate_term (State_predicate_factor (Simple_predicate (Loc_predicate (Loc_predicate_EQ (automaton_index , location_index ) ) ) ) ) )


(* Create the new automata and new clocks necessary for the observer *)
let new_elements (parsed_property : ParsingStructure.parsed_property) =
	match parsed_property.property with
	
	(* No observer required: does not build anything *)
	
	| Parsed_EF _
	| Parsed_AGnot _
	
	| Parsed_EFpmin _
	| Parsed_EFpmax _
	| Parsed_EFtmin _

	| Parsed_Cycle_Through _
	| Parsed_Cycle_Through_generalized _
	| Parsed_NZ_Cycle

	| Parsed_Deadlock_Freeness

	| Parsed_IM _
	| Parsed_ConvexIM _
	| Parsed_PRP _
	| Parsed_IMK _
	| Parsed_IMunion _

	| Parsed_Cover_cartography _
	| Parsed_Learning_cartography _
	| Parsed_Shuffle_cartography _
	| Parsed_Border_cartography _
	| Parsed_Random_cartography _
	| Parsed_RandomSeq_cartography _
	| Parsed_PRPC _
	
		-> (None , None)
	
	(* Untimed observers: add automaton, do not add clock *)
	
	| ParsingStructure.Parsed_action_precedence_acyclic _
	| ParsingStructure.Parsed_action_precedence_cyclic _
	| ParsingStructure.Parsed_action_precedence_cyclicstrict _

	| ParsingStructure.Parsed_Sequence_acyclic _
	| ParsingStructure.Parsed_Sequence_cyclic _
	
		-> (Some observer_automaton_name, None)
	
	(* Timed observers: add automaton, add clock *)
	
	| Parsed_action_deadline _
	| Parsed_TB_Action_precedence_acyclic _
	| Parsed_TB_Action_precedence_cyclic _
	| Parsed_TB_Action_precedence_cyclicstrict _

	| ParsingStructure.Parsed_TB_response_acyclic _
	| ParsingStructure.Parsed_TB_response_cyclic _
	| ParsingStructure.Parsed_TB_response_cyclicstrict _
	
		-> (Some observer_automaton_name, Some observer_clock_name)


(* Get the number of locations for this observer *)
let get_nb_locations (parsed_property : ParsingStructure.parsed_property) =
	match parsed_property.property with
	(* Not a real observer: does not build anything *)
	| Parsed_EF _
	| Parsed_AGnot _
	
	| Parsed_EFpmin _
	| Parsed_EFpmax _
	| Parsed_EFtmin _
	
	| Parsed_Cycle_Through _
	| Parsed_Cycle_Through_generalized _
	| Parsed_NZ_Cycle

	| Parsed_Deadlock_Freeness

	| Parsed_IM _
	| Parsed_ConvexIM _
	| Parsed_PRP _
	| Parsed_IMK _
	| Parsed_IMunion _
	
	| Parsed_Cover_cartography _
	| Parsed_Learning_cartography _
	| Parsed_Shuffle_cartography _
	| Parsed_Border_cartography _
	| Parsed_Random_cartography _
	| Parsed_RandomSeq_cartography _
	| Parsed_PRPC _
		-> 0
	
	(* Observers *)

	| ParsingStructure.Parsed_action_precedence_acyclic _
	| ParsingStructure.Parsed_action_precedence_cyclic _
	| ParsingStructure.Parsed_action_precedence_cyclicstrict _
		-> 3
	
	| ParsingStructure.Parsed_action_deadline _
		-> 3
		
	| ParsingStructure.Parsed_TB_Action_precedence_acyclic _ -> 4
	| ParsingStructure.Parsed_TB_Action_precedence_cyclic _ -> 3
	| ParsingStructure.Parsed_TB_Action_precedence_cyclicstrict _ -> 3
	
	| ParsingStructure.Parsed_TB_response_acyclic _ -> 4
	| ParsingStructure.Parsed_TB_response_cyclic _ -> 3
	| ParsingStructure.Parsed_TB_response_cyclicstrict _ -> 3
	
	| ParsingStructure.Parsed_Sequence_acyclic list_of_actions -> (List.length list_of_actions) + 2
	| ParsingStructure.Parsed_Sequence_cyclic list_of_actions -> (List.length list_of_actions) + 1


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
	- Invariants
	- Transitions
	- Init inequality
	- Property
*)
(*------------------------------------------------------------*)
let get_observer_automaton action_index_of_action_name (p_linear_term_of_parsed_duration : ParsingStructure.parsed_duration -> LinearConstraint.p_linear_term) (nb_actions : int) automaton_index nosync_index observer_clock_index (parsed_property : ParsingStructure.parsed_property) =
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
		Array.make nb_locations (True_guard),
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
	
	(* Print some information *)
	if verbose_mode_greater Verbose_total then(
		print_message Verbose_high ("Entering `get_observer_automaton` with"
		^ "\n      - nb_actions            = " ^ (string_of_int nb_actions)
		^ "\n      - automaton_index       = " ^ (string_of_int automaton_index)
		^ "\n      - nosync_index          = " ^ (string_of_int nosync_index)
		^ "\n      - observer_clock_index  = " ^ (string_of_int observer_clock_index)
		^ "");
	);

	
	match parsed_property.property with
	
	(*------------------------------------------------------------*)
	(* if a2 then a1 has happened before *)
	(*------------------------------------------------------------*)
	| Parsed_action_precedence_acyclic (action_name1, action_name2) ->
		
		(* Convert action names to indexes *)
		let a1 = action_index_of_action_name action_name1 in
		let a2 = action_index_of_action_name action_name2 in
	
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
		(* Reduce to safety property *)
		make_AGnot_single_location automaton_index 2


	(*------------------------------------------------------------*)
	(* everytime a2 then a1 has happened before *)
	(*------------------------------------------------------------*)
	| Parsed_action_precedence_cyclic (action_name1, action_name2) ->
		(* Convert action names to indexes *)
		let a1 = action_index_of_action_name action_name1 in
		let a2 = action_index_of_action_name action_name2 in
	
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
		(* Reduce to safety property *)
		make_AGnot_single_location automaton_index 2


	(*------------------------------------------------------------*)
	(* everytime a2 then a1 has happened once before *)
	(*------------------------------------------------------------*)
	| Parsed_action_precedence_cyclicstrict (action_name1, action_name2) ->
		(* Convert action names to indexes *)
		let a1 = action_index_of_action_name action_name1 in
		let a2 = action_index_of_action_name action_name2 in
	
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
		(* Reduce to safety property *)
		make_AGnot_single_location automaton_index 2


	(*------------------------------------------------------------*)
	(* a within d *)
	(*------------------------------------------------------------*)
	| Parsed_action_deadline (action_name, parsed_duration) ->
		(* Convert action name to index *)
		let a = action_index_of_action_name action_name in
		(* Convert parsed_duration *)
		let d = p_linear_term_of_parsed_duration parsed_duration in

		let nb_locations	= 3 in
		let location_init	= 0 in
		let location_ok		= 1 in
		let location_nok	= 2 in
		
		let all_actions = [a] in
		(* Initialize *)
		let actions_per_location, observer_location_urgency, invariants, transitions, allow_all = initialize_structures nb_locations all_actions in
		(* Update actions per location for the silent action *)
		actions_per_location.(location_init) <- nosync_index :: all_actions;
		(* Update invariants *)
		invariants.(location_init) <- True_guard (*ct_x_leq_d observer_clock_index d*) ;
		(* Compute transitions *)
		transitions.(location_init).(a) <- untimedt a location_ok;
		transitions.(location_init).(nosync_index) <-
			[{
				guard		= Continuous_guard (ct_x_g_d observer_clock_index d);
				action		= nosync_index;
				updates		= create_update No_update [] [];
				target		= location_nok;
			}];
		transitions.(location_ok) <- allow_all location_ok;
		transitions.(location_nok) <- allow_all location_nok;
		(* Return structure (and add silent action) *)
		nosync_index :: all_actions, actions_per_location, observer_location_urgency, invariants, transitions,
		(* Return observer_clock_index = 0 *)
		Some (lc_x_eq_0 observer_clock_index),
		(* Reduce to safety property *)
		make_AGnot_single_location automaton_index location_nok


		



	(*------------------------------------------------------------*)
	(* if a2 then a1 happened within d before *)
	(*------------------------------------------------------------*)
	| Parsed_TB_Action_precedence_acyclic (action_name1, action_name2, parsed_duration) ->
		(* Convert action names to index *)
		let a1 = action_index_of_action_name action_name1 in
		let a2 = action_index_of_action_name action_name2 in
		(* Convert parsed_duration *)
		let d = p_linear_term_of_parsed_duration parsed_duration in

		let nb_locations = 4 in
		let location_init	= 0 in
		let location_inter	= 1 in
		let location_ok		= 2 in
		let location_nok	= 3 in

		let all_actions = [a1; a2] in
		(* Initialize *)
		let actions_per_location, observer_location_urgency, invariants, transitions, allow_all = initialize_structures nb_locations all_actions in
		(* No need to update actions per location (no silent action here) *)
		(* Compute transitions *)
		transitions.(location_init).(a1) <- 
			[{
				guard		= True_guard;
				action		= a1;
				updates		= create_update (Resets [observer_clock_index]) [] [];
				target		= location_inter;
			}];
		transitions.(location_init).(a2) <- untimedt a2 location_nok;
		transitions.(location_inter).(a1) <-
			[{
				guard		= True_guard;
				action		= a1;
				updates		= create_update (Resets [observer_clock_index]) [] [];
				target		= location_inter;
			}];
		transitions.(location_inter).(a2) <-
			[
			{
				guard		= Continuous_guard (ct_x_leq_d observer_clock_index d);
				action		= a2;
				updates		= create_update No_update [] [];
				target		= location_ok;
			}
			;
			{
				guard		= Continuous_guard (ct_x_g_d observer_clock_index d);
				action		= a2;
				updates		= create_update No_update [] [];
				target		= location_nok;
			}
			];
		transitions.(location_ok) <- allow_all location_ok;
		transitions.(location_nok) <- allow_all location_nok;
		(* Return structure *)
		all_actions, actions_per_location, observer_location_urgency, invariants, transitions,
		(* No init constraint *)
		None,
		(* Reduce to safety property *)
		make_AGnot_single_location automaton_index location_nok


	(*------------------------------------------------------------*)
	(* everytime a2 then a1 happened within d before *)
	(*------------------------------------------------------------*)
	| Parsed_TB_Action_precedence_cyclic (action_name1, action_name2, parsed_duration) ->
		(* Convert action names to index *)
		let a1 = action_index_of_action_name action_name1 in
		let a2 = action_index_of_action_name action_name2 in
		(* Convert parsed_duration *)
		let d = p_linear_term_of_parsed_duration parsed_duration in

		let nb_locations = 3 in
		let location_init	= 0 in
		let location_ok		= 1 in
		let location_nok	= 2 in

		let all_actions = [a1; a2] in
		(* Initialize *)
		let actions_per_location, observer_location_urgency, invariants, transitions, allow_all = initialize_structures nb_locations all_actions in
		(* No need to update actions per location (no silent action here) *)
		(* Compute transitions *)
		transitions.(location_init).(a1) <-
			[{
				guard		= True_guard;
				action		= a1;
				updates		= create_update (Resets [observer_clock_index]) [] [];
				target		= location_ok;
			}];
		transitions.(location_init).(a2) <- untimedt a2 location_nok;
		transitions.(location_ok).(a1) <-
			[{
				guard		= True_guard;
				action		= a1;
				updates		= create_update (Resets [observer_clock_index]) [] [];
				target		= location_ok;
			}];
		transitions.(location_ok).(a2) <-
			[
			{
				guard		= Continuous_guard (ct_x_leq_d observer_clock_index d);
				action		= a2;
				updates		= create_update No_update [] [];
				target		= location_init;
			}
			;
			{
				guard		= Continuous_guard (ct_x_g_d observer_clock_index d);
				action		= a2;
				updates		= create_update No_update [] [];
				target		= location_nok;
			}
			];
		transitions.(location_nok) <- allow_all location_nok;
		(* Return structure *)
		all_actions, actions_per_location, observer_location_urgency, invariants, transitions,
		(* No init constraint *)
		None,
		(* Reduce to safety property *)
		make_AGnot_single_location automaton_index location_nok


	(*------------------------------------------------------------*)
	(* everytime a2 then a1 happened once within d before *)
	(*------------------------------------------------------------*)
	| Parsed_TB_Action_precedence_cyclicstrict (action_name1, action_name2, parsed_duration) ->
		(* Convert action names to index *)
		let a1 = action_index_of_action_name action_name1 in
		let a2 = action_index_of_action_name action_name2 in
		(* Convert parsed_duration *)
		let d = p_linear_term_of_parsed_duration parsed_duration in

		let nb_locations = 3 in
		let location_init	= 0 in
		let location_ok		= 1 in
		let location_nok	= 2 in

		let all_actions = [a1; a2] in
		(* Initialize *)
		let actions_per_location, observer_location_urgency, invariants, transitions, allow_all = initialize_structures nb_locations all_actions in
		(* No need to update actions per location (no silent action here) *)
		(* Compute transitions *)
		transitions.(location_init).(a1) <-
			[{
				guard		= True_guard;
				action		= a1;
				updates		= create_update (Resets [observer_clock_index]) [] [];
				target		= location_ok;
			}];
		transitions.(location_init).(a2) <- untimedt a2 location_nok;
		transitions.(location_ok).(a1) <- untimedt a1 location_nok;
		transitions.(location_ok).(a2) <-
			[
			{
				guard		= Continuous_guard (ct_x_leq_d observer_clock_index d);
				action		= a2;
				updates		= create_update No_update [] [];
				target		= location_init;
			}
			;
			{
				guard		= Continuous_guard (ct_x_g_d observer_clock_index d);
				action		= a2;
				updates		= create_update No_update [] [];
				target		= location_nok;
			}
			];
		transitions.(location_nok) <- allow_all location_nok;
		(* Return structure *)
		all_actions, actions_per_location, observer_location_urgency, invariants, transitions,
		(* No init constraint *)
		None,
		(* Reduce to safety property *)
		make_AGnot_single_location automaton_index location_nok


	(*------------------------------------------------------------*)
	(* if a1 then eventually a2 within d *)
	(*------------------------------------------------------------*)
	| Parsed_TB_response_acyclic (action_name1, action_name2, parsed_duration) ->
		(* Convert action names to index *)
		let a1 = action_index_of_action_name action_name1 in
		let a2 = action_index_of_action_name action_name2 in
		(* Convert parsed_duration *)
		let d = p_linear_term_of_parsed_duration parsed_duration in

		let nb_locations = 4 in
		let all_actions = [a1; a2] in
		(* Initialize *)
		let actions_per_location, observer_location_urgency, invariants, transitions, allow_all = initialize_structures nb_locations all_actions in
		(* Update actions per location for the silent action *)
		actions_per_location.(1) <- nosync_index :: all_actions;
		(* Update invariants *)
		invariants.(1) <-  True_guard (*ct_x_leq_d observer_clock_index d *);
		(* Compute transitions *)
		transitions.(0).(a1) <-
			[{
				guard		= True_guard;
				action		= a1;
				updates		= create_update (Resets [observer_clock_index]) [] [];
				target		= 1;
			}];
		transitions.(0).(a2) <- untimedt a2 0;
		transitions.(1).(a1) <- untimedt a1 1;
		transitions.(1).(a2) <- untimedt a2 2;
		transitions.(1).(nosync_index) <-
			[{
				guard		= Continuous_guard (ct_x_g_d observer_clock_index d);
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
		(* Reduce to safety property *)
		make_AGnot_single_location automaton_index 3

	(*------------------------------------------------------------*)
	(* everytime a1 then eventually a2 within d *)
	(*------------------------------------------------------------*)
	| Parsed_TB_response_cyclic (action_name1, action_name2, parsed_duration) ->
		(* Convert action names to index *)
		let a1 = action_index_of_action_name action_name1 in
		let a2 = action_index_of_action_name action_name2 in
		(* Convert parsed_duration *)
		let d = p_linear_term_of_parsed_duration parsed_duration in

		let nb_locations = 3 in
		let all_actions = [a1; a2] in
		(* Initialize *)
		let actions_per_location, observer_location_urgency, invariants, transitions, allow_all = initialize_structures nb_locations all_actions in
		(* Update actions per location for the silent action *)
		actions_per_location.(1) <- nosync_index :: all_actions;
		(* Update invariants *)
		invariants.(1) <-  True_guard (*ct_x_leq_d observer_clock_index d *);
		(* Compute transitions *)
		transitions.(0).(a1) <-
			[{
				guard		= True_guard;
				action		= a1;
				updates		= create_update (Resets [observer_clock_index]) [] [];
				target		= 1;
			}];
		transitions.(0).(a2) <- untimedt a2 0;
		transitions.(1).(a1) <- untimedt a1 1;
		transitions.(1).(a2) <- untimedt a2 0;
		transitions.(1).(nosync_index) <-
			[{
				guard		= Continuous_guard (ct_x_g_d observer_clock_index d);
				action		= nosync_index;
				updates		= create_update No_update [] [];
				target		= 2;
			}];
		transitions.(2) <- allow_all 2;
		(* Return structure (and add silent action) *)
		nosync_index :: all_actions, actions_per_location, observer_location_urgency, invariants, transitions,
		(* No init constraint *)
		None,
		(* Reduce to safety property *)
		make_AGnot_single_location automaton_index 2


	(*------------------------------------------------------------*)
	(* everytime a1 then eventually a2 within d once before next *)
	(*------------------------------------------------------------*)
	| Parsed_TB_response_cyclicstrict (action_name1, action_name2, parsed_duration) ->
		(* Convert action names to index *)
		let a1 = action_index_of_action_name action_name1 in
		let a2 = action_index_of_action_name action_name2 in
		(* Convert parsed_duration *)
		let d = p_linear_term_of_parsed_duration parsed_duration in

		let nb_locations = 3 in
		let all_actions = [a1; a2] in
		(* Initialize *)
		let actions_per_location, observer_location_urgency, invariants, transitions, allow_all = initialize_structures nb_locations all_actions in
		(* Update actions per location for the silent action *)
		actions_per_location.(1) <- nosync_index :: all_actions;
		(* Update invariants *)
		invariants.(1) <-  True_guard (*ct_x_leq_d observer_clock_index d *);
		(* Compute transitions *)
		transitions.(0).(a1) <-
			[{
				guard		= True_guard;
				action		= a1;
				updates		= create_update (Resets [observer_clock_index]) [] [];
				target		= 1;
			}];
		transitions.(0).(a2) <- untimedt a2 2;
		transitions.(1).(a1) <- untimedt a1 2;
		transitions.(1).(a2) <- untimedt a2 0;
		transitions.(1).(nosync_index) <-
			[{
				guard		= Continuous_guard (ct_x_g_d observer_clock_index d);
				action		= nosync_index;
				updates		= create_update No_update [] [];
				target		= 2;
			}];
		transitions.(2) <- allow_all 2;
		(* Return structure (and add silent action) *)
		nosync_index :: all_actions, actions_per_location, observer_location_urgency, invariants, transitions,
		(* No init constraint *)
		None,
		(* Reduce to safety property *)
		make_AGnot_single_location automaton_index 2


	(*------------------------------------------------------------*)
	(* sequence a1, …, an *)
	(*------------------------------------------------------------*)
	| Parsed_Sequence_acyclic list_of_actions_names ->
		(* Convert action names to index *)
		let list_of_actions = List.map action_index_of_action_name list_of_actions_names in
		
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
		(* Reduce to safety property *)
		make_AGnot_single_location automaton_index lbad


	(*------------------------------------------------------------*)
	(* always sequence a1, …, an *)
	(*------------------------------------------------------------*)
	| Parsed_Sequence_cyclic list_of_actions_names ->
		(* Convert action names to index *)
		let list_of_actions = List.map action_index_of_action_name list_of_actions_names in
		
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
		(* Reduce to safety property *)
		make_AGnot_single_location automaton_index lbad

	(*------------------------------------------------------------*)
	(* Others: should not happen! *)
	(*------------------------------------------------------------*)
	(*** NOTE: this function is only called *after* a match is done, which is why it shouldn't happen ***)
	| _ -> raise (InternalError "Non-observer pattern property detected in `get_observer_automaton`, although this should not have happened")
