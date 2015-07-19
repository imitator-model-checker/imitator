(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Defines algorithms based on state space exploration
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        Ulrich Kuehne, Etienne Andre
 * 
 * Created:       2010/07/22
 * Last modified: 2015/07/19
 *
 ****************************************************************)


(**************************************************************)
(* Modules *)
(**************************************************************)
open Exceptions
open Options
open CamlUtilities
open ImitatorUtilities
open AbstractModel
open ModelPrinter
open StateSpace
open Gc


(**************************************************************)
(* Exception *)
(**************************************************************)

exception Unsat_exception




(****************************************************************)
(** The result output by IM *)
(****************************************************************)
type im_result = {
	(* Returned constraint *)
	result : returned_constraint;
(*	(* Reachability graph *)
	reachability_graph : StateSpace.reachability_graph;*)
	(* Tile nature *)
	tile_nature : tile_nature;
	(* Premature stop? (i.e., states / depth / time limit reached) *)
	premature_stop : bool;
	(* Deterministic analysis? *)
	deterministic : bool;
	(* Number of states *)
	nb_states : int;
	(* Number of transitions *)
	nb_transitions : int;
	(* Number of iterations *)
	nb_iterations : int;
	(* Computation time *)
	total_time : float;
}



(**************************************************************)
(* Constants *)
(**************************************************************)
(* Experimental try to merge states earlier (may be more efficient and more interesting!) *)
(* let options#merge_before = false *)


(**************************************************************)
(* Global variables *)
(**************************************************************)

(* Constraint for result (used for IM and EFIM) *)
let k_result = ref ( LinearConstraint.p_true_constraint () )

(* Constraint to store the bad constraints to be negated (used for IM-complete) *)
(*** NOTE: can it be merged with p_constraints?! *)
(*** WARNING: is it always properly initialized? (even in case of repeated iterations of IM?) ***)
let k_bad = ref []

(* List of constraints for result (used for EF_synthesis / EFIM) *)
let p_constraints = ref []

(* List of last states (of runs) : used for the union mode *)
(*** WARNING: is it always properly initialized? (even in case of repeated iterations of IM?) ***)
let slast = ref []

(* Number of random selections of pi0-incompatible inequalities in IM *)
(*** WARNING: is it always properly initialized? (even in case of repeated iterations of IM?) ***)
let nb_random_selections = ref 0

(* Check whether the tile is bad (for IM and BC, and also EFIM) *)
let tile_nature = ref Unknown

(* External function to be called to check a termination condition (used for PaTATOR) *)
let patator_termination_function = ref None


(**************************************************************)
(* I/O functions *)
(**************************************************************)
let write_result_to_file constraint_str =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	(* Prepare the string to write *)
	let file_content =
		(*** WARNING: duplicate code (Cartography.ml) ***)
		"(*" 
		(* Program version *)
		^ "\n  Result output by " ^ Constants.program_name ^ ""
		^ "\n  Version  : " ^ (ImitatorUtilities.program_name_and_version_and_nickname_and_build())
		^ "\n  Model    : '" ^ options#file ^ "'"
		(* Date *)
		^ "\n  Generated: " ^ (now()) ^ ""
		(* Command *)
		^ "\n  Command  : " ^ (string_of_array_of_string_with_sep " " Sys.argv)
		^ "\n*)\n\n"
		(* The actual result *)
		^ constraint_str ^ "\n"
	in
	(* Write to file *)
	let file_name = options#files_prefix ^ Constants.result_file_extension in
	write_to_file file_name file_content;
	print_message Verbose_standard ("Result written to file '" ^ file_name ^ "'.")



(**************************************************************)
(* Costs *)
(**************************************************************)

(* Instantiated costs (no need to compute them for each location) *)
let instantiated_costs = ref (Array.make 0 (Array.make 0 NumConst.zero)) (*Array.make (Hashtbl.length index_of_automata) (Array.make 0 (NumConst.zero))*)

let instantiate_costs model pi0 =
	(* Create an empty array *)
	let costs = Array.make model.nb_automata (Array.make 0 NumConst.zero) in
	(* For each automaton *)
	for automaton_index = 0 to model.nb_automata - 1 do
		(* Retrieve the number of locations for this automaton *)
		let nb_locations = List.length (model.locations_per_automaton automaton_index) in
		(* Create the array of costs for this automaton *)
		costs.(automaton_index) <- Array.make nb_locations NumConst.zero;
		(* For each location *)
		for location_index = 0 to nb_locations - 1 do
			(* Retrieve the cost *)
			let cost = model.costs automaton_index location_index in
			(* Instantiate it *)
			let instantiated_cost = match cost with 
				| None -> NumConst.zero 
				| Some cost -> LinearConstraint.evaluate_p_linear_term pi0#get_value cost in
			(* Save it *)
			costs.(automaton_index).(location_index) <- instantiated_cost;
		done;
	done;
	(* Set the global array *)
	instantiated_costs := costs;
	()





(**************************************************************)
(* Local Clocks *)
(**************************************************************)

(* Global variable *)
(** WARNING: initial value quite ugly; but using a Some / None would be a bit troublesome *)
let useless_clocks = ref (fun automaton_index location_index -> [])


(** WARNING: duplicate function in ModelConverter *)
let get_clocks_in_updates : clock_updates -> Automaton.clock_index list = function
	(* No update at all *)
	| No_update -> []
	(* Reset to 0 only *)
	| Resets clock_reset_list -> clock_reset_list
	(* Reset to arbitrary value (including discrete, parameters and clocks) *)
	| Updates clock_update_list -> let result, _ = List.split clock_update_list in result


(*------------------------------------------------------------*)
(* Find the local clocks per automaton *)
(*------------------------------------------------------------*)
(** WARNING: the use of clock_offset is not beautiful (and error prone) here *)
let find_local_clocks model =
	(** HACK: yes, clock_offset is the number of parameters, but quite hard coded *)
	let clock_offset = model.nb_parameters in
	
	(* Create an empty array for the clocks of each automaton *)
	let clocks_per_automaton = Array.make model.nb_automata [] in
	(* Create an empty array for the local clocks of each automaton *)
	let local_clocks_per_automaton = Array.make model.nb_automata [] in
	(* Create an empty array for the automata associated with each clock *)
	let automata_per_clock = Array.make model.nb_clocks [] in
	
	(* For each automaton *)
	for automaton_index = 0 to model.nb_automata - 1 do
		(* Get the locations for this automaton *)
		let locations = model.locations_per_automaton automaton_index in
		(* For each location *)
		let clocks_for_locations = List.fold_left (fun list_of_clocks_for_previous_locations location_index ->
			(* Get the clocks in the invariant *)
			let invariant = model.invariants automaton_index location_index in
			let clocks_in_invariant = LinearConstraint.pxd_find_variables model.clocks invariant in
			(* Get the clocks from the stopwatches *)
			let clocks_in_stopwatches = model.stopwatches automaton_index location_index in
			
			(* Now find clocks in guards *)
			(* For each action for this automaton and location *)
			let actions_for_this_location = model.actions_per_location automaton_index location_index in
			let clocks_for_actions = List.fold_left (fun list_of_clocks_for_previous_actions action_index ->
				(* For each transition for this automaton, location and action *)
				let transitions_for_this_action = model.transitions automaton_index location_index action_index in
				let clocks_for_transitions = List.fold_left (fun list_of_clocks_for_previous_transitions transition ->
					(* Name the elements in the transition *)
					let guard , clock_updates , _ , _ = transition in
					let clocks_in_guards = LinearConstraint.pxd_find_variables model.clocks guard in
					let clocks_in_updates = get_clocks_in_updates clock_updates in
						(* Add these 2 new lists to the current list *)
						List.rev_append (List.rev_append clocks_in_guards clocks_in_updates) list_of_clocks_for_previous_transitions
				) [] transitions_for_this_action in
				(* Add the list for this action to the one for previous actions *)
				List.rev_append clocks_for_transitions list_of_clocks_for_previous_actions
			) [] actions_for_this_location in
			
			(* Add all clocks *)
			List.rev_append (List.rev_append (List.rev_append clocks_in_invariant clocks_in_stopwatches) clocks_for_actions) list_of_clocks_for_previous_locations
		) [] locations in
		
		(* Collapse the list *)
		let clocks_for_this_automaton = list_only_once clocks_for_locations in
		(* Update the clocks per automaton *)
		clocks_per_automaton.(automaton_index) <- clocks_for_this_automaton;
		(* Update the automaton for all clocks *)
		List.iter (fun clock ->
			(* Add current automaton to the list of automata for this clock *)
			automata_per_clock.(clock - clock_offset) <- (automaton_index :: automata_per_clock.(clock - clock_offset));
		) clocks_for_this_automaton;
	done; (* end for each automaton *)
	
	(* Now compute the local clocks *)
	for clock_index = clock_offset to clock_offset + model.nb_clocks - 1 do
		(* Retrieve the automata in which this clock appears *)
		let automata_for_this_clock = automata_per_clock.(clock_index - clock_offset) in
		(* If size is 1, the clock is local *)
		match automata_for_this_clock with
			(* Only one element: clock is local *)
			| [automaton_index] -> 
(* 				print_message Verbose_high ("Automaton " ^ (string_of_int automaton_index) ^ " has local clock " ^ (string_of_int clock_index)); *)
				(* Add the clock to the automaton *)
				local_clocks_per_automaton.(automaton_index) <- (clock_index) :: local_clocks_per_automaton.(automaton_index);
			(* Otherwise, clock is not local *)
			| _ -> ()
	done;

	(*clocks_per_automaton, automata_per_clock,*) local_clocks_per_automaton
	

(*------------------------------------------------------------*)
(* Find the useless clocks in automata locations *)
(*------------------------------------------------------------*)
(** NOTE: this function is not related to model conversion, and could (should?) be defined elsewhere *)
let find_useless_clocks_in_automata model local_clocks_per_automaton =

	(* Create the data structure *)
	let useless_clocks_per_location = Array.make model.nb_automata (Array.make 0 []) in
	
	(* For each automaton *)
	for automaton_index = 0 to model.nb_automata - 1 do
	
	
		(* Get the locations for this automaton *)
		let locations_for_this_automaton = model.locations_per_automaton automaton_index in
		let nb_locations = List.length locations_for_this_automaton in
	
		(* Initialize the data structure for this automaton *)
		useless_clocks_per_location.(automaton_index) <- Array.make nb_locations [];
		
		(* Retrieve the local clocks for this automaton *)
		let local_clocks = local_clocks_per_automaton.(automaton_index) in

		(* Compute the predecessor locations and lists of local clock reset *)
		let predecessors = Array.make nb_locations [] in
		(* For each location in this automaton: *)
		List.iter (fun location_index ->
			(* Get the actions for this location *)
			let actions_for_this_location = model.actions_per_location automaton_index location_index in
			
			(* For each action available in this location *)
			List.iter (fun action_index ->
				(* Retrieve the transitions from this location & action *)
				let transitions = model.transitions automaton_index location_index action_index in
				
				(* For each transition starting from this location *)
				List.iter (fun ((*guard*) _ , clock_updates , (*discrete_update*) _ , destination_index) ->
					(* Get the clocks updated or reset *)
					let reset_clocks =
					match clock_updates with
					| No_update -> []
					| Resets list_of_clocks -> list_of_clocks
					| Updates list_of_clocks_and_updates ->
						(* Keep only the left part (the clock indexes) *)
						let left, _ =  List.split list_of_clocks_and_updates in left
					in
					(* Compute the local clocks updated or reset *)
					let reset_local_clocks = list_inter reset_clocks local_clocks in
					(* Update the predecessors *)
					predecessors.(destination_index) <- (location_index, reset_local_clocks) :: predecessors.(destination_index);
				) transitions; (* end for each transition *)
			) actions_for_this_location; (* end for each action *)
		) locations_for_this_automaton; (* end for each location *)
		
		(* Print debug information *)
		if debug_mode_greater Verbose_total then(
			print_message Verbose_total ("Computed predecessor locations and clock resets for automaton '" ^ (model.automata_names automaton_index) ^ "'");
			(* Iterate on locations *)
			List.iter (fun location_index ->
				print_message Verbose_total ("  Location '" ^ (model.location_names automaton_index location_index) ^ "' has predecessors:");
				let predecessors_string = string_of_list_of_string_with_sep ", " (List.map (
					fun (source_index, reset_local_clocks) ->
						(model.location_names automaton_index source_index) ^ "[resets: " ^ (string_of_list_of_string_with_sep ", " (List.map model.variable_names reset_local_clocks)) ^ "]"
					) predecessors.(location_index)) in
				print_message Verbose_total ("    " ^ predecessors_string);
			) locations_for_this_automaton; (* end for each location *)
		);
	
		(* For each local clock for this automaton *)
		List.iter(fun clock_index ->
			(* Create a list of marked locations (i.e., where the clock is useful) *)
			let marked = ref (list_union 
				(* All locations with an invariant involving this clock *)
				(List.filter (fun location_index ->
					(* Retrieve the invariant *)
					let invariant = model.invariants automaton_index location_index in
					(* Check if the clock is present in the invariant *)
					let constrained = LinearConstraint.pxd_is_constrained invariant clock_index in
					(* Print some information *)
					print_message Verbose_total ("Clock '" ^ (model.variable_names clock_index) ^ "' is " ^ (if constrained then "" else "NOT ") ^ "constrained in invariant of location '" ^ (model.location_names automaton_index location_index) ^ "'");
					(* Return true or false *)
					constrained
				) locations_for_this_automaton
				) 
				(* All predecessor locations of transitions with a guard involving this clock *)
				(
					(* For each location *)
					List.fold_left (fun current_list_of_locations location_index ->
						(* Get the actions for this location *)
						let actions_for_this_location = model.actions_per_location automaton_index location_index in
						(* For each action available in this location *)
						List.fold_left (fun current_list_of_locations action_index ->
							(* Retrieve the transitions from this location & action *)
							let transitions = model.transitions automaton_index location_index action_index in
							(* Check if there exists a guard in an outgoing transition where the clock is constrained *)
							let exists_guard = List.exists (fun (guard , (*clock_updates*)_ , (*discrete_update_list*)_ , (*destination_index*)_) ->
								(* Check if the clock is present in the guard *)
								let constrained = LinearConstraint.pxd_is_constrained guard clock_index in
								(* Print some information *)
								if constrained then (
									print_message Verbose_high ("Found a transition where clock '" ^ (model.variable_names clock_index) ^ "' is constrained in guard from location '" ^ (model.location_names automaton_index location_index) ^ "', through '" ^ (model.action_names action_index) ^ "'");
								) else (
									print_message Verbose_total ("Clock '" ^ (model.variable_names clock_index) ^ "' is not constrained in guard from location '" ^ (model.location_names automaton_index location_index) ^ "' through '" ^ (model.action_names action_index) ^ "'");
								);
								(* Return true or false *)
								constrained
							) transitions in
							(* Keep the location if there exists a guard *)
							if exists_guard then location_index :: current_list_of_locations
							else current_list_of_locations
						) current_list_of_locations actions_for_this_location
					) [] locations_for_this_automaton
				)
			) in

			(* Create a waiting list *)
			let waiting = ref !marked in
			
			(* Print debug information *)
			if debug_mode_greater Verbose_medium then(
				print_message Verbose_medium ("Starting the XXX algorithm for local clock '" ^ (model.variable_names clock_index) ^ "' in automaton '" ^ (model.automata_names automaton_index) ^ "', with initial marked states:");
				print_message Verbose_medium (	"  " ^ (string_of_list_of_string_with_sep ", " (List.map (model.location_names automaton_index) !marked)));
			);
			
			(* Start the algorithm *)
			while !waiting != [] do
				(* Pick a location from the waiting list *)
				match !waiting with
				| location_index :: rest ->
					(* Debug information *)
					print_message Verbose_medium ("Pick up location '" ^ (model.location_names automaton_index location_index) ^ "'");
					(* Remove the first element *)
					waiting := rest;
					(* For each transition leading to this location *)
					List.iter (fun (source_index, reset_local_clocks) ->
						(* Debug information *)
						print_message Verbose_high ("Considering predecessor transition from '" ^ (model.location_names automaton_index source_index) ^ "'");
						(* If the clock is not reset by the transition *)
						if not (List.mem clock_index reset_local_clocks) then(
							(* Debug information *)
							print_message Verbose_high ("Clock not reset by a transition.");
							(* If the source location does not belong to the marked list *)
							if not (List.mem source_index !marked) then(
								(* Add it to the marked list *)
								marked := source_index :: !marked;
								print_message Verbose_high ("Location marked.");
								(* Add it to the waiting list (if not present) *)
								if not (List.mem source_index !waiting) then
									print_message Verbose_high ("Location added to waiting list.");
									waiting := source_index :: !waiting;
							); (* end if not in marked list *)
						);(* end if clock not reset *)
					) predecessors.(location_index); (* end for each transition *)
					
				| _ -> raise (InternalError "Impossible situation: list should not be empty.");

			(* End the algorithm *)
			done;
			
			(* Return the list of locations where the clock can be removed *)
			let useless_locations = list_diff locations_for_this_automaton !marked in
			
			(* Print debug information *)
			if debug_mode_greater Verbose_low then(
				print_message Verbose_low ("List of useless locations for local clock '" ^ (model.variable_names clock_index) ^ "' in automaton '" ^ (model.automata_names automaton_index) ^ "'");
				print_message Verbose_low ("  " ^ (string_of_list_of_string_with_sep ", " (List.map (model.location_names automaton_index) useless_locations)));
			);
			
			(* Update the data structure *)
			List.iter (fun location_index ->
				(useless_clocks_per_location.(automaton_index)).(location_index) <- clock_index :: (useless_clocks_per_location.(automaton_index)).(location_index);
			) useless_locations;
			
			
		) local_clocks; (* end for each local clock *)
	done; (* end for each automaton *)
	
	(* Return a functional structure *)
	(fun automaton_index location_index ->
		(useless_clocks_per_location.(automaton_index)).(location_index)
	)
	(* THE END *)



(*------------------------------------------------------------*)
(* Function for preparing data structures for dynamic clock elimination *)
(*------------------------------------------------------------*)
(* NOTE: This function is only called if the dynamic clock elimination option is activated *)
let prepare_clocks_elimination model =
	(* Compute the local clocks per automaton *)
	print_message Verbose_low ("*** Building local clocks per automaton...");
	let local_clocks_per_automaton = find_local_clocks model in

	(* Debug print: local clocks per automaton *)
	if debug_mode_greater Verbose_total then(
		print_message Verbose_total ("\n*** Local clocks per automaton:");
		(* For each automaton *)
		List.iter (fun automaton_index ->
			(* Get the actions *)
			let clocks = local_clocks_per_automaton.(automaton_index) in
			(* Print it *)
			let clocks_string = string_of_list_of_string_with_sep ", " (List.map model.variable_names clocks) in
			print_message Verbose_total ("  " ^ (model.automata_names automaton_index) ^ " : " ^ clocks_string)
		) model.automata;
	);
	
	
	(* Compute and update useless clocks *)
	print_message Verbose_low ("*** Building useless clocks per location per automaton...");
	useless_clocks := find_useless_clocks_in_automata model local_clocks_per_automaton;
	()


(**************************************************************)
(* Functions related to locations *)
(**************************************************************)
(* Check whether at least one local location is urgent *)
let is_location_urgent model location =
	(* Subfunction checking that one location is urgent in a given automaton *)
	let is_local_location_urgent automaton_index =
		(* Retrieve location *)
		let location_index = Automaton.get_location location automaton_index in
		(* Check if urgent *)
		model.is_urgent automaton_index location_index
	in
	List.exists is_local_location_urgent model.automata


(**************************************************************)
(* Functions related to states *)
(**************************************************************)
(* Check whether a state is bad *)
let update_tile_nature (location, (*linear_constraint*)_) =
	(* Get the model *)
	let model = Input.get_model() in
	(* Retrieve the input options *)
	let options = Input.get_options () in

	match model.correctness_condition with
	| None -> ()
	| Some (Unreachable (bad_automaton_index , bad_location_index)) ->
		(* Check if the local location is the same as the bad one *)
		let is_bad = (Automaton.get_location location bad_automaton_index) = bad_location_index in
			if is_bad then (
				(*** Quite a hack here ***)
				if options#efim && !tile_nature <> Bad then(
					print_message Verbose_standard ("  [EFIM] Bad location found! Switching to bad-driven algorithm");
				);
				tile_nature := Bad;
			);
	| _ -> raise (InternalError("IMITATOR currently ony implements the non-reachability-like properties."))



(**************************************************************)
(* Cache *)
(**************************************************************)

(* hash function for locations *)
let loc_hash locations =
	Array.fold_left (fun h l -> 
		7919 * h + l
	) 0 locations


(*(* hash function for clock updates *)
let upd_hash clock_update =
	List.fold_left (fun h v -> 
		7919 * h + v
	) 0 clock_update*)


(* Cache for computed invariants *)
let inv_cache = Cache.make loc_hash 200

(*(* Cache for clock updates *)
let upd_cache = Cache.make upd_hash 100*)


(**************************************************************)
(* Statistics *)
(**************************************************************)

(* Print statistics for cache usage *)
let print_stats _ =
	print_message Verbose_standard "invariant cache:"; 
	Cache.print_stats inv_cache(*;
 	print_message Verbose_standard "clock update cache:"; *)
(* 	Cache.print_stats upd_cache *)
	
 

(* Number of constraints checked unsatisfiable while looking for the actions *)
let nb_early_unsatisfiable = ref 0
(* Number of actions discarded *)
let nb_early_skip = ref 0
(* Number of constraints computed but unsatisfiable *)
let nb_unsatisfiable = ref 0
(* Number of different combinations considered when computing post *)
let nb_combinations = ref 0

let nb_unsat1 = ref 0
let nb_unsat2 = ref 0








(**************************************************************)
(* Merging functions *)
(**************************************************************)
(** Check if two states are mergeable: return True if the convex hull is equal to the union, False otherwise*)
(*let state_mergeable state1 state2 =
	let (loc1,constr1) = state1 in
	let (loc2,constr2) = state2 in
	if not (Automaton.location_equal loc1 loc2) then false else (
		LinearConstraint.hull_assign_if_exact constr1 constr2 
	)*)

(*------------------------------------------------------------*)
(* Check whether two states (location1 , constraint1) and (location2 , constraint2) are mergeable*)
(* Warning! Performs the merging constraint1 := constraint1 U constraint2 if indeed mergeable *)
(*------------------------------------------------------------*)
let try_to_merge location1 constraint1 location2 constraint2 =
	(* First check equality of locations *)
	if location1 <> location2 then false
	else(
		(* Check convex union of constraints *)
		LinearConstraint.px_hull_assign_if_exact constraint1 constraint2
	)

(*------------------------------------------------------------*)
(* Merge states in a list (action_index, location, constraint) *)
(* Return the updated list *)
(*------------------------------------------------------------*)
(*** WARNING: horrible imperative style (and probably not efficient and not tail-recursive at all!) programming in this function *)
let merge model action_and_state_list =

	(* Print some information *)
	print_message Verbose_low ("\nStarting merging algorithm (before pi0-compatibility test) on a list of " ^ (string_of_int (List.length (!action_and_state_list))) ^ " state" ^ (s_of_int (List.length (!action_and_state_list))) ^ "");
	
	(* Number of eated states (for printing purpose) *)
	let nb_eated = ref 0 in

	(* Outer loop: iterate on potential eaters *)
	let eater_index = ref 0 in
	(* while eater_index is in the list *)
	while !eater_index < List.length (!action_and_state_list) do
		(* Print some information *)
		print_message Verbose_high ("\n eater = " ^ (string_of_int !eater_index));

		(* Retrieve the eater *)
		let eater_actions, eater_location, eater_constraint = List.nth !action_and_state_list !eater_index in
		
		(* Inner loop: iterate on potential eated *)
		let eated_index = ref 0 in
		while !eated_index < List.length (!action_and_state_list) do
			(* Print some information *)
			print_message Verbose_high ("\n eater = " ^ (string_of_int !eater_index));
			
			(* Don't eat yourself *)
			if !eater_index <> !eated_index then(
				(* Retrieve the potential eated *)
				let eated_actions, eated_location, eated_constraint = List.nth !action_and_state_list !eated_index in
				
				(* If mergeable *)

				(* Print some information *)
				if debug_mode_greater Verbose_total then (
					print_message Verbose_total ("\nConstraint of the eated before merging attempt...\n" ^ (LinearConstraint.string_of_px_linear_constraint model.variable_names eated_constraint));
					print_message Verbose_total ("\nConstraint of the eater before merging attempt...\n" ^ (LinearConstraint.string_of_px_linear_constraint model.variable_names eater_constraint));
				);

				
				if try_to_merge eater_location eater_constraint eated_location eated_constraint then(
					if debug_mode_greater Verbose_total then (
						(* Print some information *)
						print_message Verbose_total ("\nConstraint of the eater after merging...\n" ^ (LinearConstraint.string_of_px_linear_constraint model.variable_names eater_constraint));
					);
						
					(* Due to side effects in try_to_merge, the merging is already performed at this point! *)
					
					(* Set the actions of the eated to the eater *)
					(*** TODO: first check that some actions in the eated do not appear in the eater *)
					action_and_state_list := list_set_nth !eater_index (list_union eater_actions eated_actions, eater_location, eater_constraint) !action_and_state_list;

					(* Now remove the eated *)
					action_and_state_list := list_delete_at !eated_index !action_and_state_list;
					
					
					(* If eated_index < eater_index, then decrement the eater index since the list is now shorter on the left side *)
					if !eated_index < !eater_index then(
(* 						eated_index := !eated_index - 1; *)
						eater_index := !eater_index - 1;
					);
					
					(* Update the counter *)
					nb_eated := !nb_eated + 1;
					
					(* Try again to eat from the beginning *)
					eated_index := 0;
				) (* end if mergeable *)
				else(
					(* Otherwise: Go to the next eated state *)
					eated_index := !eated_index + 1;
				) (* end if not mergeable *)

			) (* end don't eat yourself *)
			else(
				(* Otherwise: Go to the next eated state *)
				eated_index := !eated_index + 1;
			);
		done; (* end inner loop *)
		
		(* Go to the next eater *)
		eater_index := !eater_index + 1;
	done; (* end outer loop *)
	
	(* Print some information *)
	if !nb_eated > 0 then
		print_message Verbose_standard ("  " ^ (string_of_int !nb_eated) ^ " state" ^ (s_of_int !nb_eated) ^ " merged before pi0-compatibility test.")
	else
		print_message Verbose_low ("  (No state merged before pi0-compatibility test)")
	;
	
	
	!action_and_state_list

(*let rec merging_of_states graph index_state list_index_states list_new_states =
	match list_index_states with
	  | [] -> (false, -1)
	  | first :: rest  -> let (loc_state, constr_state) =  (get_state graph index_state) in
		  let (loc_hd, constr_hd) = (get_state graph first) in
		  if (state_mergeable (loc_state, constr_state) (loc_hd, constr_hd)) then (
			print_message Verbose_total ("Mergeable states");
			merge_states graph index_state first;
			print_message Verbose_total ("States merged");
			(* TO OPTIMIZE: operation already performed in state_mergeable !! *)
			LinearConstraint.hull_assign constr_state constr_hd;
			(true, first)
		  )
		  else ( merging_of_states graph index_state rest (first :: list_new_states); )*)


(*let try_to_merge_states graph list_of_states =
	print_message Verbose_high ("Starting merging");
	
	(* Count states (for statistics only *)
	let nb_merged = ref 0 in
	(* Check if we have to start the whole merging again (case a merging was found *)
	let start_again = ref true in
	(* Copy the list of states to work on it *)
	let current_list_of_states = ref list_of_states in

	(* DEFINITION OF AN AUXILIARY FUNCTION: 'eater' state id will try to merge with elements within 'eated_list' states id list *)
	let merge_states_aux eater eated_list =
		(* Flag to check if states were merged *)
		let merged_states = ref true in
		(* Copy eated list *)
		let eated_list_copy = ref eated_list in
		
		(* Loop as long as we merged states, i.e., always start again to try to marge eater if some states were merged *)
		while !merged_states do
			print_message Verbose_total ("Starting inner loop in merging");
			(* Set flag to false: no states merged yet *)
			merged_states := false;
			(* Get the real state *)
			let s1 = get_state graph eater in
			(* Iterate on all elements of eated_list, and update the eated_list *)
			eated_list_copy := List.fold_left (fun current_list current_element ->
				(* Get the real state *)
				let s2 = get_state graph current_element in
				(* Try to merge eater with current_element *)
				if state_mergeable s1 s2 then (
					print_message Verbose_total ("Found a mergeable state");
					StateSpace.merge_2_states graph eater current_element;
					print_message Verbose_total ("States successfully merged");
					(** Optimized: hull already performed in state_mergeable !! *)
					(* Update flags (we will have to start everything again) *)
					start_again := true;
					merged_states := true;
					nb_merged := !nb_merged + 1;
					(* Return current list only, i.e., discard s2 *)
					current_list
				) else (
					(* Keep s2 *)
					current_element :: current_list
				)
				(* NOTE THAT THE LIST WAS REVERSED; IMPORTANT? *)
			) [] !eated_list_copy;
		done; (* end while merge first with rest *)
		(* Result: *)
		!eated_list_copy
	(* END AUXILIARY FUNCTION *)
	in

	while !start_again do
		(* Set flag to false: no states merged yet *)
		start_again := false;

		print_message Verbose_total ("Starting one iteration of the outer loop in merging");

		let beginning = ref [] in
		let remaining = ref (!current_list_of_states) in
		
		while !remaining != [] do
			match !remaining with
			| [] -> raise (InternalError("Impossible case in 'merge_states'."))
			| first_remaining :: rest_remaining ->
				(* Call auxiliary function *)
				print_message Verbose_high ("Considered one more state");
				remaining := merge_states_aux first_remaining rest_remaining;
				(* Add first to rest, i.e., move one step within the list *)
				beginning := first_remaining :: !beginning;
		done;
		(* Update list of states *)
		current_list_of_states := !beginning;
	done;

	(* Some debug message *)
	if !nb_merged > 0 then
		print_message Verbose_standard ("  " ^ (string_of_int !nb_merged) ^ " state" ^ (s_of_int !nb_merged) ^ " merged.");
	
	(* Return something *)
	!current_list_of_states


(*
		match !new_states_after_merging with
			(* If empty list: do nothing *)
			| [] -> ()
			(* Otherwise: *)
			| first :: rest -> (
				let (result, state_merged) = merging_of_states reachability_graph first rest [] in
					print_message Verbose_total ("Test for debugging the fatal error 2/5");
					if result then (
						print_message Verbose_total ("Test for debugging the fatal error 3/5");
						merging := true;
						nb_merged := !nb_merged + 1;
						print_message Verbose_total ("Test for debugging the fatal error 4/5");
						new_states_after_merging := list_remove_first_occurence state_merged !new_states_after_merging ;
						print_message Verbose_total ("Test for debugging the fatal error 5/5");
					);
				print_message Verbose_total ("Looping merging");
				);

						(** DEBUT LOOP *)

				
				done;*)*)



(**************************************************************)
(* Main functions *)
(**************************************************************)

(*------------------------------------------------------------*)
(* Compute the invariant associated to a location   *)
(*------------------------------------------------------------*)
let compute_plain_invariant model location =
  (* construct invariant *)
	let invariants = List.map (fun automaton_index ->
		(* Get the current location *)
		let location_index = Automaton.get_location location automaton_index in
		(* Compute the invariant *)
		model.invariants automaton_index location_index
	) model.automata in
	(* Perform the intersection *)
	LinearConstraint.pxd_intersection invariants


(*------------------------------------------------------------*)
(* Compute the invariant I_l associated to a location  *)
(* including renaming and time elapse. Uses cache.  *)
(*------------------------------------------------------------*)
let compute_invariant model location =
	(* Strip off discrete for caching scheme  *)
	let locations = Automaton.get_locations location in
	(* check in cache *)
	let entry = Cache.find inv_cache locations in
	match entry with
		| Some inv -> inv
		| None -> ( 
			(* Build plain invariant I_l(X) *)
			let invariant = compute_plain_invariant model location in
			(* Store in cache *)
			Cache.store inv_cache locations invariant;
			invariant
		)

(*------------------------------------------------------------*)
(* Compute the polyhedron p projected onto rho(X) *)
(*------------------------------------------------------------*)
(*** TO OPTIMIZE: use cache (?) *)
let rho_assign model (linear_constraint : LinearConstraint.pxd_linear_constraint) clock_updates =
	if clock_updates != [] then(
		(* Merge updates *)
		
		(** TO OPTIMIZE: only create the hash if there are indeed some resets/updates *)
		
		let clocks_hash = Hashtbl.create model.nb_clocks in
		(* Check wether there are some complex updates of the form clock' = linear_term *)
		let arbitrary_updates = ref false in
		(* Iterate on the lists of clocks for all synchronized automaton *)
		List.iter (fun local_updates -> 
			match local_updates with
			| No_update -> ()
			| Resets list_of_clocks ->
				(* Iterate on the clocks, for a given automaton *)
				List.iter (fun clock_id -> 
					(* Assign this clock to true in the table *)
					Hashtbl.replace clocks_hash clock_id (LinearConstraint.make_pxd_linear_term [] NumConst.zero);
				) list_of_clocks;
			| Updates list_of_clocks_lt ->
				(* Set the flag *)
				arbitrary_updates := true;
				(* Iterate on the clocks, for a given automaton *)
				List.iter (fun (clock_id, linear_term) -> 
					(* Check if already updated *)
					if Hashtbl.mem clocks_hash clock_id then (
						(* Find its previous value *)
						let previous_update = Hashtbl.find clocks_hash clock_id in
						(* Compare with the new one *)
						if previous_update <> linear_term then (
						(* If different: warning *)
							print_warning ("The clock '" ^ (model.variable_names clock_id) ^ "' is updated several times with different values for the same synchronized action.
								The behavior of the system is now unspecified.");
						)
					);
					(* Update the update *)
					Hashtbl.replace clocks_hash clock_id linear_term;
				) list_of_clocks_lt;
		) clock_updates;
		
		(* THREE CASES: no updates, only resets (to 0) or updates (to linear terms) *)
		
		(* CASE 1: no update *)
		if Hashtbl.length clocks_hash = 0 then (
			
			(* do nothing! *)
			
		(* CASE 2: only resets *)
		)else(if not !arbitrary_updates then(
		
		
		
		
		
			(*** TODO : add "reset" function to LinearConstraint ***)
			
		
			(*** TO OPTIMIZE: Hashtbl.fold and List.map should be merged into one function ***)
			
			(* Compute the list of clocks to update from the hashtable *)
			let list_of_clocks_to_update = Hashtbl.fold (fun clock_id _ list_of_clocks -> clock_id :: list_of_clocks) clocks_hash [] in
			
			(* Compute X = 0 for the variables appearing in resets *)
			print_message Verbose_total ("\n -- Computing resets X = 0");
			let updates =
				(List.map (fun variable_index ->
					(* Consider cases for clocks *)
					match model.type_of_variables variable_index with
					(* Clocks: X = 0 *)
					| Var_type_clock -> 
						let x_lt = LinearConstraint.make_pxd_linear_term [
							NumConst.one, variable_index;
						] NumConst.zero in
						LinearConstraint.make_pxd_linear_inequality x_lt LinearConstraint.Op_eq
					| _ -> raise (InternalError "Only clocks can be updated.")
				) list_of_clocks_to_update) in
			(* Create the constraint *)
			let updates = LinearConstraint.make_pxd_constraint updates in
			(* Print some information *)
			if debug_mode_greater Verbose_total then(
				print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names updates);
			);
			
			(* Hide clocks updated within the linear constraint, viz., exists X' : lc, for X' in rho(X) *)
			print_message Verbose_total ("\n -- Computing exists X : lc for reset clocks");
			LinearConstraint.pxd_hide_assign list_of_clocks_to_update linear_constraint;
			if debug_mode_greater Verbose_total then(
				print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names linear_constraint);
			);
			
			(* Add the constraints X = 0 *)
			print_message Verbose_total ("\n -- Adding X = 0 for reset clocks");
			LinearConstraint.pxd_intersection_assign linear_constraint [updates];
			if debug_mode_greater Verbose_total then(
				print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names linear_constraint);
			)
			
		(* CASE 3: updates to linear terms *)
		)else(
		
		
			(* TODO (not urgent) : add "update" function to LinearConstraint *)
		
		
		
			(* Compute the couples (X_i , = linear_term) from the hashtable *)
			let updates = Hashtbl.fold (fun clock_id linear_term current_updates -> (clock_id, linear_term) :: current_updates) clocks_hash [] in
			(** TO OPTIMIZE (?): could be performed statically (when converting the model).
				PRO: save time because no need to compute this for each constraint;
				CON: lose time & memory (but maybe not that much) at some point because operations on constraints will have all dimensions instead of just the updated prime variables
				TO OPTIMIZE (other option): merge all operations together, so that no need for hashtable
			*)
			(* Compute the correspondance between clocks X_i and renamed clocks X_i' *)
			let prime_of_variable = Hashtbl.create (List.length updates) in
			let variable_of_prime = Hashtbl.create (List.length updates) in
			let clock_prime_id = ref model.nb_variables in
			List.iter (fun (clock_id, _) ->
				Hashtbl.add prime_of_variable clock_id !clock_prime_id;
				Hashtbl.add variable_of_prime !clock_prime_id clock_id;
				(* Debug message *)
				if debug_mode_greater Verbose_total then(
					print_message Verbose_total ("\nThe primed index of variable '" ^ (model.variable_names clock_id) ^ "' (index = " ^ (string_of_int clock_id) ^ ") is set to " ^ (string_of_int !clock_prime_id) ^ ".")
				);
				(* Increment the prime id for next variable *)
				clock_prime_id := !clock_prime_id + 1;
				()
			) updates;
			let new_max_dimension = !clock_prime_id in
			let extra_dimensions = new_max_dimension - model.nb_variables in
			print_message Verbose_total ("\nNew dimension for constraints: " ^ (string_of_int new_max_dimension) ^ "; extra dimensions : " ^ (string_of_int extra_dimensions) ^ ".");
			(* Extend the number of dimensions *)
(* 			LinearConstraint.set_manager 0 new_max_dimension; *)
			LinearConstraint.set_dimensions model.nb_parameters (model.nb_clocks + extra_dimensions) model.nb_discrete;
			LinearConstraint.pxd_add_dimensions extra_dimensions linear_constraint;

			(* Create constraints X_i' = linear_term *)
			let inequalities = List.map (fun (clock_id, linear_term) ->
				(* Build linear_term - clock_id' = 0 *)
				LinearConstraint.make_pxd_linear_inequality (
					LinearConstraint.add_pxd_linear_terms
						(* 1: The update linear term *)
						linear_term
						(* 2: - clock_id' *)
						(LinearConstraint.make_pxd_linear_term [
								NumConst.minus_one, (Hashtbl.find prime_of_variable clock_id);
							] NumConst.zero)
				) LinearConstraint.Op_eq
			) updates in
			(* Create the constraint *)
			let inequalities = LinearConstraint.make_pxd_constraint inequalities in
			(* Print some information *)
			let print_constraint c = 
				if debug_mode_greater Verbose_total then(
					let all_variable_names = fun variable_id ->
						if variable_id < model.nb_variables then 
							model.variable_names variable_id
						else
							(model.variable_names (Hashtbl.find variable_of_prime variable_id)) ^ "'"
					in
					print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint all_variable_names c);
				)else(
					()
				)
			in
			print_constraint inequalities;

			(* Add the constraints X_i' = linear_term *)
			print_message Verbose_total ("\n -- Adding X_i' = linear_term for updated clocks");
			LinearConstraint.pxd_intersection_assign linear_constraint [inequalities];
			(* Print some information *)
			print_constraint linear_constraint;
			
			(* Remove the variables X_i *)
			let list_of_clocks_to_hide, _ = List.split updates in
			(* Hide clocks updated within the linear constraint, viz., exists X_i : lc, for X_i in rho(X) *)
			print_message Verbose_total ("\n -- Computing exists X : lc for updated clocks");
			LinearConstraint.pxd_hide_assign list_of_clocks_to_hide linear_constraint;
			(* Print some information *)
			if debug_mode_greater Verbose_total then(
				print_constraint linear_constraint;
			);
			
			(* Renames clock X_i' into X_i *)
			(** TO OPTIMIZE !! *)
			(* Compute couples (X_i', X_i) *)
			let clocks_and_primes = Hashtbl.fold (fun clock_id clock_prime_id couples -> (clock_id, clock_prime_id) :: couples) prime_of_variable [] in
			print_message Verbose_total ("\n -- Renaming clocks X_i' into X_i for updated clocks");
			LinearConstraint.pxd_rename_variables_assign clocks_and_primes linear_constraint;
			(* Print some information *)
			if debug_mode_greater Verbose_total then(
				print_constraint linear_constraint;
			);

			(* Go back to the original number of dimensions *)
			print_message Verbose_total ("\nGo back to standard dimension for constraints: " ^ (string_of_int model.nb_variables) ^ ".");
(* 			LinearConstraint.set_manager 0 model.nb_variables; *)
			LinearConstraint.set_dimensions model.nb_parameters model.nb_clocks model.nb_discrete;
			LinearConstraint.pxd_remove_dimensions extra_dimensions linear_constraint;
			(* Print some information *)
			if debug_mode_greater Verbose_total then(
				print_constraint linear_constraint;
			);
			
			(** TO CHECK: what about discrete variables ?!! *)
		)
		)
	)


(*------------------------------------------------------------*)
(* Create a fresh constraint of the form 'D = d' for any discrete variable D with value d *)
(*------------------------------------------------------------*)


(*** SHALL BE REMPLACED WITH THE LINEAR_CONSTRAINT FUNCTION ***)



let instantiate_discrete discrete_values =
	let inequalities = List.map (fun (discrete_index, discrete_value) ->
		(* Create a linear term 'D - d' *)
		let linear_term = LinearConstraint.make_pxd_linear_term
			[(NumConst.one, discrete_index)]
			(NumConst.neg discrete_value)
		in
		(* Create a linear equality *)
		LinearConstraint.make_pxd_linear_inequality linear_term LinearConstraint.Op_eq
	) discrete_values in
	(* Create the linear constraint *)
	LinearConstraint.make_pxd_constraint inequalities


(*------------------------------------------------------------*)
(* Compute the list of stopped and elapsing clocks in a location *)
(*------------------------------------------------------------*)
let compute_stopwatches model location =
	(* If no stopwatches at all: just return the set of clocks *)
	if not model.has_stopwatches then ([], model.clocks) else(
		(* Hashtbl clock_id --> true if clock should be stopped by some automaton *)
		let stopwatches_hash = Hashtbl.create (List.length model.clocks) in
		let stopwatch_mode = ref false in
		(* Update hash table *)
		List.iter (fun automaton_index ->
			(* Get the current location *)
			let location_index = Automaton.get_location location automaton_index in
			(* Get the list of stopped clocks *)
			let stopped = model.stopwatches automaton_index location_index in
			(* If list non null: we have stopwatches here *)
			if stopped != [] then stopwatch_mode := true;
			(* Add each clock *)
			List.iter (fun stopwatch_id ->
				Hashtbl.replace stopwatches_hash stopwatch_id true
			) stopped;
		) model.automata;
		(* If there are no stopwatches then just return the set of clocks *)
		if (not !stopwatch_mode) then ([], model.clocks) else (
			(* Computing the list of stopped clocks, and the list of elapsing clocks *)
			List.fold_left (fun (stopped_clocks, elapsing_clocks) clock_id -> 
				(* Test if the clock should be stopped *)
				if Hashtbl.mem stopwatches_hash clock_id then
					clock_id :: stopped_clocks, elapsing_clocks
				else
					stopped_clocks, clock_id :: elapsing_clocks
			) ([], []) model.clocks
		) (* if no stopwatch for this location *)
	) (* if no stopwatch in the model *)



(*------------------------------------------------------------*)
(* Apply time elapsing in location to the_constraint (the location is needed to retrieve the stopwatches stopped in this location) *)
(*------------------------------------------------------------*)
let apply_time_elapsing location the_constraint =
	(* Get the model *)
	let model = Input.get_model() in
	(* If urgent: no time elapsing *)
	if is_location_urgent model location then (
		print_message Verbose_high ("Location urgent: NO time elapsing");
		()
	(* If not urgent: apply time elapsing *)
	)else(
		(* Compute the list of stopwatches *)
		let stopped_clocks, elapsing_clocks = compute_stopwatches model location in
		print_message Verbose_high ("Computing list of stopwatches");
		if debug_mode_greater Verbose_total then(
			let list_of_names = List.map model.variable_names stopped_clocks in
			print_message Verbose_total ("Stopped clocks : " ^ (string_of_list_of_string_with_sep ", " list_of_names));
			let list_of_names = List.map model.variable_names elapsing_clocks in
			print_message Verbose_total ("Elapsing clocks: " ^ (string_of_list_of_string_with_sep ", " list_of_names));
		);
		
		(* Perform time elapsing *)
		print_message Verbose_high ("Now applying time elapsing...");
		(*** NOTE: the comment is to be changed in alternative TE mode ***)
		LinearConstraint.pxd_time_elapse_assign
			elapsing_clocks
			(List.rev_append stopped_clocks model.parameters_and_discrete)
			the_constraint
		;
		(* Print some information *)
		if debug_mode_greater Verbose_total then(
			print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names the_constraint);
		);
		()
	)


(*------------------------------------------------------------*)
(* Compute the initial state with the initial invariants and time elapsing *)
(*------------------------------------------------------------*)
let create_initial_state model =
	(* Retrieve the input options *)
	let options = Input.get_options () in

	(* Get the declared init state with initial constraint C_0(X) *)
	let initial_location = model.initial_location in
	let initial_constraint = model.initial_constraint in
	
	(* Extend dimensions for discrete *)
	let initial_constraint = LinearConstraint.pxd_of_px_constraint initial_constraint in
	
	(* Compute the invariants I_l0(X) for the initial locations *)
	print_message Verbose_high ("\nComputing initial invariant I_l0(X)");
	(* Create the invariant *)
	let invariant = compute_plain_invariant model initial_location in
	(* Print some information *)
	if debug_mode_greater Verbose_total then
		print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names invariant);
	
	(* Compute constraint for assigning a (constant) value to discrete variables *)
	print_message Verbose_high ("Computing constraint for discrete variables");
	let discrete_values = List.map (fun discrete_index -> discrete_index, (Automaton.get_discrete_value initial_location discrete_index)) model.discrete in
	(* Constraint of the form D_i = d_i *)
	let discrete_constraint = instantiate_discrete discrete_values in
	(* Print some information *)
	if debug_mode_greater Verbose_total then
		print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names discrete_constraint);
	
	(* Perform intersection of C(X) and I_l0(X) and D_i = d_i *)
	print_message Verbose_high ("Performing intersection of C0(X) and I_l0(X) and D_i = d_i");
	let current_constraint = LinearConstraint.pxd_intersection [initial_constraint ; invariant ; discrete_constraint (*** TO OPTIMIZE: could be removed ***)] in
	(* Print some information *)
	if debug_mode_greater Verbose_total then
		print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names current_constraint);

	
	(*--- BEGIN only if time elapsing ---*)
	if not options#no_time_elapsing then(
		(* Perform time elapsing *)
		print_message Verbose_high ("Applying time elapsing to [ C0(X) and I_l0(X) and D_i = d_i ]");
		apply_time_elapsing initial_location current_constraint;
		
(*		(* Compute the list of stopwatches *)
		let stopped_clocks, elapsing_clocks = compute_stopwatches model initial_location in
		print_message Verbose_high ("Computing list of stopwatches");
		if debug_mode_greater Verbose_total then(
			let list_of_names = List.map model.variable_names stopped_clocks in
			print_message Verbose_total ("Stopped clocks : " ^ (string_of_list_of_string_with_sep ", " list_of_names));
			let list_of_names = List.map model.variable_names elapsing_clocks in
			print_message Verbose_total ("Elapsing clocks: " ^ (string_of_list_of_string_with_sep ", " list_of_names));
		);
		
		LinearConstraint.pxd_time_elapse_assign (*model.clocks model.parameters_and_discrete*)
			elapsing_clocks
			(List.rev_append stopped_clocks model.parameters_and_discrete)
			current_constraint
		;
		(* Print some information *)
		if debug_mode_greater Verbose_total then
			print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names current_constraint);*)
		
		
		(* Perform intersection of [C(X) and I_l0(X) and D_i = d_i]time with I_l0(X) and D_i = d_i *)
		(*** NOTE: intersection NOT necessary in absence of time elapsing, because the same I_l0(X) and D_i = d_i were intersected earlier ***)
		print_message Verbose_high ("Performing intersection of [C0(X) and I_l0(X) and D_i = d_i]time and I_l0(X) and D_i = d_i");
		LinearConstraint.pxd_intersection_assign current_constraint [invariant ; discrete_constraint];
		(* Print some information *)
		if debug_mode_greater Verbose_total then
			print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names current_constraint);
	);
		
	(*--- END only if time elapsing ---*)
	
	(* Hide discrete *)
	print_message Verbose_high ("Hide discrete");
	let current_constraint = LinearConstraint.pxd_hide_discrete_and_collapse (*model.discrete*) current_constraint in
	(* Print some information *)
	if debug_mode_greater Verbose_total then
		print_message Verbose_total (LinearConstraint.string_of_px_linear_constraint model.variable_names current_constraint);
		
		
	(* Remove useless clocks (if option activated) *)
	if options#dynamic_clock_elimination then(
		(* Compute the useless clocks *)
		let clocks_to_remove = List.fold_left (fun current_list_of_clocks automaton_index ->
			(* Retrieve dest location for this automaton *)
			let location_index = Automaton.get_location initial_location automaton_index in
			(* Get the clocks and append to previously computed clocks (rev_append because the order doesn't matter) *)
			List.rev_append current_list_of_clocks (!useless_clocks automaton_index location_index)
		) [] model.automata in
		(* Print some information *)
		if debug_mode_greater Verbose_low then(
			print_message Verbose_low ("The following clocks will be dynamically removed:");
			print_message Verbose_low ("  " ^ (string_of_list_of_string_with_sep ", " (List.map model.variable_names clocks_to_remove)));
		);
		
		print_message Verbose_high ("\nRemoving useless clocks ");
		LinearConstraint.px_hide_assign clocks_to_remove current_constraint;
		(* Print some information *)
		if debug_mode_greater Verbose_total then(
			print_message Verbose_total (LinearConstraint.string_of_px_linear_constraint model.variable_names current_constraint);
		);
	);
	
	
	(* Return the initial state *)
	initial_location, current_constraint



(*------------------------------------------------------------*)
(* Compute the initial state with the initial invariants and time elapsing, and check whether it is satisfiable; if not, abort *)
(*------------------------------------------------------------*)
let get_initial_state_or_abort model =
	(* Retrieve the input options *)
	let options = Input.get_options () in

	(* Print the initial state *)
	if debug_mode_greater Verbose_medium then
		print_message Verbose_medium ("\nInitial state:\n" ^ (ModelPrinter.string_of_state model (model.initial_location, model.initial_constraint)) ^ "\n");

	(* Check the satisfiability *)
	if not (LinearConstraint.px_is_satisfiable model.initial_constraint) then (
		print_warning "The initial constraint of the model is not satisfiable.";
		terminate_program();
	)else(
		print_message Verbose_total ("\nThe initial constraint of the model is satisfiable.");
	);

	(* Get the initial state after time elapsing *)
	let init_state_after_time_elapsing = create_initial_state model in
	let _, initial_constraint_after_time_elapsing = init_state_after_time_elapsing in


	(* Check the satisfiability *)
	let begin_message = "The initial constraint of the model after invariant " ^ (if not options#no_time_elapsing then " and time elapsing" else "") in
	if not (LinearConstraint.px_is_satisfiable initial_constraint_after_time_elapsing) then (
		print_warning (begin_message ^ "is not satisfiable.");
		terminate_program();
	)else(
		print_message Verbose_total ("\n" ^ begin_message ^ "is satisfiable.");
	);
	(* Print the initial state after time elapsing *)
	if debug_mode_greater Verbose_medium then
		print_message Verbose_medium ("\nInitial state computed:\n" ^ (ModelPrinter.string_of_state model init_state_after_time_elapsing) ^ "\n");
		
	(* Return the initial state *)
	init_state_after_time_elapsing


(*------------------------------------------------------------*)
(* Compute a list of possible actions for a state   *)
(*------------------------------------------------------------*)
let compute_possible_actions model original_location = 
	(* Create a boolean array for the possible actions *)
	let possible_actions = Array.make model.nb_actions false in
	(* Fill it with all the possible actions per location *)
	for automaton_index = 0 to model.nb_automata - 1 do
		(* Get the current location for automaton_index *)
		let location_index = Automaton.get_location original_location automaton_index in
		(* Print some information *)
		print_message Verbose_total ("Considering automaton " ^ (model.automata_names automaton_index) ^ " with location " ^ (model.location_names automaton_index location_index) ^ ".");
		(* Get the possible actions for this location *)
		let possible_actions_for_this_automaton =
			model.actions_per_location automaton_index location_index
		in
		(* Add all the actions to our array *)
		List.iter (fun action_index ->
			(* Add the action *)
			possible_actions.(action_index) <- true;
			(* Print some information *)
			print_message Verbose_total ("Adding action " ^ (model.action_names action_index) ^ " for automaton " ^ (model.automata_names automaton_index) ^ " with location " ^ (model.location_names automaton_index location_index) ^ ".");
		) possible_actions_for_this_automaton;
	done;
	(* Print some information *)
	if debug_mode_greater Verbose_total then (
		print_message Verbose_total ("Possible actions for all the locations are:");
		Array.iteri (fun action_index possible ->
			if possible then (print_message Verbose_total (" - " ^ (model.action_names action_index)));
		) possible_actions;
	);
	(* Remove every action where an automaton can not take this action in its *)
	(*  location although the action was declared for this automaton          *)
	let possible_actions = Array.mapi (fun action_index possible ->
		(* If this action is not possible, then false *)
		if not possible then false else(
		let automata_for_this_action = model.automata_per_action action_index in
		(* Check if the action is possible for all the automata for which it is defined *)
		(**** TO OPTIMIZE: no need to keep searching if not "still_possible" anymore ****)
		let action_possible =
			List.fold_left (fun still_possible automaton_index -> 
				still_possible
				&& (List.mem action_index (model.actions_per_location automaton_index (Automaton.get_location original_location automaton_index)))
			) possible automata_for_this_action in
		(* Print some information *)
		if not action_possible && (debug_mode_greater Verbose_total) then (
			print_message Verbose_total ("But action '" ^ (model.action_names action_index) ^ "' is not possible for all declared automata.")
		);
		(* Replace the previous value by the new one *)
		action_possible
		)
	) possible_actions in
	(* Make a list *)
	true_indexes possible_actions



(*------------------------------------------------------------------*)
(* Compute a new location for a given set of transitions            *)
(* aut_table         : indices of involved automata                 *)
(* trans_table       : indices of examined transition per automaton *)
(* action_index      : index of current action                      *)
(* original_location : the source location                          *)
(*------------------------------------------------------------------*)
(* returns the new location, the guards, the updates                *)
(*------------------------------------------------------------------*)
let compute_new_location model aut_table trans_table action_index original_location =
	(* make a copy of the location *)		
	let location = Automaton.copy_location original_location in
	(* Create a temporary hashtbl for discrete values *)
	let updated_discrete = Hashtbl.create model.nb_discrete in
	(* Check if we actually have updates *)
	let has_updates = ref false in
	(* Update the location for the automata synchronized with 'action_index'; return the list of guards and updates *)
	let guards_and_updates = Array.to_list (Array.mapi (fun local_index real_index ->
		(* Get the current location for this automaton *)
		let location_index = Automaton.get_location original_location real_index in
		(* Find the transitions for this automaton *)
		let transitions = model.transitions real_index location_index action_index in
		(* Get the index of the examined transition for this automaton *)
		let current_index = trans_table.(local_index) in
		(* Keep the 'current_index'th transition *)
		let transition = List.nth transitions current_index in
		(* Keep only the dest location *)
		let guard, clock_updates, discrete_updates, dest_index = transition in			
		(* Update discrete *)
		List.iter (fun (discrete_index, linear_term) ->
			(* Compute its new value *)

			(*** TO OPTIMIZE (in terms of dimensions) ***)
			
			let new_value = LinearConstraint.evaluate_pxd_linear_term (Automaton.get_discrete_value original_location) linear_term in
			(* Check if already updated *)
			if Hashtbl.mem updated_discrete discrete_index then (
				(* Find its value *)
				let previous_new_value = Hashtbl.find updated_discrete discrete_index in
				(* Compare with the new one *)
				if NumConst.neq previous_new_value new_value then (
				(* If different: warning *)
					print_warning ("The discrete variable '" ^ (model.variable_names discrete_index) ^ "' is updated several times with different values for the same synchronized action '" ^ (model.action_names action_index) ^ "'. The behavior of the system is now unspecified.");
				);
			) else (
				(* Else keep it in memory for update *)
				Hashtbl.add updated_discrete discrete_index new_value;
			);
		) discrete_updates;
		(* Update the global location *)
		Automaton.update_location_with [real_index, dest_index] [] location;
		(* Update the update flag *)
		begin
		match clock_updates with
			| Resets (_ :: _) -> has_updates := true
			| Updates (_ :: _) -> has_updates := true
			| _ -> ()
		end;
		(* Keep the guard and updates *)
		guard, clock_updates;
	) aut_table) in
	(* Split the list of guards and updates *)
	let guards, clock_updates = List.split guards_and_updates in
	(* Compute couples to update the discrete variables *)
	let updated_discrete_couples = ref [] in
	Hashtbl.iter (fun discrete_index discrete_value ->
		updated_discrete_couples := (discrete_index, discrete_value) :: !updated_discrete_couples;
	) updated_discrete;
	(* Update the global location *)
	Automaton.update_location_with [] !updated_discrete_couples location;
  (* return the new location, the guards, and the clock updates (if any!) *)
	(location, guards, (if !has_updates then clock_updates else []))
	
	


(*------------------------------------------------------------*)
(* Compute the new constraint for a transition      *)
(* orig_constraint : contraint in source location   *)
(* discrete_constr_src : contraint D_i = d_i in source location (discrete variables) *)
(* (* stopped_clocks  : list of clocks stopped         *) *)
(* (* elapsing_clocks : list of clocks non stopped     *) *)
(* orig_location   : source location                *)
(* dest_location   : target location                *)
(* guards          : guard constraints per automaton*)
(* clock_updates   : updated clock variables        *)
(*------------------------------------------------------------*)
let compute_new_constraint model orig_constraint (discrete_constr_src : LinearConstraint.pxd_linear_constraint) orig_location dest_location guards clock_updates =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	
	if debug_mode_greater Verbose_total then(
		print_message Verbose_total ("\n***********************************");
		print_message Verbose_total ("Entering compute_new_constraint");	
		print_message Verbose_total ("***********************************");
		print_message Verbose_total ("C = " ^ (LinearConstraint.string_of_px_linear_constraint model.variable_names (orig_constraint ())));
	);
	(* The constraint is checked on the fly for satisfiability -> exception mechanism *)
	try (
		(* Retrieve the original constraint *)
		(*** WARNING / VERY IMPORTANT: copy!!! (in fact convert, which is also a copy) ***)
		let orig_constraint_with_maybe_time_elapsing = LinearConstraint.pxd_of_px_constraint  (orig_constraint ()) in

		(* Alternative IMITATOR semantics for time-elapsing: apply time-elapsing NOW, and intersect with invariant *)
		if options#no_time_elapsing then(
			print_message Verbose_total ("\nAlternative time elapsing: Applying time elapsing NOW");
			apply_time_elapsing orig_location orig_constraint_with_maybe_time_elapsing;
			
			(* Compute the invariant in the source location I_l(X) *)
			(*** TO OPTIMIZE!!! This should be done only once in the function calling this function!! ***)
			print_message Verbose_total ("\nComputing invariant I_l(X)");
			let invariant = compute_invariant model orig_location in
			(* Print some information *)
			if debug_mode_greater Verbose_total then(
				print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names invariant);
			);

			(* Perform the intersection *)
			print_message Verbose_total ("\nAlternative time elapsing: performing intersection of C(X)time and I_l(X)");
			LinearConstraint.pxd_intersection_assign orig_constraint_with_maybe_time_elapsing [invariant];
		);

		
(*		(* Factor time-elapsing because it can be used at two different places depending on the options *)
		let apply_time_elapsing () =
			(* Compute the list of stopwatches *)
			let stopped_clocks, elapsing_clocks = compute_stopwatches model dest_location in
			print_message Verbose_high ("Computing list of stopwatches");
			if debug_mode_greater Verbose_total then(
				let list_of_names = List.map model.variable_names stopped_clocks in
				print_message Verbose_total ("Stopped clocks : " ^ (string_of_list_of_string_with_sep ", " list_of_names));
				let list_of_names = List.map model.variable_names elapsing_clocks in
				print_message Verbose_total ("Elapsing clocks: " ^ (string_of_list_of_string_with_sep ", " list_of_names));
			);
			
			(* Perform time elapsing *)
			LinearConstraint.pxd_time_elapse_assign
				elapsing_clocks
				(List.rev_append stopped_clocks model.parameters_and_discrete)
				current_constraint
			;
			(* Print some information *)
			if debug_mode_greater Verbose_total then(
				print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names current_constraint);
			);
		in*)

		let current_constraint = LinearConstraint.pxd_copy discrete_constr_src in
		
		(* Print some information *)
		if debug_mode_greater Verbose_total then(
			print_message Verbose_total ("\nComputing the guards g(x)");
			List.iter (fun guard -> 
				print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names guard);
			) guards;
		);

		print_message Verbose_total ("\nPerforming intersection of Di = di and C(X) and g(X)");
		(* Add the (old) value for discrete to the guards D_i = d_i and g(X) *)
		LinearConstraint.pxd_intersection_assign current_constraint (orig_constraint_with_maybe_time_elapsing :: guards);
		(* Print some information *)
		if debug_mode_greater Verbose_total then(
			print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names current_constraint);
		);
		
		(* Check here for unsatisfiability *)
		if not (LinearConstraint.pxd_is_satisfiable current_constraint) then (
			(* Statistics *)
			nb_unsat1 := !nb_unsat1 + 1;
			print_message Verbose_high "skip transition";
			raise Unsat_exception
		);
		
		print_message Verbose_total ("\nEliminate the discrete variables in C(X) and g(X)");
		(* Remove the discrete variables (Exists D_i : D_i = d_i and g(X)) *)
		LinearConstraint.pxd_hide_assign model.discrete current_constraint;
		(* Print some information *)
		if debug_mode_greater Verbose_total then(
			print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names current_constraint);
		);
		
		print_message Verbose_total ("\nProjecting C(X) and g(X) onto rho");
		rho_assign model current_constraint clock_updates;
		(* Print some information *)
		if debug_mode_greater Verbose_total then(
			print_message Verbose_total ("\nResult:");
			print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names current_constraint);
		);

		(* Compute the invariant in the destination location I_l'(X) *)
		print_message Verbose_total ("\nComputing invariant I_l'(X)");
		let invariant = compute_invariant model dest_location in
		(* Print some information *)
		if debug_mode_greater Verbose_total then(
			print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names invariant);
		);

		(* Perform the intersection *)
		print_message Verbose_total ("\nPerforming intersection of [C(X) and g(X)]rho and I_l'(X)");
		(* (Exists D_i : D_i = d_i and g(X)) *)
		LinearConstraint.pxd_intersection_assign current_constraint [invariant];
		(* Print some information *)
		if debug_mode_greater Verbose_total then(
			print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names current_constraint);
			if not (LinearConstraint.pxd_is_satisfiable current_constraint) then
				print_message Verbose_total ("This constraint is NOT satisfiable (after intersection of [C(X) and g(X)] rho and I_l'(X) ).");
		);
		(*** NOTE: NO USE FOR TESTING HERE FOR SATISFIABILITY (almost always satisfiable from my experiments) -- ÉA ***)
	
	
		(* Normal IMITATOR semantics for time-elapsing: apply time-elapsing now *)
		if not options#no_time_elapsing then(
			print_message Verbose_high ("Applying time elapsing to [C(X) and g(X)]rho and I_l'(X) ]");
			apply_time_elapsing dest_location current_constraint;
		);
	
	
		(* Compute the equalities for the discrete variables in destination location *)
		let discrete_values_dest = List.map (fun discrete_index -> discrete_index, (Automaton.get_discrete_value dest_location discrete_index)) model.discrete in
		(* Convert to a constraint *)
		let discrete_constraint_dest = instantiate_discrete discrete_values_dest in
		
		(* Perform the intersection *)
		print_message Verbose_total ("\nPerforming intersection of the constraint with D_i = d_i and I_l'(X) ");
		LinearConstraint.pxd_intersection_assign current_constraint
			[
				discrete_constraint_dest;
				(*** NOTE: in principle, no need to intersect with invariant if time elapsing was NOT applied (alternating semantics). This could be improved (and tested) in the future. ***)
				invariant;
			];
		(* Print some information *)
		if debug_mode_greater Verbose_total then(
			print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names current_constraint);
			if not (LinearConstraint.pxd_is_satisfiable current_constraint) then
				print_message Verbose_total ("This constraint is NOT satisfiable (after intersection of the constraint with D_i = d_i and I_l'(X)).");
		);
		
(*		(* Check here for unsatisfiability *)
		if not (LinearConstraint.is_satisfiable current_constraint) then (
			(* Statistics *)
			nb_unsat2 := !nb_unsat2 + 1;
			print_message Verbose_high "skip transition";
			raise Unsat_exception
		);*)

		(* AGAIN, NO USE FOR TESTING HERE FOR SATISFIABILITY (almost always satisfiable) *)

		(* Hide discrete' *)
		print_message Verbose_total ("\nHide discrete variables ");
		let current_constraint = LinearConstraint.pxd_hide_discrete_and_collapse (*(model.discrete)*) current_constraint in
		(* Print some information *)
		if debug_mode_greater Verbose_total then(
			print_message Verbose_total (LinearConstraint.string_of_px_linear_constraint model.variable_names current_constraint);
			if not (LinearConstraint.px_is_satisfiable current_constraint) then
				print_message Verbose_total ("This constraint is NOT satisfiable (after hiding discrete variables).");
		);
		
		
		(* Remove useless clocks (if option activated) *)
		if options#dynamic_clock_elimination then(
			(* Compute the useless clocks *)
			let clocks_to_remove = List.fold_left (fun current_list_of_clocks automaton_index ->
				(* Retrieve dest location for this automaton *)
				let location_index = Automaton.get_location dest_location automaton_index in
				(* Get the clocks and append to previously computed clocks (rev_append because the order doesn't matter) *)
				List.rev_append current_list_of_clocks (!useless_clocks automaton_index location_index)
			) [] model.automata in
			(* Print some information *)
			if debug_mode_greater Verbose_low then(
				print_message Verbose_low ("The following clocks will be dynamically removed:");
				print_message Verbose_low ("  " ^ (string_of_list_of_string_with_sep ", " (List.map model.variable_names clocks_to_remove)));
			);
			
			print_message Verbose_high ("\nRemoving useless clocks ");
			LinearConstraint.px_hide_assign clocks_to_remove current_constraint;
			(* Print some information *)
			if debug_mode_greater Verbose_total then(
				print_message Verbose_total (LinearConstraint.string_of_px_linear_constraint model.variable_names current_constraint);
			);
		);
		
		
		
		(* return the final constraint *)
		Some current_constraint
	) with Unsat_exception -> None


(*------------------------------------------------*)
(* Computes next combination of indices           *)
(* current_indexes : combination                  *)
(* max_indexes     : maximum indices              *)
(*------------------------------------------------*)
(* returns a boolean, indicating that the         *)
(* new combination is valid (false if the old     *)
(* combination was the last one)                  *)
(*------------------------------------------------*)
let next_combination combination max_indexes =
	let len = Array.length combination in 
	let valid_combination = ref true in 
	let not_is_max = ref true in
	let local_index = ref 0 in
	while !not_is_max do
		(* Look for the first index to be incremented *)
		if combination.(!local_index) < max_indexes.(!local_index) then(
			(* Increment this index *)
			combination.(!local_index) <- combination.(!local_index) + 1;
			(* Reset the smaller indexes to 0 *)
			for i = 0 to !local_index - 1 do
				combination.(i) <- 0;
			done;
			(* Stop the loop *)
			not_is_max := false;
		) else (
			local_index := !local_index + 1;
			if !local_index >= len then(
				valid_combination := false;
				not_is_max := false;
			)
		)
	done; (* end while *)
	!valid_combination



(*-----------------------------------------------------*)
(* Checks a new state for pi0-compatibility and        *)
(* updates constraint K if incompatible state is found.*)
(* pi0               : reference valuation             *)
(* rechability_graph : current reachability graph      *)
(* constr            : new state constraint            *)
(*-----------------------------------------------------*)
(* returns (true, p_constraint) if the state is pi0-compatible, and (false, _) otherwise *)
(*** BADPROG: no need to return (false, _), better return some Some/None ***)
(*-----------------------------------------------------*)
(* side effect: add the negation of the p_constraint to all computed states *)
(*-----------------------------------------------------*)
let inverse_method_check_constraint model reachability_graph constr =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	(* Retrieve the pi0 (dynamic!) *)
	let pi0 = Input.get_pi0 () in
	
	(* Hide non-parameters *)
	print_message Verbose_high ("\nHiding non parameters...");
	let p_constraint = LinearConstraint.px_hide_nonparameters_and_collapse (*model.clocks_and_discrete*) constr in
	print_message Verbose_high ("\nParameters now hidden:");
	(* Print some information *)
	if debug_mode_greater Verbose_high then(
		print_message Verbose_high (LinearConstraint.string_of_p_linear_constraint model.variable_names p_constraint);
	);
	(* Check the pi0-compatibility *)
	print_message Verbose_high ("\nChecking pi-compatibility:");
	let compatible, incompatible = LinearConstraint.partition_pi0_compatible pi0#get_value p_constraint in
	let is_pi0_incompatible = incompatible != [] in
	
	(* If pi0-incompatible: select an inequality *)
	if is_pi0_incompatible then (
		print_message Verbose_low ("\nFound a pi0-incompatible state.");
		(* Print some information *)
		if debug_mode_greater Verbose_medium then(
			print_message Verbose_high ("Associated constraint:");
			print_message Verbose_high (LinearConstraint.string_of_px_linear_constraint model.variable_names constr);
			print_message Verbose_medium ("\nThe following inequalities are pi0-incompatible:");
			List.iter (fun inequality -> print_message Verbose_medium (LinearConstraint.string_of_p_linear_inequality model.variable_names inequality)) incompatible;
			if debug_mode_greater Verbose_high then(
				print_message Verbose_high ("\nRecall that pi0 is:");
				print_message Verbose_high   (ModelPrinter.string_of_pi0 model pi0);
			);
		);
		
		(* Case EFIM: no need to select a pi-incompatible inequality if already bad *)
		if options#efim && !tile_nature = Bad then(
			print_message Verbose_low ("\n[EFIM] Cut branch.");
			(false , p_constraint)
		
		(* Case normal IM: select a pi-incompatible inequality *)
		)else(
			let p_inequality =
				(* If random selection: pick up a random inequality *)
				if not options#no_random then random_element incompatible
				(* Else select the first one *)
				else List.nth incompatible 0
			in
			(* Print some information *)
			if debug_mode_greater  Verbose_medium then(
				print_message Verbose_medium ("\nSelecting the following pi0-incompatible inequality:");
				print_message Verbose_medium (LinearConstraint.string_of_p_linear_inequality model.variable_names p_inequality);
			);

			(* Update counter *)
			if List.length incompatible > 1 then nb_random_selections := !nb_random_selections + 1;
			
			(* Negate the inequality *)
			let negated_inequality = LinearConstraint.negate_wrt_pi0 pi0#get_value p_inequality in
			(* Print some information *)
			if debug_mode_greater Verbose_standard then(
				let randomly = if not options#no_random then "randomly " else "" in
				let among = if List.length incompatible > 1 then (" (" ^ randomly ^ "selected among " ^ (string_of_int (List.length incompatible)) ^ " inequalities)") else "" in
				print_message Verbose_standard ("  Adding the following inequality" ^ among ^ ":");
				print_message Verbose_standard ("  " ^ (LinearConstraint.string_of_p_linear_inequality model.variable_names negated_inequality));
			);
			
			(* Transform to constraint *)
			let negated_constraint = LinearConstraint.make_p_constraint [negated_inequality] in
			
			
			(* Add the p_constraint to the result (except in case of variants) *)
			(*** WARNING: why not in case of variants ?! ***)
			if not (options#pi_compatible || options#union) then(
				print_message Verbose_high ("Updating k_result with the negated inequality");
				LinearConstraint.p_intersection_assign !k_result [negated_constraint];
				(* Print some information *)
				if debug_mode_greater Verbose_low then(
					print_message Verbose_low ("\nk_result now equal (after addition of neg J) to ");
					print_message Verbose_low (LinearConstraint.string_of_p_linear_constraint model.variable_names !k_result);
					print_message Verbose_low ("");
				);
			);
			
			(* Update the previous states (including the 'new_states' and the 'orig_state') *)
			(* Not for EFIM! *)
			(*** NOTE: maybe not for IMK either? ****)
			if not options#efim then(
				print_message Verbose_medium ("\nUpdating all the previous states.\n");
				StateSpace.add_p_constraint_to_states reachability_graph negated_constraint;
			)else(
				print_message Verbose_standard("  [EFIM] Storing inequality only");
			);
			
			(* If pi-incompatible *)
			(false, p_constraint)
		)(* Endif normal IM and pi-incompatible *)
	) else (true, p_constraint)


(*-----------------------------------------------------*)
(* Checks a new state for pi0-compatibility, and add its projection onto the parameters if incompatible.*)
(* pi0               : reference valuation             *)
(* rechability_graph : current reachability graph      *)
(* constr            : new state constraint            *)
(*-----------------------------------------------------*)
(* returns (true, _) if the state is pi0-compatible (false, p_constraint) otherwise *)
(*-----------------------------------------------------*)
let completeIM_check_constraint model reachability_graph constr =
	(* Retrieve the input options *)
(* 	let options = Input.get_options () in *)
	(* Retrieve the pi0 (dynamic!) *)
	let pi0 = Input.get_pi0 () in
	
	(* Hide non-parameters *)
	print_message Verbose_high ("\nHiding non parameters...");
	let p_constraint = LinearConstraint.px_hide_nonparameters_and_collapse constr in
	print_message Verbose_high ("\nParameters now hidden:");
	(* Print some information *)
	if debug_mode_greater Verbose_high then(
		print_message Verbose_high (LinearConstraint.string_of_p_linear_constraint model.variable_names p_constraint);
	);
	(* Check the pi0-compatibility *)
	print_message Verbose_high ("\nChecking pi-compatibility:");
	let is_compatible = LinearConstraint.is_pi0_compatible pi0#get_value p_constraint in
	(* Return the pi0-compatibility and the p_constraint *)
	is_compatible , p_constraint
	

(*-------------------------------------------------------*)
(* Computes all possible transition combinations for the *)
(* involved automata.                                    *)
(* constr               : current state constraint       *)
(* action index         : index of current action        *)
(* automata             : involved automata              *)
(* aut_table            : array of automata              *)
(* max_indexes          : array of maximal trans. indices*)
(* possible_transitions : array of transition indices    *)
(*-------------------------------------------------------*)
(* returns a bool, indicating iff at least one legal     *)
(* combination exists.                                   *)
(*-------------------------------------------------------*)
let compute_transitions model location constr action_index automata aut_table max_indexes possible_transitions  =
	let current_index = ref 0 in 
	(* Stop computation as soon as one automaton has no legal transition left. *)
	try (
		List.iter (fun automaton_index ->
			(* Tabulate the real index *)
			aut_table.(!current_index) <- automaton_index;
			(* Get the current location for this automaton *)
			let location_index = Automaton.get_location location automaton_index in
			(* Get transitions for this automaton *)
			let transitions = model.transitions automaton_index location_index action_index in
			
			(* REMOVED 2011/11/21 : computation always slower ; might be faster for strongly branching systems? EXCEPT FOR LSV.imi --> put it back! *)
			(* Keep only possible transitions *)
			let is_possible = fun trans -> (
				let guard, _, _, _ = trans in
				let constr_and_guard = LinearConstraint.pxd_intersection [constr; guard] in
 				let is_possible = LinearConstraint.pxd_is_satisfiable constr_and_guard in 
				if not is_possible then (
					(* Statistics *)
					nb_early_unsatisfiable := !nb_early_unsatisfiable + 1;
					print_message Verbose_medium "** early skip transition **"
				);
				is_possible(*true*)
			) in
			let legal_transitions = ref [] in
			let trans_index = ref 0 in
			List.iter (fun trans -> 
				if is_possible trans then(
					legal_transitions := !trans_index :: !legal_transitions;
				);
				trans_index := !trans_index + 1
			) transitions;
			(* Stop computation if no legal transition exists *)
			if !legal_transitions = [] then (
				(* Statistics *)
				nb_early_skip := !nb_early_skip + 1;
				print_message Verbose_medium "*** early skip action ***";
				raise Unsat_exception
			);
			(* Store possible transitions *)
			possible_transitions.(!current_index) <- !legal_transitions;
			(* Tabulate the number of transitions for this location *)
			max_indexes.(!current_index) <-	List.length !legal_transitions - 1;
			(* Increment the index *)
			current_index := !current_index + 1;
		) automata;
		(* arrived here, so each automaton must have at least one legal transition *)
		true
	) with Unsat_exception -> false






(************************************************************)
(* Post functions *)
(************************************************************)
(*-----------------------------------------------------*)
(* Add a new state to the reachability_graph (if indeed needed) *)
(* Also update tile_nature and slast *)
(*-----------------------------------------------------*)
let add_a_new_state model reachability_graph orig_state_index new_states_indexes action_index location (final_constraint : LinearConstraint.px_linear_constraint) =
	(* Retrieve the input options *)
	let options = Input.get_options () in

	(* Is the new state valid? *)
	let valid_new_state =
	(*------------------------------------------------------------*)
	(* Branching between algorithms: reachability, synthesis or IM *)
	(*------------------------------------------------------------*)
	match options#imitator_mode with 
	(* Pure state space exploration *)
	| State_space_exploration -> true
	(* Synthesis *)
	| EF_synthesis -> true
		
	(* Inverse method and the like *)
	| _ ->
		let valid_new_state =
		
		(* 1) Complete version of IM *)
		if options#completeIM then(
			let valid_new_state, bad_p_constraint = completeIM_check_constraint model reachability_graph final_constraint in
			(* Update the set of bad polyhedra *)
			k_bad := bad_p_constraint :: !k_bad;
			(* Return locally the result *)
			valid_new_state
		
		(* 2) Regular IM *)
		)else(
			(*** NOTE: the addition of neg J to all reached states is performed as a side effect inside the following function ***)
			(*** BADPROG: same reason ***)
			let valid_new_state, new_p_constraint = inverse_method_check_constraint model reachability_graph final_constraint
			in
			
			(* If pi-compatible state: add the new state's p_constraint to the on-the-fly computation of the result of IMss *)
			if valid_new_state && not (options#pi_compatible || options#union || options#efim) then(
				print_message Verbose_high ("Updating k_result");
				LinearConstraint.p_intersection_assign !k_result [new_p_constraint];
				(* Print some information *)
				if debug_mode_greater Verbose_low then(
					print_message Verbose_low ("\nk_result now equal (after addition of current state's K) to ");
					print_message Verbose_low (LinearConstraint.string_of_p_linear_constraint model.variable_names !k_result);
					print_message Verbose_low ("");
				);
			);
			(* Return locally the result *)
			valid_new_state
		
		)
		in

		(* Print some information *)
		if debug_mode_greater Verbose_high then(
			(* Means state was not compatible *)
			if not valid_new_state then(
				let new_state = location, final_constraint in
				if debug_mode_greater Verbose_high then
					print_message Verbose_high ("The pi-incompatible state had been computed through action '" ^ (model.action_names action_index) ^ "', and was:\n" ^ (string_of_state model new_state));
			);
		);
		(* Return *)
		valid_new_state
	(*------------------------------------------------------------*)
	(* end branch between algorithms *)
	(*------------------------------------------------------------*)
	in
	
	(* Only add the new state if it is actually valid *)
	if valid_new_state then (
		(* Build the state *)
		let new_state = location, final_constraint in

		(* Print some information *)
		if debug_mode_greater Verbose_total then(
			print_message Verbose_total ("Consider the state \n" ^ (string_of_state model new_state));
		);

		(* If IM or BC: Add the inequality to the result (except if case variants) *)
(*		begin
		match options#imitator_mode with 
			(* Case state space / synthesis: do nothing *)
			| State_space_exploration
			| EF_synthesis
				-> ()
			(* Case IM / BC: *)
			| _ -> if not (options#pi_compatible || options#union) then(
					print_message Verbose_high ("Updating k_result");
					LinearConstraint.p_intersection_assign !k_result [inequality];
				);
		end;*)
		
		(* Try to add this new state to the graph *)
		let new_state_index, added = (
		(*if options#dynamic then (
		StateSpace.add_state_dyn model reachability_graph new_state !k_result
		)
		else ( *)
			StateSpace.add_state model reachability_graph new_state
(* 						  ) *)
		) in
		(* If this is really a new state *)
		if added then (
			(* Check if the new state contains an integer point *)
			if options#check_ippta then(
				if not (LinearConstraint.px_contains_integer_point final_constraint) then(
					print_error ("State found with no integer point:\n" ^ 
						(ModelPrinter.string_of_state model new_state));
					raise NoIPPTA
				);
			);
			
			(* First check whether this is a bad tile according to the property and the nature of the state *)
			update_tile_nature new_state;
			
			(* Will the state be added to the list of new states (the successors of which will be computed)? *)
			let to_be_added = ref true in
			
			(* If synthesis / EFIM: add the constraint to the list of successful constraints if this corresponds to a bad location *)
			if options#imitator_mode = EF_synthesis || options#efim then(
			match model.correctness_condition with
				| None -> raise (InternalError("[EF-synthesis/EFIM] A correctness property must be defined to perform EF-synthesis or EFIM. This should have been checked before."))
				| Some (Unreachable (unreachable_automaton_index , unreachable_location_index)) ->
					(* Get the location of unreachable_automaton_index in the new state *)
					let new_location_index = Automaton.get_location location unreachable_automaton_index in
					(* Check if this is the same as the "unreachable" one *)
					if new_location_index = unreachable_location_index then(
						
						(* Project onto the parameters *)
						let p_constraint = LinearConstraint.px_hide_nonparameters_and_collapse final_constraint in
						
						(* Projecting onto SOME parameters if required *)
						begin
						match model.projection with
						(* Unchanged *)
						| None -> ()
						(* Project *)
						| Some parameters ->
							print_message Verbose_medium "  [EF-synthesis] Projecting onto some of the parameters.";
							(*** TODO! do only once for all... ***)
							let all_but_projectparameters = list_diff model.parameters parameters in
							(* Eliminate other parameters *)
							LinearConstraint.p_hide_assign all_but_projectparameters p_constraint;
						end;
						
						(* Add the constraint to the list of constraints, unless it is already present there *)
						(*** TODO: also check for REVERSE inclusion (old included in new) ***)
						(*** TODO: merge this list on-the-fly!! ***)
						(*** TODO: even better, directly use a non-convex constraint using PPL, and leave the work to PPL ***)
						if List.exists (LinearConstraint.p_is_leq p_constraint) !p_constraints then(
							print_message Verbose_low "  [EF-synthesis] Found a state violating the property but the constraint is not new.";
						)else(
							p_constraints := p_constraint :: !p_constraints;
							(* Print some information *)
							print_message Verbose_standard "  [EF-synthesis] Found a state violating the property.";
							
							(* Print some information *)
							if debug_mode_greater Verbose_medium then(
								print_message Verbose_medium "Adding the following constraint to the list of bad constraints:";
								print_message Verbose_medium (LinearConstraint.string_of_p_linear_constraint model.variable_names p_constraint);
							);
							
						);
						
						(* Do NOT compute its successors *)
						to_be_added := false;
						
					)else(
						print_message Verbose_medium "EF-synthesis: State not corresponding to the one wanted.";
					);
				| _ -> raise (InternalError("[EF-synthesis] IMITATOR currently ony implements the non-reachability-like properties. This should have been checked before."))
			); (* end if EF_synthesis *)

			(* Add the state_index to the list of new states (used to compute their successors at the next iteration) *)
			if !to_be_added then
				new_states_indexes := new_state_index :: !new_states_indexes;
			
		) (* end if new state *)
		(* ELSE : add to SLAST if mode union *)
		else (
			if options#union then (
				print_message Verbose_low ("\nMode union: adding a looping state to SLast.");
				(* Adding the state *)
				(*** TODO / TO CHECK: what if new_state_index is already in slast?!! ***)
				slast := new_state_index :: !slast;
			);
		);
		
		(* Update the transitions *)
		StateSpace.add_transition reachability_graph (orig_state_index, action_index, new_state_index);
		(* Print some information *)
		if debug_mode_greater Verbose_high then (
			let beginning_message = (if added then "NEW STATE" else "Old state") in
			print_message Verbose_high ("\n" ^ beginning_message ^ " reachable through action '" ^ (model.action_names action_index) ^ "': ");
			print_message Verbose_high (string_of_state model new_state);
		);
	); (* end if add new state *)
	(* Return void *)
	()



(*-----------------------------------------------------*)
(* Compute the list of possible destination states     *)
(* wrt. to a reachability_graph, and update this graph *)
(*-----------------------------------------------------*)
(* returns a list of (really) new states               *)
(*-----------------------------------------------------*)
let post_from_one_state model reachability_graph orig_state_index =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	(* Original location: static *)
	let original_location, _ = StateSpace.get_state reachability_graph orig_state_index in
	(* Dynamic version of the original px_constraint (can change!) *)
	(*** NOTE / TO OPTIMIZE: OK but not in all algorithms !! ***)
	let orig_constraint () =
		let _, orig_constraint = StateSpace.get_state reachability_graph orig_state_index in
		orig_constraint
	in

	(* Debug prints *)
	if debug_mode_greater Verbose_high then(
		let orig_state = StateSpace.get_state reachability_graph orig_state_index in
		let _, orig_constraint = orig_state in
		let orig_constraint_projection = LinearConstraint.px_hide_nonparameters_and_collapse orig_constraint in
		print_message Verbose_high ("Performing post from state:");
		print_message Verbose_high (string_of_state model orig_state);
		print_message Verbose_high ("\nThe projection of this constraint onto the parameters is:");
		print_message Verbose_high (LinearConstraint.string_of_p_linear_constraint model.variable_names orig_constraint_projection);
	);

	(* get possible actions originating from current state *)
	let list_of_possible_actions = compute_possible_actions model original_location in

	(* Print some information *)
	if debug_mode_greater Verbose_high then (
		let actions_string = List.map (fun action_index -> model.action_names action_index) list_of_possible_actions in
		print_message Verbose_high ("Possible synchronized actions are: " ^ (string_of_list_of_string_with_sep ", " actions_string));
	);

	(* Build the list of new states indexes *)
	let new_states_indexes = ref [] in
	
	(* Build the list of new states (for variant of merging only) *)
	(* EXPERIMENTAL BRANCHING: MERGE BEFORE OR AFTER? *)
	let new_action_and_state_list = ref [] in
	

	(* Create a constraint D_i = d_i for the discrete variables *)
	let discrete_values = List.map (fun discrete_index -> discrete_index, (Automaton.get_discrete_value original_location discrete_index)) model.discrete in
	(* Convert to a constraint *)
	let discrete_constr = instantiate_discrete discrete_values in

	(* FOR ALL ACTION DO: *)
	List.iter (fun action_index ->

		print_message Verbose_medium ("\nComputing destination states for action '" ^ (model.action_names action_index) ^ "'");
		(* Get the automata declaring the action *)
		let automata_for_this_action = model.automata_per_action action_index in
		let nb_automata_for_this_action = List.length automata_for_this_action in
	
		(*------------------------------------------------*)
		(* Compute the reachable states on the fly: i.e., all the possible transitions for all the automata belonging to 'automata' *)
		(*------------------------------------------------*)
		
		(* Compute conjunction with current constraint *)
		(*** To optimize: it seems intersection_assign could be used instead ***)
		let orig_plus_discrete = LinearConstraint.pxd_intersection [LinearConstraint.pxd_of_px_constraint (orig_constraint ()); discrete_constr] in
		
		(* In alternative semantics, apply time elapsing NOW, so as to factor this operation once for all *)
		(*** WARNING: time elapsing is AGAIN performed in compute_new_constraint, which is a loss of efficiency ***)
		if options#no_time_elapsing then(
			print_message Verbose_total ("\nAlternative time elapsing: Applying time elapsing NOW");
			apply_time_elapsing original_location orig_plus_discrete;
		);
		
		(* Give a new index to those automata *)
		let real_indexes = Array.make nb_automata_for_this_action 0 in
		(* Keep an array of possible transition indices for each automaton *)
		let possible_transitions = Array.make nb_automata_for_this_action [] in
		(* Use an array of transition indices for the search (start with 0), indicating the current index within the possible transitions for each automaton *)
		let current_indexes = Array.make nb_automata_for_this_action 0 in
		(* Keep the maximum index of possible transitions for each automaton *)
		let max_indexes = Array.make nb_automata_for_this_action 0 in
		(* Array for the currently selected transition indices *)
		let current_transitions = Array.make nb_automata_for_this_action 0 in
		
		(* compute the possible combinations of transitions *)
		let legal_transitions_exist = compute_transitions model original_location orig_plus_discrete action_index automata_for_this_action real_indexes max_indexes possible_transitions in 
	
		(* Debug: compute the number of combinations *)
		if debug_mode_greater Verbose_medium || options#statistics then(
			let new_nb_combinations = Array.fold_left (fun sum max -> sum * (max + 1)) 1 max_indexes in
			print_message Verbose_medium ("" ^ (string_of_int new_nb_combinations) ^ " combination" ^ (s_of_int new_nb_combinations) ^ " will be considered for this state and this action\n");
			(* Update for statistics *)
			nb_combinations := !nb_combinations + new_nb_combinations;
		);
	
		(* Loop on all the transition combinations *)
		let more_combinations = ref legal_transitions_exist in
		let debug_i = ref 0 in
		while !more_combinations do
			debug_i := !debug_i +1;
			(* Print some information *)
			if debug_mode_greater Verbose_total then (
				let local_indexes = string_of_array_of_string_with_sep "\n\t" (
				Array.mapi (fun local_index real_index ->
					(string_of_int local_index) ^ " -> " ^ (string_of_int real_index) ^ " : " ^ (string_of_int current_indexes.(local_index)) ^ "; ";
				) real_indexes) in
				print_message Verbose_total ("\n\n\n--- Consider the combination " ^ (string_of_int !debug_i) ^ " \n\t" ^ local_indexes);
			);
	
			(* build the current combination of transitions *)
			for i=0 to Array.length current_transitions -1 do
				current_transitions.(i) <- List.nth (possible_transitions.(i)) (current_indexes.(i))
			done; 
	
			(* Compute the new location for the current combination of transitions *)
			let location, guards, clock_updates = compute_new_location model real_indexes current_transitions action_index original_location in
			
			(* Compute the new constraint for the current transition *)
			let new_constraint = compute_new_constraint model orig_constraint discrete_constr original_location location guards clock_updates in
			
			begin
			(* Check the satisfiability *)
			match new_constraint with
				| None -> 
					(* Statistics *)
					nb_unsatisfiable := !nb_unsatisfiable + 1;
					print_message Verbose_high ("\nThis constraint is not satisfiable ('None').");
				| Some (final_constraint : LinearConstraint.px_linear_constraint) -> (
					if not (LinearConstraint.px_is_satisfiable final_constraint) then(
						(* Statistics *)
						nb_unsatisfiable := !nb_unsatisfiable + 1;
						print_message Verbose_high ("\nThis constraint is not satisfiable ('Some unsatisfiable').");
					) else (
					
					(* Increment a counter: this state IS generated (although maybe it will be discarded because equal / merged / algorithmic discarding ...) *)
					StateSpace.increment_nb_gen_states reachability_graph;
			
					(**************************************************************)
					(* EXPERIMENTAL BRANCHING: MERGE BEFORE OR AFTER? *)
					(**************************************************************)
					(* EXPERIMENTAL BRANCHING: CASE MERGE AFTER (this new version may be better?) *)
					if options#imitator_mode <> State_space_exploration && options#merge_before then(
					
						(* Only add to the local list of new states *)
						new_action_and_state_list := ([action_index], location, final_constraint) :: !new_action_and_state_list;
					
					(* EXPERIMENTAL BRANCHING: END CASE MERGE AFTER *)
					)else(
					
					(* EXPERIMENTAL BRANCHING: CASE MERGE BEFORE (classical version) *)
						add_a_new_state model reachability_graph orig_state_index new_states_indexes action_index location final_constraint;
						
					); (* EXPERIMENTAL BRANCHING: END CASE MERGE BEFORE (classical version) *)
					
				); (* end if satisfiable *)
			); (* end if Some constraint *)
			end; (* end match constraint *)
		
			(* Update the next combination *)
			more_combinations := next_combination current_indexes max_indexes;
			
		done; (* while more new states *)
	) list_of_possible_actions;
	
	(* If new_states is empty : the current state is a last state *)
	if options#union && !new_states_indexes = [] then (
		print_message Verbose_low ("\nMode union: adding a state without successor to SLast.");
		(* Adding the state *)
		slast := orig_state_index :: !slast;
	);
	
	
	(**************************************************************)
	(* EXPERIMENTAL BRANCHING: MERGE BEFORE OR AFTER? *)
	(**************************************************************)
	(* EXPERIMENTAL BRANCHING: CASE MERGE AFTER (this new version may be better?) *)
	if options#imitator_mode <> State_space_exploration && options#merge_before then(
	
		(* Merge *)
		new_action_and_state_list := merge model new_action_and_state_list;
		
		(* Add the remaining states *)
		List.iter (fun (action_index_list, location, final_constraint) ->
			(* Iterate on all actions *)
			(*** WARNING: not very beautiful !! ***)
			List.iter (fun action_index ->
				add_a_new_state model reachability_graph orig_state_index new_states_indexes action_index location final_constraint;
			) action_index_list;
		) !new_action_and_state_list
		
	); (* EXPERIMENTAL BRANCHING: END CASE MERGE AFTER *)
	
	
	(* Return the list of (really) new states *)
	(*** NOTE: List.rev really useful??!!!! ***)
	List.rev (!new_states_indexes)




(************************************************************)
(* Full reachability functions *)
(************************************************************)

(*---------------------------------------------------*)
(* Set the PaTATOR termination function *)
(*---------------------------------------------------*)
let set_patator_termination_function f =
	patator_termination_function := Some f


(*---------------------------------------------------*)
(* Check whether the limit of an exploration has been reached, according to the analysis options *)
(*---------------------------------------------------*)
let check_limit depth nb_states time =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	
	(* Disjunction between the different options *)
	
	(* Depth limit *)
	begin
	match options#post_limit with
		| None -> false
		| Some limit -> depth > limit
	end
	||
	(* States limit *)
	begin
	match options#states_limit with
		| None -> false
		| Some limit -> nb_states > limit
	end
	||
	(* Time limit *)
	begin
	match options#time_limit with
		| None -> false
		| Some limit -> time > (float_of_int limit)
	end
	||
	(* External function for PaTATOR (would raise an exception in case of stop needed) *)
	begin
	match !patator_termination_function with
		| None -> false
		| Some f -> f (); false (** Returns false but in fact will directly raise an exception in case of required termination **)
	end




(*---------------------------------------------------*)
(* Print warning(s) if the limit of an exploration has been reached, according to the analysis options *)
(*---------------------------------------------------*)
let print_warnings_limit depth nb_states time nb_states_to_visit =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	begin
	match options#post_limit with
		| None -> ()
		| Some limit -> if depth > limit then print_warning (
			"The limit depth (" ^ (string_of_int limit) ^ ") has been reached. The exploration now stops, although there were still " ^ (string_of_int nb_states_to_visit) ^ " state" ^ (s_of_int nb_states_to_visit) ^ " to explore."
		);
	end;
	begin
	match options#states_limit with
		| None -> ()
		| Some limit -> if nb_states > limit then print_warning (
			"The limit number of states (" ^ (string_of_int limit) ^ ") has been reached. The exploration now stops, although there were still " ^ (string_of_int nb_states_to_visit) ^ " state" ^ (s_of_int nb_states_to_visit) ^ " to explore."
		);
	end;
	begin
	match options#time_limit with
		| None -> ()
		| Some limit -> if time > (float_of_int limit) then print_warning (
			"The time limit (" ^ (string_of_int limit) ^ " second" ^ (s_of_int limit) ^ ") has been reached. The exploration now stops, although there were still " ^ (string_of_int nb_states_to_visit) ^ " state" ^ (s_of_int nb_states_to_visit) ^ " to explore at this iteration."
		);
	end



(*---------------------------------------------------*)
(* EXPERIMENTAL BRANCH AND BOUND FUNCTION *)
(*---------------------------------------------------*)
let branch_and_bound model init_state = 
	(* Retrieve the input options *)
	let options = Input.get_options () in
	(* Retrieve the pi0 *)
	let pi0 = Input.get_pi0 () in
	
	(* Get some variables *)
	let nb_actions = model.nb_actions in
	let nb_variables = model.nb_variables in
	let nb_automata = model.nb_automata in

	(* copy init state, as it might be destroyed later *)
	let init_loc, init_constr = init_state in
	let init_state = (init_loc, LinearConstraint.px_copy init_constr) in

	(*Initialization of k_result*)
	k_result := LinearConstraint.px_hide_nonparameters_and_collapse init_constr;

	print_message Verbose_standard ("START BRANCH AND BOUND");
	
	(* Instantiate all costs associated to locations, for once *)
	print_message Verbose_standard ("Instantiate costs");
	instantiate_costs model pi0;
	
	(* Print some information *)
	if debug_mode_greater Verbose_total then (
		print_message Verbose_standard ("Print costs");
		(* For each automaton *)
		for automaton_index = 0 to model.nb_automata - 1 do
			(* Retrieve the number of locations for this automaton *)
			let nb_locations = List.length (model.locations_per_automaton automaton_index) in
			(* For each location *)
			for location_index = 0 to nb_locations - 1 do
				(* Retrieve the cost *)
				let cost = (!instantiated_costs).(automaton_index).(location_index) in
				(* Print it *)
					print_message Verbose_total ((model.automata_names automaton_index) ^ " --> " ^ (model.location_names automaton_index location_index) ^ " : " ^ (NumConst.string_of_numconst cost));
			done;
		done;
		
	);
	
	(* Time counter *)
	let start_time = Unix.gettimeofday() in
	
	(* Reset counter for random selections *)
	nb_random_selections := 0;

	(* Debut prints *)
	print_message Verbose_low ("Starting reachability analysis from state:");
	print_message Verbose_low (string_of_state model init_state);
	(* Guess the number of reachable states *)
	let guessed_nb_states = 10 * (nb_actions + nb_automata + nb_variables) in 
	let guessed_nb_transitions = guessed_nb_states * nb_actions in 
	print_message Verbose_total ("I guess I will reach about " ^ (string_of_int guessed_nb_states) ^ " states with " ^ (string_of_int guessed_nb_transitions) ^ " transitions.");

	(* Create the reachability graph *)
	let reachability_graph = StateSpace.make guessed_nb_transitions in

	(* Add the initial state to the reachable states *)
	let init_state_index, _ = StateSpace.add_state model reachability_graph init_state in
	
	(* Increment the number of computed states *)
	StateSpace.increment_nb_gen_states reachability_graph;
	
	(* Create the tree for exploration *)
	let rtree = ReachabilityTree.create guessed_nb_states init_state_index in
	
	if debug_mode_greater Verbose_total then(
		print_message Verbose_total ("\nReachability tree initialized:\n" ^ (ReachabilityTree.string_of_rtree string_of_int rtree));
	);

	(*------------------------------------------------------------*)
	(* Start the exploration *)
	(*------------------------------------------------------------*)
	(* Current state to be analyzed *)
	let current_state_index = ref init_state_index in
	(* Limit reached? (Due to bounded analysis options) *)
	let limit_reached = ref false in
	(* Number of states to visit (in the tree) *)
	let nb_states_to_visit = ref (ReachabilityTree.nb_states_to_visit rtree) in

	
	(* Check if the list of new states is empty *)
	while not (!limit_reached  || !nb_states_to_visit = 0) do
		if debug_mode_greater Verbose_standard then (
			print_message Verbose_low ("\n");
			print_message Verbose_standard ("Computing successors of state " ^ (string_of_int !current_state_index));
			if debug_mode_greater Verbose_medium then (
				print_message Verbose_medium (string_of_state model (StateSpace.get_state reachability_graph !current_state_index ));
			);
		);
		
		(* Now compute successors *)
		let new_states = post_from_one_state model reachability_graph !current_state_index in
		
		
		(* Merge states ! *)
		let new_states =
		if options#merge || options#merge_before then (
			let eaten_states = StateSpace.merge reachability_graph new_states in
			
			
			(* TODO: remove states from rtree !! (and big problem if some previous states have been removed due to merging.....) *)
			
			
			list_diff new_states eaten_states;
		) else (
			new_states
		) in
		
		(* Update the reachability tree *)
		ReachabilityTree.add_children !current_state_index new_states rtree;
		ReachabilityTree.set_visited !current_state_index rtree;
		
		(* Print some information *)
		if debug_mode_greater Verbose_medium then (
			let beginning_message = if new_states = [] then "\nFound no new state" else ("\nFound " ^ (string_of_int (List.length new_states)) ^ " new state" ^ (s_of_int (List.length new_states)) ^ "") in
			print_message Verbose_medium (beginning_message ^ ".\n");
		);
		
		if debug_mode_greater Verbose_total then(
			print_message Verbose_total ("\nReachability tree now as follows:\n" ^ (ReachabilityTree.string_of_rtree string_of_int rtree));
		);
		(* If acyclic option: empty the list of already reached states for comparison with former states *)
		
		(** TODO : recheck this !! (I guess very dangerous, because would empty the whole list of states to compare!) *)
		
		if options#acyclic then(
			print_message Verbose_low ("\nMode acyclic: empty the list of states to be compared.");
			empty_states_for_comparison reachability_graph;
		);
		
		
		(* Clean up a little *)
		(** LOOKS LIKE COMPLETELY USELESS !!! it even increases memory x-( *)
		Gc.major ();
		
		(* Recompute the number of states to visit *)
		nb_states_to_visit := ReachabilityTree.nb_states_to_visit rtree;
		
		(* Update the new state to visit *)
		if !nb_states_to_visit > 0 then(
			print_message Verbose_high ("Recall that current element in the reachability tree is " ^ (string_of_int !current_state_index));
			print_message Verbose_medium ("Finding next element in the reachability tree...");
			try(
				current_state_index := ReachabilityTree.get_next_element rtree;
			) with Not_found -> (
				print_warning ("\nReachability tree now as follows:\n" ^ (ReachabilityTree.string_of_rtree string_of_int rtree));
				raise (InternalError "The reachability tree has an unexpected structure, when looking for the next state in branch and bound.");
			);
		);
		
		(* Check if the limit has been reached according to the options *)
		limit_reached := check_limit 0 (** TODO: check depth *) (StateSpace.nb_states reachability_graph) (time_from start_time);
	done;
	
	(* Flat to detect premature stop in case of limit reached *)
	let premature_stop = ref false in
	
	(* There were still states to explore *)
	if !limit_reached && !nb_states_to_visit > 0 then(
		(* Update flag *)
		premature_stop := true;
		(* Print some information *)
		print_warnings_limit 0 (** TODO: check depth *) (StateSpace.nb_states reachability_graph) (time_from start_time) !nb_states_to_visit;
	);
		
	print_message Verbose_standard (
		let nb_states = StateSpace.nb_states reachability_graph in
		let nb_transitions = StateSpace.nb_transitions reachability_graph in
		"\nFixpoint reached: "
		^ (string_of_int nb_states) ^ " reachable state" ^ (s_of_int nb_states)
		^ " with "
		^ (string_of_int nb_transitions) ^ " transition" ^ (s_of_int nb_transitions) ^ ".");

	(* Return the graph, the iteration and the counter *)
	reachability_graph , 0 (** TODO: check depth *) , (time_from start_time) , !premature_stop, !nb_random_selections



(*---------------------------------------------------*)
(* Compute the reachability graph from a given state *)
(*---------------------------------------------------*)
let post_star model init_state = 
	(* Retrieve the input options *)
	let options = Input.get_options () in
	
	(* Get some variables *)
	let nb_actions = model.nb_actions in
	let nb_variables = model.nb_variables in
	let nb_automata = model.nb_automata in

	(* copy init state, as it might be destroyed later *)
	let init_loc, init_constr = init_state in
	let init_state = (init_loc, LinearConstraint.px_copy init_constr) in

	(* Initialization of global variables *)
	print_message Verbose_low ("Initializing global variables...");
	k_result := LinearConstraint.px_hide_nonparameters_and_collapse init_constr;
	p_constraints := [];

	(* Print some information *)
	if debug_mode_greater Verbose_low then(
		print_message Verbose_low ("Initialized k_result to ");
		print_message Verbose_low (LinearConstraint.string_of_p_linear_constraint model.variable_names !k_result);
		print_message Verbose_low ("");
	);

	(*Initialization of slast : used in union mode only*)
	slast := [];
	(* Time counter *)
	let start_time = Unix.gettimeofday() in
	(* Set the counter of selections to 0 *)
	nb_random_selections := 0;

	(* Debut prints *)
	print_message Verbose_low ("Starting reachability analysis from state:");
	print_message Verbose_low (string_of_state model init_state);
	(* Guess the number of reachable states *)
	let guessed_nb_states = 10 * (nb_actions + nb_automata + nb_variables) in 
	let guessed_nb_transitions = guessed_nb_states * nb_actions in 
	print_message Verbose_total ("I guess I will reach about " ^ (string_of_int guessed_nb_states) ^ " states with " ^ (string_of_int guessed_nb_transitions) ^ " transitions.");
	(* Create the reachability graph *)
	let reachability_graph = StateSpace.make guessed_nb_transitions in
	
	(* Add the initial state to the reachable states *)
	let init_state_index, _ = StateSpace.add_state model reachability_graph init_state in
	
	(* Increment the number of computed states *)
	StateSpace.increment_nb_gen_states reachability_graph;
	
	
	(*------------------------------------------------------------*)
	(* Perform the post^* *)
	(*------------------------------------------------------------*)
	let newly_found_new_states = ref [init_state_index] in
	let nb_iterations = ref 1 in
	let limit_reached = ref false in

	(* Check if the list of new states is empty *)
	while not (!limit_reached  || !newly_found_new_states = []) do
		(* Print some information *)
		if debug_mode_greater Verbose_standard then (
			print_message Verbose_low ("\n");
			print_message Verbose_standard ("Computing post^" ^ (string_of_int (!nb_iterations)) ^ " from "  ^ (string_of_int (List.length !newly_found_new_states)) ^ " state" ^ (s_of_int (List.length !newly_found_new_states)) ^ ".");
		);
		
		(* Count the states for debug purpose: *)
		let num_state = ref 0 in
		(* Length of 'newly_found_new_states' for debug purpose *)
		let nb_states = List.length !newly_found_new_states in

		let new_newly_found_new_states =
		(* For each newly found state: *)
		List.fold_left (fun new_newly_found_new_states orig_state_index ->
			(* Count the states for debug purpose: *)
			num_state := !num_state + 1;
			(* Perform the post *)
			let new_states = post_from_one_state model reachability_graph orig_state_index in
			(* Print some information *)
			if debug_mode_greater Verbose_medium then (
				let beginning_message = if new_states = [] then "Found no new state" else ("Found " ^ (string_of_int (List.length new_states)) ^ " new state" ^ (s_of_int (List.length new_states)) ^ "") in
				print_message Verbose_medium (beginning_message ^ " for the post of state " ^ (string_of_int !num_state) ^ " / " ^ (string_of_int nb_states) ^ " in post^" ^ (string_of_int (!nb_iterations)) ^ ".\n");
			);
			
			(* Return the concatenation of the new states *)
			(**** OPTIMIZED: do not care about order (else shoud consider 'list_append new_newly_found_new_states (List.rev new_states)') *)
			List.rev_append new_newly_found_new_states new_states
		) [] !newly_found_new_states in


		(* Merge states! *)
		let new_states_after_merging = ref new_newly_found_new_states in
		(*** HACK here! For #merge_before, we should ONLY merge here; but, in order not to change the full structure of the post computation, we first merge locally before the pi0-compatibility test, then again here *)
		if options#merge || options#merge_before then (
(* 			new_states_after_merging := try_to_merge_states reachability_graph !new_states_after_merging; *)
			(* New version *)
			let eaten_states = StateSpace.merge reachability_graph !new_states_after_merging in
			new_states_after_merging := list_diff !new_states_after_merging eaten_states;
		);


		(* Update the newly_found_new_states *)
		newly_found_new_states := !new_states_after_merging;
		(* Print some information *)
		if debug_mode_greater Verbose_medium then (
			let beginning_message = if !newly_found_new_states = [] then "\nFound no new state" else ("\nFound " ^ (string_of_int (List.length !newly_found_new_states)) ^ " new state" ^ (s_of_int (List.length !newly_found_new_states)) ^ "") in
			print_message Verbose_medium (beginning_message ^ " for post^" ^ (string_of_int (!nb_iterations)) ^ ".\n");
		);
		
		(* If acyclic option: empty the list of already reached states for comparison with former states *)
		if options#acyclic then(
			print_message Verbose_low ("\nMode acyclic: empty the list of states to be compared.");
			empty_states_for_comparison reachability_graph;
		);
		
		(* If check-point option: check if the constraint is equal to pi0 *)
		(*** TO OPTIMIZE !!! (at least compute pi0_constraint once for all) ***)
		(*** WARNING!! ONLY works for the classical inverse method (not for variants) ***)
		(*** TODO: also allow for BC ***)
		if options#imitator_mode = Inverse_method  && options#check_point then(
			print_message Verbose_low ("\nMode check-point: checking whether the resulting constraint is restricted to pi0...");
			(* Get all constraints *)
			let all_p_constraints = StateSpace.all_p_constraints model reachability_graph in
			(* Computing the constraint intersection *)
			let current_intersection = LinearConstraint.p_intersection all_p_constraints in
			(* Get pi0 *)
			let pi0 = Input.get_pi0() in
			(* Converting pi0 to a list *)
			let pi0_list = List.map (fun p -> (p, pi0#get_value p)) model.parameters in
			(* Converting pi0 to a constraint *)
			let pi0_constraint = LinearConstraint.p_constraint_of_point pi0_list in
			(* Print *)
			if debug_mode_greater Verbose_medium then(
				print_message Verbose_medium ("\nPi0: " ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names pi0_constraint));
			);
			(* Checking whether the constraint is *included* within pi0 *)
			if LinearConstraint.p_is_leq current_intersection pi0_constraint then(
				(* Print message *)
				print_message Verbose_standard ("\nCurrent accumulated constraint is now restricted to pi0. Analysis can safely terminate.");
				(* Stop *)
				limit_reached := true;
			);
		);
		
		(* Clean up a little *)
		(*** NOTE: LOOKS LIKE COMPLETELY USELESS !!! it even increases memory x-( ***)
		Gc.major ();
		
		(* Iterate *)
		nb_iterations := !nb_iterations + 1;
		
		(* Check if the limit has been reached *)
		limit_reached := !limit_reached || (check_limit !nb_iterations (StateSpace.nb_states reachability_graph) (time_from start_time));
	done;
	
	(* Flat to detect premature stop in case of limit reached *)
	let premature_stop = ref false in
	
	(* There were still states to explore *)
	if !limit_reached && !newly_found_new_states != [] then(
		(* Update flag *)
		premature_stop := true;
		(* Print some information *)
		print_warnings_limit !nb_iterations (StateSpace.nb_states reachability_graph) (time_from start_time) (List.length !newly_found_new_states);
	);

	print_message Verbose_standard (
		let nb_states = StateSpace.nb_states reachability_graph in
		let nb_transitions = StateSpace.nb_transitions reachability_graph in
		"\nFixpoint reached after "
		^ (string_of_int !nb_iterations) ^ " iteration" ^ (s_of_int !nb_iterations) ^ ""
(* 		^ " in " ^ (string_of_seconds (time_from !counter)) *)
		^ ": "
		^ (string_of_int nb_states) ^ " state" ^ (s_of_int nb_states)
		^ " with "
		^ (string_of_int nb_transitions) ^ " transition" ^ (s_of_int nb_transitions) ^ " explored.");

	(* Return the graph, the iteration and the counter *)
	reachability_graph , !nb_iterations , (time_from start_time) , !premature_stop, !nb_random_selections




(*------------------------------------------------------------*)
(* Performances *)
(*------------------------------------------------------------*)
let print_statistics total_time reachability_graph =
	(* Retrieve the input options *)
	let options = Input.get_options () in

	(** TODO: better have an independent module (or class) 'statistics' ***)
	
	(* Generic function for float/int conversion *)
	let string_of_average average = 
		if average < 10.0 then string_of_float average
		else string_of_int (int_of_float average)
	in
	
	(* Speed (number of states in the graph) *)
	let nb_states = StateSpace.nb_states reachability_graph in
	let average = (float_of_int nb_states) /. total_time in
	print_message Verbose_standard ("\nStates per second in the graph: " ^ (string_of_average average) ^ " (" ^ (string_of_int nb_states) ^ "/" ^ (string_of_seconds total_time) ^ ")");
	
	(* Speed (number of states computed, even if not kept) *)
	let nb_gen_states = StateSpace.get_nb_gen_states reachability_graph in
	let average = (float_of_int nb_gen_states) /. total_time in
	print_message Verbose_standard ("States computed per second: " ^ (string_of_average average) ^ " (" ^ (string_of_int nb_gen_states) ^ "/" ^ (string_of_seconds total_time) ^ ")");

	
	if options#statistics then (
		(* PPL *)
		print_message Verbose_standard "--------------------";
		print_message Verbose_standard "Statistics on PPL";
		print_message Verbose_standard ("--------------------" ^ (LinearConstraint.get_statistics total_time));
		
		(* Graph *)
		print_message Verbose_standard "--------------------";
		print_message Verbose_standard "Statistics on Graph";
		print_message Verbose_standard "--------------------";
		print_message Verbose_standard (StateSpace.get_statistics ());
		print_message Verbose_standard (StateSpace.get_statistics_states reachability_graph);
		
		print_message Verbose_standard "--------------------";
		print_message Verbose_standard "Statistics on Cache";
		print_message Verbose_standard "--------------------";
		print_stats ();
		
		print_message Verbose_standard "--------------------";
		print_message Verbose_standard "Statistics on Reachability";
		print_message Verbose_standard "--------------------";
		print_message Verbose_standard ("Number of early skips because of unsatisfiable guards: " ^ (string_of_int !nb_early_unsatisfiable));
		print_message Verbose_standard ("Number of early skips because no actions: " ^ (string_of_int !nb_early_skip));
		print_message Verbose_standard ("Number of unsatisfiable constraints: " ^ (string_of_int !nb_unsatisfiable));
		print_message Verbose_standard ("Number of unsat1: " ^ (string_of_int !nb_unsat1));
		print_message Verbose_standard ("Number of unsat2: " ^ (string_of_int !nb_unsat2));
		print_message Verbose_standard ("Number of combinations considered: " ^ (string_of_int !nb_combinations));
		
		print_message Verbose_standard "--------------------";
		print_message Verbose_standard "Statistics on memory";
		print_message Verbose_standard "--------------------";
		print_memory_used Verbose_standard;
		Gc.print_stat stdout;
(*		print_message Verbose_standard "--------------------";
		Gc.major();
		Gc.print_stat stdout;
		print_message Verbose_standard "--------------------";
		Gc.full_major();
		Gc.print_stat stdout;*)
	)





(************************************************************)
(************************************************************)
(** Main functions *)
(************************************************************)
(************************************************************)


(************************************************************)
(* State space exploration analysis *)
(************************************************************)
let full_state_space_exploration model =
	(* Retrieve the input options *)
	let options = Input.get_options () in

	(* Compute the initial state *)
	let init_state = get_initial_state_or_abort model in

	(* Call to generic function *)
	let reachability_graph , _ , total_time ,  _ , _ = post_star model init_state in
	
	print_message Verbose_standard (
		"\nState space exploration completed " ^ (after_seconds ()) ^ "."
	);
		
	print_message Verbose_low (
		"Computation time: "
		^ (string_of_seconds total_time) ^ "."
	);

	(* Print statistics *)
	print_statistics total_time reachability_graph;
	
	(* Generate graphics *)
	let radical = options#files_prefix in
	Graphics.generate_graph model reachability_graph radical;
	
	(* The end*)
	()



(************************************************************)
(* EF-synthesis *)
(************************************************************)
let ef_synthesis model =
	(* Retrieve the input options *)
	let options = Input.get_options () in

	(* Compute the initial state *)
	let init_state = get_initial_state_or_abort model in

	(* Call to generic function *)
	let reachability_graph , _ , total_time , _ ,  _ = post_star model init_state in
	
	print_message Verbose_standard (
		"\nState space exploration completed " ^ (after_seconds ()) ^ "."
	);
	
	(* Convert result to string *)
	let result_str = string_of_list_of_string_with_sep "\n OR \n" (List.map (LinearConstraint.string_of_p_linear_constraint model.variable_names) !p_constraints) in

	(* Print the result *)
	print_message Verbose_standard ("\nFinal constraint such that the property is *violated* (" ^ (string_of_int (List.length !p_constraints)) ^ " constraints): ");
	print_message Verbose_standard (result_str);
	
	print_message Verbose_low (
		"Computation time: "
		^ (string_of_seconds total_time) ^ "."
	);

	(* Print on terminal *)
	print_message Verbose_standard (
		"\nEF-synthesis successfully finished " ^ (after_seconds ()) ^ "."
	);

	(* Write to file if requested *)
	if options#output_result then(
		write_result_to_file result_str;
	);
	
	(* Print statistics *)
	print_statistics total_time reachability_graph;
	
	(* Generate graphics *)
	let radical = options#files_prefix in
	Graphics.generate_graph model reachability_graph radical;
	
	(* Render zones in a graphical form *)
	let zones = [Union_of_constraints (!p_constraints, Bad)] in
	if options#cart then (
		Graphics.cartography model (Input.get_v0()) zones (options#files_prefix ^ "_cart_ef")
	) else (
			print_message Verbose_high "Graphical cartography not asked: graph not generated.";
	)

	

(*------------------------------------------------------------*)
(* Auxiliary function *)
(*------------------------------------------------------------*)
(* Try to remove useless "bad" constraints *)
let process_result_IMcomplete original_k_good original_k_bad =
	(* Print some information *)
	print_message Verbose_standard ("Trying to simplify some of the " ^ (string_of_int (List.length original_k_bad)) ^ " bad constraint" ^ (s_of_int (List.length original_k_bad)) ^ "...");

	(*	let k_good = match original_k_good with
	| [k_good] -> k_good
	| _ ->raise(InternalError ("When simplifying the constraints, one must have only 1 good constraint"))
	in*)
	(* Eliminate useless constraints *)
	let nb_useless = ref 0 in
	let k_bad = List.filter (fun k ->
		(* Only keep if intersection with k_good is not empty *)
		if LinearConstraint.p_is_satisfiable (
			LinearConstraint.p_intersection [original_k_good; k]
		)
		then (
			true
			)
		else (
			nb_useless := !nb_useless + 1;
			false
		)
	) original_k_bad
	
	in
	
	(* Print some information *)
	print_message Verbose_standard ((string_of_int !nb_useless) ^ " bad constraint" ^ (s_of_int !nb_useless) ^ " have been deleted.");
	
	(*** TODO: merge!! ***)
	
	(* Return the remaining constraints *)
	original_k_good, k_bad


(************************************************************)
(* Main inverse method functions *)
(************************************************************)


(*------------------------------------------------------------*)
(* Encapsulation function for IM, called by the real inverse method function, and by the cartography algorithms *)
(*------------------------------------------------------------*)
let inverse_method_gen model init_state =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	
	
	
	
	(*(* TEST FOR BRANCH AND BOUND *)
	if options#branch_and_bound then(
		
		print_message Verbose_standard "1) BRANCH AND BOUND";
		let _,_,_,_ = branch_and_bound model init_state in
		let constraint1 = !k_result in

		(* Remove branch and bound *)
		options#branch_and_bound_unset;
		print_message Verbose_standard "2) CLASSICAL ALGORITHM";
		let _,_,_,_ = post_star model init_state in
		let constraint2 = !k_result in
	
		print_message Verbose_standard "3) COMPARE RESULT";
		print_message Verbose_standard "-> Branch and bound:";
		print_message Verbose_standard (LinearConstraint.string_of_linear_constraint model.variable_names constraint1);
		print_message Verbose_standard "-> Classical IM:";
		print_message Verbose_standard (LinearConstraint.string_of_linear_constraint model.variable_names constraint2);
		
		if LinearConstraint.is_equal constraint1 constraint2 then(
			print_message Verbose_standard "\n\nCONSTRAINTS EQUAL :-)";
			terminate_model();
		) else (
			print_message Verbose_standard "\n\nARGH! CONSTRAINTS DIFFERENT :-(\n\n";
			raise (InternalError "byebye");
		);
	);*)
	
	
	
	(* For now, the tile is good by default *)
	begin
	match model.correctness_condition with
		| None -> ()
		| Some (Unreachable _) -> tile_nature := Good
		| _ -> raise (InternalError("IMITATOR currently ony implements the non-reachability-like properties."))
	end;
	
	(* Choose the correct algorithm *)
	let algo = if options#branch_and_bound then branch_and_bound else post_star in
	(* Call to generic functions *)
	let reachability_graph, nb_iterations, total_time, premature_stop, nb_random_selections = algo model init_state in
	
	(*------------------------------------------------------------*)
	(* Print information *)
	(*------------------------------------------------------------*)
	if not options#no_random then (
		if (nb_random_selections > 0) then (
			print_message Verbose_standard "Analysis may have been non-deterministic:";
			print_message Verbose_standard ((string_of_int nb_random_selections) ^ " random selection" ^ (s_of_int nb_random_selections) ^ " have been performed.");
		) else (
			print_message Verbose_standard "Analysis has been fully deterministic.";
		)
	);
	

	(*------------------------------------------------------------*)
	(* Computation of the returned constraint *)
	(*------------------------------------------------------------*)
	let returned_constraint =

	(* Case union : return the constraint on the parameters associated to slast*)
	if options#union then (
		print_message Verbose_total ("\nMode: union.");
		let list_of_constraints =
		List.map (fun state_index ->
			print_message Verbose_medium ("\nOne state found.");
			(* Get the constraint on clocks and parameters *)
			let (_, current_constraint) =
				StateSpace.get_state reachability_graph state_index
			(* Eliminate clocks *)
			in LinearConstraint.px_hide_nonparameters_and_collapse current_constraint
		) !slast
		in Union_of_constraints (list_of_constraints, !tile_nature)
	)
	
	(* Case IMorig : return only the current constraint, viz., the constraint of the first state *)
	(*** NOTE: why not returning just K_result? ***)
	else if options#pi_compatible then (
		let (_ , px_constraint) = get_state reachability_graph 0 in
			print_message Verbose_total ("\nMode: IMorig.");
			Convex_constraint (LinearConstraint.px_hide_nonparameters_and_collapse px_constraint , !tile_nature) 
	)

	(* Case EFIM: return k_result OR list of bad *)
	else if options#efim then (
		match !tile_nature with
		(* Bad tile: return the union of the bad constraints *)
		| Bad ->
			Union_of_constraints (!p_constraints, !tile_nature)
		(* No bad location means good! *)
		| Good ->
			Convex_constraint (!k_result , !tile_nature)
		(* Unkown is impossible *)
		| Unknown -> raise (InternalError "Impossible situation in EFIM: a returned_constraint has an unkown tile nature althoug this is not enabled here.")
	)

	(* Case IMcomplete *)
	else if options#completeIM then (
			print_message Verbose_total ("\nMode: IMcomplete.");
			let k_good, k_bad = process_result_IMcomplete !k_result !k_bad in
			NNCConstraint ([k_good] , k_bad, !tile_nature)
	)

	(* Case IM standard : return the intersection *)
	else (
		print_message Verbose_total ("\nMode: IM standard.");
		Convex_constraint (!k_result, !tile_nature)
	)
			(*		(* Case IM : intersection *)
		else (
			(** HERE PROBLEM IF ONE WANTS TO COMPUTE THE states FILE AFTER (destruction of the states) **)
			Convex_constraint (StateSpace.compute_k0_destructive model reachability_graph)
		)*)
	in

	(*------------------------------------------------------------*)
	(* Return result *)
	(*------------------------------------------------------------*)
	{
	result 				= returned_constraint;
	tile_nature			= !tile_nature;
	premature_stop		= premature_stop;
	deterministic		= (nb_random_selections > 0);
	nb_states			= StateSpace.nb_states reachability_graph;
	nb_transitions		= StateSpace.nb_transitions reachability_graph;
	nb_iterations		= nb_iterations;
	total_time			= total_time
	}
	,
	reachability_graph
	
	

(*** WARNING!!! Why a dedicated function here, whereas for BC+EFIM this function is not (?) called? ***)
(*------------------------------------------------------------*)
let efim model =
(*------------------------------------------------------------*)
	(* Retrieve the input options *)
	let options = Input.get_options () in

	(* Compute the initial state *)
	let init_state = get_initial_state_or_abort model in

	(* Call the inverse method *)
	let (*returned_constraint, reachability_graph, tile_nature, deterministic, nb_iterations, total_time*) im_result , reachability_graph = inverse_method_gen model init_state in
	
	(* Processing the result *)
	let constraint_str = string_of_returned_constraint model.variable_names im_result.result
(*	match tile_nature with
	(* Bad tile: return the union of the bad constraints *)
	| Bad _ ->
		string_of_returned_constraint model.variable_names (Union_of_constraints (!p_constraints, tile_nature))
	
	
	(* No bad location means good! *)
	| Good _ ->
		string_of_returned_constraint model.variable_names (Convex_constraint (!k_result , tile_nature))
	
	(* Unkown is impossible *)
	| Unknown _ -> raise (InternalError "Impossible situation in EFIM: a returned_constraint has an unkown tile nature althoug this is not enabled here.");*)
	in
	
	
	print_message Verbose_standard ("\nFinal constraint:");
	(* Print on terminal *)
	print_message Verbose_standard constraint_str;
	print_message Verbose_standard (
		"\nEFIM successfully finished " ^ (after_seconds ()) ^ "."
	);
	(* Write to file if requested *)
	if options#output_result then(
		write_result_to_file constraint_str;
	);

	(* Print memory information *)
	if debug_mode_greater Verbose_standard then
		print_memory_used Verbose_standard;
	
	print_message Verbose_low (
		"Computation time for EFIM only: "
		^ (string_of_seconds im_result.total_time) ^ "."
	);
	
	(* Generate graphics *)
	let radical = options#files_prefix in
	Graphics.generate_graph model reachability_graph radical;
	
	(* Print statistics *)
	print_statistics im_result.total_time reachability_graph;

	(* The end *)
	()


(*------------------------------------------------------------*)
let inverse_method model =
(*------------------------------------------------------------*)
	(* Retrieve the input options *)
	let options = Input.get_options () in
	
	(* Compute the initial state *)
	let init_state = get_initial_state_or_abort model in

	(* Call the inverse method *)
	let im_result, reachability_graph = inverse_method_gen model init_state in
	
(* 	(returned_constraint : AbstractModel.returned_constraint), im_result.reachability_graph, tile_nature, deterministic, nb_iterations, total_time  *)
	
	(* Here comes the result *)
	print_message Verbose_standard ("\nFinal constraint K0 "
		^ (if options#union
			then "(under disjunctive form) "
			else (
				match im_result.result with
					| Convex_constraint (p_linear_constraint , _) -> " (" ^ (string_of_int (LinearConstraint.p_nb_inequalities p_linear_constraint)) ^ " inequalities)"
					| NNCConstraint _ (*(k_good, k_bad, tile_nature)*) -> " (in \"good - bad\" form)"
					| _ -> raise (InternalError "Impossible situation in inverse_method: a returned_constraint is not under convex form although union mode is not enabled.");
			)
		)
		^ ":");

	(* Convert result to string *)
	let result_str = string_of_returned_constraint model.variable_names im_result.result in
	(* Print on terminal *)
	print_message Verbose_standard result_str;
	print_message Verbose_standard (
		"\nInverse method successfully finished " ^ (after_seconds ()) ^ "."
	);
	(* Write to file if requested *)
	if options#output_result then(
		write_result_to_file result_str;
	);
	
	(* Print memory information *)
	if debug_mode_greater Verbose_standard then
		print_memory_used Verbose_standard;
	
	print_message Verbose_low (
		"Computation time for IM only: "
		^ (string_of_seconds im_result.total_time) ^ "."
	);
	
	(* Generate graphics *)
	(*** TODO: move inside inverse_method_gen ***)
	let radical = options#files_prefix in
	Graphics.generate_graph model reachability_graph radical;
	
	(* Print statistics *)
	print_statistics im_result.total_time reachability_graph;

	(* The end *)
	()

