(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Merging states the union of which is convex [AFS13, ATVA]
 * 
 * File contributors : Étienne André
 * Created           : 2015/11/27
 * Last modified     : 2017/06/01
 *
 ************************************************************)


(**************************************************************)
(* Modules *)
(**************************************************************)
open OCamlUtilities
open ImitatorUtilities
open Exceptions
open AbstractModel
open Statistics 

(**************************************************************)
(* Counters *)
(**************************************************************)

(* Numbers of merging attempts (for states that have the same discrete location) *)
let nb_merging_attempts = create_discrete_counter_and_register "StatesMerging.merging attempts" States_counter Verbose_standard

(* Numbers of actual merges *)
let nb_merged = create_discrete_counter_and_register "StatesMerging.merges" States_counter Verbose_standard

 
(**************************************************************)
(* Merging functions *)
(**************************************************************)
(** Check if two states are mergeable: return True if the convex hull is equal to the union, False otherwise*)
(*let state_mergeable state1 state2 =
	let (loc1,constr1) = state1 in
	let (loc2,constr2) = state2 in
	if not (Location.location_equal loc1 loc2) then false else (
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
		(* Statistics *)
		nb_merging_attempts#increment;
		
		(* Check convex union of constraints *)
		LinearConstraint.px_hull_assign_if_exact constraint1 constraint2
	)

(*------------------------------------------------------------*)
(* Merge states in a list (action_index, location, constraint) *)
(* Return nothing as the list ref is already modified *)
(*------------------------------------------------------------*)
(*** WARNING: horrible imperative style (and probably not efficient and not tail-recursive at all!) programming in this function ***)
let merge action_and_state_list =
	(* Retrieve the model *)
	let model = Input.get_model() in

(*	(* Copy list *)
	(*** WARNING: constraints may not be copied (but this is consistent with the original code in the huge Reachability.ml file, until Nov. 2015) ***)
	(*** NOTE: in any case, this is fine, as the states are either merged, left untouched or dropped ***)
	let action_and_state_list = ref original_action_and_state_list in*)

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
				if verbose_mode_greater Verbose_total then (
					print_message Verbose_total ("\nConstraint of the eated before merging attempt…\n" ^ (LinearConstraint.string_of_px_linear_constraint model.variable_names eated_constraint));
					print_message Verbose_total ("\nConstraint of the eater before merging attempt…\n" ^ (LinearConstraint.string_of_px_linear_constraint model.variable_names eater_constraint));
				);

				
				if try_to_merge eater_location eater_constraint eated_location eated_constraint then(
					if verbose_mode_greater Verbose_total then (
						(* Print some information *)
						print_message Verbose_total ("\nConstraint of the eater after merging…\n" ^ (LinearConstraint.string_of_px_linear_constraint model.variable_names eater_constraint));
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
					
					(* Statistics *)
					nb_merged#increment;

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
		(*** WARNING: why pi0-compatibility test here ??? ***)
		print_message Verbose_low ("  (No state merged before pi0-compatibility test)")
	;
	
(* 	!action_and_state_list *)
	(* Return nothing as the reference was passed and hence modified *)
	()



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

