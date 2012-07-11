(************************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Ulrich Kuehne, Etienne Andre
 * Created:       2010/07/22
 * Last modified: 2012/06/18
 *
 ************************************************************)


(**************************************************************)
(* Modules *)
(**************************************************************)
open Options
open Global
open AbstractModel
open ModelPrinter
open Graph
open Gc


(**************************************************************)
(* Exception *)
(**************************************************************)

exception Unsat_exception




(**************************************************************)
(* Global variables *)
(**************************************************************)

(* Constraint for result *)
let k_result = ref ( LinearConstraint.true_constraint () )

(* List of last states (of runs) : used for the union mode *)
let slast = ref []

(* Number of random selections of pi0-incompatible inequalities in IM *)
let nb_random_selections = ref 0


(**************************************************************)
(* Costs *)
(**************************************************************)

(* Instantiated costs (no need to compute them for each location) *)
let instantiated_costs = ref (Array.make 0 (Array.make 0 NumConst.zero)) (*Array.make (Hashtbl.length index_of_automata) (Array.make 0 (NumConst.zero))*)

let instantiate_costs program pi0 =
	(* Create an empty array *)
	let costs = Array.make program.nb_automata (Array.make 0 NumConst.zero) in
	(* For each automaton *)
	for automaton_index = 0 to program.nb_automata - 1 do
		(* Retrieve the number of locations for this automaton *)
		let nb_locations = List.length (program.locations_per_automaton automaton_index) in
		(* Create the array of costs for this automaton *)
		costs.(automaton_index) <- Array.make nb_locations NumConst.zero;
		(* For each location *)
		for location_index = 0 to nb_locations - 1 do
			(* Retrieve the cost *)
			let cost = program.costs automaton_index location_index in
			(* Instantiate it *)
			let instantiated_cost = match cost with 
				| None -> NumConst.zero 
				| Some cost -> LinearConstraint.evaluate_linear_term pi0 cost in
			(* Save it *)
			costs.(automaton_index).(location_index) <- instantiated_cost;
		done;
	done;
	(* Set the global array *)
	instantiated_costs := costs;
	()


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
	print_message Debug_standard "invariant cache:"; 
	Cache.print_stats inv_cache(*;
 	print_message Debug_standard "clock update cache:"; *)
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
(* Fusion functions *)
(**************************************************************)
(** Check if two states are mergeable: return True if the convex hull is equal to the union, False otherwise*)
(*let state_mergeable state1 state2 =
	let (loc1,constr1) = state1 in
	let (loc2,constr2) = state2 in
	if not (Automaton.location_equal loc1 loc2) then false else (
		LinearConstraint.hull_assign_if_exact constr1 constr2 
	)*)


(*let rec merging_of_states graph index_state list_index_states list_new_states =
	match list_index_states with
	  | [] -> (false, -1)
	  | first :: rest  -> let (loc_state, constr_state) =  (get_state graph index_state) in
		  let (loc_hd, constr_hd) = (get_state graph first) in
		  if (state_mergeable (loc_state, constr_state) (loc_hd, constr_hd)) then (
			print_message Debug_total ("Mergeable states");
			merge_states graph index_state first;
			print_message Debug_total ("States merged");
			(* TO OPTIMIZE: operation already performed in state_mergeable !! *)
			LinearConstraint.hull_assign constr_state constr_hd;
			(true, first)
		  )
		  else ( merging_of_states graph index_state rest (first :: list_new_states); )*)


(*let try_to_merge_states graph list_of_states =
	print_message Debug_high ("Starting merging");
	
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
			print_message Debug_total ("Starting inner loop in merging");
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
					print_message Debug_total ("Found a mergeable state");
					Graph.merge_2_states graph eater current_element;
					print_message Debug_total ("States successfully merged");
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

		print_message Debug_total ("Starting one iteration of the outer loop in merging");

		let beginning = ref [] in
		let remaining = ref (!current_list_of_states) in
		
		while !remaining != [] do
			match !remaining with
			| [] -> raise (InternalError("Impossible case in 'merge_states'."))
			| first_remaining :: rest_remaining ->
				(* Call auxiliary function *)
				print_message Debug_high ("Considered one more state");
				remaining := merge_states_aux first_remaining rest_remaining;
				(* Add first to rest, i.e., move one step within the list *)
				beginning := first_remaining :: !beginning;
		done;
		(* Update list of states *)
		current_list_of_states := !beginning;
	done;

	(* Some debug message *)
	if !nb_merged > 0 then
		print_message Debug_standard ("  " ^ (string_of_int !nb_merged) ^ " state" ^ (s_of_int !nb_merged) ^ " merged.");
	
	(* Return something *)
	!current_list_of_states


(*
		match !new_states_after_merging with
			(* If empty list: do nothing *)
			| [] -> ()
			(* Otherwise: *)
			| first :: rest -> (
				let (result, state_merged) = merging_of_states reachability_graph first rest [] in
					print_message Debug_total ("Test for debugging the fatal error 2/5");
					if result then (
						print_message Debug_total ("Test for debugging the fatal error 3/5");
						merging := true;
						nb_merged := !nb_merged + 1;
						print_message Debug_total ("Test for debugging the fatal error 4/5");
						new_states_after_merging := list_remove_first_occurence state_merged !new_states_after_merging ;
						print_message Debug_total ("Test for debugging the fatal error 5/5");
					);
				print_message Debug_total ("Looping merging");
				);

						(** DEBUT LOOP *)

				
				done;*)*)



(**************************************************************)
(* Main functions *)
(**************************************************************)

(*--------------------------------------------------*)
(* Compute the invariant associated to a location   *)
(*--------------------------------------------------*)
let compute_plain_invariant program location =
  (* construct invariant *)
	let invariants = List.map (fun automaton_index ->
		(* Get the current location *)
		let location_index = Automaton.get_location location automaton_index in
		(* Compute the invariant *)
		program.invariants automaton_index location_index
	) program.automata in
	(* Perform the intersection *)
	LinearConstraint.intersection invariants


(*--------------------------------------------------*)
(* Compute the invariant I_q associated to a location  *)
(* including renaming and time elapse. Uses cache.  *)
(*--------------------------------------------------*)
let compute_invariant program location =
	(* Strip off discrete for caching scheme  *)
	let locations = Automaton.get_locations location in
	(* check in cache *)
	let entry = Cache.find inv_cache locations in
	match entry with
		| Some inv -> inv
		| None -> ( 
			(* Build plain invariant I_q(X) *)
			let invariant = compute_plain_invariant program location in
			(* Store in cache *)
			Cache.store inv_cache locations invariant;
			invariant
		)

(*--------------------------------------------------*)
(* Compute the polyhedron p projected onto rho(X) *)
(*--------------------------------------------------*)
(*** TO OPTIMIZE: use cache (?) *)
let rho_assign program linear_constraint clock_updates =
	if clock_updates != [] then(
		(* Merge updates *)
		
		(** TO OPTIMIZE: only create the hash if there are indeed some resets/updates *)
		
		let clocks_hash = Hashtbl.create program.nb_clocks in
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
					Hashtbl.replace clocks_hash clock_id (LinearConstraint.make_linear_term [] NumConst.zero);
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
							print_warning ("The clock '" ^ (program.variable_names clock_id) ^ "' is updated several times with different values for the same synchronized action.
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
		
			(** TO OPTIMIZE: Hashtbl.fold and List.map should be merged into one function *)
			
			(* Compute the list of clocks to update from the hashtable *)
			let list_of_clocks_to_update = Hashtbl.fold (fun clock_id _ list_of_clocks -> clock_id :: list_of_clocks) clocks_hash [] in
			
			(* Compute X = 0 for the variables appearing in resets *)
			print_message Debug_total ("\n -- Computing resets X = 0");
			let updates =
				(List.map (fun variable_index ->
					(* Consider cases for clocks *)
					match program.type_of_variables variable_index with
					(* Clocks: X = 0 *)
					| Var_type_clock -> 
						let x_lt = LinearConstraint.make_linear_term [
							NumConst.one, variable_index;
						] NumConst.zero in
						LinearConstraint.make_linear_inequality x_lt LinearConstraint.Op_eq
					| _ -> raise (InternalError "Only clocks can be updated.")
				) list_of_clocks_to_update) in
			(* Create the constraint *)
			let updates = LinearConstraint.make updates in
			(* Debug print *)
			if debug_mode_greater Debug_total then(
				print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names updates);
			);
			
			(* Hide clocks updated within the linear constraint, viz., exists X' : lc, for X' in rho(X) *)
			print_message Debug_total ("\n -- Computing exists X : lc for reset clocks");
			LinearConstraint.hide_assign list_of_clocks_to_update linear_constraint;
			if debug_mode_greater Debug_total then(
				print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names linear_constraint);
			);
			
			(* Add the constraints X = 0 *)
			print_message Debug_total ("\n -- Adding X = 0 for reset clocks");
			LinearConstraint.intersection_assign linear_constraint [updates];
			if debug_mode_greater Debug_total then(
				print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names linear_constraint);
			)
			
		(* CASE 3: updates to linear terms *)
		)else(
			(* Compute the couples (X_i , = linear_term) from the hashtable *)
			let updates = Hashtbl.fold (fun clock_id linear_term current_updates -> (clock_id, linear_term) :: current_updates) clocks_hash [] in
			(** TO OPTIMIZE (?): could be performed statically (when converting the program).
				PRO: save time because no need to compute this for each constraint;
				CON: lose time & memory (but maybe not that much) at some point because operations on constraints will have all dimensions instead of just the updated prime variables
				TO OPTIMIZE (other option): merge all operations together, so that no need for hashtable
			*)
			(* Compute the correspondance between clocks X_i and renamed clocks X_i' *)
			let prime_of_variable = Hashtbl.create (List.length updates) in
			let variable_of_prime = Hashtbl.create (List.length updates) in
			let clock_prime_id = ref program.nb_variables in
			List.iter (fun (clock_id, _) ->
				Hashtbl.add prime_of_variable clock_id !clock_prime_id;
				Hashtbl.add variable_of_prime !clock_prime_id clock_id;
				(* Debug message *)
				if debug_mode_greater Debug_standard then(
					print_message Debug_standard ("\nThe primed index of variable '" ^ (program.variable_names clock_id) ^ "' (index = " ^ (string_of_int clock_id) ^ ") is set to " ^ (string_of_int !clock_prime_id) ^ ".")
				);
				(* Increment the prime id for next variable *)
				clock_prime_id := !clock_prime_id + 1;
				()
			) updates;
			let new_max_dimension = !clock_prime_id in
			let extra_dimensions = new_max_dimension - program.nb_variables in
			print_message Debug_standard ("\nNew dimension for constraints: " ^ (string_of_int new_max_dimension) ^ "; extra dimensions : " ^ (string_of_int extra_dimensions) ^ ".");
			(* Extend the number of dimensions *)
			LinearConstraint.set_manager 0 new_max_dimension;
			LinearConstraint.add_dimensions extra_dimensions linear_constraint;

			(* Create constraints X_i' = linear_term *)
			let inequalities = List.map (fun (clock_id, linear_term) ->
				(* Build linear_term - clock_id' = 0 *)
				LinearConstraint.make_linear_inequality (
					LinearConstraint.add_linear_terms
						(* 1: The update linear term *)
						linear_term
						(* 2: - clock_id' *)
						(LinearConstraint.make_linear_term [
								NumConst.minus_one, (Hashtbl.find prime_of_variable clock_id);
							] NumConst.zero)
				) LinearConstraint.Op_eq
			) updates in
			(* Create the constraint *)
			let inequalities = LinearConstraint.make inequalities in
			(* Debug print *)
			let print_constraint c = 
				if debug_mode_greater Debug_standard then(
					let all_variable_names = fun variable_id ->
						if variable_id < program.nb_variables then 
							program.variable_names variable_id
						else
							(program.variable_names (Hashtbl.find variable_of_prime variable_id)) ^ "'"
					in
					print_message Debug_standard (LinearConstraint.string_of_linear_constraint all_variable_names c);
				)else(
					()
				)
			in
			print_constraint inequalities;

			(* Add the constraints X_i' = linear_term *)
			print_message Debug_standard ("\n -- Adding X_i' = linear_term for updated clocks");
			LinearConstraint.intersection_assign linear_constraint [inequalities];
			(* Debug print *)
			print_constraint linear_constraint;
			
			(* Remove the variables X_i *)
			let list_of_clocks_to_hide, _ = List.split updates in
			(* Hide clocks updated within the linear constraint, viz., exists X_i : lc, for X_i in rho(X) *)
			print_message Debug_standard ("\n -- Computing exists X : lc for updated clocks");
			LinearConstraint.hide_assign list_of_clocks_to_hide linear_constraint;
			(* Debug print *)
			if debug_mode_greater Debug_standard then(
				print_constraint linear_constraint;
			);
			
			(* Renames clock X_i' into X_i *)
			(** TO OPTIMIZE !! *)
			(* Compute couples (X_i', X_i) *)
			let clocks_and_primes = Hashtbl.fold (fun clock_id clock_prime_id couples -> (clock_id, clock_prime_id) :: couples) prime_of_variable [] in
			print_message Debug_standard ("\n -- Renaming clocks X_i' into X_i for updated clocks");
			LinearConstraint.rename_variables_assign clocks_and_primes linear_constraint;
			(* Debug print *)
			if debug_mode_greater Debug_standard then(
				print_constraint linear_constraint;
			);

			(* Go back to the original number of dimensions *)
			print_message Debug_standard ("\nGo back to standard dimension for constraints: " ^ (string_of_int program.nb_variables) ^ ".");
			LinearConstraint.set_manager 0 program.nb_variables;
			LinearConstraint.remove_dimensions extra_dimensions linear_constraint;
			(* Debug print *)
			if debug_mode_greater Debug_standard then(
				print_constraint linear_constraint;
			);
			
			(** TO CHECK: what about discrete variables ?!! *)
		)
		)
	)


(*--------------------------------------------------*)
(* Create a fresh constraint of the form 'D = d' for any discrete variable D with value d *)
(*--------------------------------------------------*)
let instantiate_discrete discrete_values =
	let inequalities = List.map (fun (discrete_index, discrete_value) ->
		(* Create a linear term 'D - d' *)
		let linear_term = LinearConstraint.make_linear_term
			[(NumConst.one, discrete_index)]
			(NumConst.neg discrete_value)
		in
		(* Create a linear equality *)
		LinearConstraint.make_linear_inequality linear_term LinearConstraint.Op_eq
	) discrete_values in
	(* Create the linear constraint *)
	LinearConstraint.make inequalities


(*--------------------------------------------------*)
(* Compute the list of stopped and elapsing clocks in a location *)
(*--------------------------------------------------*)
let compute_stopwatches program location =
	(* If no stopwatches at all: just return the set of clocks *)
	if not program.has_stopwatches then ([], program.clocks) else(
		(* Hashtbl clock_id --> true if clock should be stopped by some automaton *)
		let stopwatches_hash = Hashtbl.create (List.length program.clocks) in
		let stopwatch_mode = ref false in
		(* Update hash table *)
		List.iter (fun automaton_index ->
			(* Get the current location *)
			let location_index = Automaton.get_location location automaton_index in
			(* Get the list of stopped clocks *)
			let stopped = program.stopwatches automaton_index location_index in
			(* If list non null: we have stopwatches here *)
			if stopped != [] then stopwatch_mode := true;
			(* Add each clock *)
			List.iter (fun stopwatch_id ->
				Hashtbl.replace stopwatches_hash stopwatch_id true
			) stopped;
		) program.automata;
		(* If there are no stopwatches then just return the set of clocks *)
		if (not !stopwatch_mode) then ([], program.clocks) else (
			(* Computing the list of stopped clocks, and the list of elapsing clocks *)
			List.fold_left (fun (stopped_clocks, elapsing_clocks) clock_id -> 
				(* Test if the clock should be stopped *)
				if Hashtbl.mem stopwatches_hash clock_id then
					clock_id :: stopped_clocks, elapsing_clocks
				else
					stopped_clocks, clock_id :: elapsing_clocks
			) ([], []) program.clocks
		) (* if no stopwatch for this location *)
	) (* if no stopwatch in the program *)


(*--------------------------------------------------*)
(* Compute the initial state with the initial invariants and time elapsing *)
(*--------------------------------------------------*)
let create_initial_state program =
	(* Get the declared init state with initial constraint C_0(X) *)
	let initial_location = program.initial_location in
	let initial_constraint = program.initial_constraint in
	
	(* Compute the invariants I_q0(X) for the initial locations *)
	print_message Debug_high ("\nComputing initial invariant I_q0(X)");
	(* Create the invariant *)
	let invariant = compute_plain_invariant program initial_location in
	(* Debug *)
	if debug_mode_greater Debug_total then
		print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names invariant);
	
	(* Compute constraint for assigning a (constant) value to discrete variables *)
	print_message Debug_high ("Computing constraint for discrete variables");
	let discrete_values = List.map (fun discrete_index -> discrete_index, (Automaton.get_discrete_value initial_location discrete_index)) program.discrete in
	(* Constraint of the form D_i = d_i *)
	let discrete_constraint = instantiate_discrete discrete_values in
	(* Debug *)
	if debug_mode_greater Debug_total then
		print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names discrete_constraint);
	
	(* Perform intersection of C(X) and I_q0(X) and D_i = d_i *)
	print_message Debug_high ("Performing intersection of C0(X) and I_q0(X) and D_i = d_i");
	let current_constraint = LinearConstraint.intersection [initial_constraint ; invariant ; discrete_constraint (** To optimize: could be removed *)] in
	(* Debug *)
	if debug_mode_greater Debug_total then
		print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names current_constraint);
		
	(* Compute the list of stopwatches *)
	let stopped_clocks, elapsing_clocks = compute_stopwatches program initial_location in
	print_message Debug_high ("Computing list of stopwatches");
	if debug_mode_greater Debug_total then(
		let list_of_names = List.map program.variable_names stopped_clocks in
		print_message Debug_total ("Stopped clocks : " ^ (string_of_list_of_string_with_sep ", " list_of_names));
		let list_of_names = List.map program.variable_names elapsing_clocks in
		print_message Debug_total ("Elapsing clocks: " ^ (string_of_list_of_string_with_sep ", " list_of_names));
	);
	
	(* Perform time elapsing *)
	print_message Debug_high ("Performing time elapsing on [ C0(X) and I_q0(X) and D_i = d_i ]");
	LinearConstraint.time_elapse_assign (*program.clocks program.parameters_and_discrete*)
		elapsing_clocks
		(List.rev_append stopped_clocks program.parameters_and_discrete)
		current_constraint
	;
	(* Debug *)
	if debug_mode_greater Debug_total then
		print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names current_constraint);
	
	(* Perform intersection of [C(X) and I_q0(X) and D_i = d_i]time with I_q0(X) and D_i = d_i *)
	print_message Debug_high ("Performing intersection of [C0(X) and I_q0(X) and D_i = d_i]time and I_q0(X) and D_i = d_i");
	LinearConstraint.intersection_assign current_constraint [invariant ; discrete_constraint];
	(* Debug *)
	if debug_mode_greater Debug_total then
		print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names current_constraint);
	
	(* Hide discrete *)
	print_message Debug_high ("Hide discrete");
	LinearConstraint.hide_assign program.discrete current_constraint;
	(* Debug *)
	if debug_mode_greater Debug_total then
		print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names current_constraint);

	(* Return the initial state *)
	initial_location, current_constraint




(*--------------------------------------------------*)
(* Compute a list of possible actions for a state   *)
(*--------------------------------------------------*)
let compute_possible_actions program original_location = 
	(* Create a boolean array for the possible actions *)
	let possible_actions = Array.make program.nb_actions false in
	(* Fill it with all the possible actions per location *)
	for automaton_index = 0 to program.nb_automata - 1 do
		(* Get the current location for automaton_index *)
		let location_index = Automaton.get_location original_location automaton_index in
		(* Debug print *)
		print_message Debug_total ("Considering automaton " ^ (program.automata_names automaton_index) ^ " with location " ^ (program.location_names automaton_index location_index) ^ ".");
		(* Get the possible actions for this location *)
		let possible_actions_for_this_automaton =
			program.actions_per_location automaton_index location_index
		in
		(* Add all the actions to our array *)
		List.iter (fun action_index ->
			(* Add the action *)
			possible_actions.(action_index) <- true;
			(* Debug print *)
			print_message Debug_total ("Adding action " ^ (program.action_names action_index) ^ " for automaton " ^ (program.automata_names automaton_index) ^ " with location " ^ (program.location_names automaton_index location_index) ^ ".");
		) possible_actions_for_this_automaton;
	done;
	(* Debug print *)
	if debug_mode_greater Debug_total then (
		print_message Debug_total ("Possible actions for all the locations are:");
		Array.iteri (fun action_index possible ->
			if possible then (print_message Debug_total (" - " ^ (program.action_names action_index)));
		) possible_actions;
	);
	(* Remove every action where an automaton can not take this action in its *)
	(*  location although the action was declared for this automaton          *)
	let possible_actions = Array.mapi (fun action_index possible ->
		(* If this action is not possible, then false *)
		if not possible then false else(
		let automata_for_this_action = program.automata_per_action action_index in
		(* Check if the action is possible for all the automata for which it is defined *)
		(**** TO OPTIMIZE: no need to keep searching if not "still_possible" anymore ****)
		let action_possible =
			List.fold_left (fun still_possible automaton_index -> 
				still_possible
				&& (List.mem action_index (program.actions_per_location automaton_index (Automaton.get_location original_location automaton_index)))
			) possible automata_for_this_action in
		(* Debug print *)
		if not action_possible && (debug_mode_greater Debug_total) then (
			print_message Debug_total ("But action '" ^ (program.action_names action_index) ^ "' is not possible for all declared automata.")
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
let compute_new_location program aut_table trans_table action_index original_location =
	(* make a copy of the location *)		
	let location = Automaton.copy_location original_location in
	(* Create a temporary hashtbl for discrete values *)
	let updated_discrete = Hashtbl.create program.nb_discrete in
	(* Check if we actually have updates *)
	let has_updates = ref false in
	(* Update the location for the automata synchronized with 'action_index'; return the list of guards and updates *)
	let guards_and_updates = Array.to_list (Array.mapi (fun local_index real_index ->
		(* Get the current location for this automaton *)
		let location_index = Automaton.get_location original_location real_index in
		(* Find the transitions for this automaton *)
		let transitions = program.transitions real_index location_index action_index in
		(* Get the index of the examined transition for this automaton *)
		let current_index = trans_table.(local_index) in
		(* Keep the 'current_index'th transition *)
		let transition = List.nth transitions current_index in
		(* Keep only the dest location *)
		let guard, clock_updates, discrete_updates, dest_index = transition in			
		(* Update discrete *)
		List.iter (fun (discrete_index, linear_term) ->
			(* Compute its new value *)
			let new_value = LinearConstraint.evaluate_linear_term (Automaton.get_discrete_value original_location) linear_term in
			(* Check if already updated *)
			if Hashtbl.mem updated_discrete discrete_index then (
				(* Find its value *)
				let previous_new_value = Hashtbl.find updated_discrete discrete_index in
				(* Compare with the new one *)
				if NumConst.neq previous_new_value new_value then (
				(* If different: warning *)
					print_warning ("The discrete variable '" ^ (program.variable_names discrete_index) ^ "' is updated several times with different values for the same synchronized action '" ^ (program.action_names action_index) ^ "'. The behavior of the system is now unspecified.");
				);
			) else (
				(* Else keep it in memory for update *)
				Hashtbl.add updated_discrete discrete_index new_value;
			);
		) discrete_updates;
		(* Update the global location *)
		Automaton.update_location_with [real_index, dest_index] [] location;
		(* Update the update flag *)
		let _ =
		match clock_updates with
			| Resets (_ :: _) -> has_updates := true
			| Updates (_ :: _) -> has_updates := true
			| _ -> ()
		in ();
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
	
	


(*--------------------------------------------------*)	
(* Compute the new constraint for a transition      *)
(* orig_constraint : contraint in source location   *)
(* discrete_constr : contraint D_i = d_i in source location (discrete variables) *)
(* (* stopped_clocks  : list of clocks stopped         *) *)
(* (* elapsing_clocks : list of clocks non stopped     *) *)
(* orig_location   : source location                *)
(* dest_location   : target location                *)
(* guards          : guard constraints per automaton*)
(* clock_updates   : updated clock variables        *)
(*--------------------------------------------------*)
let compute_new_constraint program orig_constraint discrete_constr orig_location dest_location guards clock_updates =
	if debug_mode_greater Debug_total then(
		print_message Debug_total ("\n***********************************");
		print_message Debug_total ("Entering compute_new_constraint");	
		print_message Debug_total ("***********************************");
		print_message Debug_total ("C = " ^ (LinearConstraint.string_of_linear_constraint program.variable_names (orig_constraint ())));
	);
	(* The constraint is checked on the fly for satisfyability -> exception mechanism *)
	try (
		let current_constraint = LinearConstraint.copy discrete_constr in

		(* Debug *)
		if debug_mode_greater Debug_total then(
			print_message Debug_total ("\nComputing the guards g(x)");
			List.iter (fun guard -> 
				print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names guard);
			) guards;
		);
		print_message Debug_total ("\nPerforming intersection of Di = di and C(X) and g(X)");
		(* Add the (old) value for discrete to the guards D_i = d_i and g(X) *)
		LinearConstraint.intersection_assign current_constraint ((orig_constraint ()) :: guards);
		(* Debug print *)
		if debug_mode_greater Debug_total then(
			print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names current_constraint);
		);
		
		(* Check here for unsatisfiability *)
		if not (LinearConstraint.is_satisfiable current_constraint) then (
			(* Statistics *)
			nb_unsat1 := !nb_unsat1 + 1;
			print_message Debug_high "skip transition";
			raise Unsat_exception
		);
		
		print_message Debug_total ("\nEliminate the discrete variables in C(X) and g(X)");
		(* Remove the discrete variables (Exists D_i : D_i = d_i and g(X)) *)
		LinearConstraint.hide_assign program.discrete current_constraint;
		(* Debug print *)
		if debug_mode_greater Debug_total then(
			print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names current_constraint);
		);
		
		print_message Debug_total ("\nProjecting C(X) and g(X) onto rho");
		rho_assign program current_constraint clock_updates;
		(* Debug print *)
		if debug_mode_greater Debug_total then(
			print_message Debug_total ("\nResult:");
			print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names current_constraint);
		);

		(* Compute the invariant in the destination location I_q(X) *)
		print_message Debug_total ("\nComputing invariant I_q(X)");
		let invariant = compute_invariant program dest_location in
		(* Debug print *)
		if debug_mode_greater Debug_total then(
			print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names invariant);
		);

		(* Perform the intersection *)
		print_message Debug_total ("\nPerforming intersection of [C(X) and g(X)] rho and I_q(X)");
		(* (Exists D_i : D_i = d_i and g(X)) *)
		LinearConstraint.intersection_assign current_constraint [invariant];
		(* Debug print *)
		if debug_mode_greater Debug_total then(
			print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names current_constraint);
			if not (LinearConstraint.is_satisfiable current_constraint) then
				print_message Debug_total ("This constraint is NOT satisfiable (after intersection of [C(X) and g(X)] rho and I_q(X) ).");
		);

		(* NO USE FOR TESTING HERE FOR SATISFIABILITY (almost always satisfiable) *)
	
		(* Compute the list of stopwatches *)
		let stopped_clocks, elapsing_clocks = compute_stopwatches program dest_location in
		print_message Debug_high ("Computing list of stopwatches");
		if debug_mode_greater Debug_total then(
			let list_of_names = List.map program.variable_names stopped_clocks in
			print_message Debug_total ("Stopped clocks : " ^ (string_of_list_of_string_with_sep ", " list_of_names));
			let list_of_names = List.map program.variable_names elapsing_clocks in
			print_message Debug_total ("Elapsing clocks: " ^ (string_of_list_of_string_with_sep ", " list_of_names));
		);
		
		(* Perform time elapsing *)
		print_message Debug_total ("\nPerforming time elapsing on [C(X) and g(X)] rho and I_q(X)");
		LinearConstraint.time_elapse_assign (*program.clocks program.parameters_and_discrete*)
			elapsing_clocks
			(List.rev_append stopped_clocks program.parameters_and_discrete)
			current_constraint
		;
		(* Debug print *)
		if debug_mode_greater Debug_total then(
			print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names current_constraint);
		);

		(* Compute the equalities for the discrete variables (in destination location) *)
		let discrete_values = List.map (fun discrete_index -> discrete_index, (Automaton.get_discrete_value dest_location discrete_index)) program.discrete in
		(* Convert to a constraint *)
		let discrete_constraint = instantiate_discrete discrete_values in
		
		(* Perform the intersection *)
		print_message Debug_total ("\nPerforming intersection of the constraint with D_i = d_i and I_q(X) ");
		LinearConstraint.intersection_assign current_constraint
			[
				discrete_constraint;
				invariant;
			];
		(* Debug print *)
		if debug_mode_greater Debug_total then(
			print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names current_constraint);
			if not (LinearConstraint.is_satisfiable current_constraint) then
				print_message Debug_total ("This constraint is NOT satisfiable (after intersection of the constraint with D_i = d_i and I_q(X)).");
		);
		
(*		(* Check here for unsatisfiability *)
		if not (LinearConstraint.is_satisfiable current_constraint) then (
			(* Statistics *)
			nb_unsat2 := !nb_unsat2 + 1;
			print_message Debug_high "skip transition";
			raise Unsat_exception
		);*)

		(* AGAIN, NO USE FOR TESTING HERE FOR SATISFIABILITY (almost always satisfiable) *)

		(* Hide discrete' *)
		print_message Debug_total ("\nHide discrete variables ");
		LinearConstraint.hide_assign (program.discrete) current_constraint;
		(* Debug print *)
		if debug_mode_greater Debug_total then(
			print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names current_constraint);
			if not (LinearConstraint.is_satisfiable current_constraint) then
				print_message Debug_total ("This constraint is NOT satisfiable (after hiding discrete variables).");
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
(* returns (true, p_constraint) iff the state is pi0-compatible (false, _) otherwise *)
(*-----------------------------------------------------*)
let inverse_method_check_constraint program reachability_graph constr =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	(* Retrieve the pi0 (dynamic!) *)
	let pi0 = Input.get_pi0 () in
	
	(* Hide non parameters (X) *)
	print_message Debug_high ("\nHiding non parameters:");
	let p_constraint = LinearConstraint.hide program.clocks_and_discrete constr in
	(* Debug print *)
	if debug_mode_greater Debug_high then(
		print_message Debug_high (LinearConstraint.string_of_linear_constraint program.variable_names p_constraint);
	);
	(* Check the pi0-compatibility *)
	let compatible, incompatible = LinearConstraint.partition_pi0_compatible pi0 p_constraint in
	let is_pi0_incompatible = incompatible != [] in
	
	(* If pi0-incompatible: select an inequality *)
	if is_pi0_incompatible then (
		print_message Debug_low ("\nFound a pi0-incompatible state.");
		(* Debug print *)
		if debug_mode_greater Debug_medium then(
			print_message Debug_medium ("\nThe following inequalities are pi0-incompatible:");
			List.iter (fun inequality -> print_message Debug_medium (LinearConstraint.string_of_linear_inequality program.variable_names inequality)) incompatible;
		);

		let inequality =
			(* If random selection: pick up a random inequality *)
			if not options#no_random then random_element incompatible
			(* Else select the first one *)
			else List.nth incompatible 0
		in
		(* Debug print *)
		if debug_mode_greater  Debug_medium then(
			print_message Debug_medium ("\nSelecting the following pi0-incompatible inequality:");
			print_message Debug_medium (LinearConstraint.string_of_linear_inequality program.variable_names inequality);
		);

		(* Update counter *)
		if List.length incompatible > 1 then nb_random_selections := !nb_random_selections + 1;
		
		(* Negate the inequality *)
		let negated_inequality = LinearConstraint.negate_wrt_pi0 pi0 inequality in
		(* Debug print *)
		let randomly = if not options#no_random then "randomly " else "" in
		let among = if List.length incompatible > 1 then (" (" ^ randomly ^ "selected among " ^ (string_of_int (List.length incompatible)) ^ " inequalities)") else "" in
		print_message Debug_standard ("  Adding the following inequality" ^ among ^ ":");
		print_message Debug_standard ("  " ^ (LinearConstraint.string_of_linear_inequality program.variable_names negated_inequality));
		
		
		(* Add the p_constraint to the result (except if case variants) *)
		if not (options#pi_compatible || options#union) then(
			print_message Debug_high ("Updating k_result with the negated inequality");
			LinearConstraint.intersection_assign !k_result [LinearConstraint.make [negated_inequality]];
		);
		

		(* Update the previous states (including the 'new_states' and the 'orig_state') *)
		print_message Debug_medium ("\nUpdating all the previous states.\n");
		Graph.add_inequality_to_states reachability_graph negated_inequality;
		
		(* If pi-incompatible *)
		(false, p_constraint)
		(* If pi-compatible *)
	) else (true, p_constraint)



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
let compute_transitions program location constr action_index automata aut_table max_indexes possible_transitions  =
	let current_index = ref 0 in 
	(* Stop computation as soon as one automaton has no legal transition left. *)
	try (
		List.iter (fun automaton_index ->
			(* Tabulate the real index *)
			aut_table.(!current_index) <- automaton_index;
			(* Get the current location for this automaton *)
			let location_index = Automaton.get_location location automaton_index in
			(* Get transitions for this automaton *)
			let transitions = program.transitions automaton_index location_index action_index in
			(* REMOVED 2011/11/21 : computation always slower ; might be faster for strongly branching systems? EXCEPT FOR LSV.imi --> put it back! *)
			(* Keep only possible transitions *)
			let is_possible = fun trans -> (
				let guard, _, _, _ = trans in
				let constr_and_guard = LinearConstraint.intersection [constr; guard] in
 				let is_possible = LinearConstraint.is_satisfiable constr_and_guard in 
				if not is_possible then (
					(* Statistics *)
					nb_early_unsatisfiable := !nb_early_unsatisfiable + 1;
					print_message Debug_medium "** early skip transition **"
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
				print_message Debug_medium "*** early skip action ***";
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
(* Post function *)
(************************************************************)

(*-----------------------------------------------------*)
(* Compute the list of possible destination states     *)
(* wrt. to a reachability_graph, and update this graph *)
(*-----------------------------------------------------*)
(* returns a list of (really) new states               *)
(*-----------------------------------------------------*)
let post program reachability_graph orig_state_index =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	(* Original location: static *)
	let original_location, _ = Graph.get_state reachability_graph orig_state_index in
	(* Dynamic version of the orig_constraint (can change!) *)
	let orig_constraint () =
		let _, orig_constraint = Graph.get_state reachability_graph orig_state_index in
		orig_constraint
	in

	(* Debug prints *)
	if debug_mode_greater Debug_high then(
		let orig_state = Graph.get_state reachability_graph orig_state_index in
		let _, orig_constraint = orig_state in
		let orig_constraint_projection = LinearConstraint.hide program.clocks_and_discrete orig_constraint in
		print_message Debug_high ("Performing post from state:");
		print_message Debug_high (string_of_state program orig_state);
		print_message Debug_high ("\nThe projection of this constraint onto the parameters is:");
		print_message Debug_high (LinearConstraint.string_of_linear_constraint program.variable_names orig_constraint_projection);
	);

	(* get possible actions originating from current state *)
	let list_of_possible_actions = compute_possible_actions program original_location in

	(* Debug print *)
	if debug_mode_greater Debug_high then (
		let actions_string = List.map (fun action_index -> program.action_names action_index) list_of_possible_actions in
		print_message Debug_high ("Possible synchronized actions are: " ^ (string_of_list_of_string_with_sep ", " actions_string));
	);

	(* build the list of new states *)
	let new_states = ref [] in

	(* Create a constraint D_i = d_i for the discrete variables *)
	let discrete_values = List.map (fun discrete_index -> discrete_index, (Automaton.get_discrete_value original_location discrete_index)) program.discrete in
	(* Convert to a constraint *)
	let discrete_constr = instantiate_discrete discrete_values in

	(* FOR ALL ACTION DO: *)
	List.iter (fun action_index ->

		print_message Debug_medium ("\nComputing destination states for action '" ^ (program.action_names action_index) ^ "'");
		(* Get the automata declaring the action *)
		let automata_for_this_action = program.automata_per_action action_index in
		let nb_automata_for_this_action = List.length automata_for_this_action in
	
		(*------------------------------------------------*)
		(* Compute the reachable states on the fly: i.e., all the possible transitions for all the automata belonging to 'automata' *)
		(*------------------------------------------------*)
		
		(* Compute conjunction with current constraint *)
		let orig_plus_discrete = LinearConstraint.intersection [orig_constraint (); discrete_constr] in
		
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
		let legal_transitions_exist = compute_transitions program original_location orig_plus_discrete action_index automata_for_this_action real_indexes max_indexes possible_transitions in 
	
		(* Debug: compute the number of combinations *)
		if debug_mode_greater Debug_medium || options#statistics then(
			let new_nb_combinations = Array.fold_left (fun sum max -> sum * (max + 1)) 1 max_indexes in
			print_message Debug_medium ("I will consider " ^ (string_of_int new_nb_combinations) ^ " combination" ^ (s_of_int new_nb_combinations) ^ " for this state and this action\n");
			(* Update for statistics *)
			nb_combinations := !nb_combinations + new_nb_combinations;
		);
	
		(* Loop on all the transition combinations *)
		let more_combinations = ref legal_transitions_exist in
		let debug_i = ref 0 in
		while !more_combinations do
			debug_i := !debug_i +1;
			(* Debug *)
			if debug_mode_greater Debug_total then (
				let local_indexes = string_of_array_of_string_with_sep "\n\t" (
				Array.mapi (fun local_index real_index ->
					(string_of_int local_index) ^ " -> " ^ (string_of_int real_index) ^ " : " ^ (string_of_int current_indexes.(local_index)) ^ "; ";
				) real_indexes) in
				print_message Debug_total ("\n\n\n--- Consider the combination " ^ (string_of_int !debug_i) ^ " \n\t" ^ local_indexes);
			);
	
			(* build the current combination of transitions *)
			for i=0 to Array.length current_transitions -1 do
				current_transitions.(i) <- List.nth (possible_transitions.(i)) (current_indexes.(i))
			done; 
	
			(* Compute the new location for the current combination of transitions *)
			let location, guards, clock_updates = compute_new_location program real_indexes current_transitions action_index original_location in
			
			(* Compute the new constraint for the current transition *)
			let new_constraint = compute_new_constraint program orig_constraint discrete_constr original_location location guards clock_updates in
			
			let _ =
			(* Check the satisfiability *)
			match new_constraint with
				| None -> 
					(* Statistics *)
					nb_unsatisfiable := !nb_unsatisfiable + 1;
					print_message Debug_high ("\nThis constraint is not satisfiable ('None').");
				| Some final_constraint -> (
					if not (LinearConstraint.is_satisfiable final_constraint) then(
						(* Statistics *)
						nb_unsatisfiable := !nb_unsatisfiable + 1;
						print_message Debug_high ("\nThis constraint is not satisfiable ('Some unsatisfiable').");
					) else (
			
					let add_new_state, p_constraint =
					(*------------------------------------------------------------*)
					(* Branching between 2 algorithms here *)
					(*------------------------------------------------------------*)
					if options#imitator_mode = Reachability_analysis then ( 
						true, (LinearConstraint.true_constraint ())
					) else (
						inverse_method_check_constraint program reachability_graph final_constraint
					) in
					
					if add_new_state then (
						(* Create the state *)
						let new_state = location, final_constraint in
				  
						(* Debug print *)
						if debug_mode_greater Debug_total then(
							print_message Debug_total ("Consider the state \n" ^ (string_of_state program new_state));
						);

						(* If IM: Add the p_constraint to the result (except if case variants) *)
						if options#imitator_mode != Reachability_analysis && not (options#pi_compatible || options#union) then(
							print_message Debug_high ("Updating k_result");
							LinearConstraint.intersection_assign !k_result [p_constraint];
						);
						
						(* Try to add this new state to the graph *)
						let new_state_index, added = (
						(*if options#dynamic then (
						  Graph.add_state_dyn program reachability_graph new_state !k_result
						  )
						  else ( *)
						    Graph.add_state program reachability_graph new_state
(* 						  ) *)
						) in
						(* If this is really a new state *)
						if added then (
							(* Add the state_index to the list of new states *)
							new_states := new_state_index :: !new_states;
						)
						(* ELSE : add to SLAST if mode union *)
						else (
							if options#union then (
								print_message Debug_low ("\nMode union: adding a looping state to SLast.");
								(* Adding the state *)
								(* TO CHECK: what if new_state_index is already in slast?!! *)
								slast := new_state_index :: !slast;
							);
						);
						
						(* Update the transitions *)
						Graph.add_transition reachability_graph (orig_state_index, action_index, new_state_index);
						(* Debug print *)
						if debug_mode_greater Debug_high then (
							let beginning_message = (if added then "NEW STATE" else "Old state") in
							print_message Debug_high ("\n" ^ beginning_message ^ " reachable through action '" ^ (program.action_names action_index) ^ "': ");
							print_message Debug_high (string_of_state program new_state);
						);
					); (* end if pi0 incompatible *)
				); (* end if satisfiable *)
			); (* end if Some constraint *)
			in ();
		
			(* get the next combination *)
			more_combinations := next_combination current_indexes max_indexes;	
			
		done; (* while more new states *)
	) list_of_possible_actions;
	
	(* If new_states is empty : the current state is a last state *)
	if options#union && !new_states = [] then (
		print_message Debug_low ("\nMode union: adding a state without successor to SLast.");
		(* Adding the state *)
		slast := orig_state_index :: !slast;
	);
	
	(* Return the list of (really) new states *)
	(** TO DO: List.rev really useful??!!!! *)
	List.rev (!new_states)




(************************************************************)
(* Full reachability functions *)
(************************************************************)


(*---------------------------------------------------*)
(* Check whether the limit of an exploration has been reached, according to the analysis options *)
(*---------------------------------------------------*)
let check_limit depth nb_states time =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	(* Disjunction between the different options *)
	begin
	match options#post_limit with
		| None -> false
		| Some limit -> depth > limit
	end
	||
	begin
	match options#states_limit with
		| None -> false
		| Some limit -> nb_states > limit
	end
	||
	begin
	match options#time_limit with
		| None -> false
		| Some limit -> time > (float_of_int limit)
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
let branch_and_bound program init_state = 
	(* Retrieve the input options *)
	let options = Input.get_options () in
	(* Retrieve the pi0 *)
	let pi0 = Input.get_pi0 () in
	
	(* Get some variables *)
	let nb_actions = program.nb_actions in
	let nb_variables = program.nb_variables in
	let nb_automata = program.nb_automata in

	(* copy init state, as it might be destroyed later *)
	let init_loc, init_constr = init_state in
	let init_state = (init_loc, LinearConstraint.copy init_constr) in

	(*Initialization of k_result*)
	k_result := LinearConstraint.hide program.clocks_and_discrete init_constr;

	print_message Debug_standard ("START BRANCH AND BOUND");
	
	(* Instantiate all costs associated to locations, for once *)
	print_message Debug_standard ("Instantiate costs");
	instantiate_costs program pi0;
	
	(* Debug print *)
	if debug_mode_greater Debug_total then (
		print_message Debug_standard ("Print costs");
		(* For each automaton *)
		for automaton_index = 0 to program.nb_automata - 1 do
			(* Retrieve the number of locations for this automaton *)
			let nb_locations = List.length (program.locations_per_automaton automaton_index) in
			(* For each location *)
			for location_index = 0 to nb_locations - 1 do
				(* Retrieve the cost *)
				let cost = (!instantiated_costs).(automaton_index).(location_index) in
				(* Print it *)
					print_message Debug_total ((program.automata_names automaton_index) ^ " --> " ^ (program.location_names automaton_index location_index) ^ " : " ^ (NumConst.string_of_numconst cost));
			done;
		done;
		
	);
	
	(* Time counter *)
	let start_time = Unix.gettimeofday() in
	
	(* Reset counter for random selections *)
	nb_random_selections := 0;

	(* Debut prints *)
	print_message Debug_low ("Starting reachability analysis from state:");
	print_message Debug_low (string_of_state program init_state);
	(* Guess the number of reachable states *)
	let guessed_nb_states = 10 * (nb_actions + nb_automata + nb_variables) in 
	let guessed_nb_transitions = guessed_nb_states * nb_actions in 
	print_message Debug_total ("I guess I will reach about " ^ (string_of_int guessed_nb_states) ^ " states with " ^ (string_of_int guessed_nb_transitions) ^ " transitions.");

	(* Create the reachability graph *)
	let reachability_graph = Graph.make guessed_nb_transitions in

	(* Add the initial state to the reachable states *)
	let init_state_index, _ = Graph.add_state program reachability_graph init_state in
	
	(* Create the tree for exploration *)
	let rtree = ReachabilityTree.create guessed_nb_states init_state_index in
	
	if debug_mode_greater Debug_total then(
		print_message Debug_total ("\nReachability tree initialized:\n" ^ (ReachabilityTree.string_of_rtree string_of_int rtree));
	);

	(*--------------------------------------------------*)
	(* Start the exploration *)
	(*--------------------------------------------------*)
	(* Current state to be analyzed *)
	let current_state_index = ref init_state_index in
	(* Limit reached? (Due to bounded analysis options) *)
	let limit_reached = ref false in
	(* Number of states to visit (in the tree) *)
	let nb_states_to_visit = ref (ReachabilityTree.nb_states_to_visit rtree) in

	
	(* Check if the list of new states is empty *)
	while not (!limit_reached  || !nb_states_to_visit = 0) do
		if debug_mode_greater Debug_standard then (
			print_message Debug_low ("\n");
			print_message Debug_standard ("Computing successors of state " ^ (string_of_int !current_state_index));
			if debug_mode_greater Debug_medium then (
				print_message Debug_medium (string_of_state program (Graph.get_state reachability_graph !current_state_index ));
			);
		);
		
		(* Now compute successors *)
		let new_states = post program reachability_graph !current_state_index in
		
		
		(* Merge states ! *)
		let new_states =
		if not options#no_merging then (
			let eaten_states = Graph.merge reachability_graph new_states in
			
			
			(* TODO: remove states from rtree !! (and big problem if some previous states have been removed due to merging.....) *)
			
			
			Global.list_diff new_states eaten_states;
		) else (
			new_states
		) in
		
		(* Update the reachability tree *)
		ReachabilityTree.add_children !current_state_index new_states rtree;
		ReachabilityTree.set_visited !current_state_index rtree;
		
		(* Debug *)
		if debug_mode_greater Debug_medium then (
			let beginning_message = if new_states = [] then "\nFound no new state" else ("\nFound " ^ (string_of_int (List.length new_states)) ^ " new state" ^ (s_of_int (List.length new_states)) ^ "") in
			print_message Debug_medium (beginning_message ^ ".\n");
		);
		
		if debug_mode_greater Debug_total then(
			print_message Debug_total ("\nReachability tree now as follows:\n" ^ (ReachabilityTree.string_of_rtree string_of_int rtree));
		);
		(* If acyclic option: empty the list of already reached states for comparison with former states *)
		
		(** TODO : recheck this !! (I guess very dangerous, because would empty the whole list of states to compare!) *)
		
		if options#acyclic then(
			print_message Debug_low ("\nMode acyclic: empty the list of states to be compared.");
			empty_states_for_comparison reachability_graph;
		);
		
		
		(* Clean up a little *)
		(** LOOKS LIKE COMPLETELY USELESS !!! it even increases memory x-( *)
		Gc.major ();
		
		(* Recompute the number of states to visit *)
		nb_states_to_visit := ReachabilityTree.nb_states_to_visit rtree;
		
		(* Update the new state to visit *)
		if !nb_states_to_visit > 0 then(
			print_message Debug_high ("Recall that current element in the reachability tree is " ^ (string_of_int !current_state_index));
			print_message Debug_medium ("Finding next element in the reachability tree...");
			try(
				current_state_index := ReachabilityTree.get_next_element rtree;
			) with Not_found -> (
				print_warning ("\nReachability tree now as follows:\n" ^ (ReachabilityTree.string_of_rtree string_of_int rtree));
				raise (InternalError "The reachability tree has an unexpected structure, when looking for the next state in branch and bound.");
			);
		);
		
		(* Check if the limit has been reached according to the options *)
		limit_reached := check_limit 0 (** TODO: check depth *) (Graph.nb_states reachability_graph) (time_from start_time);
	done;
	
	(* Check whether there are still states to explore *)
	if !limit_reached && !nb_states_to_visit > 0 then(
		print_warnings_limit 0 (** TODO: check depth *) (Graph.nb_states reachability_graph) (time_from start_time) !nb_states_to_visit;
	);
	
	
	print_message Debug_standard (
		let nb_states = Graph.nb_states reachability_graph in
		let nb_transitions = Graph.nb_transitions reachability_graph in
		"\nFixpoint reached: "
		^ (string_of_int nb_states) ^ " reachable state" ^ (s_of_int nb_states)
		^ " with "
		^ (string_of_int nb_transitions) ^ " transition" ^ (s_of_int nb_transitions) ^ ".");

	(* Return the graph, the iteration and the counter *)
	reachability_graph , 0 (** TODO: check depth *) , (time_from start_time) , !nb_random_selections



(*---------------------------------------------------*)
(* Compute the reachability graph from a given state *)
(*---------------------------------------------------*)
let post_star program init_state = 
	(* Retrieve the input options *)
	let options = Input.get_options () in
	
	(* Get some variables *)
	let nb_actions = program.nb_actions in
	let nb_variables = program.nb_variables in
	let nb_automata = program.nb_automata in

	(* copy init state, as it might be destroyed later *)
	let init_loc, init_constr = init_state in
	let init_state = (init_loc, LinearConstraint.copy init_constr) in

	(*Initialization of k_result*)
(* 	k_result := LinearConstraint.true_constraint (); *)
	k_result := LinearConstraint.hide program.clocks_and_discrete init_constr;

	(*Initialization of slast : used in union mode only*)
	slast := [];
	(* Time counter *)
	let start_time = Unix.gettimeofday() in
	(* Set the counter of selections to 0 *)
	nb_random_selections := 0;

	(* Debut prints *)
	print_message Debug_low ("Starting reachability analysis from state:");
	print_message Debug_low (string_of_state program init_state);
	(* Guess the number of reachable states *)
	let guessed_nb_states = 10 * (nb_actions + nb_automata + nb_variables) in 
	let guessed_nb_transitions = guessed_nb_states * nb_actions in 
	print_message Debug_total ("I guess I will reach about " ^ (string_of_int guessed_nb_states) ^ " states with " ^ (string_of_int guessed_nb_transitions) ^ " transitions.");
	(* Create the reachability graph *)
	let reachability_graph = Graph.make guessed_nb_transitions in
	
	(* Add the initial state to the reachable states *)
	let init_state_index, _ = Graph.add_state program reachability_graph init_state in
	
	(*--------------------------------------------------*)
	(* Perform the post^* *)
	(*--------------------------------------------------*)
	let newly_found_new_states = ref [init_state_index] in
	let nb_iterations = ref 1 in
	let limit_reached = ref false in

	(* Check if the list of new states is empty *)
	while not (!limit_reached  || !newly_found_new_states = []) do
		if debug_mode_greater Debug_standard then (
			print_message Debug_low ("\n");
			print_message Debug_standard ("Computing post^" ^ (string_of_int (!nb_iterations)) ^ " from "  ^ (string_of_int (List.length !newly_found_new_states)) ^ " state" ^ (s_of_int (List.length !newly_found_new_states)) ^ ".");
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
			let new_states = post program reachability_graph orig_state_index in
(* 			let new_states = post in *)
			(* Debug *)
			if debug_mode_greater Debug_medium then (
				let beginning_message = if new_states = [] then "Found no new state" else ("Found " ^ (string_of_int (List.length new_states)) ^ " new state" ^ (s_of_int (List.length new_states)) ^ "") in
				print_message Debug_medium (beginning_message ^ " for the post of state " ^ (string_of_int !num_state) ^ " / " ^ (string_of_int nb_states) ^ " in post^" ^ (string_of_int (!nb_iterations)) ^ ".\n");
			);
			
			(* Return the concatenation of the new states *)
			(**** OPTIMIZED: do not care about order (else shoud consider 'list_append new_newly_found_new_states (List.rev new_states)') *)
			List.rev_append new_newly_found_new_states new_states
		) [] !newly_found_new_states in


		(*
			(* TO CHECK: Etienne' version should replace Romain's version *)
			let rec try_to_merge = function
				(* If empty list: do nothing *)
				| [] -> []
				(* Otherwise: *)
				| first :: rest -> (
					let (result, state_merged) = merging_of_states reachability_graph first rest [] in
						if result then try_to_merge rest else first :: (try_to_merge rest)
					)
			in
			newly_found_new_states := try_to_merge new_newly_found_new_states;*)


		(* Merge states ! *)
		let new_states_after_merging = ref new_newly_found_new_states in
		if not options#no_merging then (
(* 			new_states_after_merging := try_to_merge_states reachability_graph !new_states_after_merging; *)
			(* New version *)
			let eaten_states = Graph.merge reachability_graph !new_states_after_merging in
			new_states_after_merging := Global.list_diff !new_states_after_merging eaten_states;
		);


		(* Update the newly_found_new_states *)
		newly_found_new_states := !new_states_after_merging;
		(* Debug *)
		if debug_mode_greater Debug_medium then (
			let beginning_message = if !newly_found_new_states = [] then "\nFound no new state" else ("\nFound " ^ (string_of_int (List.length !newly_found_new_states)) ^ " new state" ^ (s_of_int (List.length !newly_found_new_states)) ^ "") in
			print_message Debug_medium (beginning_message ^ " for post^" ^ (string_of_int (!nb_iterations)) ^ ".\n");
		);
		
		(* If acyclic option: empty the list of already reached states for comparison with former states *)
		if options#acyclic then(
			print_message Debug_low ("\nMode acyclic: empty the list of states to be compared.");
			empty_states_for_comparison reachability_graph;
		);
		
		(* Clean up a little *)
		(** LOOKS LIKE COMPLETELY USELESS !!! it even increases memory x-( *)
		Gc.major ();
		
		(* Iterate *)
		nb_iterations := !nb_iterations + 1;
		
		(* Check if the limit has been reached *)
		limit_reached := check_limit !nb_iterations (Graph.nb_states reachability_graph) (time_from start_time);
	done;
	
	(* There were still states to explore *)
	if !limit_reached && !newly_found_new_states != [] then(
		print_warnings_limit !nb_iterations (Graph.nb_states reachability_graph) (time_from start_time) (List.length !newly_found_new_states);
	);

	print_message Debug_standard (
		let nb_states = Graph.nb_states reachability_graph in
		let nb_transitions = Graph.nb_transitions reachability_graph in
		"\nFixpoint reached after "
		^ (string_of_int !nb_iterations) ^ " iteration" ^ (s_of_int !nb_iterations) ^ ""
(* 		^ " in " ^ (string_of_seconds (time_from !counter)) *)
		^ ": "
		^ (string_of_int nb_states) ^ " reachable state" ^ (s_of_int nb_states)
		^ " with "
		^ (string_of_int nb_transitions) ^ " transition" ^ (s_of_int nb_transitions) ^ ".");

	(* Return the graph, the iteration and the counter *)
	reachability_graph , !nb_iterations , (time_from start_time) , !nb_random_selections




(*--------------------------------------------------*)
(* Performances *)
(*--------------------------------------------------*)
let print_statistics reachability_graph =
	(* Retrieve the input options *)
	let options = Input.get_options () in

	if options#statistics then (
		(* PPL *)
		print_message Debug_standard "--------------------";
		print_message Debug_standard "Statistics on PPL";
		print_message Debug_standard ("--------------------" ^ (LinearConstraint.get_statistics ()));
		
		(* Graph *)
		print_message Debug_standard "--------------------";
		print_message Debug_standard "Statistics on Graph";
		print_message Debug_standard "--------------------";
		print_message Debug_standard (Graph.get_statistics ());
		print_message Debug_standard (Graph.get_statistics_states reachability_graph);
		
		print_message Debug_standard "--------------------";
		print_message Debug_standard "Statistics on Cache";
		print_message Debug_standard "--------------------";
		print_stats ();
		
		print_message Debug_standard "--------------------";
		print_message Debug_standard "Statistics on Reachability";
		print_message Debug_standard "--------------------";
		print_message Debug_standard ("Number of early skips because of unsatisfiable guards: " ^ (string_of_int !nb_early_unsatisfiable));
		print_message Debug_standard ("Number of early skips because no actions: " ^ (string_of_int !nb_early_skip));
		print_message Debug_standard ("Number of unsatisfiable constraints: " ^ (string_of_int !nb_unsatisfiable));
		print_message Debug_standard ("Number of unsat1: " ^ (string_of_int !nb_unsat1));
		print_message Debug_standard ("Number of unsat2: " ^ (string_of_int !nb_unsat2));
		print_message Debug_standard ("Number of combinations considered: " ^ (string_of_int !nb_combinations));
		
		print_message Debug_standard "--------------------";
		print_message Debug_standard "Statistics on memory";
		print_message Debug_standard "--------------------";
		let gc_stat = Gc.stat () in
		let nb_words = gc_stat.minor_words +. gc_stat.major_words -. gc_stat.promoted_words in
		let nb_ko = nb_words *. 4.0 /. 1024.0 in
		print_message Debug_standard ("Total memory: " ^ (string_of_float nb_ko) ^ " KB (i.e., " ^ (string_of_float nb_words) ^ " words)");
		Gc.print_stat stdout;
(*		print_message Debug_standard "--------------------";
		Gc.major();
		Gc.print_stat stdout;
		print_message Debug_standard "--------------------";
		Gc.full_major();
		Gc.print_stat stdout;*)
	)





(************************************************************)
(************************************************************)
(** Main functions *)
(************************************************************)
(************************************************************)


(************************************************************)
(* Full reachability analysis *)
(************************************************************)
let full_reachability program init_state =
	(* Retrieve the input options *)
	let options = Input.get_options () in

	(* Call to generic function *)
	let reachability_graph , _ , total_time ,  _ = post_star program init_state in
	
	print_message Debug_standard (
		"\nReachabiliy analysis completed " ^ (after_seconds ()) ^ "."
	);
	print_message Debug_low (
		"Computation time for reachability analysis: "
		^ (string_of_seconds total_time) ^ "."
	);

	(* Print statistics *)
	print_statistics reachability_graph;
	
	(* Generate graphics *)
	let radical = options#program_prefix in
	Graphics.generate_graph program reachability_graph radical;
	
	(* The end*)
	()



(************************************************************)
(* Main inverse method functions *)
(************************************************************)

(*--------------------------------------------------*)
(* Encapsulation function for IM, called by the real inverse method function, and by the cartography algorithms *)
(*--------------------------------------------------*)
let inverse_method_gen program init_state =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	
	
	
	
	(*(* TEST FOR BRANCH AND BOUND *)
	if options#branch_and_bound then(
		
		print_message Debug_standard "1) BRANCH AND BOUND";
		let _,_,_,_ = branch_and_bound program init_state in
		let constraint1 = !k_result in

		(* Remove branch and bound *)
		options#branch_and_bound_unset;
		print_message Debug_standard "2) CLASSICAL ALGORITHM";
		let _,_,_,_ = post_star program init_state in
		let constraint2 = !k_result in
	
		print_message Debug_standard "3) COMPARE RESULT";
		print_message Debug_standard "-> Branch and bound:";
		print_message Debug_standard (LinearConstraint.string_of_linear_constraint program.variable_names constraint1);
		print_message Debug_standard "-> Classical IM:";
		print_message Debug_standard (LinearConstraint.string_of_linear_constraint program.variable_names constraint2);
		
		if LinearConstraint.is_equal constraint1 constraint2 then(
			print_message Debug_standard "\n\nCONSTRAINTS EQUAL :-)";
			terminate_program();
		) else (
			print_message Debug_standard "\n\nARGH! CONSTRAINTS DIFFERENT :-(\n\n";
			raise (InternalError "byebye");
		);
	);*)
	
	
	
	
	
	
	(* Choose the correct algorithm *)
	let algo = if options#branch_and_bound then branch_and_bound else post_star in
	(* Call to generic functions *)
	let reachability_graph, nb_iterations, total_time, nb_random_selections = algo program init_state in
	
	(*--------------------------------------------------*)
	(* Print information *)
	(*--------------------------------------------------*)
	if not options#no_random then (
		if (nb_random_selections > 0) then (
			print_message Debug_standard "Analysis may have been non-deterministic:";
			print_message Debug_standard ((string_of_int nb_random_selections) ^ " random selection" ^ (s_of_int nb_random_selections) ^ " have been performed.");
		) else (
			print_message Debug_standard "Analysis has been fully deterministic.";
		)
	);
	

	(*--------------------------------------------------*)
	(* Computation of the returned constraint *)
	(*--------------------------------------------------*)
	let returned_constraint =
	(* Case IM standard : return the intersection *)
	if (*options#dynamic ||*) (not options#union && not options#pi_compatible) then (
		print_message Debug_total ("\nMode: IM standard.");
		Convex_constraint !k_result
	) else (
	(* Case union : return the constraint on the parameters associated to slast*)
		if options#union then (
			print_message Debug_total ("\nMode: union.");
			let list_of_constraints =
			List.map (fun state_index ->
				print_message Debug_medium ("\nOne state found.");
				(* Get the constraint on clocks and parameters *)
				let (_, current_constraint) =
					Graph.get_state reachability_graph state_index
				(* Eliminate clocks *)
				in LinearConstraint.hide program.clocks_and_discrete current_constraint
			) !slast
			in Union_of_constraints list_of_constraints
		)
	(* Case IMorig : return only the current constraint, viz., the constraint of the first state *)
		else if options#pi_compatible then (
			let (_ , k_constraint) = get_state reachability_graph 0 in
				print_message Debug_total ("\nMode: IMorig.");
				Convex_constraint (LinearConstraint.hide program.clocks_and_discrete k_constraint) 
		) else (
			raise (InternalError ("This code should be unreachable (in end of inverse_method, when returning the constraint)."));
		)
(*		(* Case IM : intersection *)
		else (
			(** HERE PROBLEM IF ONE WANTS TO COMPUTE THE states FILE AFTER (destruction of the states) **)
			Convex_constraint (Graph.compute_k0_destructive program reachability_graph)
		)*)
	)
	in

	(*--------------------------------------------------*)
	(* Return result *)
	(*--------------------------------------------------*)
	returned_constraint, reachability_graph, nb_iterations, total_time
	
	

(*--------------------------------------------------*)
let inverse_method program init_state =
(*--------------------------------------------------*)
	(* Retrieve the input options *)
	let options = Input.get_options () in

	(* Call the inverse method *)
	let returned_constraint, reachability_graph, nb_iterations, total_time = inverse_method_gen program init_state in
	
	(* Here comes the result *)
	print_message Debug_standard ("\nFinal constraint K0 "
		^ (if options#union
			then "(under disjunctive form) "
			else (
				let linear_constraint =
				match returned_constraint with
					| Convex_constraint linear_constraint -> linear_constraint
					| _ -> raise (InternalError "Impossible situation in inverse_method: a returned_constraint is not under convex form although union mode is not enabled.");
				in
				" (" ^ (string_of_int (LinearConstraint.nb_inequalities linear_constraint)) ^ " inequalities)"
			)
		)
		^ ":");
	print_message Debug_nodebug (string_of_returned_constraint program.variable_names returned_constraint);
	print_message Debug_standard (
		"\nInverse method successfully finished " ^ (after_seconds ()) ^ "."
	);
	print_message Debug_low (
		"Computation time for IM only: "
		^ (string_of_seconds total_time) ^ "."
	);
	
	(* Generate graphics *)
	let radical = options#program_prefix in
	Graphics.generate_graph program reachability_graph radical;
	
	(* Print statistics *)
	print_statistics reachability_graph;

	(* The end *)
	()


