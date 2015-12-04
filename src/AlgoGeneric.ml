(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: main virtual class to explore the state space: only defines post-related function, i.e., to compute the successor states of ONE state. That (still) represents a large list of functions.
 * 
 * File contributors : Étienne André
 * Created           : 2015/12/02
 * Last modified     : 2015/12/03
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open OCamlUtilities
open ImitatorUtilities
open Exceptions
open AbstractModel




(************************************************************)
(************************************************************)
(* Object-independent functions *)
(************************************************************)
(************************************************************)

(************************************************************)
(* Exception *)
(************************************************************)

exception Unsat_exception




(************************************************************)
(* Costs *)
(************************************************************)

(*** TODO: move to the proper class for costs ***)

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






(************************************************************)
(* Functions related to locations *)
(************************************************************)

(*------------------------------------------------------------*)
(* Check whether at least one local location is urgent *)
(*------------------------------------------------------------*)

(*** TODO: move to Location.mli (but creates a dependency problem, as the model is needed...) ***)

let is_location_urgent model location =
	(* Subfunction checking that one location is urgent in a given automaton *)
	let is_local_location_urgent automaton_index =
		(* Retrieve location *)
		let location_index = Location.get_location location automaton_index in
		(* Check if urgent *)
		model.is_urgent automaton_index location_index
	in
	List.exists is_local_location_urgent model.automata



(************************************************************)
(* Cache *)
(************************************************************)

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



(*** TODO: move to the class! (otherwise, if several algorithms in the row, cache will not be reinitialized... ***)


(* Cache for computed invariants *)
let inv_cache = Cache.make loc_hash 200

(*(* Cache for clock updates *)
let upd_cache = Cache.make upd_hash 100*)


(************************************************************)
(* Statistics *)
(************************************************************)

(*** TODO: re use from ResultProcessor (temporarily disabled) ***)

(* Print statistics for cache usage *)
let print_stats _ =
	print_message Verbose_standard "invariant cache:"; 
	Cache.print_stats inv_cache(*;
 	print_message Verbose_standard "clock update cache:"; *)
(* 	Cache.print_stats upd_cache *)
	
 
 
(*** TODO: move to the class ***)
(*** TODO: use integer counters in a new class... ***)


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





(************************************************************)
(* Main functions *)
(************************************************************)

(*------------------------------------------------------------*)
(* Compute the invariant associated to a location   *)
(*------------------------------------------------------------*)
let compute_plain_invariant model location =
  (* construct invariant *)
	let invariants = List.map (fun automaton_index ->
		(* Get the current location *)
		let location_index = Location.get_location location automaton_index in
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
	let locations = Location.get_locations location in
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
			if verbose_mode_greater Verbose_total then(
				print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names updates);
			);
			
			(* Hide clocks updated within the linear constraint, viz., exists X' : lc, for X' in rho(X) *)
			print_message Verbose_total ("\n -- Computing exists X : lc for reset clocks");
			LinearConstraint.pxd_hide_assign list_of_clocks_to_update linear_constraint;
			if verbose_mode_greater Verbose_total then(
				print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names linear_constraint);
			);
			
			(* Add the constraints X = 0 *)
			print_message Verbose_total ("\n -- Adding X = 0 for reset clocks");
			LinearConstraint.pxd_intersection_assign linear_constraint [updates];
			if verbose_mode_greater Verbose_total then(
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
				if verbose_mode_greater Verbose_total then(
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
				if verbose_mode_greater Verbose_total then(
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
			if verbose_mode_greater Verbose_total then(
				print_constraint linear_constraint;
			);
			
			(* Renames clock X_i' into X_i *)
			(** TO OPTIMIZE !! *)
			(* Compute couples (X_i', X_i) *)
			let clocks_and_primes = Hashtbl.fold (fun clock_id clock_prime_id couples -> (clock_id, clock_prime_id) :: couples) prime_of_variable [] in
			print_message Verbose_total ("\n -- Renaming clocks X_i' into X_i for updated clocks");
			LinearConstraint.pxd_rename_variables_assign clocks_and_primes linear_constraint;
			(* Print some information *)
			if verbose_mode_greater Verbose_total then(
				print_constraint linear_constraint;
			);

			(* Go back to the original number of dimensions *)
			print_message Verbose_total ("\nGo back to standard dimension for constraints: " ^ (string_of_int model.nb_variables) ^ ".");
(* 			LinearConstraint.set_manager 0 model.nb_variables; *)
			LinearConstraint.set_dimensions model.nb_parameters model.nb_clocks model.nb_discrete;
			LinearConstraint.pxd_remove_dimensions extra_dimensions linear_constraint;
			(* Print some information *)
			if verbose_mode_greater Verbose_total then(
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
			let location_index = Location.get_location location automaton_index in
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
		if verbose_mode_greater Verbose_total then(
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
		if verbose_mode_greater Verbose_total then(
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
	if verbose_mode_greater Verbose_total then
		print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names invariant);
	
	(* Compute constraint for assigning a (constant) value to discrete variables *)
	print_message Verbose_high ("Computing constraint for discrete variables");
	let discrete_values = List.map (fun discrete_index -> discrete_index, (Location.get_discrete_value initial_location discrete_index)) model.discrete in
	(* Constraint of the form D_i = d_i *)
	let discrete_constraint = instantiate_discrete discrete_values in
	(* Print some information *)
	if verbose_mode_greater Verbose_total then
		print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names discrete_constraint);
	
	(* Perform intersection of C(X) and I_l0(X) and D_i = d_i *)
	print_message Verbose_high ("Performing intersection of C0(X) and I_l0(X) and D_i = d_i");
	let current_constraint = LinearConstraint.pxd_intersection [initial_constraint ; invariant ; discrete_constraint (*** TO OPTIMIZE: could be removed ***)] in
	(* Print some information *)
	if verbose_mode_greater Verbose_total then
		print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names current_constraint);

	
	(*--- BEGIN only if time elapsing ---*)
	if not options#no_time_elapsing then(
		(* Perform time elapsing *)
		print_message Verbose_high ("Applying time elapsing to [ C0(X) and I_l0(X) and D_i = d_i ]");
		apply_time_elapsing initial_location current_constraint;
		
(*		(* Compute the list of stopwatches *)
		let stopped_clocks, elapsing_clocks = compute_stopwatches model initial_location in
		print_message Verbose_high ("Computing list of stopwatches");
		if verbose_mode_greater Verbose_total then(
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
		if verbose_mode_greater Verbose_total then
			print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names current_constraint);*)
		
		
		(* Perform intersection of [C(X) and I_l0(X) and D_i = d_i]time with I_l0(X) and D_i = d_i *)
		(*** NOTE: intersection NOT necessary in absence of time elapsing, because the same I_l0(X) and D_i = d_i were intersected earlier ***)
		print_message Verbose_high ("Performing intersection of [C0(X) and I_l0(X) and D_i = d_i]time and I_l0(X) and D_i = d_i");
		LinearConstraint.pxd_intersection_assign current_constraint [invariant ; discrete_constraint];
		(* Print some information *)
		if verbose_mode_greater Verbose_total then
			print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names current_constraint);
	);
		
	(*--- END only if time elapsing ---*)
	
	(* Hide discrete *)
	print_message Verbose_high ("Hide discrete");
	let current_constraint = LinearConstraint.pxd_hide_discrete_and_collapse (*model.discrete*) current_constraint in
	(* Print some information *)
	if verbose_mode_greater Verbose_total then
		print_message Verbose_total (LinearConstraint.string_of_px_linear_constraint model.variable_names current_constraint);
		
		
	(* Remove useless clocks (if option activated) *)
	if options#dynamic_clock_elimination then(
		ClocksElimination.dynamic_clock_elimination initial_location current_constraint;
	);
	
	
	(* Return the initial state *)
	initial_location, current_constraint



(*------------------------------------------------------------*)
(* Compute the initial state with the initial invariants and time elapsing, and check whether it is satisfiable; if not, abort *)
(*------------------------------------------------------------*)
let compute_initial_state_or_abort () =
	(* Retrieve the model *)
	let model = Input.get_model() in
	(* Retrieve the input options *)
	let options = Input.get_options () in

	(* Print the initial state *)
	if verbose_mode_greater Verbose_medium then
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
	if verbose_mode_greater Verbose_medium then
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
		let location_index = Location.get_location original_location automaton_index in
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
	if verbose_mode_greater Verbose_total then (
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
				&& (List.mem action_index (model.actions_per_location automaton_index (Location.get_location original_location automaton_index)))
			) possible automata_for_this_action in
		(* Print some information *)
		if not action_possible && (verbose_mode_greater Verbose_total) then (
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
(*** TODO: remove the model from the arguments, and retrieve it ***)
let compute_new_location model aut_table trans_table action_index original_location =
	(* make a copy of the location *)		
	let location = Location.copy_location original_location in
	(* Create a temporary hashtbl for discrete values *)
	let updated_discrete = Hashtbl.create model.nb_discrete in
	(* Check if we actually have updates *)
	let has_updates = ref false in
	(* Update the location for the automata synchronized with 'action_index'; return the list of guards and updates *)
	let guards_and_updates = Array.to_list (Array.mapi (fun local_index real_index ->
		(* Get the current location for this automaton *)
		let location_index = Location.get_location original_location real_index in
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
			
			let new_value = LinearConstraint.evaluate_pxd_linear_term (Location.get_discrete_value original_location) linear_term in
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
		Location.update_location_with [real_index, dest_index] [] location;
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
	Location.update_location_with [] !updated_discrete_couples location;
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
(*** TODO: remove the model from the arguments, and retrieve it ***)
let compute_new_constraint model orig_constraint (discrete_constr_src : LinearConstraint.pxd_linear_constraint) orig_location dest_location guards clock_updates =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	
	if verbose_mode_greater Verbose_total then(
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
			if verbose_mode_greater Verbose_total then(
				print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names invariant);
			);

			(* Perform the intersection *)
			print_message Verbose_total ("\nAlternative time elapsing: performing intersection of C(X)time and I_l(X)");
			LinearConstraint.pxd_intersection_assign orig_constraint_with_maybe_time_elapsing [invariant];
		);

		
		let current_constraint = LinearConstraint.pxd_copy discrete_constr_src in
		
		(* Print some information *)
		if verbose_mode_greater Verbose_total then(
			print_message Verbose_total ("\nComputing the guards g(x)");
			List.iter (fun guard -> 
				print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names guard);
			) guards;
		);

		print_message Verbose_total ("\nPerforming intersection of Di = di and C(X) and g(X)");
		(* Add the (old) value for discrete to the guards D_i = d_i and g(X) *)
		LinearConstraint.pxd_intersection_assign current_constraint (orig_constraint_with_maybe_time_elapsing :: guards);
		(* Print some information *)
		if verbose_mode_greater Verbose_total then(
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
		if verbose_mode_greater Verbose_total then(
			print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names current_constraint);
		);
		
		print_message Verbose_total ("\nProjecting C(X) and g(X) onto rho");
		rho_assign model current_constraint clock_updates;
		(* Print some information *)
		if verbose_mode_greater Verbose_total then(
			print_message Verbose_total ("\nResult:");
			print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names current_constraint);
		);

		(* Compute the invariant in the destination location I_l'(X) *)
		print_message Verbose_total ("\nComputing invariant I_l'(X)");
		let invariant = compute_invariant model dest_location in
		(* Print some information *)
		if verbose_mode_greater Verbose_total then(
			print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names invariant);
		);

		(* Perform the intersection *)
		print_message Verbose_total ("\nPerforming intersection of [C(X) and g(X)]rho and I_l'(X)");
		(* (Exists D_i : D_i = d_i and g(X)) *)
		LinearConstraint.pxd_intersection_assign current_constraint [invariant];
		(* Print some information *)
		if verbose_mode_greater Verbose_total then(
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
		let discrete_values_dest = List.map (fun discrete_index -> discrete_index, (Location.get_discrete_value dest_location discrete_index)) model.discrete in
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
		if verbose_mode_greater Verbose_total then(
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
		if verbose_mode_greater Verbose_total then(
			print_message Verbose_total (LinearConstraint.string_of_px_linear_constraint model.variable_names current_constraint);
			if not (LinearConstraint.px_is_satisfiable current_constraint) then
				print_message Verbose_total ("This constraint is NOT satisfiable (after hiding discrete variables).");
		);
		
		
		(* Remove useless clocks (if option activated) *)
		(*** WARNING: code duplication!!! ***)
		if options#dynamic_clock_elimination then(
			ClocksElimination.dynamic_clock_elimination dest_location current_constraint;
		);
		
		
		(* return the final constraint *)
		Some current_constraint
	) with Unsat_exception -> None


(*------------------------------------------------------------*)
(* Computes next combination of indices           *)
(* current_indexes : combination                  *)
(* max_indexes     : maximum indices              *)
(*------------------------------------------------------------*)
(* returns a boolean, indicating that the         *)
(* new combination is valid (false if the old     *)
(* combination was the last one)                  *)
(*------------------------------------------------------------*)
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
			let location_index = Location.get_location location automaton_index in
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
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)

class virtual algoGeneric =
	object (self)
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)

	(* Start time for the algorithm *)
	val mutable start_time = 0.
	
	(* Nature of the trace set *)
	val mutable trace_set_nature = Unknown
	
	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Virtual method: the algorithm name is not defined for BFS as it is not supposed to be called *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual algorithm_name : string
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual initialize_variables : unit

	
	

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Update the nature of the trace set *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method update_trace_set_nature ((location : Location.global_location), (_ : LinearConstraint.px_linear_constraint)) =
		(* Retrieve the model *)
		let model = Input.get_model() in
		(* Retrieve the input options *)
		let options = Input.get_options () in

		match model.correctness_condition with
		| None -> ()
		| Some (Unreachable unreachable_global_locations) ->
			(* Check whether the current location matches one of the unreachable global locations *)
			if StateSpace.match_unreachable_global_locations unreachable_global_locations location then(
			
				(*** Quite a hack here ***)
				(*** TODO: move elsewhere ***)
				if options#efim && trace_set_nature <> Bad then(
					print_message Verbose_standard ("  [EFIM] Bad location found! Switching to bad-driven algorithm");
				);
				
				trace_set_nature <- Bad;
			);
		| _ -> raise (InternalError("IMITATOR currently ony implements the non-reachability-like properties."))

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Add a new state to the reachability_graph (if indeed needed) *)
	(* Also update tile_nature and slast (*** TODO: remove these operations, and move them back to their algorithms ***) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** TODO: simplify signature by removing the orig_state_index and returning the list of actually added states ***)
	method virtual add_a_new_state : StateSpace.state_space -> StateSpace.state_index -> StateSpace.state_index list ref -> Automaton.action_index -> Location.global_location -> LinearConstraint.px_linear_constraint -> unit

		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Compute the list of successor states of a given state, and update the state space; returns the list of new states' indexes actually added *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** TODO: to get a more abstract method, should get rid of the state space, and update the state space from another function ***)
	method post_from_one_state state_space orig_state_index =
		(* Retrieve the model *)
		let model = Input.get_model () in

		(* Retrieve the input options *)
		let options = Input.get_options () in
		
		(* Original location: static *)
		let original_location, _ = StateSpace.get_state state_space orig_state_index in
		(* Dynamic version of the original px_constraint (can change!) *)
		(*** NOTE / TO OPTIMIZE: OK but not in all algorithms !! ***)
		let orig_constraint () =
			let _, orig_constraint = StateSpace.get_state state_space orig_state_index in
			orig_constraint
		in

		(* Print some information *)
		if verbose_mode_greater Verbose_high then(
			let orig_state = StateSpace.get_state state_space orig_state_index in
			let _, orig_constraint = orig_state in
			let orig_constraint_projection = LinearConstraint.px_hide_nonparameters_and_collapse orig_constraint in
			print_message Verbose_high ("Performing post from state:");
			print_message Verbose_high (ModelPrinter.string_of_state model orig_state);
			print_message Verbose_high ("\nThe projection of this constraint onto the parameters is:");
			print_message Verbose_high (LinearConstraint.string_of_p_linear_constraint model.variable_names orig_constraint_projection);
		);

		(* get possible actions originating from current state *)
		let list_of_possible_actions = compute_possible_actions model original_location in

		(* Print some information *)
		if verbose_mode_greater Verbose_high then (
			let actions_string = List.map (fun action_index -> model.action_names action_index) list_of_possible_actions in
			print_message Verbose_high ("Possible synchronized actions are: " ^ (string_of_list_of_string_with_sep ", " actions_string));
		);

		(* Build the list of new states indexes *)
		let new_states_indexes = ref [] in
		
		(* Build the list of new states (for variant of merging only) *)
		(* EXPERIMENTAL BRANCHING: MERGE BEFORE OR AFTER? *)
		let new_action_and_state_list = ref [] in
		

		(* Create a constraint D_i = d_i for the discrete variables *)
		let discrete_values = List.map (fun discrete_index -> discrete_index, (Location.get_discrete_value original_location discrete_index)) model.discrete in
		(* Convert to a constraint *)
		let discrete_constr = instantiate_discrete discrete_values in

		(* FOR ALL ACTION DO: *)
		List.iter (fun action_index ->

			print_message Verbose_medium ("\nComputing destination states for action '" ^ (model.action_names action_index) ^ "'");
			(* Get the automata declaring the action *)
			let automata_for_this_action = model.automata_per_action action_index in
			let nb_automata_for_this_action = List.length automata_for_this_action in
		
			(*------------------------------------------------------------*)
			(* Compute the reachable states on the fly: i.e., all the possible transitions for all the automata belonging to 'automata' *)
			(*------------------------------------------------------------*)
			
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
		
			(* Print some information: compute the number of combinations *)
			if verbose_mode_greater Verbose_medium || options#statistics then(
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
				if verbose_mode_greater Verbose_total then (
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
						StateSpace.increment_nb_gen_states state_space;
				
						(************************************************************)
						(* EXPERIMENTAL BRANCHING: MERGE BEFORE OR AFTER? *)
						(************************************************************)
						(* EXPERIMENTAL BRANCHING: CASE MERGE AFTER (this new version may be better?) *)
						(*** NOTE: why not in mode state space??? ***)
						if options#imitator_mode <> State_space_exploration && options#merge_before then(
						
							(* Only add to the local list of new states *)
							new_action_and_state_list := ([action_index], location, final_constraint) :: !new_action_and_state_list;
						
						(* EXPERIMENTAL BRANCHING: END CASE MERGE AFTER *)
						)else(
						
						(* EXPERIMENTAL BRANCHING: CASE MERGE BEFORE (classical version) *)
							self#add_a_new_state state_space orig_state_index new_states_indexes action_index location final_constraint;
							
						); (* EXPERIMENTAL BRANCHING: END CASE MERGE BEFORE (classical version) *)
						
					); (* end if satisfiable *)
				); (* end if Some constraint *)
				end; (* end match constraint *)
			
				(* Update the next combination *)
				more_combinations := next_combination current_indexes max_indexes;
				
			done; (* while more new states *)
		) list_of_possible_actions;
		
	(**********************************************************************************************************************)
	(**********************************************************************************************************************)
	(**********************************************************************************************************************)
	(**********************************************************************************************************************)
	(***                                        BEGIN ALGORITHM-SPECIFIC CODE                                             ***)
	(**********************************************************************************************************************)
	(**********************************************************************************************************************)
	(**********************************************************************************************************************)
	(**********************************************************************************************************************)
	(**********************************************************************************************************************)
(*		(* If new_states is empty : the current state is a last state *)
		if options#union && !new_states_indexes = [] then (
			print_message Verbose_low ("\nMode union: adding a state without successor to SLast.");
			(* Adding the state *)
			slast := orig_state_index :: !slast;
		);*)
	(**********************************************************************************************************************)
	(**********************************************************************************************************************)
	(**********************************************************************************************************************)
	(**********************************************************************************************************************)
	(***                                          END ALGORITHM-SPECIFIC CODE                                             ***)
	(**********************************************************************************************************************)
	(**********************************************************************************************************************)
	(**********************************************************************************************************************)
	(**********************************************************************************************************************)
	(**********************************************************************************************************************)

		
		(************************************************************)
		(* EXPERIMENTAL BRANCHING: MERGE BEFORE OR AFTER? *)
		(************************************************************)
		(* EXPERIMENTAL BRANCHING: CASE MERGE AFTER (this new version may be better?) *)
		(*** NOTE: why not in mode state space ??? ***)
		if options#imitator_mode <> State_space_exploration && options#merge_before then(
		
			(* Merge *)
			StatesMerging.merge new_action_and_state_list;
			
			(* Add the remaining states *)
			List.iter (fun (action_index_list, location, final_constraint) ->
				(* Iterate on all actions *)
				(*** WARNING: not very beautiful !! ***)
				List.iter (fun action_index ->
					self#add_a_new_state state_space orig_state_index new_states_indexes action_index location final_constraint;
				) action_index_list;
			) !new_action_and_state_list
			
		); (* EXPERIMENTAL BRANCHING: END CASE MERGE AFTER *)
		
		
		(* Return the list of (really) new states *)
		(*** NOTE: List.rev really useful??!!!! ***)
		List.rev (!new_states_indexes)


	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Main method to run the algorithm: virtual method to be defined in subclasses *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual run : unit -> Result.imitator_result
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual compute_result : Result.imitator_result

(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
