(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * 
 * Module description: EFsynth algorithm [JLR15]
 * 
 * File contributors : Étienne André
 * Created           : 2015/11/25
 * Last modified     : 2019/07/05
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
open Result
open AlgoStateBased
open Statistics
open State




(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class virtual algoEFsynth =
	object (self) inherit algoStateBased as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	(* Non-necessarily convex constraint allowing the reachability of the bad location *)
	val mutable bad_constraint : LinearConstraint.p_nnconvex_constraint = LinearConstraint.false_p_nnconvex_constraint ()

	
	(* Non-necessarily convex parameter constraint of the initial state (constant object used as a shortcut, as it is used at the end of the algorithm) *)
	(*** WARNING: these lines are copied from AlgoDeadlockFree ***)
	val init_p_nnconvex_constraint : LinearConstraint.p_nnconvex_constraint =
		(* Retrieve the model *)
		let model = Input.get_model () in
		LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint model.initial_p_constraint

	(* Counters *)
	(*** NOTE: if EF is called several times, then each call will create a counter ***)
	
	(* The bad state has been found *)
	val counter_found_bad = create_discrete_counter_and_register "found bad state" PPL_counter Verbose_low
	(* The constraint of a new state is smaller than the bad constraint: cut branch *)
	val counter_cut_branch = create_discrete_counter_and_register "cut branch (constraint <= bad)" PPL_counter Verbose_low
	(* How many times the cache was useful *)
	val counter_cache = create_discrete_counter_and_register "cache (EF)" PPL_counter Verbose_low
	(* Number of cache misses *)
	val counter_cache_miss = create_discrete_counter_and_register "cache miss (EF)" PPL_counter Verbose_low
	(* Methods counters *)
	val counter_process_state = create_hybrid_counter_and_register "EFsynth.process_state" States_counter Verbose_experiments
	val counter_compute_p_constraint_with_cache = create_hybrid_counter_and_register "EFsynth.compute_p_constraint_with_cache" States_counter Verbose_experiments
	val counter_add_a_new_state = create_hybrid_counter_and_register "EFsynth.add_a_new_state" States_counter Verbose_experiments

	
	
	(* Mini cache system: keep in memory the current p-constraint to save computation time *)
	(*** WARNING: a bit dangerous, as its handling is not very very strictly controlled ***)
	val mutable cached_p_constraint : LinearConstraint.p_linear_constraint option = None
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "EFsynth"
	
	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
		super#initialize_variables;
		(*** NOTE: duplicate operation ***)
		bad_constraint <- LinearConstraint.false_p_nnconvex_constraint ();

		(* The end *)
		()
	

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Process a symbolic state: returns false if the state is a target state (and should not be added to the next states to explore), true otherwise *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private process_state (state : state) =
	
		(* Statistics *)
		counter_process_state#increment;
		counter_process_state#start;
	
		(* Print some information *)
		if verbose_mode_greater Verbose_medium then(
			self#print_algo_message Verbose_medium "Entering process_state…";
		);

		let state_location, state_constraint = state.global_location, state.px_constraint in
		
		let to_be_added = match model.correctness_condition with
		| None -> raise (InternalError("A correctness property must be defined to perform EF-synthesis or PRP. This should have been checked before."))
		| Some (Unreachable unreachable_global_locations) ->
			
			(* Check whether the current location matches one of the unreachable global locations *)
			if State.match_unreachable_global_locations unreachable_global_locations state_location then(
			
				(* Print some information *)
				if verbose_mode_greater Verbose_medium then(
					self#print_algo_message Verbose_medium "Projecting onto the parameters (using the cache)…";
				);

				(* Project onto the parameters *)
				(*** NOTE: here, we use the cache system ***)
				let p_constraint = self#compute_p_constraint_with_cache state_constraint in

				(* Projecting onto SOME parameters if required *)
				(*** BADPROG: Duplicate code (AlgoLoopSynth / AlgoPRP) ***)
				begin
				match model.projection with
				(* Unchanged *)
				| None -> ()
				(* Project *)
				| Some parameters ->
					(* Print some information *)
					self#print_algo_message Verbose_medium "Projecting onto some of the parameters.";

					(*** TODO! do only once for all… ***)
					let all_but_projectparameters = list_diff model.parameters parameters in
					
					(* Eliminate other parameters *)
					LinearConstraint.p_hide_assign all_but_projectparameters p_constraint;

					(* Print some information *)
					if verbose_mode_greater Verbose_medium then(
						print_message Verbose_medium (LinearConstraint.string_of_p_linear_constraint model.variable_names p_constraint);
					);
				end;

				(* Statistics *)
				counter_found_bad#increment;
				
				(*** TODO: test if these two operations are more or less expensive than just adding without testing ***)
				(*** NOTE: advantage of doing it twice: print less information :-) ***)
				
				(*** NOTE: do NOT do it in mode no_leq_test_in_ef ***)
				
				if (not options#no_leq_test_in_ef) && LinearConstraint.p_nnconvex_constraint_is_leq (LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint p_constraint) bad_constraint then(
					self#print_algo_message Verbose_low "Found a state violating the property (but the constraint was already known).";
				)else(
					(* The constraint is new! *)
					
					(* Print some information *)
					self#print_algo_message Verbose_standard "Found a new state violating the property.";
						
					(* Update the bad constraint using the current constraint *)
					(*** NOTE: perhaps try first whether p_constraint <= bad_constraint ? ***)
					LinearConstraint.p_nnconvex_p_union bad_constraint p_constraint;
					
					(* Print some information *)
					if verbose_mode_greater Verbose_low then(
						self#print_algo_message Verbose_medium "Adding the following constraint to the bad constraint:";
						print_message Verbose_low (LinearConstraint.string_of_p_linear_constraint model.variable_names p_constraint);
						
						self#print_algo_message Verbose_low "The bad constraint is now:";
						print_message Verbose_low (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names bad_constraint);
					);
				); (* end if new bad constraint *)
				
				(* Do NOT compute its successors; cut the branch *)
				false
				
			)else(
				self#print_algo_message Verbose_medium "State not corresponding to the one wanted.";
				
				(* Keep the state as it is not a bad state *)
				true
			)
		| _ -> raise (InternalError("[EFsynth/PRP] IMITATOR currently ony implements the non-reachability-like properties. This should have been checked before."))
		
		in
		
		(* Statistics *)
		counter_process_state#stop;

		(* Return result *)
		to_be_added
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Actions to perform with the initial state; returns true unless the initial state cannot be kept (in which case the algorithm will stop immediately) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_initial_state initial_state = self#process_state initial_state


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Compute the p-constraint only if it is not cached *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private compute_p_constraint_with_cache px_linear_constraint =

		(* Statistics *)
		counter_compute_p_constraint_with_cache#increment;
		counter_compute_p_constraint_with_cache#start;
		
		let result =
		match cached_p_constraint with
		(* Cache empty: *)
		| None ->
			(* Compute the p_constraint *)
			let p_constraint = LinearConstraint.px_hide_nonparameters_and_collapse px_linear_constraint in
			(* Statistics *)
			counter_cache_miss#increment;
			(* Print some information *)
			if verbose_mode_greater Verbose_medium then(
				self#print_algo_message Verbose_medium "\nCache miss!";
			);
			(* Update cache *)
			cached_p_constraint <- Some p_constraint;
			(* Return result *)
			p_constraint
		(* Cache not empty: directly use it *)
		| Some p_constraint ->
			(* Statistics *)
			counter_cache#increment;
			(* Print some information *)
			if verbose_mode_greater Verbose_medium then(
				self#print_algo_message Verbose_medium "\nCache hit!";
			);
			(* Return the value in cache *)
			p_constraint
		in
		
		(* Statistics *)
		counter_compute_p_constraint_with_cache#stop;

		result
		
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Add a new state to the reachability_graph (if indeed needed) *)
	(* Return true if the state is not discarded by the algorithm, i.e., if it is either added OR was already present before *)
	(* Can raise an exception TerminateAnalysis to lead to an immediate termination *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** TODO: return the list of actually added states ***)
	(*** WARNING/BADPROG: the following is partially copy/paste to AlgoPRP.ml ***)
	method add_a_new_state source_state_index combined_transition new_state =
		(* Statistics *)
		counter_add_a_new_state#increment;
		counter_add_a_new_state#start;
		
		(* Print some information *)
		if verbose_mode_greater Verbose_medium then(
			self#print_algo_message Verbose_medium "Entering add_a_new_state (and reset cache)…";
		);
		
		(* Reset the cached p-constraint *)
		cached_p_constraint <- None;
		
		(* Try to add the new state to the state space *)
		let addition_result = StateSpace.add_state state_space (self#state_comparison_operator_of_options) new_state in
		
		(* Boolean to check whether the analysis should be terminated immediately *)
		let terminate_analysis_immediately = ref false in
		
		begin
		match addition_result with
		(* If the state was present: do nothing *)
		| StateSpace.State_already_present _ -> ()
		(* If this is really a new state, or a state larger than a former state *)
		| StateSpace.New_state new_state_index | StateSpace.State_replacing new_state_index ->

			(* First check whether this is a bad tile according to the property and the nature of the state *)
			self#update_statespace_nature new_state;
			
			(* Will the state be added to the list of new states (the successors of which will be computed)? *)
			(*** BADPROG: ugly bool ref that may be updated in an IF condition below ***)
			let to_be_added = ref (self#process_state new_state) in
			
			(* If the state is a target state (i.e., process_state returned false) AND the option to stop the analysis as soon as a counterexample is found is activated, then we will throw an exception *)
			terminate_analysis_immediately := options#counterex && not !to_be_added;
			
			(* If to be added: if the state is included into the bad constraint, no need to explore further, and hence do not add *)
			if !to_be_added then(
			
				(* Print some information *)
				if verbose_mode_greater Verbose_medium then(
					self#print_algo_message Verbose_medium "Projecting onto the parameters…";
				);

				(* Project onto the parameters *)
				(*** NOTE: here, we use the cache system ***)
				let p_constraint = self#compute_p_constraint_with_cache new_state.px_constraint in
				
				(* Print some information *)
				self#print_algo_message Verbose_medium "Checking whether the new state is included into known bad valuations…";
				if verbose_mode_greater Verbose_high then(
					self#print_algo_message Verbose_high "\nNew constraint:";
					print_message Verbose_high (LinearConstraint.string_of_p_linear_constraint model.variable_names p_constraint);
					
					self#print_algo_message Verbose_high "\nCurrent bad constraint:";
					print_message Verbose_high (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names bad_constraint);
				);

				(* if p_constraint <= bad_constraint *)
				(*** NOTE: don't perform this test if the associated option is enabled ***)
				if not options#no_leq_test_in_ef then(
					if LinearConstraint.p_nnconvex_constraint_is_leq (LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint p_constraint) bad_constraint then (
						(* Statistics *)
						counter_cut_branch#increment;
						
						(* Print some information *)
						self#print_algo_message Verbose_low "Found a state included in bad valuations; cut branch.";

						(* Do NOT compute its successors; cut the branch *)
						to_be_added := false;
					);
				);
			);

			(* Add the state_index to the list of new states (used to compute their successors at the next iteration) *)
			if !to_be_added then
				new_states_indexes <- new_state_index :: new_states_indexes;
			
		end (* end if new state *)
		;
		
		(*** TODO: move the rest to a higher level function? (post_from_one_state?) ***)
		
		(* Retrieve the new state index *)
		(*** HACK ***)
		let new_state_index = match addition_result with | StateSpace.State_already_present new_state_index | StateSpace.New_state new_state_index | StateSpace.State_replacing new_state_index -> new_state_index in
		
		(* Add the transition to the state space *)
		self#add_transition_to_state_space (source_state_index, combined_transition, new_state_index) addition_result;
		
		
		(* If an immediate termination is requested: raise exception! *)
		if !terminate_analysis_immediately then(
			
			(*------------------------------------------------------------*)
			(* Counter-example reconstruction (to be moved?) *)
			(*------------------------------------------------------------*)
			
			(* Print the counter-example *)
			
			(* Print some information *)
			print_message Verbose_medium "Counterexample found: reconstructing counterexample…";
			
			(* First get the predecessors table *)
			let predecessors = StateSpace.compute_predecessors_with_combined_transitions state_space in
			
			(* Print some information *)
			print_message Verbose_medium "Predecessor table built";

			(* Also retrieve the initial state *)
			let initial_state_index = StateSpace.get_initial_state_index state_space in
			
			(* Get the path, i.e., a list of a pair of a symbolic state followed by a combined transition (the last state does not belong to the path) *)
			let path : (State.state_index * StateSpace.combined_transition) list = StateSpace.backward_path state_space new_state_index initial_state_index (Some predecessors) in
			
			(* Print some information *)
			print_message Verbose_medium "Path built";

			(* Print some information *)
			if verbose_mode_greater Verbose_low then (
				print_message Verbose_low "\nCounter-example found:";
				
				(* Function to pretty-print combined transitions *)
				let debug_string_of_combined_transition combined_transition = string_of_list_of_string_with_sep ", " (
					List.map (fun transition_index ->
						(* Get automaton index *)
						let automaton_index = model.automaton_of_transition transition_index in
						(* Get actual transition *)
						let transition = model.transitions_description transition_index in
						(* Print *)
						ModelPrinter.debug_string_of_transition model automaton_index transition
					) combined_transition
				) in

				(* Iterate *)
				List.iter (fun (state_index, combined_transition) ->
					(* Get actual state *)
					let state = StateSpace.get_state state_space state_index in
				
					print_message Verbose_low (" " ^ (ModelPrinter.string_of_state model state));
					print_message Verbose_low (" | ");
					print_message Verbose_low (" | via combined transition " ^ (debug_string_of_combined_transition combined_transition));
					print_message Verbose_low (" | ");
					print_message Verbose_low (" v ");
				) path;
				
				(* Print target *)
				let final_state = StateSpace.get_state state_space new_state_index in
				print_message Verbose_low (" " ^ (ModelPrinter.string_of_state model final_state));
				
			);
			

			(* Exhibit a concrete parameter valuation in the final state *)
			(*** NOTE: here, we use the cache system ***)
			let p_constraint = self#compute_p_constraint_with_cache new_state.px_constraint in
			let concrete_p_valuation = LinearConstraint.p_exhibit_point p_constraint in
			
			(* Print it *)
			if verbose_mode_greater Verbose_standard then(
				(* Convert to PVal *)
				let pval = new PVal.pval in
				List.iter (fun parameter ->
					pval#set_value parameter (concrete_p_valuation parameter);
				) model.parameters;
				
				print_message Verbose_standard "Example of parameter valuation:";
				print_message Verbose_standard (ModelPrinter.string_of_pi0 model pval);
			);
			
			(* Exhibit a concrete clock+parameter valuation in the final state *)
			let concrete_px_valuation = LinearConstraint.px_exhibit_point new_state.px_constraint in
			
			(* Print it *)
			if verbose_mode_greater Verbose_low then(
				print_message Verbose_low "Example of px-valuation:";
				print_message Verbose_low (ModelPrinter.string_of_px_valuation model concrete_px_valuation);
			);
			
			(* We reconstruct a concrete run, for which we need absolute time *)
			
			
			(*** BEGIN CODE THAT WON'T WORK DUE TO THE DIMENSION HANDLING IN LINEAR.CONSTRAINT ***)
			(*
			(* 1. we backup the model *)
			let original_model = model in
			
			(* 2. We rebuild a model with one more clock *)
			(*** HACK (quite!) ***)
			let extra_clock = model.nb_variables in
			
			
			let px_clocks_non_negative = LinearConstraint.px_constraint_of_nonnegative_variables (original_model.clocks @ [extra_clock]) in
			
			(* Add >= 0 to the initial constraint *)
			let initial_constraint = LinearConstraint.px_intersection [original_model.initial_constraint ; TODO ] in
			
			let px_clocks_non_negative_and_initial_p_constraint = LinearConstraint.px_intersection [px_clocks_non_negative; (LinearConstraint.px_of_p_constraint original_model.initial_p_constraint)
			
			
			let model_with_one_extra_clock =
			{
				(* Cardinality *)
				nb_automata    = original_model.nb_automata;
				nb_actions     = original_model.nb_actions;
				nb_clocks      = original_model.nb_clocks + 1;
				nb_discrete    = original_model.nb_discrete;
				nb_parameters  = original_model.nb_parameters;
				nb_variables   = original_model.nb_variables + 1;
				nb_locations   = original_model.nb_locations;
				nb_transitions = original_model.nb_transitions;

				(* Is there any stopwatch in the model? *)
				has_stopwatches = original_model.has_stopwatches;
				(* Is the model an L/U-PTA? *)
				lu_status = original_model.lu_status;


				(* The observer *)
				observer_pta = original_model.observer_automaton;
				is_observer = original_model.is_observer;

				(* The list of clock indexes *)
				clocks = list_append original_model.clocks [extra_clock];
				(* True for clocks, false otherwise *)
				is_clock = fun variable_index -> if variable_index = extra_clock then true else original_model.is_clock variable_index;
				(* Index of the special clock to be reset at each transition to measure time elapsing (only used in NZ checking) *)
				special_reset_clock = original_model.special_reset_clock;
				(* Index of a special clock meant to measure the global time (how this clock is actually used is up to the model designer *)
				global_time_clock = original_model.global_time_clock;
				(* The list of clock indexes except the reset clock (used, e.g., to print the model *)
				clocks_without_special_reset_clock = list_append original_model.clocks_without_special_reset_clock [extra_clock];
				(* The list of discrete indexes *)
				discrete = original_model.discrete;
				(* True for discrete, false otherwise *)
				is_discrete = original_model.is_discrete;
				(* The list of parameter indexes *)
				parameters = original_model.parameters;
				(* The non parameters (clocks and discrete) *)
				clocks_and_discrete = list_append (list_append clocks discrete) [extra_clock];
				(* The non clocks (parameters and discrete) *)
				parameters_and_discrete = list_append parameters discrete;
				(* The non discrete (clocks and parameters) *)
				parameters_and_clocks = list_append (list_append parameters clocks) [extra_clock];
				(* The function : variable_index -> variable name *)
				variable_names = fun variable_index -> if variable_index = extra_clock then "x_absolute" else original_model.variable_names variable_index;
				(* The type of variables *)
				type_of_variables = fun variable_index -> if variable_index = extra_clock then Var_type_clock else original_model.type_of_variables variable_index;

				(* The automata *)
				automata = original_model.automata;
				(* The automata names *)
				automata_names = original_model.automata_names;

				(* The locations for each automaton *)
				locations_per_automaton = original_model.locations_per_automaton;
				(* The location names for each automaton *)
				location_names = original_model.location_names;
				(* The location names for each automaton *)
				(*** HACK ***)
				is_urgent = original_model.is_urgent;

				(* All action indexes *)
				actions = original_model.actions;
				(* Action names *)
				action_names = original_model.action_names;
				(* The type of actions *)
				action_types = original_model.action_types;
				(* The list of actions for each automaton *)
				actions_per_automaton = original_model.actions_per_automaton;
				(* The list of automatons for each action *)
				automata_per_action = original_model.automata_per_action;
				(* The list of actions for each automaton for each location *)
				actions_per_location = original_model.actions_per_location;

				(* The cost for each automaton and each location *)
				costs = original_model.costs;

				(* The invariant for each automaton and each location *)
				invariants = original_model.invariants;
				(* The transitions for each automaton and each location and each action *)
				transitions = original_model.transitions;
				(* The list of clocks stopped for each automaton and each location *)
				stopwatches = original_model.stopwatches;
				(* An array transition_index -> transition *)
				transitions_description = original_model.transitions_description;
				(* An array transition_index -> automaton_index *)
				automaton_of_transition = original_model.automaton_of_transition;

				(* All clocks non-negative *)
				px_clocks_non_negative = px_clocks_non_negative;
				(* Initial location of the model *)
				initial_location = original_model.initial_location;
				(* Initial constraint of the model *)
				initial_constraint = initial_constraint;
				(* Initial constraint of the model projected onto P *)
				initial_p_constraint = original_model.initial_p_constraint;
				(* Initial constraint of the model projected onto P and all clocks non-negative *)
				px_clocks_non_negative_and_initial_p_constraint = px_clocks_non_negative_and_initial_p_constraint;


				(* Property defined by the user *)
				user_property = original_model.property;
				(* Property defined by the model *)
				correctness_condition = original_model.correctness_condition;
				(* List of parameters to project the result onto *)
				projection = original_model.projection;
				(* Parameter to be minimized or maximized *)
				optimized_parameter = original_model.optimization;
			}
			in
			
			*)
			(*** END CODE THAT WON'T WORK DUE TO THE DIMENSION HANDLING IN LINEAR.CONSTRAINT ***)
			
			(* Now construct valuations for the whole path, i.e. pair (time elapsed, valuation) *)
			
			print_message Verbose_low "\nBuilding concrete path:";
			
			(*** TODO: change this system one day, as it costs us one more clock in the ENTIRE analysis, although it would only be used in the path regeneration ***)
			
			(* Get the absolute time clock *)
			let absolute_time_clock = match model.global_time_clock with
				| Some clock_index -> clock_index
				| None -> raise (InternalError ("No absolute time clock is defined in the model, which is (so far) necessary to reconstruct the counterexample."))
			in
			
			
			(* Iterate starting from s_n and going backward *)
			
			let valuation_n_plus_1 = ref concrete_px_valuation in
			let te_and_valuations = ref [] in
			
			for n = List.length path - 1 downto 0 do
			
				print_message Verbose_low ("\n\nComputing concrete valuation in path at position " ^ (string_of_int n) ^ "…");
				
				(* Get the values *)
				
				let state_index_n_plus_1 = if n = List.length path - 1 then new_state_index else (let first,_ = List.nth path (n+1) in first) in
				let state_index_n, combined_transition_n = List.nth path n in
				
				(* Get state n *)
				let state_n = StateSpace.get_state state_space state_index_n in
				(* Get state n+1 *)
				let state_n_plus_1 = StateSpace.get_state state_space state_index_n_plus_1 in
				
				(* Get the zones *)
				let location_n, z_n = state_n.global_location, state_n.px_constraint in
				let z_n_plus_1 = state_n_plus_1.px_constraint in
				
				(* Get the n-1 elements *)
				let z_n_minus_1, continuous_guard_n_minus_1, updates_n_minus_1 =
					(* Normal case *)
					if n > 0 then(
						(* Get the state index *)
						let state_index_n_minus_1, combined_transition_n_minus_1 = List.nth path (n-1) in

						(* Get the state *)
						let state_n_minus_1 = StateSpace.get_state state_space state_index_n_minus_1 in
						(* Get location and constraint *)
						let location_n_minus_1, z_n_minus_1 = state_n_minus_1.global_location, state_n_minus_1.px_constraint in
						
						(* Reconstruct the continuous guard from n-1 to n *)
						(*** WARNING/BADPROG/TOOPTIMIZE: double computation as we recompute it at the next n-1 ***)
						let _, _, continuous_guards_n_minus_1, updates_n_minus_1 = compute_new_location_guards_updates location_n_minus_1 combined_transition_n_minus_1 in
						
						z_n_minus_1, LinearConstraint.pxd_intersection continuous_guards_n_minus_1, updates_n_minus_1
					
					(* Case "before" 0 *)
					) else(
						(* True "previous" zone *)
						LinearConstraint.px_true_constraint(),
						(* Guard: set the absolute time clock to 0 *)
						LinearConstraint.pxd_constraint_of_point [(absolute_time_clock, NumConst.zero)],
						(* No updates *)
						[]
				)
				in
				
				(* Get all updates from the combined transition n *)
				let clock_updates, _ = AlgoStateBased.get_updates_in_combined_transition location_n combined_transition_n in
				
				(* Reconstruct the continuous guard from n to n+1 *)
				let _, _, continuous_guards_n, _ = compute_new_location_guards_updates location_n combined_transition_n in
				let continuous_guard_n = LinearConstraint.pxd_intersection continuous_guards_n in
				
(*				(* Compute the set of clocks impacted by the updates *)
				let resets = match clock_updates with
				| No_update -> []
				| Resets cl -> cl
				| Updates updates ->
(*					(* We need to check that no update in the form x -> f(x') or x -> f(p) occurs, otherwise the method does not work *)
					(*** IN FACT it does not matter ***)
					List.iter (fun (clock_index, pxd_linear_term) -> 
						(* Get the list of variables in the term *)
						let variables = 
						(* If clocks or parameters: problem *)
						
					) updates;*)
					
					(* Return the list of clocks *)
					let clocks, _ = List.split updates in
					clocks
				
				in*)
				
				(* Build the constraint from the valuation *)
				(* Convert to set of pairs *)
(*				let pairs = List.map (fun variable_index ->
					variable_index , (!valuation_n_plus_1 variable_index)
				) model.parameters_and_clocks in

				let constraint_from_valuation_n_plus_1 = LinearConstraint.px_constraint_of_point pairs in*)
				
				(* Get the elapsed and stopped clocks (+ other variables) *)
				let stopped_clocks, elapsing_clocks = compute_stopwatches location_n in
				let all_stopped_variables = List.rev_append stopped_clocks model.parameters_and_discrete in
				
				(* Compute a set of valuations that can be a predecessor of the valuation of n+1 *)
				let predecessors_of_valuation_n_plus_1 = AlgoStateBased.constraint_zone_predecessor_g_u
					(* Zn-1 *) z_n_minus_1
					(* gn-1 *) continuous_guard_n_minus_1
					(* Un-1 *) updates_n_minus_1
					(* Zn *)   z_n
					(* t *)    elapsing_clocks
					(* nont *) all_stopped_variables
					(* gn *)   continuous_guard_n
					(*** NOTE: a bit twice the work here, as they were processed by get_updates_in_combined_transition ***)
					(* Un *)   [clock_updates]
					(* Zn+1 *) z_n_plus_1
					in
				
				
				(* Pick a valuation *)
				let valuation_n = LinearConstraint.px_exhibit_point predecessors_of_valuation_n_plus_1 in
				
				(* Now compute the time spent between the previous and the new valuation *)

				(* Compute the time elapsing *)
				let time_elapsed_n = NumConst.sub (!valuation_n_plus_1 absolute_time_clock) (valuation_n absolute_time_clock) in
				
				(*** DEBUG: test that it is indeed a good valuation, belonging to n! ***)
				(*** TODO ***)
				
				(* Print some information *)
				if verbose_mode_greater Verbose_medium then(
					print_message Verbose_medium ("Valuation " ^ (string_of_int n) ^ " just computed:\n" ^ (ModelPrinter.string_of_px_valuation model valuation_n));
				);
				
				(*** NOTE: we need a px AND d valuation, therefore a bit a hack here ***)
				let pxd_valuation = fun variable_index ->
					match model.type_of_variables variable_index with
					| Var_type_clock | Var_type_parameter -> valuation_n variable_index
					| Var_type_discrete -> Location.get_discrete_value location_n variable_index
				in
				
				(* Add the valuation to the list, and replace n+1 with n *)
				te_and_valuations := (time_elapsed_n , pxd_valuation) :: !te_and_valuations;
				valuation_n_plus_1 := valuation_n;
			
			done;

			
			(*** TODO: initial valuation + initial time elapsing ***)
			(* Get 2nd time elapsing and 2nd valuation *)
(*			let time_elapsed_2, concrete_px_valuation_2 = match !te_and_valuations, path with 
				| time_elapsed :: _, concrete_px_valuation :: _ -> time_elapsed , concrete_px_valuation
				| _ -> raise (InternalError ("Empty lists spotted while reconstructing concrete run"))
			in*)
			
			
			(* Compute 1st valuation, i.e., a predecessor of concrete_px_valuation_2 intersected with absolute_time_clock = 0 *)
			
			
			(*** NOTE: alternatively, we could just take the initial state and intersect with absolute_time_clock = 0 ***)
			
			
			(* Print some information *)
			if verbose_mode_greater Verbose_medium then(
				(* Print the list*)
			
				(* Dummy counter for printing *)
				let i = ref 0 in
				List.iter (fun (time_elapsed, valuation) -> 
					incr i;
					print_message Verbose_low ("Valuation " ^ (string_of_int !i) ^ ":");
					print_message Verbose_low (ModelPrinter.string_of_px_valuation model valuation);
					print_message Verbose_low ("Time elapsing: " ^ (NumConst.string_of_numconst time_elapsed) ^ "");
				) !te_and_valuations;
				(* Print last one *)
				print_message Verbose_low (ModelPrinter.string_of_px_valuation model concrete_px_valuation);
			);
			
			(*** NOTE: we need a px AND d valuation, therefore a bit a hack here ***)
			let concrete_pxd_valuation = fun variable_index ->
				match model.type_of_variables variable_index with
				| Var_type_clock | Var_type_parameter -> concrete_px_valuation variable_index
				| Var_type_discrete -> Location.get_discrete_value new_state.global_location variable_index
			in
			
			(* Create a representation with the absolute time, and the last element too *)
			let valuations_and_time = (List.map (fun (_, valuation) -> valuation , (valuation absolute_time_clock) ) !te_and_valuations) @ [concrete_pxd_valuation , (concrete_pxd_valuation absolute_time_clock)] in
			
			(* Generate the graphics *)
			Graphics.draw_valuations valuations_and_time (options#files_prefix ^ "_signals");

			
			(*------------------------------------------------------------*)
			(* END Counter-example reconstruction *)
			(*------------------------------------------------------------*)

			(* Update termination status *)
			(*** NOTE/HACK: the number of unexplored states is not known, therefore we do not add it… ***)
			self#print_algo_message Verbose_standard "Target state found! Terminating…";
			termination_status <- Some Target_found;
			
			raise TerminateAnalysis;
		);
	
		(* Statistics *)
		counter_add_a_new_state#stop;

		(* The state is kept in any case *)
		true
(*** WARNING/BADPROG: what preceedes is partially copy/paste to AlgoPRP.ml ***)
	

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Actions to perform when meeting a state with no successors: nothing to do for this algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_deadlock_state state_index = ()
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Actions to perform at the end of the computation of the *successors* of post^n (i.e., when this method is called, the successors were just computed). Nothing to do for this algorithm. *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_post_n (post_n : State.state_index list) = ()

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Check whether the algorithm should terminate at the end of some post, independently of the number of states to be processed (e.g., if the constraint is already true or false) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** TODO: could be stopped when the bad constraints are equal to the initial p-constraint ***)
	method check_termination_at_post_n = false

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual compute_result : Result.imitator_result


	
(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
