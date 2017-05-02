(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: EFsynth algorithm [JLR15]
 * 
 * File contributors : Étienne André
 * Created           : 2015/11/25
 * Last modified     : 2017/05/02
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
	(** Process a symbolic state *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private process_state state =
		(* Print some information *)
		if verbose_mode_greater Verbose_medium then(
			self#print_algo_message Verbose_medium "Entering process_state…";
		);

		(* Retrieve the model *)
		let model = Input.get_model () in
		
		let state_location, state_constraint = state in
		
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
				
				if LinearConstraint.p_nnconvex_constraint_is_leq (LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint p_constraint) bad_constraint then(
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
		(* Return result *)
		to_be_added
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Actions to perform with the initial state; returns true unless the initial state cannot be kept (in which case the algorithm will stop immediately) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_initial_state initial_state = self#process_state initial_state

	
	(* Compute the p-constraint only if it is not cached *)
	method private compute_p_constraint_with_cache px_linear_constraint =
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
			(* Return the value in cach *)
			p_constraint
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Add a new state to the state_space (if indeed needed) *)
	(* Side-effects: modify new_states_indexes *)
	(*** TODO: move new_states_indexes to a variable of the class ***)
	(* Return true if the state is not discarded by the algorithm, i.e., if it is either added OR was already present before *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** WARNING/BADPROG: the following is partially copy/paste to AlgoPRP.ml ***)
	method add_a_new_state source_state_index new_states_indexes action_index location (current_constraint : LinearConstraint.px_linear_constraint) =
		(* Print some information *)
		if verbose_mode_greater Verbose_medium then(
			self#print_algo_message Verbose_medium "Entering add_a_new_state (and reset cache)…";
		);
		
		(* Reset the cached p-constraint *)
		cached_p_constraint <- None;
		
		(* Retrieve the model *)
		let model = Input.get_model () in

		(* Build the state *)
		let new_state = location, current_constraint in

		(* Try to add the new state to the state space *)
		let addition_result = StateSpace.add_state state_space (self#state_comparison_operator_of_options) new_state in
		
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
			
			(* If to be added: if the state is included into the bad constraint, no need to explore further, and hence do not add *)
			if !to_be_added then(
			
				(* Print some information *)
				if verbose_mode_greater Verbose_medium then(
					self#print_algo_message Verbose_medium "Projecting onto the parameters…";
				);

				(* Project onto the parameters *)
				(*** NOTE: here, we use the cache system ***)
				let p_constraint = self#compute_p_constraint_with_cache current_constraint in
				
				(* Print some information *)
				self#print_algo_message Verbose_medium "Checking whether the new state is included into known bad valuations…";
				if verbose_mode_greater Verbose_high then(
					self#print_algo_message Verbose_high "\nNew constraint:";
					print_message Verbose_high (LinearConstraint.string_of_p_linear_constraint model.variable_names p_constraint);
					
					self#print_algo_message Verbose_high "\nCurrent bad constraint:";
					print_message Verbose_high (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names bad_constraint);
				);

				(* if p_constraint <= bad_constraint *)
				if LinearConstraint.p_nnconvex_constraint_is_leq (LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint p_constraint) bad_constraint then (
					(* Statistics *)
					counter_cut_branch#increment;
					
					(* Print some information *)
					self#print_algo_message Verbose_low "Found a state included in bad valuations; cut branch.";

					(* Do NOT compute its successors; cut the branch *)
					to_be_added := false;
				);
			);

			(* Add the state_index to the list of new states (used to compute their successors at the next iteration) *)
			if !to_be_added then
				new_states_indexes := new_state_index :: !new_states_indexes;
			
		end (* end if new state *)
		;
		
		(*** TODO: move the rest to a higher level function? (post_from_one_state?) ***)
		
		(* Add the transition to the state space *)
		self#add_transition_to_state_space (source_state_index, action_index, (*** HACK ***) match addition_result with | StateSpace.State_already_present new_state_index | StateSpace.New_state new_state_index | StateSpace.State_replacing new_state_index -> new_state_index) addition_result;
	
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
