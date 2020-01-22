(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: EFsynth algorithm [JLR15]
 * 
 * File contributors : Étienne André
 * Created           : 2015/11/25
 * Last modified     : 2020/01/22
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
open AbstractProperty
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
	
	(*** NOTE: dummy initialization ***)
	val state_predicate : AbstractProperty.state_predicate =
		(*** TODO: pass as a PARAMETER of the algorithm ***)
		(*** UGLY!!! ***)
		match (Input.get_property()).property with
			| EF state_predicate
			| AGnot state_predicate
				-> state_predicate
			
			(*** TODO ***)
			
			| _ -> raise (InternalError ("Unexpected error when getting the state predicate when initializing EF"))
		
	

	
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
(* 	method algorithm_name = "EF virtual" *)
	
	
	
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

		let state_constraint = state.px_constraint in
		
		let to_be_added =
			(* Check whether the current location matches one of the unreachable global locations *)
			if State.match_state_predicate state_predicate state then(
			
				(* Print some information *)
				if verbose_mode_greater Verbose_medium then(
					self#print_algo_message Verbose_medium "Projecting onto the parameters (using the cache)…";
				);

				(* Project onto the parameters *)
				(*** NOTE: here, we use the cache system ***)
				let p_constraint = self#compute_p_constraint_with_cache state_constraint in

				(* Projecting onto SOME parameters if required *)
				(*** BADPROG: Duplicate code (AlgoLoopSynth / AlgoPRP) ***)
				if Input.has_property() then(
					let abstract_property = Input.get_property() in
					match abstract_property.projection with
					(* Unchanged *)
					| None -> ()
					(* Project *)
					| Some parameters ->
						(* Print some information *)
						if verbose_mode_greater Verbose_high then
							self#print_algo_message Verbose_high "Projecting onto some of the parameters…";

						(*** TODO! do only once for all… ***)
						let all_but_projectparameters = list_diff model.parameters parameters in
						
						(* Eliminate other parameters *)
						LinearConstraint.p_hide_assign all_but_projectparameters p_constraint;

						(* Print some information *)
						if verbose_mode_greater Verbose_medium then(
							print_message Verbose_medium (LinearConstraint.string_of_p_linear_constraint model.variable_names p_constraint);
						);
				); (* end if projection *)

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
					LinearConstraint.p_nnconvex_p_union_assign bad_constraint p_constraint;
					
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
				
			) (* end if match state predicate *)
			else(
				self#print_algo_message Verbose_medium "State not corresponding to the one wanted.";
				
				(* Keep the state as it is not a bad state *)
				true
			) (* end if not match state predicate *)
		
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
	(* Generate counter-example(s) if required by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	method process_counterexample target_state_index =
	(* Only process counterexample if needed *)
		let property = Input.get_property() in
		if property.synthesis_type = Witness then(
			
			(*** NOTE: so far, the reconstruction needs an absolute time clock ***)
			begin
			match model.global_time_clock with
				| None -> print_warning "No counterexample reconstruction, as the model requires an absolute time clock.";
		
				| Some _ ->
					let concrete_run = AlgoStateBased.reconstruct_counterexample state_space target_state_index in

					(* Generate the graphics *)
					Graphics.draw_concrete_run concrete_run (options#files_prefix ^ "_signals");
			end;
		);
		
		(* If the state is a target state (i.e., process_state returned false) AND the option to stop the analysis as soon as a counterexample is found is activated, then we will throw an exception *)
		if property.synthesis_type = Witness (*&& !is_target*) then(
			(* Update termination status *)
			(*** NOTE/HACK: the number of unexplored states is not known, therefore we do not add it… ***)
			self#print_algo_message Verbose_standard "Target state found! Terminating…";
			termination_status <- Some Target_found;
		
			raise TerminateAnalysis;
		);
		
		(* The end *)
		()
	

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Add a new state to the state space (if indeed needed) *)
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
(* 		let terminate_analysis_immediately = ref false in *)
		
		(* Boolean to check whether the state is a target state *)
		let is_target = ref false in
		
		begin
		match addition_result with
		(* If the state was present: do nothing *)
		| StateSpace.State_already_present _ -> ()
		(* If this is really a new state, or a state larger than a former state *)
		| StateSpace.New_state new_state_index | StateSpace.State_replacing new_state_index ->

			(* First check whether this is a bad tile according to the property and the nature of the state *)
			self#update_statespace_nature new_state;
			
			(* Will the state be added to the list of new states (the successors of which will be computed)? *)
			(*** NOTE: if the answer is false, then the new state is a target state ***)
			(*** BADPROG: ugly bool ref that may be updated in an IF condition below ***)
			let to_be_added = ref (self#process_state new_state) in
			
			(* Update the target flag *)
			is_target := not !to_be_added;
			
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
		
		(*** TODO: move the two following statements to a higher level function? (post_from_one_state?) ***)
		
		(* Retrieve the new state index *)
		(*** HACK ***)
		let new_state_index = match addition_result with | StateSpace.State_already_present new_state_index | StateSpace.New_state new_state_index | StateSpace.State_replacing new_state_index -> new_state_index in
		
		(* Add the transition to the state space *)
		self#add_transition_to_state_space (source_state_index, combined_transition, new_state_index) addition_result;

		
		(* Construct counterexample if requested by the algorithm (and stop termination by raising a TerminateAnalysis exception, if needed) *)
		if !is_target then
			self#process_counterexample new_state_index;
		
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
