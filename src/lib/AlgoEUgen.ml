(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: generic EFsynth algorithm [JLR15]
 * 
 * File contributors : Étienne André
 * Created           : 2015/11/25
 *
 ************************************************************)


(************************************************************)
(************************************************************)
(* Modules *)
(************************************************************)
(************************************************************)
(* open OCamlUtilities *)
open ImitatorUtilities
open AbstractModel
open AbstractProperty
open AlgoStateBased
open Statistics
open State




(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class virtual algoEUgen (model : AbstractModel.abstract_model) (property : AbstractProperty.abstract_property) (options : Options.imitator_options) (state_predicate_phi_option : AbstractProperty.state_predicate option) (state_predicate_psi : AbstractProperty.state_predicate) (timed_interval_option : AbstractProperty.timed_interval option) =
	object (self) inherit algoStateBased model options (*as super*)
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	(*------------------------------------------------------------*)
	(* Constants *)
	(*------------------------------------------------------------*)
	(** The constraint to be added to a symbolic state to check whether it matches the timed interval (if any) *)
	val timed_interval_constraint_option : LinearConstraint.px_linear_constraint option =
		match timed_interval_option with
		| Some timed_interval -> Some (AlgoStateBased.px_linear_constraint_of_timed_interval model timed_interval)
		| None -> None

	(** The constraint to check whether a constraint is already beyond the upper bound of an interval *)
	val timed_interval_upper_bound_constraint_option : LinearConstraint.px_linear_constraint option =
		match timed_interval_option with
		| Some timed_interval -> AlgoStateBased.upper_bound_px_linear_constraint_option_of_timed_interval model timed_interval
		| None -> None

	(*------------------------------------------------------------*)
	(* Counters *)
	(*------------------------------------------------------------*)

	(*** NOTE: if EF is called several times, then each call will create a counter ***)
	
	(* The target state has been found *)
	val counter_found_target = create_discrete_counter_and_register "found target state" PPL_counter Verbose_low

	(* Methods counters *)
	val counter_process_state = create_hybrid_counter_and_register "EFsynth.process_state" States_counter Verbose_experiments
	val counter_add_a_new_state = create_hybrid_counter_and_register "EFsynth.add_a_new_state" States_counter Verbose_experiments



	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Process a symbolic state: returns a pair of Booleans (to_be_explored, is_target) such that to_be_explored = true if the state be added to the next states to explore, and is_target iff it is a target state ("psi predicate") *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private process_state (state : State.state) : (bool * bool) =
	
		(* Statistics *)
		counter_process_state#increment;
		counter_process_state#start;
	
		(* Print some information *)
		if verbose_mode_greater Verbose_medium then(
			self#print_algo_message Verbose_medium "Entering process_state…";
		);

		let state_constraint = state.px_constraint in
		
		let result =
			(* Check whether the current location matches one of the target (final) locations *)
			if State.match_state_predicate_and_timed_constraint model state_predicate_psi timed_interval_constraint_option state then(
			
				(* Print some information *)
				if verbose_mode_greater Verbose_medium then(
					self#print_algo_message Verbose_medium "Projecting onto the parameters (using the cache)…";
				);

				(* If timed version: first add the timed_interval_constraint_option to the resulting state *)
				let state_constraint_for_projection : LinearConstraint.px_linear_constraint = AlgoStateBased.intersect_with_timed_interval_constraint_option model timed_interval_constraint_option state_constraint in

				(* Project onto the parameters *)
				(*** NOTE: here, we use the mini-cache system ***)
				let p_constraint = self#compute_p_constraint_with_minicache state_constraint_for_projection in

(*				(* Projecting onto some parameters if required by the property *)
				let p_constraint = AlgoStateBased.project_p_constraint_if_requested model property p_constraint in*)

				(* Statistics *)
				counter_found_target#increment;
				
				(*** TODO: test if these two operations are more or less expensive than just adding without testing ***)
				(*** NOTE: advantage of doing it twice: print less information :-) ***)
				
				(*** NOTE: do NOT do it if NOT cumulative_pruning ***)
				
				if (options#cumulative_pruning) && LinearConstraint.p_nnconvex_constraint_is_leq (LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint p_constraint) synthesized_constraint then(
					self#print_algo_message Verbose_low "Found a target state (but the constraint was already known).";
				)else(
					(* The constraint is new! *)
					
					(* Print some information *)
					self#print_algo_message Verbose_standard "Found a new target state.";
						
					(* Update the bad constraint using the current constraint *)
					(*** NOTE: perhaps try first whether p_constraint <= synthesized_constraint ? ***)
					LinearConstraint.p_nnconvex_p_union_assign synthesized_constraint p_constraint;
					
					(* Print some information *)
					if verbose_mode_greater Verbose_low then(
						self#print_algo_message Verbose_medium "Adding the following constraint to the synthesized constraint:";
						print_message Verbose_low (LinearConstraint.string_of_p_linear_constraint model.variable_names p_constraint);
						
						self#print_algo_message Verbose_low "The synthesized constraint is now:";
						print_message Verbose_low (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names synthesized_constraint);
					);
				); (* end if new bad constraint *)
				
				(* Do NOT compute its successors; cut the branch; and return it is a target state *)
				false, true
				
			) (* end if match state predicate *)
			else(
				self#print_algo_message Verbose_medium "State not matching a target location.";

				(* Case timed version: Cut branch if we went too far time-wise *)
				let time_went_too_far =
				match timed_interval_upper_bound_constraint_option with
				| Some timed_interval_upper_bound_constraint ->
					let checking_time_went_too_far : LinearConstraint.px_linear_constraint = LinearConstraint.px_intersection [state_constraint; timed_interval_upper_bound_constraint] in

					(* Unsatisfiable: cut branch! *)
					if LinearConstraint.px_is_false checking_time_went_too_far then(
						(* Print some information *)
						if verbose_mode_greater Verbose_medium then(
							self#print_algo_message Verbose_medium "Cut branch as the state constraint:";
							print_message Verbose_medium (LinearConstraint.string_of_px_linear_constraint model.variable_names state_constraint);
							self#print_algo_message Verbose_medium "is beyond the timed operator:";
							print_message Verbose_medium (LinearConstraint.string_of_px_linear_constraint model.variable_names timed_interval_upper_bound_constraint);
						);
						true
					)else false

				| None -> false
				in
				if time_went_too_far then(
					(* Cut branch! *)
					false, false
				)else(
					(* Normal case *)
					(* In case algorithm EU: check whether the first predicate (temporary, "phi") is matched *)
					match state_predicate_phi_option with
					| Some state_predicate_phi ->
						(* Check whether the current location matches one of the target phi predicates *)
						if State.match_state_predicate model state_predicate_phi state then(
							self#print_algo_message Verbose_medium "State matching a phi predicate: keep";
							true, false
						)else(
							self#print_algo_message Verbose_medium "State NOT matching a phi predicate: discard";
							false, false
						)

					| None ->
						(* No phi predicate (a.k.a. True) -> Keep the state *)
						true, false
				) (* end if time went too far *)
			) (* end if not match state predicate *)
		
		in
		
		(* Statistics *)
		counter_process_state#stop;

		(* Return result *)
		result
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Actions to perform with the initial state; returns None unless the initial state cannot be kept, in which case the algorithm returns an imitator_result *)
	(*** NOTE: this function is redefined here ***)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method! try_termination_at_initial_state : Result.imitator_result option =
		(* Retrieve the initial state *)
		let initial_px_constraint : LinearConstraint.px_linear_constraint = self#get_initial_px_constraint_or_die in
		let initial_state : State.state = {global_location = model.initial_location ; px_constraint = initial_px_constraint} in


		let to_be_explored, _ = self#process_state initial_state in
		if to_be_explored then None
		else(
			(* Set termination status *)
			termination_status <- Some (Result.Regular_termination);

			(* Terminate *)
			Some (self#compute_result)
		)



	
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
			self#print_algo_message Verbose_medium "Entering `add_a_new_state` (and reset cache)…";
		);
		
		(* Reset the mini-cache (for the p-constraint) *)
		self#reset_minicache;

		(* Case IH: apply IH to the state before its (potential) addition *)
		let state_to_add = if options#ih then (
			let global_location: DiscreteState.global_location = new_state.global_location in
			let px_constraint : LinearConstraint.px_linear_constraint = new_state.px_constraint in
			(* Apply IH and rebuild state *)
			{
				global_location = global_location;
				px_constraint = LinearConstraint.px_ih px_constraint;
			}
		)else new_state in

		(* Try to add the new state to the state space *)
		let addition_result = state_space#add_state options#comparison_operator model.global_time_clock state_to_add in
		
		(* Boolean to check whether the state is a target state *)
		let is_target = ref false in
		
		begin
		match addition_result with
		(* If the state was present: do nothing *)
		| StateSpace.State_already_present _ -> ()

		(* If this is really a new state, or a state larger than a former state *)
		| StateSpace.New_state new_state_index | StateSpace.State_replacing new_state_index ->

			(* Will the state be added to the list of new states (the successors of which will be computed)? *)
			(*** NOTE: if the answer is false, then the new state is a target state ***)
			(*** BADPROG: ugly bool ref that may be updated in an IF condition below ***)
			let is_to_be_added, is_a_target = self#process_state new_state in
			let to_be_added = ref is_to_be_added in
			
			(* Update the target flag *)
			is_target := is_a_target;
			
			(* If to be added: if the state is included into the synthesized constraint, no need to explore further, and hence do not add *)
			if !to_be_added then(
			
				(*** NOTE: do NOT perform this test depending on the option ***)
				if options#cumulative_pruning then(
					(* Check whether new_state.px_constraint <= synthesized_constraint *)
					if self#check_whether_px_included_into_synthesized_constraint new_state.px_constraint then(
						(* Print some information *)
						self#print_algo_message Verbose_low "Found a state included in synthesized valuations; cut branch.";

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

		
		
		(* Case accepting state *)
		if !is_target then(
			(* 1. Construct counterexample if requested by the algorithm (and stop termination by raising a TerminateAnalysis exception, if needed) *)
			if property.synthesis_type = Exemplification then(
				self#construct_counterexamples new_state_index;
			);
			
			(* 2. If #witness mode, then we will throw an exception *)
			self#terminate_if_witness property;
		); (* end if target *)


		
		(* Statistics *)
		counter_add_a_new_state#stop;

		(* The state is kept in any case *)
		true
(*** WARNING/BADPROG: what preceedes is partially copy/paste to AlgoPRP.ml ***)
	

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Actions to perform when meeting a state with no successors: nothing to do for this algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_deadlock_state _ = ()
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Actions to perform at the end of the computation of the *successors* of post^n (i.e., when this method is called, the successors were just computed). Nothing to do for this algorithm. *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_post_n (_ : State.state_index list) = ()

	
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
