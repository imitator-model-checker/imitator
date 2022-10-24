(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
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
class virtual algoEFgen (model : AbstractModel.abstract_model) (state_predicate : AbstractProperty.state_predicate) =
	object (self) inherit algoStateBased model as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	(* Counters *)
	(*** NOTE: if EF is called several times, then each call will create a counter ***)
	
	(* The bad state has been found *)
	val counter_found_bad = create_discrete_counter_and_register "found target state" PPL_counter Verbose_low
	(* Methods counters *)
	val counter_process_state = create_hybrid_counter_and_register "EFsynth.process_state" States_counter Verbose_experiments
	val counter_add_a_new_state = create_hybrid_counter_and_register "EFsynth.add_a_new_state" States_counter Verbose_experiments


	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
		super#initialize_variables;
		
		(*** NOTE: duplicate operation ***)
		synthesized_constraint <- LinearConstraint.false_p_nnconvex_constraint ();

		(* The end *)
		()
	

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Process a symbolic state: returns false if the state is a target state (and should not be added to the next states to explore), true otherwise *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private process_state (state : State.state) : bool =
	
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
			if State.match_state_predicate model model.is_accepting state_predicate state then(
			
				(* Print some information *)
				if verbose_mode_greater Verbose_medium then(
					self#print_algo_message Verbose_medium "Projecting onto the parameters (using the cache)…";
				);

				(* Project onto the parameters *)
				(*** NOTE: here, we use the mini-cache system ***)
				let p_constraint = self#compute_p_constraint_with_minicache state_constraint in

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
				
				if (not options#no_leq_test_in_ef) && LinearConstraint.p_nnconvex_constraint_is_leq (LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint p_constraint) synthesized_constraint then(
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
(* 	method process_initial_state initial_state = self#process_state initial_state *)


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Actions to perform with the initial state; returns None unless the initial state cannot be kept, in which case the algorithm returns an imitator_result *)
	(*** NOTE: this function is redefined here ***)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method try_termination_at_initial_state : Result.imitator_result option =
		(* Retrieve the initial state *)
		let initial_px_constraint : LinearConstraint.px_linear_constraint = self#get_initial_px_constraint_or_die in
		let initial_state : State.state = {global_location = model.initial_location ; px_constraint = initial_px_constraint} in


		if self#process_state initial_state then None
		else(
			(* Set termination status *)
			termination_status <- Some (Result.Regular_termination);

			(* Terminate *)
			Some (self#compute_result)
		)


(*	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Generate counter-example(s) if required by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	
	(*** NOTE: old version from EFSynth ***)

	method construct_counterexamples target_state_index =
		()
		
		(*** NOTE: temporarily (?) disabled 2021/07 by ÉA, because merging is used by default, but not sound for counterexample reconstruction ***)
(*		(* Only process counterexample if needed *)
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
		
		(* The end *)
		()
		*)*)


	
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
		
		(* Try to add the new state to the state space *)
		let addition_result = state_space#add_state options#comparison_operator new_state in
		
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
			
			(* If to be added: if the state is included into the synthesized constraint, no need to explore further, and hence do not add *)
			if !to_be_added then(
			
				(*** NOTE: don't perform this test if the associated option is enabled ***)
				if not options#no_leq_test_in_ef then(
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
			let property = Input.get_property() in
			if property.synthesis_type = Exemplification then(
				self#construct_counterexamples new_state_index;
			);
			
			(* 2. If #witness mode, then we will throw an exception *)
			self#terminate_if_witness;
		); (* end if target *)


		
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
