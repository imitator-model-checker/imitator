(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * 
 * Module description: Parametric timed game with reachability condition
 * 
 * File contributors : Étienne André
 * Created           : 2022/11/29
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
class algoPTG (model : AbstractModel.abstract_model) (state_predicate : AbstractProperty.state_predicate) =
	object (self) inherit algoStateBased model as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	(* Depth in the explored state space *)
	(*** NOTE: private ***)
	val mutable bfs_current_depth = 0

	(*------------------------------------------------------------*)
	(* Counters *)
	(*------------------------------------------------------------*)

	(* The target state has been found *)
	val counter_found_target = create_discrete_counter_and_register "found target state" PPL_counter Verbose_low

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
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "PTG"


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Process a symbolic state: returns false if the state is a target state (and should not be added to the next states to explore), true otherwise *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** WARNING (2022/11, ÉA): copied from AlgoEF ***)
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
				counter_found_target#increment;
				
				(*** TODO: test if these two operations are more or less expensive than just adding without testing ***)
				(*** NOTE: advantage of doing it twice: print less information :-) ***)
				
				(*** NOTE: do NOT do it in mode no_leq_test_in_ef ***)

				(*** NOTE (ÉA, 2022/11): this is the "cumulative pruning" *)

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
	(*** END WARNING (2022/11, ÉA): copied from AlgoEF ***)

	
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
(*** WARNING/BADPROG: what precedes is partially copy/paste to AlgoPRP.ml ***)
	

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
	(* Main method to run the BFS algorithm  *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** WARNING (2022/11, ÉA): copied from AlgoStateBased ***)
	method explore_layer_bfs init_state_index =

		(* Statistics *)
(*		counter_explore_using_strategy#increment;
		counter_explore_using_strategy#start;*)

		(* Set the depth to 1 *)
		bfs_current_depth <- 1;

		(* To check whether the time limit / state limit is reached *)
		limit_reached <- Keep_going;

		(* Flag modified by the algorithm to perhaps terminate earlier *)
		algorithm_keep_going <- true;


		(*------------------------------------------------------------*)
		(* Perform the post^* *)
		(*------------------------------------------------------------*)
		(* Set of states computed at the previous depth *)
		let post_n = ref [init_state_index] in

		(* Explore further until the limit is reached or the list of states computed at the previous depth is empty *)
		while limit_reached = Keep_going && !post_n <> [] && algorithm_keep_going do
			(* Print some information *)
			if verbose_mode_greater Verbose_standard then (
				print_message Verbose_low ("\n");
				print_message Verbose_standard ("Computing post^" ^ (string_of_int bfs_current_depth) ^ " from "  ^ (string_of_int (List.length !post_n)) ^ " state" ^ (s_of_int (List.length !post_n)) ^ ".");
			);

			(* Count the states for verbose purpose: *)
			let num_state = ref 0 in

			(* Statistics *)
(*			counter_nplus1#increment;
			counter_nplus1#start;*)

			(* The concrete function post_from_one_state may raise exception TerminateAnalysis, and update termination_status *)
			let post_n_plus_1 =
			try(
			(* For each newly found state: *)
			List.fold_left (fun current_post_n_plus_1 source_state_index ->
				(* Count the states for verbose purpose: *)
				num_state := !num_state + 1;

				(* Perform the post *)
				let new_states = self#post_from_one_state source_state_index in

				(* Print some information *)
				if verbose_mode_greater Verbose_medium then (
					let beginning_message = if new_states = [] then "Found no new state" else ("Found " ^ (string_of_int (List.length new_states)) ^ " new state" ^ (s_of_int (List.length new_states)) ^ "") in
					print_message Verbose_medium (beginning_message ^ " for the post of state " ^ (string_of_int !num_state) ^ " / " ^ (string_of_int (List.length !post_n)) ^ " in post^" ^ (string_of_int bfs_current_depth) ^ ".\n");
				);

				(* Return the concatenation of the new states *)
				(**** OPTIMIZED: do not care about order (else shoud consider 'list_append current_post_n_plus_1 (List.rev new_states)') *)
				List.rev_append current_post_n_plus_1 new_states
			) [] !post_n
			)
			with TerminateAnalysis ->(
				(* Set the flag *)
				algorithm_keep_going <- false;
				(* If analysis terminated: successors are just the empty list *)
				(*** TODO: it should be possible to change the flag algorithm_keep_going from inside the function instead of deleting this list ***)
				[]
			)

			in

			(* Statistics *)
(* 			counter_nplus1#stop; *)

			(* Statistics *)
(*			counter_process_post_n#increment;
			counter_process_post_n#start;*)

			self#process_post_n !post_n;

			(* Statistics *)
(* 			counter_process_post_n#stop; *)

			(*------------------------------------------------------------*)
			(* Begin merging *)
			(*------------------------------------------------------------*)
			(* Merge states! *)
			let new_states_after_merging = ref post_n_plus_1 in
			(*** HACK here! For #merge_before, we should ONLY merge here; but, in order not to change the full structure of the post computation, we first merge locally before the pi0-compatibility test, then again here ***)

            (*** BEGIN CALL OF MERGING ***)
			begin
            match options#merge_algorithm with
            | Merge_reconstruct
            | Merge_onthefly    ->
                new_states_after_merging := state_space#merge !new_states_after_merging;
            | Merge_212 ->
                let eaten_states = state_space#merge212 !new_states_after_merging in
                new_states_after_merging := list_diff !new_states_after_merging eaten_states;
            | Merge_none ->
                ()
            end;
			(*** END CALL OF MERGING ***)

			(* Update the post_n, i.e., at that point we replace the post^n by post^n+1 in our BFS algorithm, and go one step deeper in the state space *)
			post_n := !new_states_after_merging;
			(*------------------------------------------------------------*)
			(* End merging *)
			(*------------------------------------------------------------*)

			(* Print some information *)
			if verbose_mode_greater Verbose_medium then (
				let beginning_message = if !post_n = [] then "\nFound no new state" else ("\nFound " ^ (string_of_int (List.length !post_n)) ^ " new state" ^ (s_of_int (List.length !post_n)) ^ "") in
				print_message Verbose_medium (beginning_message ^ " for post^" ^ (string_of_int bfs_current_depth) ^ ".\n");
			);

			(* If acyclic option: empty the list of already reached states for comparison with former states *)
			if options#acyclic then(
				print_message Verbose_low ("\nMode acyclic: empty the list of states to be compared.");
				state_space#empty_states_for_comparison;
			);

			(* Print some memory information *)
			if options#statistics then(
				(*** TODO ***)
			);

			(* Statistics *)
(*			counter_gcmajor#increment;
			counter_gcmajor#start;

			(* Statistics *)
			counter_gcmajor#stop;*)


			(* Go one step deeper *)
			bfs_current_depth <- bfs_current_depth + 1;

			(* Check if the limit has been reached *)
			(*** NOTE (ÉA, 2022/11): disabled so far (would check the time limit, states limit, depth limit…) ***)
(* 			self#check_and_update_layer_bfs_limit; *)

			(* If still going, ask the concrete algorithm whether it wants to terminate for other reasons *)
			if limit_reached = Keep_going then(
				(* Print some information *)
				(*** HACK: 'bfs_current_depth - 1' because bfs_current_depth was just incremented… ***)
				self#print_algo_message Verbose_low("Checking termination at post^" ^ (string_of_int (bfs_current_depth - 1)) ^ " with a queue of " ^ (string_of_int (List.length !post_n)) ^ " unexplored state" ^ (s_of_int (List.length !post_n)) ^ "…");

				if self#check_termination_at_post_n then(
					algorithm_keep_going <- false;
				);
			);

		done; (* END WHILE *)

		(* Were they any more states to explore? *)
		let nb_unexplored_successors = List.length !post_n in

		(* Set the list of states with unexplored successors, if any *)
		if nb_unexplored_successors > 0 then(
			(*** NOTE: if an exception TerminateAnalysis was raised, this list is empty :( ***)
			unexplored_successors <- UnexSucc_some !post_n;
		);

		(* Update termination condition *)
		begin
		match limit_reached with
			(* No limit: regular termination *)
			(*** NOTE: check None, as it may have been edited from outside, in which case it should not be Regular_termination ***)
			| Keep_going when termination_status = None -> termination_status <- Some (Result.Regular_termination)
			(*** NOTE: obliged to comment out the condition below, otherwise there is a compiling warning… ***)
			| Keep_going (*when termination_status <> None*) -> ()

			(* Termination due to time limit reached *)
			| Time_limit_reached -> termination_status <- Some (Result.Time_limit nb_unexplored_successors)

			(* Termination due to state space depth limit reached *)
			| Depth_limit_reached -> termination_status <- Some (Result.Depth_limit nb_unexplored_successors)

			(* Termination due to a number of explored states reached *)
			| States_limit_reached -> termination_status <- Some (Result.States_limit nb_unexplored_successors)

			(* Termination because a witness has been found *)
			(*** NOTE/TODO: add a new result termination type? ***)
			| Witness_found -> termination_status <- Some (Result.Regular_termination)
		end
		;

		(* Print some information *)
		(*** NOTE: must be done after setting the limit (above) ***)
		(*** NOTE (ÉA, 2022/11): disabled so far (would check the time limit, states limit, depth limit…) ***)
(* 		self#bfs_print_warnings_limit (); *)

		if not algorithm_keep_going && nb_unexplored_successors > 0 then(
			self#print_algo_message Verbose_standard ("A sufficient condition to ensure termination was met although there were still " ^ (string_of_int nb_unexplored_successors) ^ " state" ^ (s_of_int nb_unexplored_successors) ^ " to explore");
		);


		print_message Verbose_standard (
			let nb_states = state_space#nb_states in
			let nb_transitions = state_space#nb_transitions in
			let fixpoint_str = if nb_unexplored_successors > 0 then "State space exploration stopped" else "Fixpoint reached" in
			"\n" ^ fixpoint_str ^ " at a depth of "
			^ (string_of_int bfs_current_depth) ^ ""
			^ ": "
			^ (string_of_int nb_states) ^ " state" ^ (s_of_int nb_states)
			^ " with "
			^ (string_of_int nb_transitions) ^ " transition" ^ (s_of_int nb_transitions) ^ " in the final state space."
		);
		(*** NOTE: in fact, more states and transitions may have been explored (and deleted); here, these figures are the number of states in the state space. ***)

		(* Statistics *)
		counter_explore_using_strategy#stop;

		(* The end *)
		()
	(*** END WARNING (2022/11, ÉA): copied from AlgoStateBased ***)


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_result =
		(* Print some information *)
		self#print_algo_message_newline Verbose_standard (
			"Algorithm completed " ^ (after_seconds ()) ^ "."
		);


		(*** TODO: compute as well *good* zones, depending whether the analysis was exact, or early termination occurred ***)

		(* Retrieve the property *)
		let abstract_property = Input.get_property() in

		(* Projecting onto SOME parameters if required *)
		(*** NOTE: Useless test as we are in EF, so there is a property ***)
		let result =
			match abstract_property.projection with
				(* No projection: copy the initial p constraint *)
				| None -> synthesized_constraint
				(* Project *)
				| Some parameters ->
					(* Print some information *)
					if verbose_mode_greater Verbose_medium then(
						self#print_algo_message Verbose_medium "Projecting the bad constraint onto some of the parameters.";
						self#print_algo_message Verbose_medium "Before projection:";
						print_message Verbose_medium (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names synthesized_constraint);
					);

					(*** TODO! do only once for all… ***)
					let all_but_projectparameters = list_diff model.parameters parameters in

					(* Eliminate other parameters *)
					let projected_synthesized_constraint = LinearConstraint.p_nnconvex_hide all_but_projectparameters synthesized_constraint in

					(* Print some information *)
					if verbose_mode_greater Verbose_medium then(
						self#print_algo_message Verbose_medium "After projection:";
						print_message Verbose_medium (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names projected_synthesized_constraint);
					);

					(* Return *)
					projected_synthesized_constraint
		in

		(* Get the termination status *)
		 let termination_status = match termination_status with
			| None -> raise (InternalError ("Termination status not set in " ^ (self#algorithm_name) ^ ".compute_result"))
			| Some status -> status
		in

		(* Constraint is exact if termination is normal, possibly under-approximated otherwise *)
		(*** NOTE/TODO: technically, if the constraint is true/false, its soundness can be further refined easily ***)
		let soundness = if termination_status = Regular_termination then Constraint_exact else Constraint_maybe_under in

		(* Return the result *)
		Single_synthesis_result
		{
			(* Non-necessarily convex constraint guaranteeing the reachability of the desired states *)
			result				= Good_constraint (result, soundness);

			(* English description of the constraint *)
			constraint_description = "constraint guaranteeing the existence of a winning strategy";

			(* Explored state space *)
			state_space			= state_space;

			(* Total computation time of the algorithm *)
			computation_time	= time_from start_time;

			(* Termination *)
			termination			= termination_status;
		}


	
(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
