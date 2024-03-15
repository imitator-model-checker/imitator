(************************************************************
 *
 *                       IMITATOR
 *
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: algorithms based on "AU": "AF", "AU", "AW"
 *
 * File contributors : Étienne André
 * Created           : 2024/01/26
 *
 ************************************************************)


(************************************************************)
(************************************************************)
(* Modules *)
(************************************************************)
(************************************************************)
(* open OCamlUtilities *)
open ImitatorUtilities
open Exceptions
open AbstractProperty
open AlgoGeneric
open Result



(************************************************************)
(************************************************************)
(* Class definition: AU (virtual) *)
(************************************************************)
(************************************************************)
class virtual algoAUgen (model : AbstractModel.abstract_model) (property : AbstractProperty.abstract_property) (options : Options.imitator_options) (weak : bool) (state_predicate_phi_option : AbstractProperty.state_predicate option) (state_predicate_psi : AbstractProperty.state_predicate) (timed_interval_option : AbstractProperty.timed_interval option) =
	object (self) inherit algoGeneric model options (*as super*)

	
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

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Non-necessarily convex constraint storing the parameter synthesis result (for selected algorithms) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	val mutable synthesized_constraint : LinearConstraint.p_nnconvex_constraint = LinearConstraint.false_p_nnconvex_constraint ()

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** State space *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	val mutable state_space : StateSpace.stateSpace = new StateSpace.stateSpace 0

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Hash table for caching known results of AF *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	val mutable cache_result_AF : (State.state_index, LinearConstraint.p_nnconvex_constraint) Hashtbl.t = Hashtbl.create Constants.guessed_nb_states_for_hashtable


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Status of the analysis *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** TODO ***)
	val mutable termination_status : Result.state_based_algorithm_termination = Regular_termination

	(* Convex parameter constraint ensuring all parameters are compatible with the initial p_constraint (constant object used as a shortcut, as it is often used in the algorithm) *)
	val mutable parameters_consistent_with_init : LinearConstraint.p_linear_constraint = LinearConstraint.p_false_constraint () (* Dummy initialization *)


	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Main body of AU (recursive version) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(* Compute the successors of a symbolic state and computes AF on this branch, recursively calling the same method *)
	method private au_rec (state_index : State.state_index) (passed : State.state_index list) : LinearConstraint.p_nnconvex_constraint =

	print_string ("\n" ^ (string_of_int (List.length passed)));

		(* First check limits, which may raise exceptions *)
		AlgoStateBased.check_limits options (Some ((List.length passed) + 1)) (Some state_space#nb_states) (Some start_time);

		(* First check whether the result of AF(state_index) is known from the cache *)
		if Hashtbl.mem cache_result_AF state_index then(
			LinearConstraint.p_nnconvex_copy (Hashtbl.find cache_result_AF state_index)
		)else(

		(* Get state *)
		let symbolic_state : State.state = state_space#get_state state_index in

		(* Useful shortcut *)
		let state_px_constraint = symbolic_state.px_constraint in

		let af_result =

		(* Case 0 (timed version): Cut branch if we went too far time-wise *)
		let time_went_too_far =
		match timed_interval_upper_bound_constraint_option with
		| Some timed_interval_upper_bound_constraint ->
			let checking_time_went_too_far : LinearConstraint.px_linear_constraint = LinearConstraint.px_intersection [state_px_constraint; timed_interval_upper_bound_constraint] in

			(* Unsatisfiable: cut branch! *)
			if LinearConstraint.px_is_false checking_time_went_too_far then(
				(* Print some information *)
				if verbose_mode_greater Verbose_medium then(
					self#print_algo_message Verbose_medium "Cut branch as the state constraint:";
					print_message Verbose_medium (LinearConstraint.string_of_px_linear_constraint model.variable_names state_px_constraint);
					self#print_algo_message Verbose_medium "is beyond the timed operator:";
					print_message Verbose_medium (LinearConstraint.string_of_px_linear_constraint model.variable_names timed_interval_upper_bound_constraint);
				);
				true
			)else false

		| None -> false
		in

		if time_went_too_far then(
			(* Return false *)
			LinearConstraint.false_p_nnconvex_constraint ()
		)else(
			(* Case 1: target state found: return the associated constraint *)
			if State.match_state_predicate_and_timed_constraint model state_predicate_psi timed_interval_constraint_option symbolic_state  then(

				(* Print some information *)
				if verbose_mode_greater Verbose_low then(
					self#print_algo_message Verbose_low ("Target state found");
					self#print_algo_message Verbose_medium (ModelPrinter.string_of_state model symbolic_state);
				);

				(* If timed version: first add the timed_interval_constraint_option to the resulting state *)
				let state_constraint_for_projection : LinearConstraint.px_linear_constraint = AlgoStateBased.intersect_with_timed_interval_constraint_option model timed_interval_constraint_option state_px_constraint in

				(* Return the constraint projected onto the parameters *)
				LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint (LinearConstraint.px_hide_nonparameters_and_collapse state_constraint_for_projection)
			)else(
				(* Case 1b: For AU, if the state does not satisfy phi, then false *)
				let falsified_phi = match state_predicate_phi_option with
					(* AF: no phi, no reason to return False *)
					| None -> false
					(* AU: some phi *)
					| Some state_predicate_phi ->
						(* If unsatisfied: return false *)
						let unsatisfied = not (State.match_state_predicate model state_predicate_phi symbolic_state) in
						(* Print some information *)
						if unsatisfied && verbose_mode_greater Verbose_low then(
							self#print_algo_message Verbose_low ("The state does not match phi: discard!");
							self#print_algo_message Verbose_medium (ModelPrinter.string_of_state model symbolic_state);
						);
						unsatisfied
				in
				if falsified_phi then(
					(* Return false *)
					LinearConstraint.false_p_nnconvex_constraint ()
				)
				(* Case 2: state already met *)
				else if List.mem state_index passed then(

					(* If weak version: loop (necessarily over phi) => found good valuations! *)
					(*** NOTE: this is a loop because state_index is met twice on the *same* path, i.e., of the form (state_index , …, state_index) ***)
					if weak then(
						(* Print some information *)
						if verbose_mode_greater Verbose_low then(
							self#print_algo_message Verbose_low ("State " ^ (string_of_int state_index) ^ " belongs to passed: found loop!");
						);

						(* Return the state constraint *)
						LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint (LinearConstraint.px_hide_nonparameters_and_collapse state_px_constraint)

					(* Normal version: a loop means False *)
					)else(
						(* Print some information *)
						if verbose_mode_greater Verbose_medium then(
							self#print_algo_message Verbose_low ("State " ^ (string_of_int state_index) ^ " belongs to passed: skip");
						);

						(* Return false *)
						LinearConstraint.false_p_nnconvex_constraint ()
					)
				)else(
					(* Valuate local variables *)
					let k		: LinearConstraint.p_nnconvex_constraint  = LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint (LinearConstraint.p_copy model.initial_p_constraint) in
					let k_live	: LinearConstraint.px_nnconvex_constraint = LinearConstraint.false_px_nnconvex_constraint () in

					(* Compute all successors via all possible outgoing transitions *)
					let transitions_and_successors_list : (StateSpace.combined_transition * State.state) list = AlgoStateBased.combined_transitions_and_states_from_one_state_functional options model symbolic_state in

					(* For each successor *)
					List.iter (fun ((combined_transition , successor) : (StateSpace.combined_transition * State.state)) ->
						(* Increment a counter: this state IS generated (although maybe it will be discarded because equal / merged / algorithmic discarding …) *)
						state_space#increment_nb_gen_states;

						(* Add or get the state_index of the successor *)
						(*** NOTE/TODO: so far, in AF, we compare using Equality_check ***)
						let addition_result = state_space#add_state Equality_check None successor in
						let successor_state_index = match addition_result with
						| New_state state_index
						| State_already_present state_index
						| State_replacing state_index
							-> state_index
						in

						(* Print some information *)
						if verbose_mode_greater Verbose_high then(
							self#print_algo_message_newline Verbose_high ("Considering successor " ^ (string_of_int successor_state_index) ^ " of " ^ (string_of_int state_index) ^ "…");
						);

						(* Add the transition to the state space *)
						state_space#add_transition (state_index, combined_transition, successor_state_index);


						(* Print some information *)
						if verbose_mode_greater Verbose_high then(
							self#print_algo_message_newline Verbose_high ("Calling recursively AU(" ^ (string_of_int successor_state_index) ^ ")…");
						);

						(* Recursive call to AF on the successor *)
						let k_good : LinearConstraint.p_nnconvex_constraint = LinearConstraint.p_nnconvex_copy(self#au_rec successor_state_index (state_index :: passed)) in

						(* Print some information *)
						if verbose_mode_greater Verbose_high then(
							self#print_algo_message_newline Verbose_high ("Result of AU(" ^ (string_of_int successor_state_index) ^ "):");
							self#print_algo_message Verbose_high (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names k_good);
						);

						(* k_block <- T \ successor|_P *)
						let k_block : LinearConstraint.p_nnconvex_constraint = LinearConstraint.true_p_nnconvex_constraint () in
						LinearConstraint.p_nnconvex_difference_assign k_block (LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint (LinearConstraint.px_hide_nonparameters_and_collapse successor.px_constraint));

						(* Print some information *)
						if verbose_mode_greater Verbose_high then(
							self#print_algo_message_newline Verbose_high ("Blocking constraint:");
							self#print_algo_message Verbose_high (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names k_block);
						);

						(* K <- K ^ (k_good U k_block) *)

						if verbose_mode_greater Verbose_total then(
							self#print_algo_message_newline Verbose_total ("About to compute k_good <- k_good U k_block:");
							self#print_algo_message Verbose_total ("k_good = " ^ (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names k_good));
							self#print_algo_message Verbose_total ("k_block = " ^ (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names k_block));
						);

						LinearConstraint.p_nnconvex_union_assign k_good k_block;
						LinearConstraint.p_nnconvex_intersection_assign k k_good;

						(* Print some information *)
						if verbose_mode_greater Verbose_high then(
							self#print_algo_message Verbose_high ("k:");
							self#print_algo_message Verbose_high (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names k);
						);

						(* k_live <- k_live U (C ^ g)\past *)
						let eventually_exiting_valuations = DeadlockExtra.live_valuations_precondition model state_space state_index combined_transition successor_state_index in

						(* NOTE: unnecessary intersection as we remove the final valuations from C anyway *)
	(* 					LinearConstraint.px_intersection_assign eventually_exiting_valuations [state_px_constraint]; *)

						(* Print some information *)
						if verbose_mode_greater Verbose_high then(
							self#print_algo_message Verbose_high ("Eventually exiting valuations:");
							self#print_algo_message Verbose_high (LinearConstraint.string_of_px_linear_constraint model.variable_names eventually_exiting_valuations);
						);
						LinearConstraint.px_nnconvex_px_union_assign k_live eventually_exiting_valuations;

						(* Print some information *)
						if verbose_mode_greater Verbose_high then(
							self#print_algo_message Verbose_high ("k_live after adding exiting valuations:");
							self#print_algo_message Verbose_high (LinearConstraint.string_of_px_nnconvex_constraint model.variable_names k_live);
						);

						()
					) transitions_and_successors_list;
					(* End for each successor *)

					(* Print some information *)
					if verbose_mode_greater Verbose_high then(
						self#print_algo_message_newline Verbose_high ("Finalizing the result of AU(" ^ (string_of_int state_index) ^ ")…");
					);

					(* k <- k \ (C \ k_live)|_P *)
					let not_k_live : LinearConstraint.px_nnconvex_constraint = LinearConstraint.px_nnconvex_constraint_of_px_linear_constraint (LinearConstraint.px_copy state_px_constraint) in
					LinearConstraint.px_nnconvex_difference_assign not_k_live k_live;
					let p_not_k_live : LinearConstraint.p_nnconvex_constraint = LinearConstraint.px_nnconvex_hide_nonparameters_and_collapse not_k_live in
					LinearConstraint.p_nnconvex_difference_assign k p_not_k_live;

					(* Print some information *)
					if verbose_mode_greater Verbose_high then(
						self#print_algo_message Verbose_high ("Negation of k_live");
						self#print_algo_message Verbose_high (LinearConstraint.string_of_px_nnconvex_constraint model.variable_names not_k_live);
						self#print_algo_message Verbose_high ("Projection of not(k_live)");
						self#print_algo_message Verbose_high (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names p_not_k_live);
					);

					(* Intersect with initial parameter domain *)
					LinearConstraint.p_nnconvex_p_intersection_assign k parameters_consistent_with_init;

					(* Print some information *)
					if verbose_mode_greater Verbose_medium then(
						self#print_algo_message_newline Verbose_medium ("Final constraint in AU(" ^ (string_of_int state_index) ^ ")…");
						self#print_algo_message Verbose_medium (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names k);
					);

					(* return k *)
					k
				)
			)
		) (* end elseif time went too far *)
		in

		(* Cache the result *)
		(*** NOTE (ÉA, 2024/03/14): copy the constraint first, as it might be manipulated in the future ***)
		Hashtbl.replace cache_result_AF state_index (LinearConstraint.p_nnconvex_copy af_result);

		(* Return result *)
		(*** NOTE (ÉA, 2024/03/14): copy the constraint first, as it might be manipulated in the future as the result of AF ***)
		(LinearConstraint.p_nnconvex_copy af_result)
		)




	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Main method to run the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method run : Result.imitator_result =

		start_time <- Unix.gettimeofday();

		(* Build initial state *)
		let initial_state : State.state = AlgoStateBased.create_initial_state options model true (* abort_if_unsatisfiable_initial_state *) in

		if verbose_mode_greater Verbose_high then(
			self#print_algo_message Verbose_high "The initial state has been created";
			self#print_algo_message Verbose_high (ModelPrinter.string_of_state model initial_state);
		);

		(* Add it to the state space *)
		(*** BEGIN copied from AlgoStateBased ***)
		(* Add the initial state to the state space; no need to check whether the state is present since it is the first state anyway *)
		let init_state_index = match state_space#add_state AbstractAlgorithm.No_check model.global_time_clock initial_state with
			(* The state is necessarily new as the state space was empty *)
			| StateSpace.New_state state_index -> state_index
			| _ -> raise (InternalError "The result of adding the initial state to the state space should be New_state")
		in
		(*** END copied from AlgoStateBased ***)

		(* Increment the number of computed states *)
		state_space#increment_nb_gen_states;

		(* Update initial parameter constraint *)
		let initial_px_constraint = initial_state.px_constraint in
		parameters_consistent_with_init <- LinearConstraint.px_hide_nonparameters_and_collapse initial_px_constraint;

		(* Main call to the AF dedicated function *)
		begin
		try(
			synthesized_constraint <- self#au_rec init_state_index [];
		) with AlgoStateBased.LimitDetectedException reason ->
			begin
			match reason with

				(*** TODO: add warnings ***)

				(* Termination due to time limit reached *)
				| Time_limit_reached -> termination_status <- Result.Time_limit Result.Unknown_number

				(* Termination due to state space depth limit reached *)
				| Depth_limit_reached -> termination_status <- Result.Depth_limit Result.Unknown_number

				(* Termination due to a number of explored states reached *)
				| States_limit_reached -> termination_status <- Result.States_limit Result.Unknown_number

				| Keep_going
				| Witness_found
					-> raise (InternalError "Keep_going or Witness_found cannot be passed as exception in AlgoAU")
			end;
			ResultProcessor.print_warnings_of_termination_status termination_status;
		end;

		(* Return the result *)
		self#compute_result;

		(* The end *)



	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private compute_result =
		(* Print some information *)
		self#print_algo_message_newline Verbose_standard (
			"Algorithm completed " ^ (after_seconds ()) ^ "."
		);

		(* Projecting onto some parameters if required by the property *)
		let result = AlgoStateBased.project_p_nnconvex_constraint_if_requested model property synthesized_constraint in

		(* Constraint is exact if termination is normal, possibly under-approximated otherwise *)
		(*** TODO: double check ***)
		let soundness = if property.synthesis_type = Synthesis && termination_status = Regular_termination then Constraint_exact else Constraint_maybe_invalid in

		(* Return the result *)
		Single_synthesis_result
		{
			(* Non-necessarily convex constraint guaranteeing the reachability of the desired states *)
			result				= Good_constraint (result, soundness);

			(* English description of the constraint *)
			constraint_description = "constraint guaranteeing " ^ self#algorithm_name;

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

(************************************************************)
(************************************************************)
(* Class definition: AF *)
(************************************************************)
(************************************************************)
class algoAF (model : AbstractModel.abstract_model) (property : AbstractProperty.abstract_property) (options : Options.imitator_options) (state_predicate : AbstractProperty.state_predicate) =
	object (*(self)*) inherit algoAUgen model property options false None state_predicate None (*as super*)


	(************************************************************)
	(* Class variables *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "AF"


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)


(************************************************************)
(************************************************************)
(* Class definition: AF (timed) *)
(************************************************************)
(************************************************************)
class algoAFtimed (model : AbstractModel.abstract_model) (property : AbstractProperty.abstract_property) (options : Options.imitator_options) (state_predicate : AbstractProperty.state_predicate) (timed_interval : AbstractProperty.timed_interval) =
	object (*(self)*) inherit algoAUgen model property options false None state_predicate (Some timed_interval) (*as super*)


	(************************************************************)
	(* Class variables *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "AF (timed)"


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)


(************************************************************)
(************************************************************)
(* Class definition: AU *)
(************************************************************)
(************************************************************)
class algoAU (model : AbstractModel.abstract_model) (property : AbstractProperty.abstract_property) (options : Options.imitator_options) (state_predicate_phi : AbstractProperty.state_predicate) (state_predicate_psi : AbstractProperty.state_predicate) =
	object (*(self)*) inherit algoAUgen model property options false (Some state_predicate_phi) state_predicate_psi None (*as super*)


	(************************************************************)
	(* Class variables *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "AU"


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)



(************************************************************)
(************************************************************)
(* Class definition: AU (timed) *)
(************************************************************)
(************************************************************)
class algoAUtimed (model : AbstractModel.abstract_model) (property : AbstractProperty.abstract_property) (options : Options.imitator_options) (state_predicate_phi : AbstractProperty.state_predicate) (state_predicate_psi : AbstractProperty.state_predicate) (timed_interval : AbstractProperty.timed_interval) =
	object (*(self)*) inherit algoAUgen model property options false (Some state_predicate_phi) state_predicate_psi (Some timed_interval) (*as super*)


	(************************************************************)
	(* Class variables *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "AU (timed)"


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)



(************************************************************)
(************************************************************)
(* Class definition: AW *)
(************************************************************)
(************************************************************)
class algoAW (model : AbstractModel.abstract_model) (property : AbstractProperty.abstract_property) (options : Options.imitator_options) (state_predicate_phi : AbstractProperty.state_predicate) (state_predicate_psi : AbstractProperty.state_predicate) =
	object (*(self)*) inherit algoAUgen model property options true (Some state_predicate_phi) state_predicate_psi None (*as super*)


	(************************************************************)
	(* Class variables *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "AW"


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)



(************************************************************)
(************************************************************)
(* Class definition: AW (timed) *)
(************************************************************)
(************************************************************)
class algoAWtimed (model : AbstractModel.abstract_model) (property : AbstractProperty.abstract_property) (options : Options.imitator_options) (state_predicate_phi : AbstractProperty.state_predicate) (state_predicate_psi : AbstractProperty.state_predicate) (timed_interval : AbstractProperty.timed_interval) =
	object (*(self)*) inherit algoAUgen model property options true (Some state_predicate_phi) state_predicate_psi (Some timed_interval) (*as super*)


	(************************************************************)
	(* Class variables *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "AW (timed)"


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)

