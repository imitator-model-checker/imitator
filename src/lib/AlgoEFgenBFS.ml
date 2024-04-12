(************************************************************
 *
 *                       IMITATOR
 *
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: algorithms based on "EF" (real BFS version)
 *
 * File contributors : Étienne André
 * Created           : 2024/02/07
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
open AbstractProperty
open AlgoGeneric
open Result

(************************************************************)
(************************************************************)
(* Exceptions *)
(************************************************************)
(************************************************************)
exception Witness_found

(************************************************************)
(************************************************************)
(* Class definition: EU (virtual) *)
(************************************************************)
(************************************************************)
class virtual algoEUgenBFS (model : AbstractModel.abstract_model) (property : AbstractProperty.abstract_property) (options : Options.imitator_options) (weak : bool) (state_predicate_phi_option : AbstractProperty.state_predicate option) (state_predicate_psi : AbstractProperty.state_predicate) (timed_interval_option : AbstractProperty.timed_interval option) =
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
	(** Current queue for the BFS analysis *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** NOTE: not so nice, here: we use a type option ONLY because of the initial state ***)
	val mutable queue : ( (State.state_index * StateSpace.combined_transition) option * State.state) Queue.t = Queue.create()

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Status of the analysis *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** TODO ***)
	val mutable termination_status : Result.state_based_algorithm_termination = Regular_termination


	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Main body of EU (BFS version) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(* Main method to compute EU and variants in a BFS manner using a queue *)
	method private ef_bfs (initial_state : State.state) : (*LinearConstraint.p_nnconvex_constraint*) unit =
		(* Create queue *)
		queue <- Queue.create();

		(* Add initial state with no incoming transition *)
		Queue.push (None, initial_state) queue;

		(* While queue is not empty *)
		while not (Queue.is_empty queue) do
			(* Take first element *)
			let (source_state_index_and_transition_option, current_symbolic_state) : ( (State.state_index * StateSpace.combined_transition) option * State.state) = Queue.pop queue in

			(* Useful shortcut *)
			let state_px_constraint = current_symbolic_state.px_constraint in

			(* Case 1: if EU mode, check whether phi is NOT satisfied *)
			let stop_due_to_unsatisfied_phi =
				match state_predicate_phi_option with
				(* Stop if phi is satisfied *)
				| Some state_predicate_phi ->
					State.match_state_predicate model state_predicate_phi current_symbolic_state
				(* No need to stop *)
				| None -> false
				in
			if stop_due_to_unsatisfied_phi then
				(* Do nothing, i.e., skip to next state *)
				()
			else (
				(* Shortcut (but costly) *)
				let state_p_constraint = LinearConstraint.px_hide_nonparameters_and_collapse state_px_constraint in

				(* Case 2: cumulative pruning *)
				let stop_due_to_already_computed_valuations =
					if options#cumulative_pruning then(
						(* Check if the constraint is included into known valuations *)
						(*** WARNING: costly operation here ***)
						if LinearConstraint.p_nnconvex_constraint_is_leq (LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint state_p_constraint) synthesized_constraint then(
							self#print_algo_message Verbose_medium "Eliminate a state because its constraint is less than the synthesized constraint (\"cumulative pruning\").";
							true
						) else false
					) else false
				in
				if stop_due_to_already_computed_valuations then
					(* Do nothing, i.e., skip to next state *)
					()
				else (
					(* Flag to check whether successors must be computed (by default true) *)
					let compute_successors = ref true in

					(* Try to add the new state to the state space *)
					let addition_result = state_space#add_state options#comparison_operator model.global_time_clock current_symbolic_state in

					(*** NOTE: we first add the transition before handling the rest (important for EW and loop detection) ***)
					begin
					let new_state_index =
					match addition_result with
					(* If this is really a new state, or a state larger than a former state *)
					| StateSpace.New_state new_state_index | StateSpace.State_replacing new_state_index -> new_state_index
					(* If the state was present *)
					| StateSpace.State_already_present old_state_index -> old_state_index
					in
					(* Actually add transition *)
					match source_state_index_and_transition_option with
					| Some (source_state_index, combined_transition) ->
						(* Add the transition to the state space *)
						state_space#add_transition (source_state_index, combined_transition, new_state_index);

					(* The current state is necessarily the initial state: no transition to add *)
					| None -> ()
					end;

					let new_state_index_option : State.state_index option =
					match addition_result with
					(* If this is really a new state, or a state larger than a former state *)
					| StateSpace.New_state new_state_index | StateSpace.State_replacing new_state_index ->
						(* Return the new state index *)
						Some new_state_index

					(* If the state was present: nothing to do *)
					| StateSpace.State_already_present _ ->

						(*** TODO: handle loop for EW here and update compute_successors if needed ***)

						None
					in

					(*** TODO: handle loop for EW here ***)

					(* Check limits, which may raise exceptions *)
					(*** TODO: encode depth somewhere, to avoid this `None` ***)
					AlgoStateBased.check_limits options None (Some state_space#nb_states) (Some start_time);

					match new_state_index_option with
					(* No addition: skip *)
					| None -> ()
					(* Addition: *)
					| Some state_index -> (
						(* Check whether the state satisfies psi *)
						if State.match_state_predicate model state_predicate_psi current_symbolic_state then(

							(* Print some information *)
							self#print_algo_message Verbose_standard ("Found a new target state (" ^ (string_of_int state_space#nb_states) ^ " state" ^ (s_of_int state_space#nb_states) ^ " explored, " ^ (string_of_int (Queue.length queue)) ^ " state" ^ (s_of_int ((Queue.length queue))) ^ " in the queue).");
							if verbose_mode_greater Verbose_low then(
								self#print_algo_message Verbose_medium (ModelPrinter.string_of_state model current_symbolic_state);
							);

							(* Add the constraint projected onto the parameters to the result *)
							LinearConstraint.p_nnconvex_p_union_assign synthesized_constraint (LinearConstraint.px_hide_nonparameters_and_collapse state_px_constraint);

							(* Case witness: terminate *)
							if property.synthesis_type = Witness then(
								self#print_algo_message Verbose_standard "Witness found! Terminating…";
								raise Witness_found
							);

							(* Do NOT compute successors *)
							compute_successors := false
						);

						if !compute_successors then(
							(* Compute all successors via all possible outgoing transitions *)
							let transitions_and_successors_list : (StateSpace.combined_transition * State.state) list = AlgoStateBased.combined_transitions_and_states_from_one_state_functional options model current_symbolic_state in
							(* Enqueue *)
							List.iter (fun (combined_transition, state) ->
								(* Increment the number of computed states *)
								state_space#increment_nb_gen_states;
								(* Actually enqueue *)
								Queue.add (Some (state_index, combined_transition), state) queue
							) transitions_and_successors_list;


							(*** TODO: merge ***)

						);

						(* Check limits, which may raise exceptions *)
						(*** TODO: encode depth somewhere, to avoid this `None` ***)
						AlgoStateBased.check_limits options None (Some state_space#nb_states) (Some start_time);

					) (* end case addition *)
					;

				) (* end case 2: cumulating pruning *)
			) (* end case 1: EU and check psi *)

		done;

		(* Dummy return *)
		()





	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Main method to run the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method run : Result.imitator_result =

		start_time <- Unix.gettimeofday();

		(* Build initial state *)
		let initial_state : State.state = AlgoStateBased.create_initial_state options model true (* abort_if_unsatisfiable_initial_state *) in

		(* Increment the number of computed states *)
		state_space#increment_nb_gen_states;

		if verbose_mode_greater Verbose_high then(
			self#print_algo_message Verbose_high "The initial state has been created";
			self#print_algo_message Verbose_high (ModelPrinter.string_of_state model initial_state);
		);

(*		(* Add it to the state space *)
		(*** BEGIN copied from AlgoStateBased ***)
		(* Add the initial state to the state space; no need to check whether the state is present since it is the first state anyway *)
		let init_state_index = match state_space#add_state AbstractAlgorithm.No_check model.global_time_clock initial_state with
			(* The state is necessarily new as the state space was empty *)
			| StateSpace.New_state state_index -> state_index
			| _ -> raise (InternalError "The result of adding the initial state to the state space should be New_state")
		in
		(*** END copied from AlgoStateBased ***)*)

		(* Main call to the EF dedicated function *)
		begin
		try(
			self#ef_bfs initial_state;
		) with
			| AlgoStateBased.LimitDetectedException reason ->
			begin
			match reason with

				(*** TODO: add warnings ***)

				(* Termination due to time limit reached *)
				| Time_limit_reached -> termination_status <- Result.Time_limit Result.Unknown_number

				(* Termination due to state space depth limit reached *)
				| Depth_limit_reached -> termination_status <- Result.Depth_limit Result.Unknown_number

				(* Termination due to a number of explored states reached *)
				| States_limit_reached -> termination_status <- Result.States_limit Result.Unknown_number

				| Keep_going | Witness_found
					-> raise (InternalError ("Keep_going or Witness_found cannot be passed as exception in " ^ self#algorithm_name))
			end;
			ResultProcessor.print_warnings_of_termination_status termination_status;
			(* Termination due to a witness found *)
			| Witness_found -> (
				termination_status <- Result.Witness_found;
				ResultProcessor.print_warnings_of_termination_status termination_status;
			);
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
		let soundness = if property.synthesis_type = Synthesis && termination_status = Regular_termination then Constraint_exact else Constraint_maybe_under in

		(* Return the result *)
		Single_synthesis_result
		{
			(* Non-necessarily convex constraint guaranteeing the reachability of the desired states *)
			result				= Good_constraint (result, soundness);

			(* English description of the constraint *)
			constraint_description = "constraint guaranteeing EF";

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
(* Class definition: EF *)
(************************************************************)
(************************************************************)
class algoEFBFS (model : AbstractModel.abstract_model) (property : AbstractProperty.abstract_property) (options : Options.imitator_options) (state_predicate : AbstractProperty.state_predicate) =
	object (*(self)*) inherit algoEUgenBFS model property options false None state_predicate None (*as super*)


	(************************************************************)
	(* Class variables *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "EF (NEW EXPERIMENTAL BFS VERSION)"


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)

(*(************************************************************)
(************************************************************)
(* Class definition: EF (timed) *)
(************************************************************)
(************************************************************)
class algoEFBFStimed (model : AbstractModel.abstract_model) (property : AbstractProperty.abstract_property) (options : Options.imitator_options) (state_predicate : AbstractProperty.state_predicate) =
	object (*(self)*) inherit algoEUgenBFS model property options false None state_predicate None (*as super*)


	(************************************************************)
	(* Class variables *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "EF (timed) (NEW EXPERIMENTAL BFS VERSION)"


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)*)


(************************************************************)
(************************************************************)
(* Class definition: EU *)
(************************************************************)
(************************************************************)
class algoEUBFS (model : AbstractModel.abstract_model) (property : AbstractProperty.abstract_property) (options : Options.imitator_options) (state_predicate_phi : AbstractProperty.state_predicate) (state_predicate_psi : AbstractProperty.state_predicate) =
	object (*(self)*) inherit algoEUgenBFS model property options false (Some state_predicate_phi) state_predicate_psi None (*as super*)


	(************************************************************)
	(* Class variables *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "EU (NEW EXPERIMENTAL BFS VERSION)"


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)




(************************************************************)
(************************************************************)
(* Class definition: AGnot *)
(************************************************************)
(************************************************************)
class algoAGnotBFS (model : AbstractModel.abstract_model) (property : AbstractProperty.abstract_property) (options : Options.imitator_options) (state_predicate : AbstractProperty.state_predicate) =
	object (self) inherit algoEUgenBFS model property options false None state_predicate None (*as super*)


	(************************************************************)
	(* Class variables *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "AGnot (NEW EXPERIMENTAL BFS VERSION)"


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** NOTE: overriden method ***)
	method! private compute_result =
		(* Print some information *)
		self#print_algo_message_newline Verbose_standard (
			"Algorithm completed " ^ (after_seconds ()) ^ "."
		);

		(* Print some information *)
		self#print_algo_message_newline Verbose_low (
			"Performing initial constraint \ final resulting constraint…"
		);


		(* Perform result = initial_state|P \ synthesized_constraint *)

		(* Retrieve the initial parameter constraint *)
		let result : LinearConstraint.p_nnconvex_constraint = LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint model.initial_p_constraint in

		if verbose_mode_greater Verbose_medium then(
			self#print_algo_message Verbose_medium "As a reminder, the initial constraint is:";
			self#print_algo_message Verbose_medium (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names result);
			self#print_algo_message Verbose_medium "As a reminder, the result constraint is:";
			self#print_algo_message Verbose_medium (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names synthesized_constraint);
		);

		(* Perform the difference *)
		LinearConstraint.p_nnconvex_difference_assign result synthesized_constraint;

		(* Projecting onto some parameters if required by the property *)
		let result = AlgoStateBased.project_p_nnconvex_constraint_if_requested model property result in

		(* Print some information *)
		if verbose_mode_greater Verbose_medium then(
			self#print_algo_message_newline Verbose_medium (
				"Negation of final constraint completed:"
			);
			self#print_algo_message Verbose_medium (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names result);
		);

		(*** NOTE/TODO: technically, if the constraint is true/false, its soundness can be further refined easily ***)
		let soundness = if property.synthesis_type = Synthesis && termination_status = Regular_termination then Constraint_exact else Constraint_maybe_over in

		(* Return the result *)
		Single_synthesis_result
		{
			(* Non-necessarily convex constraint guaranteeing the reachability of the desired states *)
			result				= Good_constraint (result, soundness);

			(* English description of the constraint *)
			constraint_description = "constraint guaranteeing AGnot";

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
