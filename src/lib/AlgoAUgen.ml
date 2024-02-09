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
class virtual algoAUgen (model : AbstractModel.abstract_model) (property : AbstractProperty.abstract_property) (options : Options.imitator_options) (weak : bool) (state_predicate_phi_option : AbstractProperty.state_predicate option) (state_predicate_psi : AbstractProperty.state_predicate) =
	object (self) inherit algoGeneric model options (*as super*)

	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Non-necessarily convex constraint storing the parameter synthesis result (for selected algorithms) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	val mutable synthesized_constraint : LinearConstraint.p_nnconvex_constraint = LinearConstraint.false_p_nnconvex_constraint ()

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** State space *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	val mutable state_space : StateSpace.stateSpace = new StateSpace.stateSpace 0

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

		(* Get state *)
		let symbolic_state : State.state = state_space#get_state state_index in

		(* Useful shortcut *)
		let state_px_constraint = symbolic_state.px_constraint in

		(* Case 1: target state found: return the associated constraint *)
		if State.match_state_predicate model state_predicate_psi symbolic_state then(

			(* Print some information *)
			if verbose_mode_greater Verbose_low then(
				self#print_algo_message Verbose_low ("Target state found");
				self#print_algo_message Verbose_medium (ModelPrinter.string_of_state model symbolic_state);
			);

			(* Return the constraint projected onto the parameters *)
			LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint (LinearConstraint.px_hide_nonparameters_and_collapse state_px_constraint)
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
					if verbose_mode_greater Verbose_low then(
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
				(* Instantiate local variables *)
				let k		: LinearConstraint.p_nnconvex_constraint  = LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint (LinearConstraint.p_copy model.initial_p_constraint) in
				let k_live	: LinearConstraint.px_nnconvex_constraint = LinearConstraint.false_px_nnconvex_constraint () in

				(* Compute all successors via all possible outgoing transitions *)
				let transitions_and_successors_list : (StateSpace.combined_transition * State.state) list = AlgoStateBased.combined_transitions_and_states_from_one_state_functional model symbolic_state in

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
					let k_good : LinearConstraint.p_nnconvex_constraint = self#au_rec successor_state_index (state_index :: passed) in

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





	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Main method to run the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method run : Result.imitator_result =

		start_time <- Unix.gettimeofday();

		(* Build initial state *)
		let initial_state : State.state = AlgoStateBased.create_initial_state model true (* abort_if_unsatisfiable_initial_state *) in

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
		synthesized_constraint <- self#au_rec init_state_index [];

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
	object (*(self)*) inherit algoAUgen model property options false None state_predicate (*as super*)


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
(* Class definition: AU *)
(************************************************************)
(************************************************************)
class algoAU (model : AbstractModel.abstract_model) (property : AbstractProperty.abstract_property) (options : Options.imitator_options) (state_predicate_phi : AbstractProperty.state_predicate) (state_predicate_psi : AbstractProperty.state_predicate) =
	object (*(self)*) inherit algoAUgen model property options false (Some state_predicate_phi) state_predicate_psi (*as super*)


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
(* Class definition: AW *)
(************************************************************)
(************************************************************)
class algoAW (model : AbstractModel.abstract_model) (property : AbstractProperty.abstract_property) (options : Options.imitator_options) (state_predicate_phi : AbstractProperty.state_predicate) (state_predicate_psi : AbstractProperty.state_predicate) =
	object (*(self)*) inherit algoAUgen model property options true (Some state_predicate_phi) state_predicate_psi (*as super*)


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
