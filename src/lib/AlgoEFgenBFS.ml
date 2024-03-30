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
(* open OCamlUtilities *)
open ImitatorUtilities
open Exceptions
open AbstractProperty
open AlgoGeneric
open Result



(************************************************************)
(************************************************************)
(* Class definition: EU (virtual) *)
(************************************************************)
(************************************************************)
class virtual algoEUgenBFS (model : AbstractModel.abstract_model) (property : AbstractProperty.abstract_property) (options : Options.imitator_options) (weak : bool) (state_predicate_phi_option : AbstractProperty.state_predicate option) (state_predicate_psi : AbstractProperty.state_predicate) =
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


	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Main body of EU (BFS version) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(* Main method to compute EU and variants in a BFS manner using a queue *)
	method private ef_bfs (initial_state_index : State.state_index) : (*LinearConstraint.p_nnconvex_constraint*) unit =
		(* Create queue *)
		let queue : State.state_index Queue.t = Queue.create() in

		(* Add first state *)
		Queue.push initial_state_index queue;

		(* While queue is not empty *)
		while not (Queue.is_empty queue) do
			(* Take first element *)
			let current_state_index : State.state_index = Queue.pop queue in

			(* Get state *)
			let symbolic_state : State.state = state_space#get_state current_state_index in

			(* Useful shortcut *)
			let state_px_constraint = symbolic_state.px_constraint in

			(* Case 1: if EU mode, check whether phi is NOT satisfied *)
			let stop_due_to_unsatisfied_phi =
				match state_predicate_phi_option with
				(* Stop if phi is satisfied *)
				| Some state_predicate_phi ->
					State.match_state_predicate model state_predicate_phi symbolic_state
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
					let compute_successors = ref true in

					raise (NotImplemented "EUgen BFS")



				) (* end case 2: cumulating pruning *)
			) (* end case 1: EU and check psi *)
(*
			(* Case 1: target state found: add the associated constraint to the result *)
			if State.match_state_predicate model state_predicate_psi symbolic_state then(

				(* Print some information *)
				if verbose_mode_greater Verbose_low then(
					self#print_algo_message Verbose_low ("Target state found");
					self#print_algo_message Verbose_medium (ModelPrinter.string_of_state model symbolic_state);
				);

				(* Add the constraint projected onto the parameters to the result *)
				LinearConstraint.p_nnconvex_p_union_assign synthesized_constraint (LinearConstraint.px_hide_nonparameters_and_collapse state_px_constraint)
			)else(

				(*** TODO ***)

			) (* end case 1: psi satisfied *)*)

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

		(* Main call to the EF dedicated function *)
		(*synthesized_constraint <- *)self#ef_bfs init_state_index;

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
	object (*(self)*) inherit algoEUgenBFS model property options false None state_predicate (*as super*)


	(************************************************************)
	(* Class variables *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "EF"


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
