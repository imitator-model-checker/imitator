(************************************************************
 *
 *                       IMITATOR
 *
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: "AF" algorithm (always eventually)
 *
 * File contributors : Étienne André
 * Created           : 2024/01/08
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
(* Class definition *)
(************************************************************)
(************************************************************)
class algoAF (model : AbstractModel.abstract_model) (property : AbstractProperty.abstract_property) (options : Options.imitator_options) (state_predicate : AbstractProperty.state_predicate) =
	object (self) inherit algoGeneric model options (*as super*)

	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "AF"

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
(* 	val parameters_consistent_with_init : LinearConstraint.px_linear_constraint = LinearConstraint.px_of_p_constraint model.initial_p_constraint *)


	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Main body of AF (recursive version) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(* Compute the successors of a symbolic state and computes AF on this branch, recursively calling the same method *)
	method private af_rec (state_index : State.state_index) (passed : State.state_index list) : LinearConstraint.p_nnconvex_constraint =

		(* Get state *)
		let symbolic_state : State.state = state_space#get_state state_index in

		(* Useful shortcut *)
		let state_px_constraint = symbolic_state.px_constraint in

		(* Case 1: target state found: return the associated constraint *)
		if State.match_state_predicate model state_predicate symbolic_state then(
			(* Return the constraint projected onto the parameters *)
			LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint (LinearConstraint.px_hide_nonparameters_and_collapse state_px_constraint)
		)else(
			(* Case 2: state already met *)
			if List.mem state_index passed then(
				(* Return false *)
				LinearConstraint.false_p_nnconvex_constraint ()
			)else(
				(* Instantiate local variables *)
				let k		: LinearConstraint.p_nnconvex_constraint = LinearConstraint.true_p_nnconvex_constraint () in
				let k_live	: LinearConstraint.px_nnconvex_constraint = LinearConstraint.false_px_nnconvex_constraint () in

				(* Compute all successors via all possible outgoing transitions *)
				let transitions_and_successors_list : (StateSpace.combined_transition * State.state) list = AlgoStateBased.combined_transitions_and_states_from_one_state_functional model symbolic_state in

				(* For each successor *)
				List.iter (fun ((combined_transition , successor) : (StateSpace.combined_transition * State.state)) ->

					(* Add or get the state_index of the successor *)
					(*** NOTE/TODO: so far, in AF, we compare using Equality_check ***)
					let addition_result = state_space#add_state Equality_check None successor in
					let successor_state_index = match addition_result with
					| New_state state_index
					| State_already_present state_index
					| State_replacing state_index
						-> state_index
					in

					(*** TODO: add state and transition to the state space ***)

					let k_good : LinearConstraint.p_nnconvex_constraint = self#af_rec successor_state_index (state_index :: passed) in

					(* Recursive call to AF on the successor *)
					(* k_block <- T \ successor|_P *)
					let k_block : LinearConstraint.p_nnconvex_constraint = LinearConstraint.true_p_nnconvex_constraint () in
					LinearConstraint.p_nnconvex_difference_assign k_block (LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint (LinearConstraint.px_hide_nonparameters_and_collapse successor.px_constraint));

					(* K <- K ^ (k_good U k_block) *)
					LinearConstraint.p_nnconvex_union_assign k_good k_block;
					LinearConstraint.p_nnconvex_intersection_assign k k_good;

					(* k_live <- k_live U (C ^ g)\past *)
					LinearConstraint.px_nnconvex_px_union_assign k_live (DeadlockExtra.live_valuations_precondition model state_space state_index combined_transition successor_state_index);


					()
				) transitions_and_successors_list;
				(* End for each successor *)

				(* k <- k \ (True \ k_live)|_P *)
				let not_k_live : LinearConstraint.px_nnconvex_constraint = LinearConstraint.true_px_nnconvex_constraint () in
				LinearConstraint.px_nnconvex_difference_assign not_k_live k_live;
				(*** TODO: intersect with parameters_consistent_with_init first? ***)
				let p_not_k_live : LinearConstraint.p_nnconvex_constraint = LinearConstraint.px_nnconvex_hide_nonparameters_and_collapse not_k_live in
				LinearConstraint.p_nnconvex_difference_assign k p_not_k_live;

				(* return k *)
				k
			)
		)





	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Main method to run the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method run : Result.imitator_result =

		start_time <- Unix.gettimeofday();

		(*** TODO ***)
		raise (NotImplemented "AF is not implemented")

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
			constraint_description = "constraint guaranteeing AF";

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
