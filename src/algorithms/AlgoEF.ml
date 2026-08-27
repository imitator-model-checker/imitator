(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * 
 * Module description: "EF" algorithm (reachability-synthesis) [JLR15]
 * 
 * File contributors : Étienne André
 * Created           : 2023/12/18
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
(* open AbstractModel *)
open AbstractProperty
open Result
open AlgoEUgen



(************************************************************)
(************************************************************)
(* Class definition: algoEF_timed_or_untimed *)
(************************************************************)
(************************************************************)
class virtual algoEF_timed_or_untimed (model : AbstractModel.abstract_model) (property : AbstractProperty.abstract_property) (options : Options.imitator_options) (state_predicate : AbstractProperty.state_predicate) (timed_interval_option : AbstractProperty.timed_interval option) =
	object (self) inherit algoEUgen model property options None state_predicate timed_interval_option (*as super*)

	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(* 	method algorithm_name = "EF" *)
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_result =
		(* Print some information *)
		self#print_algo_message_newline Verbose_standard (
			"Algorithm completed " ^ (after_seconds ()) ^ "."
		);


		(*** TODO: compute as well *good* zones, depending whether the analysis was exact, or early termination occurred ***)

		(* Projecting onto some parameters if required by the property *)
		let result = AlgoStateBased.project_p_nnconvex_constraint_if_requested model property synthesized_constraint in

		(* Get the termination status *)
		 let termination_status = match termination_status with
			| None -> raise (InternalError ("Termination status not set in " ^ (self#algorithm_name) ^ ".compute_result"))
			| Some status -> status
		in

		(* Branching between Witness/Synthesis and Exemplification *)
		if property.synthesis_type = Exemplification then(
			(* Return the result *)
			Runs_exhibition_result
			{
				(* Non-necessarily convex constraint guaranteeing the reachability of the bad location *)
				(*** NOTE: use rev since we added the runs by reversed order ***)
				runs				= List.rev_append positive_examples (List.rev negative_examples);

				(* Explored state space *)
				state_space			= state_space;

				(* Total computation time of the algorithm *)
				computation_time	= time_from start_time;

				(* Termination *)
				termination			= termination_status;
			}

		(* Normal mode: Witness/Synthesis *)
		)else(

			(* Constraint is exact if termination is normal, possibly under-approximated otherwise *)
			(*** NOTE/TODO: technically, if the constraint is true/false, its soundness can be further refined easily ***)
			let soundness = if termination_status = Regular_termination then Constraint_exact else(
				(* Check if the set of valuations is the entire set of possible valuations *)

				(* Retrieve the initial parameter constraint *)
				let initial_p_nnconvex_constraint : LinearConstraint.p_nnconvex_constraint = AlgoStateBased.project_p_nnconvex_constraint_if_requested model property (self#get_initial_p_nnconvex_constraint_or_die) in

				(* Check equality *)
				if LinearConstraint.p_nnconvex_constraint_is_equal initial_p_nnconvex_constraint result then Constraint_exact
				else Constraint_maybe_under
			) in

			(* Return the result *)
			Single_synthesis_result
			{
				(* Non-necessarily convex constraint guaranteeing the reachability of the desired states *)
				result				= Good_constraint (result, soundness);

				(* English description of the constraint *)
				constraint_description = "constraint guaranteeing reachability";

				(* Explored state space *)
				state_space			= state_space;

				(* Total computation time of the algorithm *)
				computation_time	= time_from start_time;

				(* Termination *)
				termination			= termination_status;
			}
		)

(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)


(************************************************************)
(************************************************************)
(* Class definition: algoEF *)
(************************************************************)
(************************************************************)
class algoEF (model : AbstractModel.abstract_model) (property : AbstractProperty.abstract_property) (options : Options.imitator_options) (state_predicate : AbstractProperty.state_predicate) =
	object (*(self)*) inherit algoEF_timed_or_untimed model property options state_predicate None (*as super*)

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


(************************************************************)
(************************************************************)
(* Class definition: algoEFtimed *)
(************************************************************)
(************************************************************)
class algoEFtimed (model : AbstractModel.abstract_model) (property : AbstractProperty.abstract_property) (options : Options.imitator_options) (state_predicate : AbstractProperty.state_predicate) (timed_interval : AbstractProperty.timed_interval) =
	object (*(self)*) inherit algoEF_timed_or_untimed model property options state_predicate (Some timed_interval) (*as super*)

	(************************************************************)
	(* Class variables *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "EF (timed)"

(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
