(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: "AG not" algorithm (safety from a set of bad states) [JLR15]
 * 
 * File contributors : Étienne André
 * Created           : 2017/02/03
 *
 ************************************************************)


(************************************************************)
(************************************************************)
(* Modules *)
(************************************************************)
(************************************************************)
open ImitatorUtilities
open Exceptions
open AbstractModel
open Result
open AlgoEUgen



(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
(*** NOTE: AG is implemented as the negation of EF ***)
class algoAGnot (model : AbstractModel.abstract_model) (property : AbstractProperty.abstract_property) (options : Options.imitator_options) (state_predicate : AbstractProperty.state_predicate) =
	object (self) inherit algoEUgen model property options None state_predicate None (*as super*)
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "AGnot"
	
	
	
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
		
		(* Print some information *)
		self#print_algo_message_newline Verbose_low (
			"Performing initial constraint \ final resulting constraint…"
		);
		
		
		(* Perform result = initial_state|P \ synthesized_constraint *)
		
		(* Retrieve the initial parameter constraint *)
		let initial_p_nnconvex_constraint : LinearConstraint.p_nnconvex_constraint = self#get_initial_p_nnconvex_constraint_or_die in
		(* Copy *)
		let result : LinearConstraint.p_nnconvex_constraint = LinearConstraint.p_nnconvex_copy initial_p_nnconvex_constraint in

		if verbose_mode_greater Verbose_medium then(
			self#print_algo_message Verbose_medium "As a reminder, the initial constraint is:";
			self#print_algo_message Verbose_medium (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names initial_p_nnconvex_constraint);
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

		(* managing strategic computation resutl here*)
		if LinearConstraint.p_nnconvex_constraint_is_false result && state_space#has_coalition then (
			try
		
				(* propagate the killed strategies, and keep one or all the biggest depending of synth/witness*)
				state_space#propagate_killed_strategy ;
				let state_index_to_print: int list  = if (property.synthesis_type = Synthesis) then (state_space#keep_biggest_strategies (state_space#find_all_alive_strategies)) else ([state_space#find_last_alive_strategy])
				in

				state_space#display_strategy_constraint_index_list state_index_to_print model.automata_names model.location_names model.action_names model.variable_names;

				state_space#initialize_winning_states state_index_to_print;
				let total = List.length state_index_to_print in
				print_message Verbose_standard ("Total alive strategies : " ^ (string_of_int total));

			with
			| NoAliveStrategy ->
					(* No strategy survives after propagation — property cannot be guaranteed *)
					self#print_algo_message Verbose_standard "No alive strategy can guarantee global safety."
			| _ -> ()  (* Ignore any other unexpected exceptions *)
		);
		
		(* Get the termination status *)
		 let termination_status = match termination_status with
			| None -> raise (InternalError ("Termination status not set in " ^ self#algorithm_name ^ ".compute_result"))
			| Some status -> status
		in
		
		(* Constraint is exact if termination is normal, possibly over-approximated otherwise (as it is the negation of a possible under-approximation of the bad constraint) *)
		let soundness = if termination_status = Regular_termination then Constraint_exact else(
				(* Check if the set of valuations is empty *)

				(* If the constraint is false: then exact *)
				if LinearConstraint.p_nnconvex_constraint_is_false result then Constraint_exact

				(* Otherwise: over-approximation *)
				else Constraint_maybe_over
		) in

		(* Return the result *)
		Single_synthesis_result
		{
			(* Non-necessarily convex constraint guaranteeing the non-reachability of the bad location *)
			result				= Good_constraint (result, soundness);
			
			(* English description of the constraint *)
			constraint_description = "constraint guaranteeing global safety";
	
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
