(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: "EF min" algorithm: minimization of a parameter valuation for which there exists a run leading to some states
 * 
 * File contributors : Étienne André
 * Created           : 2017/05/02
 * Last modified     : 2017/05/02
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
open Result
open AlgoEFsynth



(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoEFmin =
	object (self) inherit algoEFsynth as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	val mutable current_min : LinearConstraint.p_linear_constraint option = None
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "EFmin"
	
	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Actions to perform when trying to minimize/maximize a parameter. Returns true if the same should be kept, false if discarded. *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method optimize_parameter state = 
		(* Retrieve the model *)
		let model = Input.get_model () in

		(* Retrieve the constraint *)
		let _, px_constraint = state in
		
		(* Retrieve the parameter to be projected onto *)
		let parameter_index = match model.optimized_parameter with
			| Minimize parameter_index -> parameter_index
			| _ -> raise (InternalError("A minimized parameter should be defined in the model to run " ^ self#algorithm_name))
		in

		(*** TODO: compute once for all ***)
		let parameters_to_hide = OCamlUtilities.list_remove_first_occurence parameter_index model.parameters in

		(* Projet the constraint onto that parameter *)
		let projected_constraint = LinearConstraint.px_hide_allclocks_and_someparameters_and_collapse parameters_to_hide px_constraint in

		(* Check if a minimum constraint was defined *)
		match current_min with
			| None ->
				(* Set the minimum *)
				current_min <- Some projected_constraint;
				(* Keep the state *)
				true
			| Some current_min_constraint ->
				(* Test if the current minimum is already larger *)
				(*** TODO ***)
				
				(* If so: discard the state *)
				(*** TODO ***)
				
				(* Otherwise: keep it *)
				(*** TODO ***)
				
				false


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_result =
		(* Retrieve the model *)
		let model = Input.get_model () in

		(* Print some information *)
		self#print_algo_message_newline Verbose_standard (
			"Algorithm completed " ^ (after_seconds ()) ^ "."
		);
		
		
		(*** TODO: compute as well *good* zones, depending whether the analysis was exact, or early termination occurred ***)
		
		(* Print some information *)
		self#print_algo_message_newline Verbose_low (
			"Performing negation of final constraint..."
		);
		
		
		(* Perform result = initial_state|P \ bad_constraint *)
		
		(* Projecting onto SOME parameters if required *)
		let result =
		match model.projection with
		(* No projection: copy the initial p constraint *)
		| None -> LinearConstraint.p_nnconvex_copy init_p_nnconvex_constraint
		(* Project *)
		| Some parameters ->
			(* Print some information *)
			if verbose_mode_greater Verbose_medium then(
				self#print_algo_message Verbose_medium "Projecting the initial constraint onto some of the parameters.";
				self#print_algo_message Verbose_medium "Before projection:";
				print_message Verbose_medium (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names init_p_nnconvex_constraint);
			);

			(*** TODO! do only once for all... ***)
			let all_but_projectparameters = list_diff model.parameters parameters in
			
			(* Eliminate other parameters *)
			let projected_init_p_nnconvex_constraint = LinearConstraint.p_nnconvex_hide all_but_projectparameters init_p_nnconvex_constraint in

			(* Print some information *)
			if verbose_mode_greater Verbose_medium then(
				self#print_algo_message Verbose_medium "After projection:";
				print_message Verbose_medium (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names projected_init_p_nnconvex_constraint);
			);
			
			(* Return *)
			projected_init_p_nnconvex_constraint
		in
		
		(* Perform the difference *)
		LinearConstraint.p_nnconvex_difference result bad_constraint;
		
		
		(* Print some information *)
		self#print_algo_message_newline Verbose_medium (
			"Negation of final constraint completed."
		);
		
		(* Get the termination status *)
		 let termination_status = match termination_status with
			| None -> raise (InternalError "Termination status not set in AGsafe.compute_result")
			| Some status -> status
		in

(*		(* The tile nature is good if 1) it is not bad, and 2) the analysis terminated normally *)
		let statespace_nature =
			if statespace_nature = StateSpace.Unknown && termination_status = Regular_termination then StateSpace.Good
			(* Otherwise: unchanged *)
			else statespace_nature
		in*)
		
		(* Constraint is exact if termination is normal, possibly over-approximated otherwise (as it is the negation of a possible under-approximation of the bad constraint) *)
		let soundness = if termination_status = Regular_termination then Constraint_exact else Constraint_maybe_over in

		(* Return the result *)
		Single_synthesis_result
		{
			(* Non-necessarily convex constraint guaranteeing the non-reachability of the bad location *)
			result				= Good_constraint (result, soundness);
			
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
