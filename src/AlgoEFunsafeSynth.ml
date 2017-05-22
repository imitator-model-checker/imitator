(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: "EF" algorithm (unsafe w.r.t. a set of bad states) [JLR15]
 * 
 * File contributors : Étienne André
 * Created           : 2017/02/03
 * Last modified     : 2017/02/03
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
class algoEFunsafeSynth =
	object (self) inherit algoEFsynth as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "EFunsafe"
	
	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

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
				
		(* Projecting onto SOME parameters if required *)
		let result =
		match model.projection with
		(* No projection: copy the initial p constraint *)
		| None -> bad_constraint
		(* Project *)
		| Some parameters ->
			(* Print some information *)
			if verbose_mode_greater Verbose_medium then(
				self#print_algo_message Verbose_medium "Projecting the bad constraint onto some of the parameters.";
				self#print_algo_message Verbose_medium "Before projection:";
				print_message Verbose_medium (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names bad_constraint);
			);

			(*** TODO! do only once for all... ***)
			let all_but_projectparameters = list_diff model.parameters parameters in
			
			(* Eliminate other parameters *)
			let projected_init_p_nnconvex_constraint = LinearConstraint.p_nnconvex_hide all_but_projectparameters bad_constraint in

			(* Print some information *)
			if verbose_mode_greater Verbose_medium then(
				self#print_algo_message Verbose_medium "After projection:";
				print_message Verbose_medium (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names projected_init_p_nnconvex_constraint);
			);
			
			(* Return *)
			projected_init_p_nnconvex_constraint
		in
		
		(* Get the termination status *)
		 let termination_status = match termination_status with
			| None -> raise (InternalError "Termination status not set in EFunsafe.compute_result")
			| Some status -> status
		in

(*		(* The tile nature is good if 1) it is not bad, and 2) the analysis terminated normally *)
		let statespace_nature =
			if statespace_nature = StateSpace.Unknown && termination_status = Regular_termination then StateSpace.Good
			(* Otherwise: unchanged *)
			else statespace_nature
		in*)
		
		(* Constraint is exact if termination is normal, possibly under-approximated otherwise *)
		(*** NOTE/TODO: technically, if the constraint is true/false, its soundness can be further refined easily ***)
		let soundness = if termination_status = Regular_termination then Constraint_exact else Constraint_maybe_under in

		(* Return the result *)
		Single_synthesis_result
		{
			(* Non-necessarily convex constraint guaranteeing the reachability of the bad location *)
			result				= Bad_constraint (result, soundness);
			
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
