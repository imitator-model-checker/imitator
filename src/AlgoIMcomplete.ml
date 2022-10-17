(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: IM algorithm with complete result, i.e., possibly non-convex and without random selection [AM15]
 * 
 * File contributors : Étienne André
 * Created           : 2017/03/21
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
open AlgoStateBased
open AlgoIMK



(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoIMcomplete (model : AbstractModel.abstract_model) (pval : PVal.pval) =
	object (self) inherit algoIMK model pval as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	(* Non-necessarily convex parameter constraint *)
	val mutable k_result : LinearConstraint.p_nnconvex_constraint = LinearConstraint.true_p_nnconvex_constraint ()

	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "IM"

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
		super#initialize_variables;
		(* The end *)
		()
	

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Checks a new state for pi0-compatibility .*)
	(* constr            : new state constraint            *)
	(*------------------------------------------------------------*)
	(* returns true if the state is pi0-compatible, and false otherwise *)
	(*------------------------------------------------------------*)
	(* side effect: add the negation of the p_constraint to all computed states *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method check_pi0compatibility (constr : LinearConstraint.px_linear_constraint) : bool =
		(* Retrieve the pi0 (dynamic!) *)
		let pi0 = self#get_reference_pval in
		
		self#print_algo_message_newline Verbose_medium ("Sarting pi0-compatibility check…");
		
		self#print_algo_message_newline Verbose_high ("Hiding non parameters…");
		
		(* Hide non-parameters *)
		let p_constraint = LinearConstraint.px_hide_nonparameters_and_collapse constr in
		
		self#print_algo_message_newline Verbose_high ("Parameters now hidden:");
		(* Print some information *)
		if verbose_mode_greater Verbose_high then(
			print_message Verbose_high (LinearConstraint.string_of_p_linear_constraint model.variable_names p_constraint);
		);
		(* Check the pi0-compatibility *)
		self#print_algo_message_newline Verbose_high ("Checking pi-compatibility:");
		let is_pi0_compatible = LinearConstraint.is_pi0_compatible pi0#get_value p_constraint in
		
		(* If pi0-incompatible: negate *)
		if not is_pi0_compatible then (
			self#print_algo_message_newline Verbose_low ("Found a pi0-incompatible state.");
			(* Print some information *)
			if verbose_mode_greater Verbose_medium then(
				self#print_algo_message Verbose_high ("Associated constraint:");
				print_message Verbose_high (LinearConstraint.string_of_px_linear_constraint model.variable_names constr);
				self#print_algo_message_newline Verbose_medium ("Projected onto the parameters:");
				print_message Verbose_medium (LinearConstraint.string_of_p_linear_constraint model.variable_names p_constraint);
				if verbose_mode_greater Verbose_high then(
					self#print_algo_message_newline Verbose_high ("Recall that pi0 is:");
					print_message Verbose_high   (ModelPrinter.string_of_pval model pi0);
				);
			);
			
			(* Should we explore pi-incompatible states? *)
			if not (self#process_pi_incompatible_states()) then(
				self#print_algo_message Verbose_low ("Cut pi-incompatible branch.");
				false
			)else(
			
				(* Update K := K ^ not s *)
				LinearConstraint.p_nnconvex_difference_assign k_result (LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint p_constraint);

				(* Print some information *)
				if verbose_mode_greater Verbose_low then(
					self#print_algo_message Verbose_low ("K now equal to:");
					print_message Verbose_low ("  " ^ (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names k_result));
				);
				
				(* If pi-incompatible *)
				false
 			)(* Endif explore pi-incompatible state *)
		) (* end if pi-incompatible *)
		else(
			(* Update K := K ^ s *)
			LinearConstraint.p_nnconvex_intersection_assign k_result p_constraint;
			
			(* Print some information *)
			if verbose_mode_greater Verbose_low then(
				self#print_algo_message Verbose_low ("K now equal to:");
				print_message Verbose_low ("  " ^ (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names k_result));
			);

			(* Return true (as it is pi-compatible) *)
			true
		)

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_result =
		(* Retrieve the input options *)
		let options = Input.get_options () in
		
		(* Get the termination status *)
		 let termination_status = match termination_status with
			| None -> raise (InternalError "Termination status not set in IMcomplete.compute_result")
			| Some status -> status
		in

		(* The state space nature is good if 1) it is not bad, and 2) the analysis terminated normally *)
		let statespace_nature =
			if statespace_nature = StateSpace.Unknown && termination_status = Regular_termination then StateSpace.Good
			(* Otherwise: unchanged *)
			else statespace_nature
		in
		
		(* Constraint is… *)
		let soundness =
			let dangerous_inclusion = options#comparison_operator = AbstractAlgorithm.Inclusion_check || options#comparison_operator = AbstractAlgorithm.Including_check || options#comparison_operator = AbstractAlgorithm.Double_inclusion_check in

			(* EXACT if termination is normal and no incl and no merge were performed *)
			if termination_status = Regular_termination && not dangerous_inclusion && (options#merge_algorithm = Merge_none) then Constraint_exact
			(* OVER-APPROXIMATED if no random selections were performed and either termination is not normal or merging was used or state inclusion was used *)
			else if termination_status <> Regular_termination || dangerous_inclusion || (options#merge_algorithm <> Merge_none) then Constraint_maybe_over
			(* UNKNOWN otherwise *)
			else Constraint_maybe_invalid
		in
		
		let result = match statespace_nature with
			(*** NOTE: if a safety property is defined and if the state space reaches some unsafe states, then the constraint is considered as bad.
	In any other case (safe state space, or no safety property defined), the constraint nature is considered as good. ***)
			| StateSpace.Good | StateSpace.Unknown -> Good_constraint(k_result, soundness)
			| StateSpace.Bad -> Bad_constraint(k_result, soundness)
		in

		(* Return result *)
		Point_based_result
		{
			(* Reference valuation *)
			reference_val		= self#get_reference_pval;
			
			(* Result of the algorithm *)
			result				= result;
			
			(* Explored state space *)
			state_space			= state_space;
			
			(* Nature of the state space *)
(* 			statespace_nature	= statespace_nature; *)
			
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
