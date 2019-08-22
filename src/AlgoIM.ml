(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * 
 * Module description: IM algorithm [ACEF09]
 * 
 * File contributors : Étienne André
 * Created           : 2016/01/06
 * Last modified     : 2019/06/11
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
open State



(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoIM =
	object (self) inherit algoIMK as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	
	
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
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_result =
		(* Retrieve the input options *)
		let options = Input.get_options () in
		
		(*** NOTE: Method used here: intersection of all p-constraints ***)
		(* Alternative methods would have been: 1) on-the-fly intersection (everytime a state is met) or 2) intersection of all final states, i.e., member of a loop, or deadlock states *)

		(* Create the result *)
		let p_constraint = LinearConstraint.p_true_constraint() in
		
		self#print_algo_message Verbose_low ("Performing the intersection of all p-constraints...");
		
		(* Iterate on all states *)
(* 		val iterate_on_states : (state_index -> abstract_state -> unit) -> state_space -> unit *)
		StateSpace.iterate_on_states (fun state_index abstract_state ->
			(* Retrieve the px-constraint *)
			let px_linear_constraint = abstract_state.px_constraint in
			(* Project onto the parameters *)
			let projection = LinearConstraint.px_hide_nonparameters_and_collapse px_linear_constraint in
			(* Intersect with the result *)

			(*** TODO: check if only one intersection with the list of all projections gathered would be more efficient ??? ***)
			
			LinearConstraint.p_intersection_assign p_constraint [projection];
		) state_space;
		
	
		self#print_algo_message_newline Verbose_standard (
			"Successfully terminated " ^ (after_seconds ()) ^ "."
		);

		(* Get the termination status *)
		 let termination_status = match termination_status with
			| None -> raise (InternalError "Termination status not set in IM.compute_result")
			| Some status -> status
		in

		(* The state space nature is good if 1) it is not bad, and 2) the analysis terminated normally *)
		let statespace_nature =
			if statespace_nature = StateSpace.Unknown && termination_status = Regular_termination then StateSpace.Good
			(* Otherwise: unchanged *)
			else statespace_nature
		in
		
		(* Constraint is... *)
		let soundness = 
			(* EXACT if termination is normal and no random selections and no incl and no merge were performed *)
			if termination_status = Regular_termination && nb_random_selections = 0 && not options#inclusion && not options#merge then Constraint_exact
			(* UNDER-APPROXIMATED if termination is normal and random selections and no incl and no merge were performed  were performed *)
			else if termination_status = Regular_termination && nb_random_selections > 0 && not options#inclusion && not options#merge then Constraint_maybe_under
			(* OVER-APPROXIMATED if no random selections were performed and either termination is not normal or merging was used or state inclusion was used *)
			else if nb_random_selections = 0 && (termination_status <> Regular_termination || options#inclusion || options#merge) then Constraint_maybe_over
			(* UNKNOWN otherwise *)
			else Constraint_maybe_invalid
		in
		
		let result = match statespace_nature with
			(*** NOTE: if a safety property is defined and if the state space reaches some unsafe states, then the constraint is considered as bad.
	In any other case (safe state space, or no safety property defined), the constraint nature is considered as good. ***)
			| StateSpace.Good | StateSpace.Unknown -> Good_constraint(LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint p_constraint, soundness)
			| StateSpace.Bad -> Bad_constraint(LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint p_constraint, soundness)
		in

		(* Return result *)
		Point_based_result
		{
			(* Reference valuation *)
			reference_val		= Input.get_pi0();
			
			(* Result of the algorithm *)
			result				= result;
			
			(* Explored state space *)
			state_space			= state_space;
			
			(* Nature of the state space *)
(* 			statespace_nature	= statespace_nature; *)
			
			(* Number of random selections of pi-incompatible inequalities performed *)
(* 			nb_random_selections= nb_random_selections; *)
	
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
