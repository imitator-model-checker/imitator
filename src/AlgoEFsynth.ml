(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: EFsynth algorithm [JLR15]
 * 
 * File contributors : Étienne André
 * Created           : 2015/11/25
 * Last modified     : 2016/08/13
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
open AlgoBFS



(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoEFsynth =
	object (self) inherit algoBFS as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	(* Non-necessarily convex constraint allowing the reachability of the bad location *)
	val mutable bad_constraint : LinearConstraint.p_nnconvex_constraint = LinearConstraint.false_p_nnconvex_constraint ()

	
	(* Non-necessarily convex parameter constraint of the initial state (constant object used as a shortcut, as it is used at the end of the algorithm) *)
	(*** WARNING: these lines are copied from AlgoDeadlockFree ***)
	val init_p_nnconvex_constraint : LinearConstraint.p_nnconvex_constraint =
		(* Retrieve the model *)
		let model = Input.get_model () in
		LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint model.initial_p_constraint

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "EFsynth experimental"
	
	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
		super#initialize_variables;
		(*** NOTE: duplicate operation ***)
		bad_constraint <- LinearConstraint.false_p_nnconvex_constraint ();

		(* The end *)
		()
	

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Add a new state to the state_space (if indeed needed) *)
	(* Side-effects: modify new_states_indexes *)
	(*** TODO: move new_states_indexes to a variable of the class ***)
	(* Return true if the state is not discarded by the algorithm, i.e., if it is either added OR was already present before *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** WARNING/BADPROG: the following is partially copy/paste to AlgoPRP.ml ***)
	method add_a_new_state state_space orig_state_index new_states_indexes action_index location (current_constraint : LinearConstraint.px_linear_constraint) =
		(* Retrieve the model *)
		let model = Input.get_model () in

		(* Retrieve the input options *)
(* 		let options = Input.get_options () in *)

		(* Build the state *)
		let new_state = location, current_constraint in

		(* Print some information *)
		if verbose_mode_greater Verbose_total then(
			(*** TODO: move that comment to a higher level function? (post_from_one_state?) ***)
			self#print_algo_message Verbose_total ("Consider the state \n" ^ (ModelPrinter.string_of_state model new_state));
		);

		let new_state_index, added = (
			StateSpace.add_state state_space new_state
		) in
		(* If this is really a new state *)
		if added then (

			(* First check whether this is a bad tile according to the property and the nature of the state *)
			self#update_statespace_nature new_state;
			
			(* Will the state be added to the list of new states (the successors of which will be computed)? *)
			let to_be_added = ref true in
			
			(* If synthesis / PRP: add the constraint to the list of successful constraints if this corresponds to a bad location *)
			begin
			match model.correctness_condition with
			| None -> raise (InternalError("A correctness property must be defined to perform EF-synthesis or PRP. This should have been checked before."))
			| Some (Unreachable unreachable_global_locations) ->
				
				(* Check whether the current location matches one of the unreachable global locations *)
				if State.match_unreachable_global_locations unreachable_global_locations location then(
				
					(* Project onto the parameters *)
					let p_constraint = LinearConstraint.px_hide_nonparameters_and_collapse current_constraint in
					
					(* Projecting onto SOME parameters if required *)
					begin
					match model.projection with
					(* Unchanged *)
					| None -> ()
					(* Project *)
					| Some parameters ->
						(* Print some information *)
						self#print_algo_message Verbose_medium "Projecting onto some of the parameters.";

						(*** TODO! do only once for all... ***)
						let all_but_projectparameters = list_diff model.parameters parameters in
						
						(* Eliminate other parameters *)
						LinearConstraint.p_hide_assign all_but_projectparameters p_constraint;

						(* Print some information *)
						if verbose_mode_greater Verbose_medium then(
							print_message Verbose_medium (LinearConstraint.string_of_p_linear_constraint model.variable_names p_constraint);
						);
					end;

					(* Print some information *)
					self#print_algo_message Verbose_standard "Found a state violating the property.";
						
					(* Update the bad constraint using the current constraint *)
					(*** NOTE: perhaps try first whether p_constraint <= bad_constraint ? ***)
					LinearConstraint.p_nnconvex_p_union bad_constraint p_constraint;
					
					(* Print some information *)
					if verbose_mode_greater Verbose_medium then(
						self#print_algo_message Verbose_medium "Adding the following constraint to the list of bad constraints:";
						print_message Verbose_medium (LinearConstraint.string_of_p_linear_constraint model.variable_names p_constraint);
						
						self#print_algo_message Verbose_medium "The bad constraint is now:";
						print_message Verbose_medium (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names bad_constraint);
					);
					
					(* Do NOT compute its successors; cut the branch *)
					to_be_added := false;
					
				)else(
					self#print_algo_message Verbose_medium "State not corresponding to the one wanted.";
				);
			| _ -> raise (InternalError("[EFsynth/PRP] IMITATOR currently ony implements the non-reachability-like properties. This should have been checked before."))
			end
			;
			
			(* If to be added: if the state is included into the bad constraint, no need to explore further, and hence do not add *)
			if !to_be_added then(
			
				(*** WARNING: this may have been computed above! ***)
				(*** TODO: use a small ad-hoc cache system ***)
				(* Project onto the parameters *)
				let p_constraint = LinearConstraint.px_hide_nonparameters_and_collapse current_constraint in
				
				(* if p_constraint <= bad_constraint *)
				if LinearConstraint.p_nnconvex_constraint_is_leq (LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint p_constraint) bad_constraint then (
					(* Print some information *)
					self#print_algo_message Verbose_standard "Found a state included in bad valuations; cut branch.";
					
					(* Do NOT compute its successors; cut the branch *)
					to_be_added := false;
				);
			);

			(* Add the state_index to the list of new states (used to compute their successors at the next iteration) *)
			if !to_be_added then
				new_states_indexes := new_state_index :: !new_states_indexes;
			
		) (* end if new state *)
		;
		
		(*** TODO: move the rest to a higher level function? (post_from_one_state?) ***)
		
		(* Update the transitions *)
		StateSpace.add_transition state_space (orig_state_index, action_index, new_state_index);
		(* Print some information *)
		if verbose_mode_greater Verbose_high then (
			let beginning_message = (if added then "NEW STATE" else "Old state") in
			print_message Verbose_high ("\n" ^ beginning_message ^ " reachable through action '" ^ (model.action_names action_index) ^ "': ");
			print_message Verbose_high (ModelPrinter.string_of_state model new_state);
		);
	
		(* The state is kept in any case *)
		true
	(*** WARNING/BADPROG: what preceedes is partially copy/paste to AlgoPRP.ml ***)
	

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Actions to perform when meeting a state with no successors: nothing to do for this algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_deadlock_state state_index = ()
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Actions to perform at the end of the computation of the *successors* of post^n (i.e., when this method is called, the successors were just computed). Nothing to do for this algorithm. *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_post_n (post_n : State.state_index list) = ()

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Check whether the algorithm should terminate at the end of some post, independently of the number of states to be processed (e.g., if the constraint is already true or false) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** TODO: could be stopped when the bad constraints are equal to the initial p-constraint ***)
	method check_termination_at_post_n = false

	
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
			| None -> raise (InternalError "Termination status not set in EFsynth.compute_result")
			| Some status -> status
		in

		(* The tile nature is good if 1) it is not bad, and 2) the analysis terminated normally *)
		let statespace_nature =
			if statespace_nature = StateSpace.Unknown && termination_status = Regular_termination then StateSpace.Good
			(* Otherwise: unchanged *)
			else statespace_nature
		in
		
		(* Constraint is exact if termination is normal, possibly under-approximated otherwise *)
		let soundness = if termination_status = Regular_termination then Constraint_exact else Constraint_maybe_under in

		(* Return the result *)
		PDFC_result
		{
			(* Non-necessarily convex constraint guaranteeing the non-reachability of the bad location *)
			result				= Single_constraint(result, soundness);
			
			(* Explored state space *)
			state_space			= state_space;
			
			(* Nature of the state space *)
			statespace_nature	= statespace_nature;
			
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
