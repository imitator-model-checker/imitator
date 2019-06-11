(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * 
 * Module description: EFsynth algorithm [JLR15]
 * 
 * File contributors : Étienne André
 * Created           : 2015/11/25
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
open State



(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoEFsynth =
	object (self) inherit algoStateBased as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	(* List of constraints allowing the reachability of the bad location *)
	val mutable bad_constraints = []

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "EFsynth (old version)"
	
	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
		super#initialize_variables;
		bad_constraints <- [];

		(* The end *)
		()
	

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Process a symbolic state *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private process_state (state : state) =
		(* Retrieve the model *)
		let model = Input.get_model () in
		
		let state_location, state_constraint = state.global_location, state.px_constraint in
		
		let to_be_added = match model.correctness_condition with
		| None -> raise (InternalError("A correctness property must be defined to perform EF-synthesis or PRP. This should have been checked before."))
		| Some (Unreachable unreachable_global_locations) ->
			
			(* Check whether the current location matches one of the unreachable global locations *)
			if State.match_unreachable_global_locations unreachable_global_locations state_location then(
			
				(* Project onto the parameters *)
				let p_constraint = LinearConstraint.px_hide_nonparameters_and_collapse state_constraint in
				
				(* Projecting onto SOME parameters if required *)
				begin
				match model.projection with
				(* Unchanged *)
				| None -> ()
				(* Project *)
				| Some parameters ->
					self#print_algo_message Verbose_medium "Projecting onto some of the parameters.";
					(*** TODO! do only once for all... ***)
					let all_but_projectparameters = list_diff model.parameters parameters in
					(* Eliminate other parameters *)
					LinearConstraint.p_hide_assign all_but_projectparameters p_constraint;
				end;
				
				(* Add the constraint to the list of constraints, unless it is already present there *)
				(*** TODO: also check for REVERSE inclusion (old included in new) ***)
				(*** TODO: merge this list on-the-fly!! ***)
				(*** TODO: even better, directly use a non-convex constraint using PPL, and leave the work to PPL ***)
				if List.exists (LinearConstraint.p_is_leq p_constraint) bad_constraints then(
					self#print_algo_message Verbose_low "Found a state violating the property but the constraint is not new.";
				)else(
					bad_constraints <- p_constraint :: bad_constraints;
					(* Print some information *)
					self#print_algo_message Verbose_standard "Found a state violating the property.";
					
					(* Print some information *)
					if verbose_mode_greater Verbose_medium then(
						self#print_algo_message Verbose_medium "Adding the following constraint to the list of bad constraints:";
						print_message Verbose_medium (LinearConstraint.string_of_p_linear_constraint model.variable_names p_constraint);
					);
					
				);
				
				(* Do NOT compute its successors *)
				false
				
			)else(
				self#print_algo_message Verbose_medium "State not corresponding to the one wanted.";
				
				(* Keep the state as it is not a bad state *)
				true
			);
		| _ -> raise (InternalError("IMITATOR currently ony supports non-reachability-like properties. This should have been checked before."))

		in
		(* Return result *)
		to_be_added
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Actions to perform with the initial state; returns true unless the initial state cannot be kept (in which case the algorithm will stop immediately) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_initial_state initial_state = self#process_state initial_state

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Add a new state to the state_space (if indeed needed) *)
	(* Side-effects: modify new_states_indexes *)
	(*** TODO: move new_states_indexes to a variable of the class ***)
	(* Return true if the state is not discarded by the algorithm, i.e., if it is either added OR was already present before *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** WARNING/BADPROG: the following is partially copy/paste to AlgoPRP.ml ***)
	method add_a_new_state orig_state_index new_states_indexes action_index location (final_constraint : LinearConstraint.px_linear_constraint) =
		(* Retrieve the model *)
(* 		let model = Input.get_model () in *)

		(* Build the state *)
		let new_state = { global_location = location ; px_constraint = final_constraint } in

		(* Try to add the new state to the state space *)
		let addition_result = StateSpace.add_state state_space (self#state_comparison_operator_of_options) new_state in
		
		begin
		match addition_result with
		(* If the state was present: do nothing *)
		| StateSpace.State_already_present _ -> ()
		(* If this is really a new state, or a state larger than a former state *)
		| StateSpace.New_state new_state_index | StateSpace.State_replacing new_state_index ->

			(* First check whether this is a bad tile according to the property and the nature of the state *)
			self#update_statespace_nature new_state;
			
			(* Will the state be added to the list of new states (the successors of which will be computed)? *)
			let to_be_added = self#process_state new_state in

			(* Add the state_index to the list of new states (used to compute their successors at the next iteration) *)
			if to_be_added then
				new_states_indexes := new_state_index :: !new_states_indexes;
			
		end (* end if new state *)
		;
		
		(*** TODO: move the rest to a higher level function? (post_from_one_state?) ***)
		
		(* Update the transitions *)
		self#add_transition_to_state_space (orig_state_index, action_index, (*** HACK ***) match addition_result with | StateSpace.State_already_present new_state_index | StateSpace.New_state new_state_index | StateSpace.State_replacing new_state_index -> new_state_index) addition_result;
	
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
		self#print_algo_message_newline Verbose_standard (
			"Algorithm completed " ^ (after_seconds ()) ^ "."
		);
		
		
		(*** TODO: compute as well *good* zones, depending whether the analysis was exact, or early termination occurred ***)
		
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
		Deprecated_efsynth_result
		{
			(* List of constraints ensuring EF location *)
			constraints			= bad_constraints;
			
			(* Explored state space *)
			state_space			= state_space;
			
			(* Nature of the state space *)
			statespace_nature	= statespace_nature;
			
			(* Total computation time of the algorithm *)
			computation_time	= time_from start_time;
			
			(* Soudndness of the result *)
			soundness			= soundness;
	
			(* Termination *)
			termination			= termination_status;
		}
	
(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
