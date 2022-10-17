(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * 
 * Module description: Non-zenoness emptiness check using CUB transformation (synthesizes valuations for which there exists a non-zeno loop in the PTA)
 * 
 * File contributors : Étienne André
 * Created           : 2016/10/10
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
open AlgoLoopSynth
open State



(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoNZCUB (model : AbstractModel.abstract_model) =
	object (self) inherit algoLoopSynth model as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	(* Flag to force under-approximation *)
	val mutable force_underapproximation = false


	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "NZCUB"
	
	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
		super#initialize_variables;

		(* The end *)
		()
	

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* When called, this method sets a flag that forces the algorithm to say that the returned constraint is an under-approximation *)
	(*** NOTE: used when NZ CUB is called after a CUB-detection for which the constraint does not cover all parameter valuations ***)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method force_underapproximation =
		force_underapproximation <- true
	

	(*	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Actions to perform when found a loop, before updating the state space *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_loop_constraint_before_state_space_update _ _ =
		(* Nothing! because we will check non-Zenoness AFTER updating state space *)
		()*)


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Actions to perform when found a loop (after updating the state space) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_loop_constraint loop_starting_point_state_index scc loop_px_constraint =
(*		(* Compute the SCC (after the transitions were updated, of course) *)
		self#print_algo_message Verbose_medium ("Computing SCC starting from s_" ^ (string_of_int loop_starting_point_state_index) ^ "…");
		let scc = StateSpace.reconstruct_scc state_space loop_starting_point_state_index in*)

		(* Print it *)
		if verbose_mode_greater Verbose_medium then(
			let verbose_string_of_state_index state_index =
				let state = StateSpace.get_state state_space state_index in
				"\n\ts_" ^ (string_of_int state_index) ^ "{" ^ (ModelPrinter.string_of_state model state) ^ "}"
			in
			self#print_algo_message Verbose_medium ("\nSCC: [" ^ (string_of_list_of_string_with_sep "\n\t- " (List.map verbose_string_of_state_index scc)) ^ "\n]");
		);
		
		(* Retrieve the extra clock *)
		let reset_clock = match model.special_reset_clock with
			| None -> raise (InternalError("A special reset clock should have been defined in AlgoNZCUB"))
			| Some clock_index -> clock_index
		in
		
		(* Compute the "b" variable in [WSWLSDYL14] *)
		let states_and_b = List.map (fun state_index ->
			let state_constraint = (StateSpace.get_state state_space state_index).px_constraint in
			
			(* b is true if time can elapse if the reset_clock is non-necessarily 0 *)
			let b = not (LinearConstraint.px_is_zero_in reset_clock state_constraint) in
			
			(* Print some information *)
			if verbose_mode_greater Verbose_high then(
				self#print_algo_message Verbose_high ("\nIs there some time elapsing for the reset clock in the following constraint? " ^ (string_of_bool b) ^ " \n" ^ (LinearConstraint.string_of_px_linear_constraint model.variable_names state_constraint) ^ "");
			);
			
			(* Return a pair state_index , b *)
			state_index , b
		) scc
		in
		
		(* First check that time-elapsing is possible, i.e., the zero clock is non-necessarily bound to 0 *)
		if not (List.exists (fun (_, b) -> b) states_and_b) then(
			(* If not: Zeno (even zero) cycle *)
			self#print_algo_message Verbose_standard ("Cycle found, but in 0-time");
		)else(

			(* Find transitions in the scc *)
			let all_transitions = StateSpace.find_transitions_in state_space scc in
			
			(* Compute the set of clocks that must be reset *)
			let transitions_with_resets_and_b = List.map (fun (source_index, combined_transition, target_index ) -> 
				(* Compute resets *)
				let resets = StateSpace.get_resets state_space source_index combined_transition target_index in
				
				(* Find the 'b' (which is that of the target) *)
				(*** NOTE: quite expensive, but much much less than computing resets (or any polyhedra operation) so we don't optimize here ***)
				let _ , b = List.find (fun (state_index , b) -> state_index  = target_index) states_and_b in
				
				(* Combine! *)
				source_index, combined_transition, b, resets , target_index
				
			) all_transitions in
	
			(* Print some information *)
			if verbose_mode_greater Verbose_high then(
				self#print_algo_message Verbose_high ("All transitions with flags: \n " ^ (string_of_list_of_string_with_sep "\n -- \n" (List.map (fun (source_index, combined_transition, b, resets , target_index) ->
					(StateSpace.string_of_state_index source_index)
					^ " --" ^ (model.action_names (StateSpace.get_action_from_combined_transition combined_transition))
					^ "," ^ (string_of_bool b)
					^ ",[" ^ (string_of_list_of_string_with_sep "," (List.map model.variable_names resets)) ^ "]"
					^ "--> " 
					^ (StateSpace.string_of_state_index target_index)
				) transitions_with_resets_and_b)) ^ "");
			);

			(* Apply CUB-PTA Emptiness Check *)
			(*** NOTE: we iterate on the clocks; while it may not be entirely optimal, we do not call clock_upper_bound_in more than needed (which is by far the most expensive function here ***)
			let check_violation = List.exists (fun clock_index ->
				(* Let us check whether this clock violates the check *)
				
				(* 1. We check whether the clock is not unbounded in one of the locations *)
				if List.exists (fun state_index ->
					(* Let us find the location *)
					let global_location = (StateSpace.get_state state_space state_index).global_location in
					
					(* Construct the invariant *)
					let invariant = AlgoStateBased.compute_valuated_invariant global_location in
					
					(* Check if not unbounded, i.e., bounded, i.e., upper bound is not undefined *)
					let is_bounded = (LinearConstraint.clock_upper_bound_in clock_index invariant <> None) in
					
					(* Print some information *)
					if verbose_mode_greater Verbose_high then(
						self#print_algo_message Verbose_high ("Is clock " ^ (model.variable_names clock_index)  ^ " bounded in the following invariant constraint? " ^ (string_of_bool is_bounded) ^ " \n" ^ (LinearConstraint.string_of_px_linear_constraint model.variable_names invariant) ^ "");
					);
					
					is_bounded
				) scc then(
					(* 2. Since the clock is bounded somewhere, we look for a label (X, b) s.t. this clock belongs to X *)
					let clock_never_reset = not (List.exists (fun (_, _, _, resets , _) ->
						List.mem clock_index resets
					) transitions_with_resets_and_b) in
					(* Print some information *)
					self#print_algo_message Verbose_high ("Is clock " ^ (model.variable_names clock_index)  ^ " never reset in the SCC? " ^ (string_of_bool clock_never_reset) ^ "");
					
					(* Return *)
					clock_never_reset
				)else(
					(* Print some information *)
					self#print_algo_message Verbose_high ("Clock " ^ (model.variable_names clock_index)  ^ " has no upper bound along the SCC.");

					(* Otherwise the clock is always unbounded: no problem, no violation *)
					false
				)
			) model.clocks_without_special_reset_clock
			in
			
			(* Print some information *)
			if check_violation then(
				self#print_algo_message Verbose_low ("Cycle found! But does not respect the conditions for non-Zenoness");
			)else(
				(* Print some information *)
				self#print_algo_message Verbose_standard ("Non-Zeno cycle found!");
				
				(* Real non-Zeno loop: update constraint *)
				self#update_loop_constraint loop_px_constraint;
			)
			
		);

		(* The end *)
		()



	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_result =
		(* Print some information *)
		self#print_algo_message_newline Verbose_standard (
			"Algorithm completed " ^ (after_seconds ()) ^ "."
		);
		
		(* Get the termination status *)
		 let termination_status = match termination_status with
			| None -> raise (InternalError "Termination status not set in NZCUB.compute_result")
			| Some status -> status
		in

		(* Constraint is exact if termination is normal, possibly under-approximated otherwise *)
		let soundness =
			(* …unless flag is set, in which case we have an under-approximation *)
			if force_underapproximation then Constraint_maybe_under else
			(if termination_status = Regular_termination then Constraint_exact else Constraint_maybe_under)
		in

		(* Return the result *)
		Single_synthesis_result
		{
			(* Non-necessarily convex constraint guaranteeing the existence of at least one loop *)
			result				= Good_constraint (synthesized_constraint, soundness);
			
			(* English description of the constraint *)
			constraint_description = "constraint guaranteeing the presence of at least one non-Zeno loop";
	
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
