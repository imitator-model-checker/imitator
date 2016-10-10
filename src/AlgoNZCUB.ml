(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Non-zenoness emptiness check using CUB transformation (synthesizes valuations for which there exists a non-zeno loop in the PTA)
 * 
 * File contributors : Étienne André
 * Created           : 2016/10/10
 * Last modified     : 2016/10/10
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



(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoNZCUB =
	object (self) inherit algoLoopSynth as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	

	
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
	(* Actions to perform when found a loop, before updating the state space *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_loop_constraint_before_state_space_update _ _ =
		(* Nothing! because we will check non-Zenoness AFTER updating state space *)
		()


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Actions to perform when found a loop, after updating the state space *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_loop_constraint_after_state_space_update loop_starting_point_state_index px_constraint =
		(* Retrieve the model *)
		let model = Input.get_model () in

		(* Compute the SCC (after the transitions were updated, of course) *)
		if verbose_mode_greater Verbose_low then(
			let scc = StateSpace.reconstruct_scc state_space loop_starting_point_state_index in
			(* Print it *)
			let verbose_string_of_state_index state_index =
				let state = StateSpace.get_state state_space state_index in
				"\n\ts_" ^ (string_of_int state_index) ^ "{" ^ (ModelPrinter.string_of_state model state) ^ "}"
			in
			print_message Verbose_low ("\nSCC: [" ^ (string_of_list_of_string_with_sep "\n\t- " (List.map verbose_string_of_state_index scc)) ^ "\n]");
			
			
			(* Retrieve the extra clock *)
			let reset_clock = match model.special_reset_clock with
				| None -> raise (InternalError("A special reset clock should have been defined in AlgoNZCUB"))
				| Some clock_index -> clock_index
			in
			
			(* Compute the "b" variable in [WSWLSDYL14] *)
			let states_and_b = List.map (fun state_index ->
				let _, state_constraint = StateSpace.get_state state_space state_index in
				(* Return a pair state_index , b *)
				state_index , (LinearConstraint.px_is_zero_in reset_clock state_constraint)
			) scc
			in
			
			(* First check that time-elapsing is possible, i.e., the zero clock is non-necessarily bound to 0 *)
			if not (List.exists (fun (_, b) -> b) states_and_b) then(
				(* If not: Zeno (even zero) cycle *)
				print_message Verbose_low ("Cycle found! But in 0-time");
			)else(

				(* Find transitions in the scc *)
				let all_transitions = StateSpace.find_transitions_in state_space scc in
				
				(* Compute the set of clocks that must be reset *)
				let transitions_with_resets_and_b = List.map (fun (source_index, action_index, target_index ) -> 
					(* Compute resets *)
					(*** WARNING! big hack: due to the fact that StateSpace only maintains the action, then we have to hope that the PTA is deterministic to retrieve the edge, and hence the set of clocks to be reset along a transition ***)
					let resets = StateSpace.get_resets state_space source_index action_index target_index in
					
					(* Find the 'b' (which is that of the target) *)
					(*** NOTE: quite expensive, but much much less than computing resets (or any polyhedra operation) so we don't optimize here ***)
					let _ , b = List.find (fun (state_index , b) -> state_index  = target_index) states_and_b in
					
					(* Combine! *)
					source_index, action_index, b, resets , target_index
					
				) all_transitions in
				
				(* Apply CUB-PTA Emptiness Check *)
				(*** NOTE: we iterate on the clocks; while it may not be entirely optimal, we do not call px_is_bounded_from_above_in more than needed (which is by far the most expensive function here ***)
				let check_violation = List.exists (fun clock_index ->
					(* Let us check whether this clock violates the check *)
					
					(* 1. We check whether the clock is not unbounded in one of the states *)
					if List.exists (fun state_index ->
						(* Let us find the linear constraint *)
						let _ , state_constraint = StateSpace.get_state state_space state_index in
						(* Check if not unbounded, i.e., bounded *)
						LinearConstraint.px_is_bounded_from_above_in clock_index state_constraint 
					) scc then(
						(* 2. Since the clock is bounded somewhere, we look for a label (X, b) s.t. this clock belongs to X *)
						not (List.exists (fun (_, _, _, resets , _) ->
							List.mem clock_index resets
						) transitions_with_resets_and_b)
					)else(
						(* Otherwise the clock is always unbounded: no problem, no violation *)
						false
					)
				) model.clocks_without_special_reset_clock
				in
				
				(*  *)
				if check_violation then(
					print_message Verbose_low ("Cycle found! But does not respect the conditions for non-Zenoness");
				)else(
					(* Print some information *)
					print_message Verbose_standard ("Non-Zeno cycle found!");
					
					(* Real non-Zeno loop: update constraint *)
					self#update_loop_constraint px_constraint;
				)
				
			)
			
		);
		(* The end *)
		()



	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_result =
		(* Retrieve the model *)
(* 		let model = Input.get_model () in *)

		(* Print some information *)
		self#print_algo_message_newline Verbose_standard (
			"Algorithm completed " ^ (after_seconds ()) ^ "."
		);
		
		(* Get the termination status *)
		 let termination_status = match termination_status with
			| None -> raise (InternalError "Termination status not set in LoopSynth.compute_result")
			| Some status -> status
		in

		(* Constraint is exact if termination is normal, possibly under-approximated otherwise *)
		let soundness = if termination_status = Regular_termination then Constraint_exact else Constraint_maybe_under in

		(* Return the result *)
		Single_synthesis_result
		{
			(* Non-necessarily convex constraint guaranteeing the existence of at least one loop *)
			result				= Good_constraint (loop_constraint, soundness);
			
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
