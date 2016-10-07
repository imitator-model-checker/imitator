(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: LoopSynth algorithm [AL16] (synthesizes valuations for which there exists a loop in the PTA)
 * 
 * File contributors : Étienne André
 * Created           : 2016/08/24
 * Last modified     : 2016/10/07
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
class algoLoopSynth =
	object (self) inherit algoBFS as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	(* Non-necessarily convex constraint allowing the presence of a loop *)
	val mutable loop_constraint : LinearConstraint.p_nnconvex_constraint = LinearConstraint.false_p_nnconvex_constraint ()

	
	(* Non-necessarily convex parameter constraint of the initial state (constant object used as a shortcut, as it is used at the end of the algorithm) *)
	(*** WARNING: these lines are copied from AlgoDeadlockFree ***)
	val init_p_nnconvex_constraint : LinearConstraint.p_nnconvex_constraint =
		(* Retrieve the model *)
		let model = Input.get_model () in
		LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint model.initial_p_constraint

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "LoopSynth"
	
	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
		super#initialize_variables;
		(*** NOTE: duplicate operation ***)
		loop_constraint <- LinearConstraint.false_p_nnconvex_constraint ();

		(* The end *)
		()
	

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Add a new state to the state_space (if indeed needed) *)
	(* Side-effects: modify new_states_indexes *)
	(*** TODO: move new_states_indexes to a variable of the class ***)
	(* Return true if the state is not discarded by the algorithm, i.e., if it is either added OR was already present before *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** WARNING/BADPROG: the following is partially copy/paste from AlgoEF.ml (though much modified) ***)
	method add_a_new_state state_space orig_state_index new_states_indexes action_index location (current_constraint : LinearConstraint.px_linear_constraint) =
		(* Retrieve the model *)
		let model = Input.get_model () in

		(* Build the state *)
		let new_state = location, current_constraint in

		let new_state_index, added = (
			StateSpace.add_state state_space new_state
		) in
		(* If this is really a new state *)
		if added then (

			(* Add the state_index to the list of new states (used to compute their successors at the next iteration) *)
			new_states_indexes := new_state_index :: !new_states_indexes;
			
		) (* end if new state *)
		else (
			(* Not added: means this state was already present before, hence we found a loop! *)
			
			(* Project onto the parameters *)
			let p_constraint = LinearConstraint.px_hide_nonparameters_and_collapse current_constraint in
			
			(* Projecting onto SOME parameters if required *)
			(*** BADPROG: Duplicate code (AlgoEF / AlgoPRP) ***)
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
			self#print_algo_message Verbose_standard "Found a loop.";
			
			(* Update the loop constraint using the current constraint *)
			LinearConstraint.p_nnconvex_p_union loop_constraint p_constraint;
			
			(* Print some information *)
			if verbose_mode_greater Verbose_medium then(
				self#print_algo_message Verbose_medium "Adding the following constraint to loop constraint:";
				print_message Verbose_medium (LinearConstraint.string_of_p_linear_constraint model.variable_names p_constraint);
				
				self#print_algo_message Verbose_medium "The loop constraint is now:";
				print_message Verbose_medium (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names loop_constraint);
			);		
			)
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
		
		(* If found a loop *)
		if not added then(
			(* Compute the SCC (after the transitions were updated, of course) *)
			if verbose_mode_greater Verbose_low then(
				let scc = StateSpace.reconstruct_scc state_space new_state_index in
				(* Print it *)
				let verbose_string_of_state_index state_index =
					let state = StateSpace.get_state state_space state_index in
					"\n\ts_" ^ (string_of_int state_index) ^ "{" ^ (ModelPrinter.string_of_state model state) ^ "}"
				in
				print_message Verbose_low ("\nSCC: [" ^ (string_of_list_of_string_with_sep "\n\t- " (List.map verbose_string_of_state_index scc)) ^ "\n]");
			);
		); (* end if found a loop *)
		
		(* The state is kept in any case *)
		true
	

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
