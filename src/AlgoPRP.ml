(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: PRP algorithm [ALNS15]
 * 
 * File contributors : Étienne André
 * Created           : 2016/01/11
 * Last modified     : 2016/01/15
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
open AlgoIMK



(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoPRP =
	object (self) inherit algoIMK as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	(* Determines the mode of the algorithm: was a bad state already found? *)
	val mutable bad_state_found: bool = false
	
	(* Convex constraint ensuring unreachability of the bad states *)
	val mutable good_constraint : LinearConstraint.p_linear_constraint = LinearConstraint.p_true_constraint ()
	
	(* Non-necessarily convex constraint ensuring reachability of at least one bad state *)
	val mutable bad_constraint : LinearConstraint.p_nnconvex_constraint = LinearConstraint.false_p_nnconvex_constraint ()
	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "PRP"

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
		(* Retrieve the model *)
(* 		let model = Input.get_model () in *)

		super#initialize_variables;
		
		bad_state_found <- false;
		
		(* Parameter valuations cannot go beyond what is defined in the initial state of the model *)
		good_constraint <- (
			match initial_constraint with
			| None -> raise (InternalError("The initial constraint was not yet set in PRP, although it should have been."))
			| Some c -> LinearConstraint.px_hide_nonparameters_and_collapse c
		);
		
		bad_constraint <- LinearConstraint.false_p_nnconvex_constraint ();
		
		(* The end *)
		()
		
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Add a new state to the state_space (if indeed needed) *)
	(* Also update tile_nature and slast *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** WARNING/BADPROG: the following is partially copy/paste from AlgoEFsynth.ml and AlgoPRP.ml***)
	(*** TODO: factorize ***)
	method add_a_new_state state_space orig_state_index new_states_indexes action_index location (final_constraint : LinearConstraint.px_linear_constraint) =
		(* Retrieve the model *)
		let model = Input.get_model () in

		(* Retrieve the input options *)
(* 		let options = Input.get_options () in *)

		let pi0compatible = self#check_pi0compatibility final_constraint in

		(* Print some information *)
		if verbose_mode_greater Verbose_high then(
			(* Means state was not compatible *)
			if not pi0compatible then(
				let new_state = location, final_constraint in
				if verbose_mode_greater Verbose_high then
					self#print_algo_message Verbose_high ("The pi-incompatible state had been computed through action '" ^ (model.action_names action_index) ^ "', and was:\n" ^ (ModelPrinter.string_of_state model new_state));
			);
		);

		(* Only add the new state if it is pi0-compatible *)
		(*** NOTE: this is a key principle of PRP to NOT explore pi0-incompatible states ***)
		if pi0compatible then (
			(* Build the state *)
			let new_state = location, final_constraint in

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
				self#update_trace_set_nature new_state;
				
				(* Will the state be added to the list of new states (the successors of which will be computed)? *)
				let to_be_added = ref true in
				
				(* If synthesis / EFIM: add the constraint to the list of successful constraints if this corresponds to a bad location *)
				begin
				match model.correctness_condition with
				| None -> raise (InternalError("[EF-synthesis/EFIM] A correctness property must be defined to perform EF-synthesis or EFIM. This should have been checked before."))
				| Some (Unreachable unreachable_global_locations) ->
					
					(* Check whether the current location matches one of the unreachable global locations *)
					if StateSpace.match_unreachable_global_locations unreachable_global_locations location then(
					
						(* Project onto the parameters *)
						let p_constraint = LinearConstraint.px_hide_nonparameters_and_collapse final_constraint in
						
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
						
						(* Print some information *)
						self#print_algo_message Verbose_standard "Found a state violating the property.";
						if verbose_mode_greater Verbose_medium then(
							self#print_algo_message Verbose_medium "Adding the following constraint to the list of bad constraints:";
							print_message Verbose_medium (LinearConstraint.string_of_p_linear_constraint model.variable_names p_constraint);
						);
						
						(*** NOTE: not copy paste (actually, to copy when EFsynth will be improved with non-convex constraints) ***)
						LinearConstraint.p_nnconvex_union bad_constraint p_constraint;
						
						if verbose_mode_greater Verbose_low then(
							self#print_algo_message_newline Verbose_low ("Kbad now equal to:");
							print_message Verbose_low (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names bad_constraint);
						);

						(* PRP switches to bad-state algorithm *)
						if not bad_state_found then(
							(* Print some information *)
							self#print_algo_message Verbose_standard "Switching to EFsynth-like algorithm";
						);
						bad_state_found <- true;
					
						(* Do NOT compute its successors *)
						to_be_added := false;
						
					)else(
						self#print_algo_message Verbose_medium "State not corresponding to the one wanted.";
					);
				| _ -> raise (InternalError("[EFsynth/PRP] IMITATOR currently ony implements the non-reachability-like properties. This should have been checked before."))
				end
				;

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
			
		); (* end if valid new state *)
	
		(* The end: do nothing *)
		()
	(*** END WARNING/BADPROG: the following is almost entirely copy/paste from AlgoEFsynth.ml ***)
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Actions to perform when meeting a state with no successors: nothing to do for this algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_deadlock_state state_index = ()
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Should we process a pi-incompatible inequality? *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_pi_incompatible_states () =
		(* Only explore if no bad states found *)
		let answer = not bad_state_found in
		(* Print some information *)
		self#print_algo_message Verbose_medium ("Exploring pi-incompatible state? " ^ (string_of_bool answer));
		(* Return *)
		answer

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Actions to perform when a pi-incompatible inequality is found. Add its negation to the accumulated good constraint. *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_negated_incompatible_inequality negated_inequality =
		self#print_algo_message_newline Verbose_medium ("Adding the negation of a pi-incompatible inequality to Kgood.\n");
		
		let negated_constraint = LinearConstraint.make_p_constraint [negated_inequality] in

		LinearConstraint.p_intersection_assign good_constraint [negated_constraint];

		if verbose_mode_greater Verbose_low then(
			(* Retrieve the model *)
			let model = Input.get_model () in
			self#print_algo_message_newline Verbose_low ("Kgood now equal to:");
			print_message Verbose_low (LinearConstraint.string_of_p_linear_constraint model.variable_names good_constraint);
		);
		()
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_result =

		let result = if bad_state_found then(
			(* Return Kbad *)
			bad_constraint
		)else(
			(* Return Kgood *)
			LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint good_constraint
		)
		in
		
		self#print_algo_message_newline Verbose_standard (
			"Successfully terminated " ^ (after_seconds ()) ^ "."
		);

		(* Return result *)
		IMNonconvex_result
		{
			(* Result of the algorithm *)
			nonconvex_constraint= result;
			
			(* Explored state space *)
			state_space			= state_space;
			
			(* Nature of the state space (needed??) *)
		(* 	tile_nature			: AbstractModel.tile_nature; *)
			
			(* Number of random selections of pi-incompatible inequalities performed *)
			nb_random_selections= nb_random_selections;
	
			(* Total computation time of the algorithm *)
			computation_time	= time_from start_time;
			
			(* Termination *)
			termination			= 
				match termination_status with
				| None -> raise (InternalError "Termination status not set in PRP.compute_result")
				| Some status -> status
			;
		}
	
(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
