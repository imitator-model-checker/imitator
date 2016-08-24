(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: IMK algorithm [AS11]
 * 
 * File contributors : Étienne André
 * Created           : 2015/12/04
 * Last modified     : 2016/08/24
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
class algoIMK =
	object (self) inherit algoBFS as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	val mutable nb_random_selections = 0

	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "IMK"

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
		super#initialize_variables;
		
		nb_random_selections <- 0;

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
		(* Retrieve the model *)
		let model = Input.get_model() in
		(* Retrieve the input options *)
		let options = Input.get_options () in
		(* Retrieve the pi0 (dynamic!) *)
		let pi0 = Input.get_pi0 () in
		
		self#print_algo_message_newline Verbose_medium ("Sarting pi0-compatibility check...");
		
		self#print_algo_message_newline Verbose_high ("Hiding non parameters...");
		
		(* Hide non-parameters *)
		let p_constraint = LinearConstraint.px_hide_nonparameters_and_collapse constr in
		
		self#print_algo_message_newline Verbose_high ("Parameters now hidden:");
		(* Print some information *)
		if verbose_mode_greater Verbose_high then(
			print_message Verbose_high (LinearConstraint.string_of_p_linear_constraint model.variable_names p_constraint);
		);
		(* Check the pi0-compatibility *)
		self#print_algo_message_newline Verbose_high ("Checking pi-compatibility:");
		let compatible, incompatible = LinearConstraint.partition_pi0_compatible pi0#get_value p_constraint in
		let is_pi0_incompatible = incompatible != [] in
		
		(* If pi0-incompatible: select an inequality *)
		if is_pi0_incompatible then (
			self#print_algo_message_newline Verbose_low ("Found a pi0-incompatible state.");
			(* Print some information *)
			if verbose_mode_greater Verbose_medium then(
				self#print_algo_message Verbose_high ("Associated constraint:");
				print_message Verbose_high (LinearConstraint.string_of_px_linear_constraint model.variable_names constr);
				self#print_algo_message_newline Verbose_medium ("The following inequalities are pi0-incompatible:");
				List.iter (fun inequality -> print_message Verbose_medium (LinearConstraint.string_of_p_linear_inequality model.variable_names inequality)) incompatible;
				if verbose_mode_greater Verbose_high then(
					self#print_algo_message_newline Verbose_high ("Recall that pi0 is:");
					print_message Verbose_high   (ModelPrinter.string_of_pi0 model pi0);
				);
			);
			
			
			(*** TODO: add back later ***)
			
			(* Should we explore pi-incompatible states? *)
			if not (self#process_pi_incompatible_states()) then(
				self#print_algo_message Verbose_low ("Cut pi-incompatible branch.");
				false
			)else(
			
(*			(* Case EFIM: no need to select a pi-incompatible inequality if already bad *)
			if options#efim && !tile_nature = Bad then(
				print_message Verbose_low ("\n[EFIM] Cut branch.");
				(false , p_constraint)
*)
			(* Case normal IM: select a pi-incompatible inequality *)
				let p_inequality =
					(* If random selection: pick up a random inequality *)
					if not options#no_random then random_element incompatible
					(* Else select the first one *)
					else List.nth incompatible 0
				in
				(* Print some information *)
				if verbose_mode_greater Verbose_medium then(
					self#print_algo_message_newline Verbose_medium ("Selecting the following pi0-incompatible inequality:");
					print_message Verbose_medium (LinearConstraint.string_of_p_linear_inequality model.variable_names p_inequality);
				);

				(* Update counter *)
				if List.length incompatible > 1 then nb_random_selections <- nb_random_selections + 1;
				
				(* Negate the inequality *)
				let negated_inequality = LinearConstraint.negate_wrt_pi0 pi0#get_value p_inequality in
				(* Print some information *)
				if verbose_mode_greater Verbose_standard then(
					let randomly = if not options#no_random then "randomly " else "" in
					let among = if List.length incompatible > 1 then (" (" ^ randomly ^ "selected among " ^ (string_of_int (List.length incompatible)) ^ " inequalities)") else "" in
					self#print_algo_message Verbose_standard ("Adding the following inequality" ^ among ^ ":");
					print_message Verbose_standard ("  " ^ (LinearConstraint.string_of_p_linear_inequality model.variable_names negated_inequality));
				);
				
				
				(* Generic function handling the inequality: by default, add its negation to all previous states *)
				self#process_negated_incompatible_inequality negated_inequality;
				
				(* Add the p_constraint to the result (except in case of variants) *)
				(*** WARNING: why not in case of variants ?! ***)
				
				(*** TODO: add back later ***)
(* 				if not (options#pi_compatible || options#union) then( 

					print_message Verbose_high ("Updating k_result with the negated inequality");
					LinearConstraint.p_intersection_assign !k_result [negated_constraint];
					(* Print some information *)
					if verbose_mode_greater Verbose_low then(
						print_message Verbose_low ("\nk_result now equal (after addition of neg J) to ");
						print_message Verbose_low (LinearConstraint.string_of_p_linear_constraint model.variable_names !k_result);
						print_message Verbose_low ("");
					);
 				); *)
				
				(* Update the previous states (including the 'new_states' and the 'orig_state') *)
				(* Not for EFIM! *)

				(*** TODO: add back later ***)
(* 				if not options#efim then( *)
				
(*								(* Transform to constraint *)
				let negated_constraint = LinearConstraint.make_p_constraint [negated_inequality] in

				if self#update_all_previous_states () then(
					print_message Verbose_medium "";
					self#print_algo_message Verbose_medium ("Updating all the previous states.\n");
					StateSpace.add_p_constraint_to_states state_space negated_constraint;
				)else(
					print_message Verbose_standard("  [EFIM] Storing inequality only");
				);*)
				
				(* If pi-incompatible *)
				false
 			)(* Endif explore pi-incompatible state *)
		) (* end if pi-incompatible *)
		else true


	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Add a new state to the state_space (if indeed needed) *)
	(* Side-effects: modify new_states_indexes *)
	(*** TODO: move new_states_indexes to a variable of the class ***)
	(* Return true if the state is not discarded by the algorithm, i.e., if it is either added OR was already present before *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method add_a_new_state state_space orig_state_index new_states_indexes action_index location (final_constraint : LinearConstraint.px_linear_constraint) =
		(* Retrieve the model *)
		let model = Input.get_model () in

		(* Is the new state valid? *)
		
		(*** TODO: add back ***)
(*		(* 1) Complete version of IM *)
		if options#completeIM then(
			let valid_new_state, bad_p_constraint = completeIM_check_constraint model state_space final_constraint in
			(* Update the set of bad polyhedra *)
			k_bad := bad_p_constraint :: !k_bad;
			(* Return locally the result *)
			valid_new_state
		
		(* 2) Regular IM *)
		)else( *)
		(*** NOTE: the addition of neg J to all reached states is performed as a side effect inside the following function ***)
		(*** BADPROG: same reason ***)
		let pi0_compatible = self#check_pi0compatibility final_constraint
		in
		
		(* If pi-compatible state: add the new state's p_constraint to the on-the-fly computation of the result of IMss *)
		(*** TODO: add back later (?) ***)
		(*if valid_new_state && not (options#pi_compatible || options#union || options#efim) then(
			print_message Verbose_high ("Updating k_result");
			LinearConstraint.p_intersection_assign !k_result [new_p_constraint];
			(* Print some information *)
			if verbose_mode_greater Verbose_low then(
				print_message Verbose_low ("\nk_result now equal (after addition of current state's K) to ");
				print_message Verbose_low (LinearConstraint.string_of_p_linear_constraint model.variable_names !k_result);
				print_message Verbose_low ("");
			);
		);*)
		

		(* Print some information *)
		if verbose_mode_greater Verbose_high then(
			(* Means state was not compatible *)
			if not pi0_compatible then(
				let new_state = location, final_constraint in
				if verbose_mode_greater Verbose_high then
					self#print_algo_message Verbose_high ("The pi-incompatible state had been computed through action '" ^ (model.action_names action_index) ^ "', and was:\n" ^ (ModelPrinter.string_of_state model new_state));
			);
		);
		
		(* Only add the new state if it is actually valid *)
		if pi0_compatible then (
			(* Build the state *)
			let new_state = location, final_constraint in

			(* If IM or BC: Add the inequality to the result (except if case variants) *)
	(*		begin
			match options#imitator_mode with 
				(* Case state space / synthesis: do nothing *)
				| State_space_exploration
				| EF_synthesis
					-> ()
				(* Case IM / BC: *)
				| _ -> if not (options#pi_compatible || options#union) then(
						print_message Verbose_high ("Updating k_result");
						LinearConstraint.p_intersection_assign !k_result [inequality];
					);
			end;*)
			
			(* Try to add this new state to the graph *)
			let new_state_index, added = (
				StateSpace.add_state state_space new_state
			) in
			(* If this is really a new state *)
			if added then (

			(*** TODO: add back later ***)
			
(*				(* Check if the new state contains an integer point *)
				if options#check_ippta then(
					if not (LinearConstraint.px_contains_integer_point final_constraint) then(
						print_error ("State found with no integer point:\n" ^ 
							(ModelPrinter.ModelPrinter.string_of_state model new_state));
						raise NoIPPTA
					);
				);*)
	
				
				(* First check whether this is a bad tile according to the property and the nature of the state *)
				self#update_statespace_nature new_state;
				
				(* Add the state_index to the list of new states (used to compute their successors at the next iteration) *)
				new_states_indexes := new_state_index :: !new_states_indexes;
				
			) (* end if new state *)
			else (
				(* This is a loop *)
				self#process_looping_state new_state_index;
	
	(*** TODO: add back ***)
(*			(* ELSE : add to SLAST if mode union *)
				if options#union then (
					print_message Verbose_low ("\nMode union: adding a looping state to SLast.");
					(* Adding the state *)
					(*** TODO / TO CHECK: what if new_state_index is already in slast?!! ***)
					slast := new_state_index :: !slast;
				);*)
	
			); (* end else if added *)
			
			
		(*** TODO: move the rest to a higher level function? (post_from_one_state?) ***)

			(* Update the transitions *)
			StateSpace.add_transition state_space (orig_state_index, action_index, new_state_index);
			(* Print some information *)
			if verbose_mode_greater Verbose_high then (
				let beginning_message = (if added then "NEW STATE" else "Old state") in
				self#print_algo_message Verbose_high ("\n" ^ beginning_message ^ " reachable through action '" ^ (model.action_names action_index) ^ "': ");
				print_message Verbose_high (ModelPrinter.string_of_state model new_state);
			);
		); (* end if valid new state *)
		
		(* Return true if the state is pi-compatible *)
		pi0_compatible



	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Actions to perform when meeting a state with no successors: nothing to do for this algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_deadlock_state state_index = ()
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Actions to perform when meeting a state that is on a loop: nothing to do for this algorithm, but can be defined in subclasses *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_looping_state state_index = ()
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Should we process a pi-incompatible inequality? By default yes *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_pi_incompatible_states () =
		(* Print some information *)
		self#print_algo_message Verbose_medium ("Exploring pi-incompatible state? yes (default)");
		true
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Actions to perform when a pi-incompatible inequality is found. By default: add its negation to all previous states *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_negated_incompatible_inequality negated_inequality =
		let negated_constraint = LinearConstraint.make_p_constraint [negated_inequality] in
		
		self#print_algo_message_newline Verbose_medium ("Updating all the previous states.\n");
		
		StateSpace.add_p_constraint_to_states state_space negated_constraint;
		()
				
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Actions to perform at the end of the computation of the *successors* of post^n (i.e., when this method is called, the successors were just computed). Nothing to do for this algorithm. *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_post_n (post_n : State.state_index list) = ()

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Check whether the algorithm should terminate at the end of some post, independently of the number of states to be processed (e.g., if the constraint is already true or false) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** TODO: could be stopped when the constraint (i.e., the dynamic projection of the initial state onto P) is reduced to pi0 ***)
	method check_termination_at_post_n = false

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_result =
	
		(* IMK: return only the current constraint, viz., the constraint of the first state *)
		(*** NOTE: better not use just "0" as the initial state may have been merged with another state ***)
		let initial_state_index = StateSpace.get_initial_state_index state_space in
		let initial_state = StateSpace.get_state state_space initial_state_index in
		(* Retrieve the constraint of the initial state *)
		let (_ , px_constraint ) = initial_state in
		
		self#print_algo_message Verbose_total ("projecting the initial state constraint onto the parameters...");
		
		let p_constraint = LinearConstraint.px_hide_nonparameters_and_collapse px_constraint in
(* 		Convex_constraint (LinearConstraint.px_hide_nonparameters_and_collapse px_constraint , !tile_nature)  *)
	
		self#print_algo_message_newline Verbose_standard (
			"Successfully terminated " ^ (after_seconds ()) ^ "."
		);

		(* Get the termination status *)
		 let termination_status = match termination_status with
			| None -> raise (InternalError "Termination status not set in IMK.compute_result")
			| Some status -> status
		in

		(* The state space nature is good if 1) it is not bad, and 2) the analysis terminated normally *)
		(*** NOTE: unsure of this computation (if it has any meaning for this algorithm anyway) ***)
		let statespace_nature =
			if statespace_nature = StateSpace.Unknown && termination_status = Regular_termination then StateSpace.Good
			(* Otherwise: unchanged *)
			else statespace_nature
		in

		(* Constraint is exact if termination is normal, possibly over-approximated otherwise (as there may be pi-incompatible inequalities missing) *)
		let soundness = if termination_status = Regular_termination then Constraint_exact else Constraint_maybe_over in

		let result = match statespace_nature with
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
