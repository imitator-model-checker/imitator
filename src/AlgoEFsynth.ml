(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: EFsynth algorithm [JLR15]
 * 
 * File contributors : Étienne André
 * Created           : 2015/11/25
 * Last modified     : 2015/12/03
 *
 ************************************************************)


(**************************************************************)
(* Modules *)
(**************************************************************)
open OCamlUtilities
open ImitatorUtilities
open Exceptions
open AbstractModel
open Result
open AlgoBFS



(**************************************************************)
(* Class definition *)
(**************************************************************)
class algoEFsynth =
	object (self) inherit algoBFS as super
	
	(* List of constraints allowing the reachability of the bad location *)
	val mutable bad_constraints = []

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "EFsynth"
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
		bad_constraints <- [];

(*		(* Print some information *)
		if verbose_mode_greater Verbose_low then(
			(* Retrieve the model *)
			let model = Input.get_model () in
			print_message Verbose_low ("Initialized k_result to ");
			print_message Verbose_low (LinearConstraint.string_of_p_linear_constraint model.variable_names k_result);
			print_message Verbose_low ("");
		)*)

		(* The end *)
		()
	

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Add a new state to the reachability_graph (if indeed needed) *)
	(* Also update tile_nature and slast *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** NOTE: just a HACK to allow compiling ***)
	(*** TODO: implement ! ***)
	method add_a_new_state reachability_graph orig_state_index new_states_indexes action_index location (final_constraint : LinearConstraint.px_linear_constraint) =
		(* Retrieve the model *)
		let model = Input.get_model () in

		(* Retrieve the input options *)
(* 		let options = Input.get_options () in *)

		(* Build the state *)
		let new_state = location, final_constraint in

		(* Print some information *)
		if verbose_mode_greater Verbose_total then(
			(*** TODO: move that comment to a higher level function? (post_from_one_state?) ***)
			print_message Verbose_total ("Consider the state \n" ^ (ModelPrinter.string_of_state model new_state));
		);

		let new_state_index, added = (
			StateSpace.add_state reachability_graph new_state
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
						print_message Verbose_medium "  [EF-synthesis] Projecting onto some of the parameters.";
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
						print_message Verbose_low "  [EF-synthesis] Found a state violating the property but the constraint is not new.";
					)else(
						bad_constraints <- p_constraint :: bad_constraints;
						(* Print some information *)
						print_message Verbose_standard "  [EF-synthesis] Found a state violating the property.";
						
						(* Print some information *)
						if verbose_mode_greater Verbose_medium then(
							print_message Verbose_medium "Adding the following constraint to the list of bad constraints:";
							print_message Verbose_medium (LinearConstraint.string_of_p_linear_constraint model.variable_names p_constraint);
						);
						
					);
					
					(* Do NOT compute its successors *)
					to_be_added := false;
					
				)else(
					print_message Verbose_medium "EF-synthesis: State not corresponding to the one wanted.";
				);
			| _ -> raise (InternalError("[EF-synthesis] IMITATOR currently ony implements the non-reachability-like properties. This should have been checked before."))
			end
			;

			(* Add the state_index to the list of new states (used to compute their successors at the next iteration) *)
			if !to_be_added then
				new_states_indexes := new_state_index :: !new_states_indexes;
			
		) (* end if new state *)
		;
		
		
		(*** TODO: move the rest to a higher level function? (post_from_one_state?) ***)
		
		(* Update the transitions *)
		StateSpace.add_transition reachability_graph (orig_state_index, action_index, new_state_index);
		(* Print some information *)
		if verbose_mode_greater Verbose_high then (
			let beginning_message = (if added then "NEW STATE" else "Old state") in
			print_message Verbose_high ("\n" ^ beginning_message ^ " reachable through action '" ^ (model.action_names action_index) ^ "': ");
			print_message Verbose_high (ModelPrinter.string_of_state model new_state);
		);
	
		(* The end: do nothing *)
		()
	

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_result =
		(*** HACK: incomplete result to ensure compiling ***)
		EFsynth_result
		{
			(* List of constraints ensuring EF location *)
			constraints			= bad_constraints;
			
			(* Explored state space *)
			state_space			= state_space;
			
			(* Nature of the state space (needed??) *)
		(* 	tile_nature			: AbstractModel.tile_nature; *)
			
			(* Total computation time of the algorithm *)
			computation_time	= time_from start_time;
			
			(* Termination *)
			(*** TODO ***)
			termination			= Regular_termination;
		}
	
end;;