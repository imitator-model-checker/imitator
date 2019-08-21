(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * 
 * Module description: EFexemplify algorithm [work in progress]. Structurally identical to EFsynth (at the beginning), so the code processes with simple add-ons
 * 
 * File contributors : Étienne André
 * Created           : 2019/07/08
 * Last modified     : 2019/08/21
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
open AlgoEFsynth
open Statistics
open State
open StateSpace




(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoEFexemplify =
	object (self) inherit algoEFsynth as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	(* Number of positive examples spotted (positive examples: concrete runs to the target state) *)
	val mutable nb_positive_examples : int = 0
	
	(* Number of negative examples spotted (negative examples: *impossible* concrete runs to the target state) *)
	val mutable nb_negative_examples : int = 0
	
	val nb_POSITIVE_EXAMPLES_MAX = 3
	val nb_NEGATIVE_EXAMPLES_MAX = 3


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "EFexemplify"
	
	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
		super#initialize_variables;

		nb_positive_examples <- 0;

		(* The end *)
		()

		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Generate counter-example(s) if required by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	method process_counterexample target_state_index =
		(* Update the number of counterexamples processed *)
		nb_positive_examples <- nb_positive_examples + 1;
		
		(* Print some information *)
		self#print_algo_message Verbose_standard ("Target state #" ^ (string_of_int nb_positive_examples) ^ " found!");
		
		(*** NOTE: so far, the reconstruction needs an absolute time clock ***)
		begin
		match model.global_time_clock with
			| None -> raise (InternalError ("No absolute time clock detected in " ^ self#algorithm_name ^ " although this should have been checked before."));
			
			| Some _ ->
				(*------------------------------------------------------------*)
				(* Part 1: positive counterexample *)
				(*------------------------------------------------------------*)
				(* Print some information *)
				print_message Verbose_medium "Counterexample found: reconstructing counterexample…";
				
				(* First build the predecessors table *)
				let predecessors = StateSpace.compute_predecessors_with_combined_transitions state_space in
				
				(* Print some information *)
				print_message Verbose_medium "Predecessor table built";

				(* Also retrieve the initial state *)
				let initial_state_index = StateSpace.get_initial_state_index state_space in
				
				(* Get the symbolic run, i.e., a list of a pair of a symbolic state *followed* by a combined transition *)
				let symbolic_run : StateSpace.symbolic_run = StateSpace.backward_symbolic_run state_space target_state_index initial_state_index (Some predecessors) in
				
				(* Print some information *)
				if verbose_mode_greater Verbose_low then (
					print_message Verbose_low "\nSymbolic run reconstructed:";
					
					(* Debug print *)
					print_message Verbose_low (ModelPrinter.debug_string_of_symbolic_run model state_space symbolic_run);
				);
				
				(* Get the final state *)
				let target_state = StateSpace.get_state state_space target_state_index in

				(* Exhibit a concrete clock+parameter valuation in the final state *)
				let concrete_target_px_valuation : (Automaton.variable_index -> NumConst.t) = LinearConstraint.px_exhibit_point target_state.px_constraint in
				
				(* Print it *)
				if verbose_mode_greater Verbose_low then(
					print_message Verbose_low "Example of px-valuation:";
					print_message Verbose_low (ModelPrinter.string_of_px_valuation model concrete_target_px_valuation);
				);
				
				(* Exhibit a concrete parameter valuation in the final state *)
			(*	let p_constraint = LinearConstraint.px_hide_nonparameters_and_collapse target_state.px_constraint in
				let concrete_p_valuation = LinearConstraint.p_exhibit_point p_constraint in*)
				
				
				(* Print some information *)
				if verbose_mode_greater Verbose_standard then(
					(* Convert to PVal *)
					let pval = PVal.pval_from_valuation_function concrete_target_px_valuation in
					print_message Verbose_standard "Example of parameter valuation:";
					print_message Verbose_standard (ModelPrinter.string_of_pi0 model pval);
				);
				
				(* Exhibit a concrete run from the symbolic run *)
				let concrete_run = AlgoStateBased.concrete_run_of_symbolic_run state_space (predecessors : StateSpace.predecessors_table) (symbolic_run : StateSpace.symbolic_run) concrete_target_px_valuation in

				(* Generate the graphics: run *)
				let prefix = options#files_prefix ^ "_expos_" ^ (string_of_int nb_positive_examples) in
				Graphics.draw_concrete_run concrete_run prefix;
				(* Generate the graphics: parameters *)
				let p_constraint = LinearConstraint.px_hide_nonparameters_and_collapse target_state.px_constraint in
				let zones = [LinearConstraint.Convex_p_constraint p_constraint, Good] in
				Graphics.draw_cartography zones prefix;
				
				
				(*------------------------------------------------------------*)
				(* Part 2: negative counterexample *)
				(*------------------------------------------------------------*)
				
				(*** TODO: handle non-deterministic ***)
				
				if model.strongly_deterministic && not model.has_silent_actions then(
					(* Print some information *)
					print_message Verbose_low "\n\nLooking for a negative counterexample (deterministic system without silent actions)";
					
					(* Part 2.a: try to find a parameter valuation NOT going to the final state using this run *)
					(* Idea: any parameter valuation "deadlocked" along this run is a valuation for which no identical symbolic run leads to the target, or any other target (in case of several discrete target locations) *)

					(* Reason backward *)
					let i = ref ((List.length symbolic_run.symbolic_steps) - 1) in
					(* Define the next valuation along the run, and reason backward *)
					let pconstraint_i_plus_one = ref (LinearConstraint.px_hide_nonparameters_and_collapse (StateSpace.get_state state_space symbolic_run.final_state).px_constraint) in
					
					(* Print some information *)
					print_message Verbose_medium "\nLooking for larger valuations by exploring backwards…";
					
					while !i > 0 do
						(* Print some information *)
						print_message Verbose_high ("\nConsidering position " ^ (string_of_int !i) ^ "");
						
						(* Get the state index at position i *)
						let state_index_i = (List.nth symbolic_run.symbolic_steps !i).source in
						(* Get the p-constraint at position i *)
						let pconstraint_i : LinearConstraint.p_linear_constraint = LinearConstraint.px_hide_nonparameters_and_collapse (StateSpace.get_state state_space state_index_i).px_constraint in
						
						(* Check if difference is non-null *)
						(*** NOTE: we rather use p_is_le, even though we have to then compute the difference, if indeed smaller, for (presumably) efficiency reasons ***)
						if LinearConstraint.p_is_le !pconstraint_i_plus_one pconstraint_i then(
							(* Print some information *)
							print_message Verbose_medium ("\nFound a shrinking of parameter constraint between position " ^ (string_of_int !i) ^ " to " ^ (string_of_int (!i+1)) ^ ":");
							
							(* Update the number of counterexamples processed *)
							nb_negative_examples <- nb_negative_examples + 1;

							(* Convert to a nnconvex_constraint *)
							let difference = LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint pconstraint_i in
							(* Compute the difference K_i \ K_i+1 *)
							LinearConstraint.p_nnconvex_difference_assign difference (LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint !pconstraint_i_plus_one);
							
							(* Print some information *)
							if verbose_mode_greater Verbose_high then(
								print_message Verbose_high ("\nParameter valuations blocked between position " ^ (string_of_int !i) ^ " to " ^ (string_of_int (!i+1)) ^ ":");
								print_message Verbose_high (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names difference);
							);
							
							(* Exhibit a point *)
							let concrete_p_valuation = LinearConstraint.p_nnconvex_exhibit_point difference in
							
							(* Print some information *)
							if verbose_mode_greater Verbose_standard then(
								(* Convert to PVal *)
								let pval = PVal.pval_from_valuation_function concrete_p_valuation in
								print_message Verbose_standard "Example of parameter valuation:";
								print_message Verbose_standard (ModelPrinter.string_of_pi0 model pval);
							);
							
							(* Intersect with the px-constraint to then obtain px-valuation *)
							
							(* Get the px-constraint *)
							let pxconstraint_i = (StateSpace.get_state state_space (List.nth symbolic_run.symbolic_steps !i).source).px_constraint in
							(* Convert the p-valuation to a constraint *)
							let concrete_p_valuation_constraint = LinearConstraint.p_constraint_of_point (List.map (fun parameter_index -> parameter_index , concrete_p_valuation parameter_index) model.parameters ) in
							(* Convert to px-dimensions *)
							let concrete_p_valuation_px_constraint = LinearConstraint.px_of_p_constraint concrete_p_valuation_constraint in
							(* Intersect *)
							LinearConstraint.px_intersection_assign concrete_p_valuation_px_constraint [pxconstraint_i];
							
							(* Exhibit a px-point in this constraint *)
							let concrete_px_valuation_i = LinearConstraint.px_exhibit_point concrete_p_valuation_px_constraint in
							
							(* Print some information *)
							if verbose_mode_greater Verbose_low then(
								print_message Verbose_low ("Example of point at position " ^ (string_of_int !i) ^ ":");
								print_message Verbose_low (ModelPrinter.string_of_px_valuation model concrete_px_valuation_i);
							);
							
							(* Generate the concrete run up to this point *)
							(*------------------------------------------------------------*)

							(* Cut the symbolic run *)
							let symbolic_run_prefix : StateSpace.symbolic_run = {
								(* Take the sublist of steps from position 0 to the current position *)
								symbolic_steps	= OCamlUtilities.sublist 0 !i symbolic_run.symbolic_steps;
								(* Final state becomes the current state *)
								final_state		= state_index_i;
							} in
							
							(* Generate a concrete run for this cut symbolic run *)
							let concrete_run_prefix = AlgoStateBased.concrete_run_of_symbolic_run state_space (predecessors : StateSpace.predecessors_table) (symbolic_run_prefix : StateSpace.symbolic_run) concrete_px_valuation_i in
						
							(* Print it *)
							if verbose_mode_greater Verbose_medium then(
								print_message Verbose_medium "Concrete run prefix:";
								print_message Verbose_medium (ModelPrinter.debug_string_of_concrete_run model concrete_run_prefix);
							);
							
							(* Now create an impossible concrete run from this point to the accepting location *)
							(*------------------------------------------------------------*)
							
							(* Print some information *)
							if verbose_mode_greater Verbose_low then(
								print_message Verbose_low ("Now generating the \"impossible\" concrete run from position " ^ (string_of_int !i) ^ "…");
							);
							
							(* Starting point: the last known existing valuation *)
							let current_valuation = ref concrete_px_valuation_i in
							
							(* For debug purpose *)
							let current_position = ref (!i) in
							
							let impossible_steps_suffix : StateSpace.impossible_concrete_step list = List.map (fun symbolic_step ->
							
								(* Idea: keep everything, including the actions and discrete values, but increment (arbitrarily!) the time by 1 at each step *)
								
								(* Arbitrarily choose 1 *)
								let chosen_time_elapsing = NumConst.one in
								
								(* Get the location *)
								let current_location = (StateSpace.get_state state_space symbolic_step.source).global_location in
								
								(* Apply time elapsing (let us not care about resets, because this transition does not exist; we could care about resets to be closer to the original automaton BUT the guards/invariants could not be satisfied, precisely because this parameter valuation does not allow to take this run!) *)
								(*** NOTE: we still care about urgency and stopwatches though ***)
								let valuation_after_elapsing : LinearConstraint.px_valuation = AlgoStateBased.apply_time_elapsing_to_concrete_valuation current_location chosen_time_elapsing !current_valuation in
								
								(* Print some information *)
								if verbose_mode_greater Verbose_medium then(
									print_message Verbose_medium ("Valuation for position " ^ (string_of_int !current_position) ^ ":");
									print_message Verbose_medium (ModelPrinter.string_of_px_valuation model valuation_after_elapsing);
								);
								
								(* Update the valuation for next step *)
								current_valuation := valuation_after_elapsing;
								
								(* Update the position *)
								incr current_position;
								
								(* Return the impossible_concrete_step *)
								{
									(* First let time elapse: arbitrarily take one *)
									time			= chosen_time_elapsing;
									(* Then take a discrete transition: keep the action *)
									action			= StateSpace.get_action_from_combined_transition symbolic_step.transition;
									(* Then reach the target state *)
									target			= {
										global_location= current_location;
										px_valuation   = valuation_after_elapsing;
									}
								}

							) (OCamlUtilities.sublist (!i+1) ((List.length symbolic_run.symbolic_steps) - 1) symbolic_run.symbolic_steps)
							in

							(* Now create the "impossible" concrete run *)
							let impossible_concrete_run : StateSpace.impossible_concrete_run = {
								(* The parameter valuation for which this run exists *)
								p_valuation		= concrete_run_prefix.p_valuation;
								(* The initial concrete state *)
								initial_state	= concrete_run_prefix.initial_state;
								(* A possibly empty list of steps *)
								steps			= concrete_run_prefix.steps;
								(* A non-empty list of imaginary steps *)
								impossible_steps= impossible_steps_suffix;
							}
							in
							
							(* Print some information *)
							if verbose_mode_greater Verbose_low then (
								print_message Verbose_low "\nNegative counterexample run constructed:";
								
								(* Debug print *)
								print_message Verbose_low (ModelPrinter.debug_string_of_impossible_concrete_run model impossible_concrete_run);
							);
							
							(* Generate the graphics: run *)
							let prefix = options#files_prefix ^ "_exneg_" ^ (string_of_int nb_negative_examples) in
							Graphics.draw_impossible_concrete_run impossible_concrete_run prefix;
							(* Generate the graphics: parameters *)
							let zones = [LinearConstraint.Nonconvex_p_constraint difference, Bad] in
							Graphics.draw_cartography zones prefix;

							
							(*** TODO ***)
							()
						);
						
						(* Move to previous step *)
						pconstraint_i_plus_one := pconstraint_i;
						decr i;
					done;
					
					(*** TODO: not sure to find something ***)
				
				);
				
				
		end;
		
		(* If maximum number of counterexamples processed: stop *)
		if nb_positive_examples >= nb_POSITIVE_EXAMPLES_MAX then(
			(* Update termination status *)
			(*** NOTE/HACK: the number of unexplored states is not known, therefore we do not add it… ***)
			self#print_algo_message Verbose_standard ("Target state #" ^ (string_of_int nb_positive_examples) ^ " is the maximum number sought. Terminating…");
			termination_status <- Some Target_found;
		
			raise TerminateAnalysis;
		)else(
			(* Add the target state to the set of states to explore (a bit a hack); indeed, for exemplification, we may be interested in exploring beyond bad states, as we may find more! *)
			new_states_indexes <- target_state_index :: new_states_indexes;
		);

		(* The end *)
		()
	

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	
	(*** NOTE: temporary version ***)
	
	(*** BADPROG: copied from EFunsafeSynth ***)
	
	method compute_result =
		(* Retrieve the model *)
		let model = Input.get_model () in

		(* Print some information *)
		self#print_algo_message_newline Verbose_standard (
			"Algorithm completed " ^ (after_seconds ()) ^ "."
		);
		
		
		(*** TODO: compute as well *good* zones, depending whether the analysis was exact, or early termination occurred ***)
				
		(* Projecting onto SOME parameters if required *)
		let result =
		match model.projection with
		(* No projection: copy the initial p constraint *)
		| None -> bad_constraint
		(* Project *)
		| Some parameters ->
			(* Print some information *)
			if verbose_mode_greater Verbose_medium then(
				self#print_algo_message Verbose_medium "Projecting the bad constraint onto some of the parameters.";
				self#print_algo_message Verbose_medium "Before projection:";
				print_message Verbose_medium (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names bad_constraint);
			);

			(*** TODO! do only once for all… ***)
			let all_but_projectparameters = list_diff model.parameters parameters in
			
			(* Eliminate other parameters *)
			let projected_init_p_nnconvex_constraint = LinearConstraint.p_nnconvex_hide all_but_projectparameters bad_constraint in

			(* Print some information *)
			if verbose_mode_greater Verbose_medium then(
				self#print_algo_message Verbose_medium "After projection:";
				print_message Verbose_medium (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names projected_init_p_nnconvex_constraint);
			);
			
			(* Return *)
			projected_init_p_nnconvex_constraint
		in
		
		(* Get the termination status *)
		 let termination_status = match termination_status with
			| None -> raise (InternalError ("Termination status not set in " ^ (self#algorithm_name) ^ ".compute_result"))
			| Some status -> status
		in

		(* Constraint is exact if termination is normal, possibly under-approximated otherwise *)
		(*** NOTE/TODO: technically, if the constraint is true/false, its soundness can be further refined easily ***)
		let soundness = if termination_status = Regular_termination then Constraint_exact else Constraint_maybe_under in

		
		
		
		
		(*** TODO ***)
		
		
		(* Return the result *)
		Single_synthesis_result
		{
			(* Non-necessarily convex constraint guaranteeing the reachability of the bad location *)
			result				= Bad_constraint (result, soundness);
			
			(*** TODO ***)
			(*** NOTE: it will ultimately not be a Single_synthesis_result, in any case! ***)
			constraint_description = "TODO";
			
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
