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
 * Last modified     : 2019/08/22
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
	
	(* Positive examples spotted (positive examples: concrete runs to the target state) *)
	val mutable positive_examples : Result.valuation_and_concrete_run list = []
	
	(* Negative examples spotted (negative examples: *impossible* concrete runs to the target state) *)
	val mutable negative_examples : Result.valuation_and_concrete_run list = []
	
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

		positive_examples <- [];
		negative_examples <- [];

		(* The end *)
		()

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Create an arbitrary impossible concrete run from a symbolic run *)
	(* The debug_offset variable is used for pretty-printing; it represents the offset between the actual position in the original list of symbolic steps, and the sublist provided here in symbolic_steps *)
	(*** NOTE: the starting valuation is already known to be impossible, therefore any concrete run corresponding to the symbolic run will do ***)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private impossible_concrete_steps_of_symbolic_steps (start_valuation : LinearConstraint.px_valuation) (debug_offset : int) (symbolic_steps : symbolic_step list) : impossible_concrete_step list =
		(* Starting point: the last known existing valuation *)
		let current_valuation = ref start_valuation in
		
		(* For debug purpose *)
		let current_position = ref 0 in
		
		List.map (fun symbolic_step ->
		
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
				print_message Verbose_medium ("Valuation for position " ^ (string_of_int (!current_position + debug_offset)) ^ ":");
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

		) symbolic_steps


	

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Generate counter-example(s) if required by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	method process_counterexample target_state_index =
		(* Print some information *)
		let nb_positive_examples = List.length positive_examples + 1 in
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

				(* Get the associated parameter valuations *)
				let target_p_constraint = LinearConstraint.px_hide_nonparameters_and_collapse target_state.px_constraint in
				
				(* Exhibit a concrete clock+parameter valuation in the final state *)
				let concrete_target_px_valuation : (Automaton.variable_index -> NumConst.t) = LinearConstraint.px_exhibit_point target_state.px_constraint in
				
				(* Print it *)
				if verbose_mode_greater Verbose_low then(
					print_message Verbose_low "Example of px-valuation:";
					print_message Verbose_low (ModelPrinter.string_of_px_valuation model concrete_target_px_valuation);
				);
				
				(* Convert to PVal *)
				let pval_positive = PVal.pval_from_valuation_function concrete_target_px_valuation in
				
				(* Convert the positive valuation to a functional representation *)
				let functional_pval_positive = fun parameter_index -> pval_positive#get_value parameter_index in
					
				(* Print some information *)
				if verbose_mode_greater Verbose_standard then(
					print_message Verbose_standard "Example of positive parameter valuation:";
					print_message Verbose_standard (ModelPrinter.string_of_pval model pval_positive);
				);
				
				(* Exhibit a concrete run from the symbolic run *)
				let concrete_run = AlgoStateBased.concrete_run_of_symbolic_run state_space (predecessors : StateSpace.predecessors_table) (symbolic_run : StateSpace.symbolic_run) concrete_target_px_valuation in
				
				(* Project onto the parameters *)
				let p_constraint = LinearConstraint.px_hide_nonparameters_and_collapse target_state.px_constraint in

				(* Add the run to the list of results *)
				let valuation_and_concrete_run = {
					(* The parameter valuation for which this run exists *)
					valuation		= pval_positive;
					(* Sometimes, we can even infer more valuations for which an equivalent DISCRETE run exist (note that the exact timings of the run might differ!!!) *)
					valuations		= LinearConstraint.Convex_p_constraint p_constraint;
					(* The concrete run *)
					concrete_run	= Result.Concrete_run concrete_run;
				} in
				
				(* Print some information *)
				print_message Verbose_standard "Positive concrete run constructed!";
				
				(* Update the counterexamples processed *)
				positive_examples <- valuation_and_concrete_run :: positive_examples;

				
				(*------------------------------------------------------------*)
				(* Part 2: negative counterexample *)
				(*------------------------------------------------------------*)
				
				(*** TODO: handle non-deterministic ***)
				
				if model.strongly_deterministic && not model.has_silent_actions then(

					(*------------------------------------------------------------*)
					(* Part 2a: negative counterexample for a different parameter valuation*)
					(*------------------------------------------------------------*)
					(* Part 2.a: try to find a parameter valuation NOT going to the final state using this run *)
					
					(* Print some information *)
					print_message Verbose_low "\n\nLooking for a negative counterexample using a different parameter valuation (case of deterministic system without silent actions)";

					(* Idea: any parameter valuation "deadlocked" along this run is a valuation for which no identical symbolic run leads to the target, or any other target (in case of several discrete target locations) *)

					(* Reason backward from the last but one state (as we compare i and i+1) *)
					let i = ref ((List.length symbolic_run.symbolic_steps) - 1) in
					(* Define the next valuation along the run, and reason backward *)
					let pconstraint_i_plus_one : LinearConstraint.p_linear_constraint ref = ref target_p_constraint in
					
					(* Print some information *)
					print_message Verbose_medium ("\nLooking for larger parameter valuations by exploring backwards from position " ^ (string_of_int !i) ^ "…");
					
					(* Flag to interrupt the loop when found *)
					let found = ref false in
					
					while !i >= 0 && not !found do
						(* Print some information *)
						print_message Verbose_high ("\nConsidering position " ^ (string_of_int !i) ^ "");
						
						(* Get the state index at position i *)
						let state_index_i = (List.nth symbolic_run.symbolic_steps !i).source in
						(* Get the p-constraint at position i *)
						let pconstraint_i : LinearConstraint.p_linear_constraint = LinearConstraint.px_hide_nonparameters_and_collapse (StateSpace.get_state state_space state_index_i).px_constraint in
						
						(* Print some information *)
						if verbose_mode_greater Verbose_high then(
							print_message Verbose_high ("\nAbout to compare parameter constraint at position " ^ (string_of_int !i) ^ ":");
							print_message Verbose_high (LinearConstraint.string_of_p_linear_constraint model.variable_names pconstraint_i);
							print_message Verbose_high ("\n…with parameter constraint at position " ^ (string_of_int (!i+1)) ^ ":");
							print_message Verbose_high (LinearConstraint.string_of_p_linear_constraint model.variable_names (!pconstraint_i_plus_one));
						);
						
						(* Check if difference is non-empty *)
						(*** NOTE: we rather use p_is_le, even though we have to then compute the difference, if indeed smaller, for (presumably) efficiency reasons ***)
						if LinearConstraint.p_is_le !pconstraint_i_plus_one pconstraint_i then(
							(* Print some information *)
							print_message Verbose_medium ("\nFound a shrinking of parameter constraint between positions " ^ (string_of_int !i) ^ " and " ^ (string_of_int (!i+1)) ^ ":");
							
							(* Update flag *)
							found := true;
							
							(* Convert to a nnconvex_constraint *)
							let difference = LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint pconstraint_i in
							(* Compute the difference K_i \ K_i+1 *)
							LinearConstraint.p_nnconvex_difference_assign difference (LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint !pconstraint_i_plus_one);
							
							(* Print some information *)
							if verbose_mode_greater Verbose_high then(
								print_message Verbose_high ("\nParameter valuations blocked between positions " ^ (string_of_int !i) ^ " and " ^ (string_of_int (!i+1)) ^ ":");
								print_message Verbose_high (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names difference);
							);
							
							(* Exhibit a point *)
							let concrete_p_valuation = LinearConstraint.p_nnconvex_exhibit_point difference in
							
							(* Convert to PVal *)
							let pval_negative = PVal.pval_from_valuation_function concrete_p_valuation in
							
							(* Print some information *)
							if verbose_mode_greater Verbose_standard then(
								print_message Verbose_standard "Example of \"negative\" parameter valuation, i.e., not allowing to reach target:";
								print_message Verbose_standard (ModelPrinter.string_of_pval model pval_negative);
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
								print_message Verbose_low ("Example of blocking point at position " ^ (string_of_int !i) ^ ":");
								print_message Verbose_low (ModelPrinter.string_of_px_valuation model concrete_px_valuation_i);
							);
							
							(* Generate the concrete run up to this point *)
							(*------------------------------------------------------------*)

							(*** WARNING/BADPROG: the following few lines are duplicate code with below ***)
							
							(* Cut the symbolic run *)
							let symbolic_run_prefix : StateSpace.symbolic_run = {
								(* Take the sublist of steps from position 0 to the current position *)
								symbolic_steps	= if !i > 0 then OCamlUtilities.sublist 0 (!i-1) symbolic_run.symbolic_steps else [];
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
								print_message Verbose_low ("Now generating the \"impossible\" concrete run for negative parameter valuation from position " ^ (string_of_int !i) ^ "…");
							);
							
							(* Print some information *)
							if verbose_mode_greater Verbose_high then(
								print_message Verbose_high ("Considering subset of symbolic run of length " ^ (string_of_int (List.length symbolic_run.symbolic_steps)) ^ " from position " ^ (string_of_int (!i)) ^ " to position " ^ (string_of_int ((List.length symbolic_run.symbolic_steps) - 1)) ^ "…");
							);
							
							(* Convert the symbolic existing steps to concrete steps from the impossible valuation *)
							let impossible_steps_suffix : StateSpace.impossible_concrete_step list = self#impossible_concrete_steps_of_symbolic_steps concrete_px_valuation_i (!i) (OCamlUtilities.sublist (!i) ((List.length symbolic_run.symbolic_steps) - 1) symbolic_run.symbolic_steps) in
							
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
							print_message Verbose_standard "Negative counterexample run constructed for a negative parameter valuation!";
							if verbose_mode_greater Verbose_low then (
								(* Debug print *)
								print_message Verbose_low (ModelPrinter.debug_string_of_impossible_concrete_run model impossible_concrete_run);
							);
							
							(* Add the run to the list of results *)
							let valuation_and_concrete_run = {
								(* The parameter valuation for which this run exists *)
								valuation		= pval_negative;
								(* Sometimes, we can even infer more valuations for which an equivalent DISCRETE run exist (note that the exact timings of the run might differ!!!) *)
								valuations		= LinearConstraint.Nonconvex_p_constraint difference;
								(* The concrete run *)
								concrete_run	= Result.Impossible_concrete_run impossible_concrete_run;
							} in

							(* Update the counterexamples processed *)
							negative_examples <- valuation_and_concrete_run :: negative_examples;

							()
						); (* end if found restrained constraint *)
						
						(* Move to previous step *)
						pconstraint_i_plus_one := pconstraint_i;
						decr i;
					done; (* end while backward *)
					
					if not !found then(
						print_message Verbose_standard "\n\nFound no parameter valuation allowing a negative counterexample for this run";
						
						(*** TODO: try some other heuristics? ***)

					);
					
					(*------------------------------------------------------------*)
					(* Part 2b: negative counterexample for the same parameter valuation *)
					(*------------------------------------------------------------*)
					
					(* Print some information *)
					print_message Verbose_low "\n\nLooking for a negative counterexample using the same parameter valuation (case of deterministic system without silent actions)";

					(* Idea: any clock valuation "deadlocked" along this run is a valuation for which no identical symbolic run leads to the target, or any other target (in case of several discrete target locations) *)

					(* Define the next valuation along the run, and reason backward *)
					
					(* Start from the last but one state (as we compare i and i+1) *)
					let i = ref ((List.length symbolic_run.symbolic_steps) - 1) in
					
					
					let xconstraint_i_plus_one : LinearConstraint.x_linear_constraint ref = ref (LinearConstraint.px_valuate_parameters functional_pval_positive target_state.px_constraint) in
					
					(* Print some information *)
					print_message Verbose_medium ("\nLooking for blocking clock valuations by exploring backwards from position " ^ (string_of_int !i) ^ "…");
					
					(* Flag to interrupt the loop when found *)
					let found = ref false in
					
					while !i >= 0 && not !found do
						(* Print some information *)
						print_message Verbose_high ("\nConsidering position " ^ (string_of_int !i) ^ "");
						
						(* Get the state index at position i *)
						let state_index_i = (List.nth symbolic_run.symbolic_steps !i).source in
						(* Get the x-constraint at position i *)
						let xconstraint_i : LinearConstraint.x_linear_constraint = LinearConstraint.px_valuate_parameters functional_pval_positive (StateSpace.get_state state_space state_index_i).px_constraint in
						
						(* Print some information *)
						if verbose_mode_greater Verbose_high then(
							print_message Verbose_high ("\nAbout to compare clock constraint at position " ^ (string_of_int !i) ^ ":");
							print_message Verbose_high (LinearConstraint.string_of_x_linear_constraint model.variable_names xconstraint_i);
							print_message Verbose_high ("\n…with clock constraint at position " ^ (string_of_int (!i+1)) ^ ":");
							print_message Verbose_high (LinearConstraint.string_of_x_linear_constraint model.variable_names (!xconstraint_i_plus_one));
						);
						
						(* Convert to a nnconvex_constraint *)
						let difference = LinearConstraint.x_nnconvex_constraint_of_x_linear_constraint xconstraint_i in
						(* Compute the difference K_i \ K_i+1 *)
						LinearConstraint.x_nnconvex_difference_assign difference (LinearConstraint.x_nnconvex_constraint_of_x_linear_constraint !xconstraint_i_plus_one);
						
						(* Check if difference is non-empty *)
						if not (LinearConstraint.x_nnconvex_constraint_is_false difference) then(
							(* Print some information *)
							print_message Verbose_low ("\nFound a shrinking of clock constraint between positions " ^ (string_of_int !i) ^ " and " ^ (string_of_int (!i+1)) ^ ":");
							
							(* Update flag *)
							found := true;
							
							(* Print some information *)
							if verbose_mode_greater Verbose_high then(
								print_message Verbose_high ("\nClock valuations blocked between positions " ^ (string_of_int !i) ^ " and " ^ (string_of_int (!i+1)) ^ ":");
								print_message Verbose_high (LinearConstraint.string_of_x_nnconvex_constraint model.variable_names difference);
							);
							
							(* Exhibit a point *)
							let concrete_x_valuation = LinearConstraint.x_nnconvex_exhibit_point difference in
							
							(* Construct the px-valuation *)
							(*** NOTE: technically (internally), the concrete_x_valuation already contains the parameter valuations! but for type soundness, we pretend to take parameters from pval ***)
							let concrete_px_valuation_i variable_index = match model.type_of_variables variable_index with
								| Var_type_clock -> concrete_x_valuation variable_index
								| Var_type_parameter -> functional_pval_positive variable_index
								| _ -> raise (InternalError ("Only clocks or parameters are expected at this point (in algoEFexemplify#process_counterexample)"))
							in
(*							(*** NOTE: technically (internally), the concrete_x_valuation already contains the parameter valuations! but for type soundness, we pretend to re-intersect with the pval ***)
							(* Convert the p-valuation to a constraint *)
							let concrete_p_valuation_constraint = LinearConstraint.p_constraint_of_point (List.map (fun parameter_index -> parameter_index , functional_pval_positive parameter_index) model.parameters ) in
							let concrete_px_valuation_i = LinearConstraint.px_of_p_constraint concrete_p_valuation_constraint in
							LinearConstraint.px_intersection_assign_x concrete_px_valuation_i [LinearConstraint.x_constraint_of_point (List.map (fun clock_index -> clock_index , concrete_x_valuation clock_index) model.clocks)];*)
							
							(* Print some information *)
							if verbose_mode_greater Verbose_low then(
								print_message Verbose_low ("Example of blocking point at position " ^ (string_of_int !i) ^ ":");
								print_message Verbose_low (ModelPrinter.string_of_px_valuation model concrete_px_valuation_i);
							);
							
							(* Generate the concrete run up to this point *)
							(*------------------------------------------------------------*)
							
							(*** WARNING/BADPROG: the following few lines are duplicate code with above ***)

							(* Cut the symbolic run *)
							let symbolic_run_prefix : StateSpace.symbolic_run = {
								(* Take the sublist of steps from position 0 to the current position *)
								symbolic_steps	= if !i > 0 then OCamlUtilities.sublist 0 (!i-1) symbolic_run.symbolic_steps else [];
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
								print_message Verbose_low ("Now generating the \"impossible\" concrete run for positive parameter valuation from position " ^ (string_of_int !i) ^ "…");
							);
							
							(*** NOTE: now, the only way to choose the NEXT point at position i+1 is to consider a 0-time transition from position i, because we know that the point exhibited at position i does not belong to the i+1 zone ***)
							
							let state_i_plus_one =
								(* Careful! If run is too short, choose final state *)
								if !i = List.length symbolic_run.symbolic_steps - 1 then symbolic_run.final_state
								else (List.nth symbolic_run.symbolic_steps (!i+1)).source
							in
							let transition_i_plus_one = (List.nth symbolic_run.symbolic_steps (!i)).transition in
							let impossible_step_i =
							{
								(* NO time elapsing *)
								time			= NumConst.zero;
								(* Then take a discrete transition: keep the action *)
								action			= StateSpace.get_action_from_combined_transition transition_i_plus_one;
								(* Then reach the target state *)
								target			= {
									global_location= (StateSpace.get_state state_space state_i_plus_one).global_location;
									px_valuation   = concrete_px_valuation_i;
								}
							}
							in
							
							(* Now build the rest of the impossible run *)
							print_message Verbose_medium ("Building suffix from step " ^ (string_of_int (!i+1)) ^ "…");
							
							(* First check whether there is any state to build *)
							let impossible_steps_suffix =
							if List.length symbolic_run.symbolic_steps = !i+1 then(
								print_message Verbose_medium ("No suffix to generate as the symbolic run has length " ^ (string_of_int (!i+1)) ^ ".");
								
								(* Nothing to do *)
								[]
							
							)else(
							
								(* Print some information *)
								if verbose_mode_greater Verbose_high then(
									print_message Verbose_high ("Considering subset of symbolic run of length " ^ (string_of_int (List.length symbolic_run.symbolic_steps)) ^ " from position " ^ (string_of_int (!i+1)) ^ " to position " ^ (string_of_int ((List.length symbolic_run.symbolic_steps) - 1)) ^ "…");
								);
								
								(* Convert the symbolic existing steps to concrete steps from the impossible valuation *)
								self#impossible_concrete_steps_of_symbolic_steps concrete_px_valuation_i (!i+1) (OCamlUtilities.sublist (!i+1) ((List.length symbolic_run.symbolic_steps) - 1) symbolic_run.symbolic_steps)
							)
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
								impossible_steps= impossible_step_i :: impossible_steps_suffix;
							}
							in
							
							(* Print some information *)
							print_message Verbose_standard "Negative counterexample run constructed for the positive parameter valuation!";
							if verbose_mode_greater Verbose_low then (
								(* Debug print *)
								print_message Verbose_low (ModelPrinter.debug_string_of_impossible_concrete_run model impossible_concrete_run);
							);

							(* Add the run to the list of results *)
							let valuation_and_concrete_run = {
								(* The parameter valuation for which this run exists *)
								valuation		= pval_positive;
								(* Sometimes, we can even infer more valuations for which an equivalent DISCRETE run exist (note that the exact timings of the run might differ!!!) *)
								(*** WARNING: is that sure??? ***)
								valuations		= LinearConstraint.Convex_p_constraint target_p_constraint;
								(* The concrete run *)
								concrete_run	= Result.Impossible_concrete_run impossible_concrete_run;
							} in

							(* Update the counterexamples processed *)
							negative_examples <- valuation_and_concrete_run :: negative_examples;

							()
							
						); (* end if found restrained constraint *)
						
						(* Move to previous step *)
						xconstraint_i_plus_one := xconstraint_i;
						decr i;
					done; (* end while backward *)
					
					if not !found then(
						print_message Verbose_standard "\n\nFound no clock valuation allowing a negative counterexample for the same parameter valuaton for this run";
						
						(*** TODO: try some other heuristics? ***)
						
					);
							
				
				); (* end if strongly deterministic without silent actions *)
				
				
		end; (* end match global time clock *)
		
		(* If maximum number of counterexamples processed: stop *)
		if List.length positive_examples >= nb_POSITIVE_EXAMPLES_MAX then(
			(* Update termination status *)
			self#print_algo_message Verbose_standard ("Target state #" ^ (string_of_int (List.length positive_examples)) ^ " is the maximum number sought. Terminating…");
			(*** NOTE/HACK: the number of unexplored states is not known, therefore we do not add it… ***)
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
	method compute_result =
		(* Print some information *)
		self#print_algo_message_newline Verbose_standard (
			"Algorithm completed " ^ (after_seconds ()) ^ "."
		);
		
		(* Get the termination status *)
		 let termination_status = match termination_status with
			| None -> raise (InternalError ("Termination status not set in " ^ (self#algorithm_name) ^ ".compute_result"))
			| Some status -> status
		in
		
		(* Return the result *)
		Runs_exhibition_result
		{
			(* Non-necessarily convex constraint guaranteeing the reachability of the bad location *)
			(*** NOTE: use rev since we added the runs by reversed order ***)
			runs				= List.rev_append positive_examples (List.rev negative_examples);
			
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
