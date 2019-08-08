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
 * Last modified     : 2019/08/08
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
					(*** TODO: convert to string ***)
					ModelPrinter.debug_print_symbolic_run model state_space symbolic_run;
				);
				
				(* Exhibit a concrete run from the symbolic run *)
				let concrete_run = AlgoStateBased.concrete_run_of_symbolic_run state_space (predecessors : StateSpace.predecessors_table) (symbolic_run : StateSpace.symbolic_run) in

				(* Generate the graphics *)
				Graphics.draw_concrete_run concrete_run (options#files_prefix ^ "_signals_" ^ (string_of_int nb_positive_examples));
				
				
				(*------------------------------------------------------------*)
				(* Part 2: negative counterexample *)
				(*------------------------------------------------------------*)
				
				(*** TODO: handle non-deterministic ***)
				
				if model.strongly_deterministic && not model.has_silent_actions then(
					(* Part 2.a: try to find a parameter valuation NOT going to the final state using this run *)
					(* Idea: any parameter valuation "deadlocked" along this run is a valuation for which no identical symbolic run leads to the target, or any other target (in case of several discrete target locations) *)

					(* Reason backward *)
					let i = ref ((List.length symbolic_run.symbolic_steps) - 1) in
					(* Define the next valuation along the run, and reason backward *)
					let pconstraint_i_plus_one = ref (LinearConstraint.px_hide_nonparameters_and_collapse (StateSpace.get_state state_space symbolic_run.final_state).px_constraint) in
					
					while !i > 0 do
						(* Get the p-constraint at position i *)
						let pconstraint_i : LinearConstraint.p_linear_constraint = LinearConstraint.px_hide_nonparameters_and_collapse (StateSpace.get_state state_space (List.nth symbolic_run.symbolic_steps !i).source).px_constraint in
						
						(* Check if difference is non-null *)
						();
						
						(* Move to previous step *)
						pconstraint_i_plus_one := pconstraint_i;
						decr i;
					done;
					
					
					
				
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
