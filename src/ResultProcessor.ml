(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: process the result of IMITATOR: print results, creates files, generates graphics, etc.
 * 
 * File contributors : Étienne André
 * Created           : 2015/12/03
 * Last modified     : 2020/01/08
 *
 ************************************************************)

 
 
(************************************************************)
(* Modules *)
(************************************************************)
open OCamlUtilities
open ImitatorUtilities
open Exceptions
open Statistics
open AbstractModel
open Result



(************************************************************)
(* Conversion of a good_bad_constraint into a list of (1 or 2) zones *)
(************************************************************)
let zones_of_good_bad_constraint = function
	(* Only good valuations: convert to GOOD convex polyhedron *)
	| Good_constraint (p_nnconvex_constraint, _) ->
		[LinearConstraint.Nonconvex_p_constraint p_nnconvex_constraint, StateSpace.Good]
		
	(* Only bad valuations: convert to BAD convex polyhedron *)
	| Bad_constraint (p_nnconvex_constraint, _) ->
		[LinearConstraint.Nonconvex_p_constraint p_nnconvex_constraint, StateSpace.Bad]

	(* Both good and bad valuations *)
	| Good_bad_constraint good_and_bad_constraint ->
		let good_p_nnconvex_constraint, _ = good_and_bad_constraint.good in
		let bad_p_nnconvex_constraint, _ = good_and_bad_constraint.bad in
		[
			LinearConstraint.Nonconvex_p_constraint good_p_nnconvex_constraint, StateSpace.Good;
			LinearConstraint.Nonconvex_p_constraint bad_p_nnconvex_constraint, StateSpace.Bad;
		]


(************************************************************)
(* Conversion of enumerated types to string *)
(************************************************************)

(*** NOTE: this part should be modified with care, as external tools calling IMITATOR may use this syntax ***)

(** Subclass of the model *)
let string_of_lu_status = function
	(* General PTA *)
	| PTA_notLU -> "not L/U"
	(* L/U-PTA with parameters partitioned into L- and U-parameters *)
	| PTA_LU _ -> "L/U-PTA"
	(* L-PTA *)
	| PTA_L -> "L-PTA"
	(* U-PTA *)
	| PTA_U -> "U-PTA"

	

let string_of_bfs_algorithm_termination = function
	(* Fixpoint-like termination *)
	| Regular_termination -> "regular termination"
	(* Termination due to time limit reached *)
	| Time_limit nb_unexplored_successors -> "time limit (" ^ (string_of_int nb_unexplored_successors) ^ " successor" ^ (s_of_int nb_unexplored_successors) ^ " unexplored)"
	(* Termination due to state space depth limit reached *)
	| Depth_limit nb_unexplored_successors -> "depth limit (" ^ (string_of_int nb_unexplored_successors) ^ " successor" ^ (s_of_int nb_unexplored_successors) ^ " unexplored)"
	(* Termination due to a number of explored states reached *)
	| States_limit nb_unexplored_successors -> "states limit (" ^ (string_of_int nb_unexplored_successors) ^ " successor" ^ (s_of_int nb_unexplored_successors) ^ " unexplored)"
	(* Termination due to a target state found *)
	(*** NOTE/HACK: the number of unexplored states is not known, therefore we do not add it… ***)
	| Target_found -> "terminated after reaching a target state (some states may have been unexplored)"


let string_of_bc_algorithm_termination = function
	(* Fixpoint-like termination *)
	| BC_Regular_termination -> "regular termination"

	(* Termination due to a maximum number of tiles computed *)
	| BC_Tiles_limit -> "tiles limit"

	(* Termination due to time limit reached *)
	| BC_Time_limit -> "time limit"
	
	(* Termination due to several limits (only possible in distributed setting) *)
	| BC_Mixed_limit -> "mixed limit"
	
	
	
let string_of_soundness = function
	(* Constraint included in or equal to the real result *)
	| Constraint_maybe_under -> "possible under-approximation"
	(* Exact result *)
	| Constraint_exact -> "exact"
	(* Constraint equal to or larger than the real result *)
	| Constraint_maybe_over -> "possible over-approximation"
	(* Pair of constraints: one under-approximation and one over-approximation *)
(* 	| Constraint_under_over -> "both a possible under- and a possible over-approximation" *)
	(* Impossible to compare the constraint with the original result *)
	| Constraint_maybe_invalid -> "possibly invalid"


let verbose_string_of_soundness_suffix = function
	(* Constraint included in or equal to the real result *)
	| Constraint_maybe_under -> "is an under-approximation of the actual result (or the actual result itself)"
	(* Exact result *)
	| Constraint_exact -> "is exact (sound and complete)"
	(* Constraint equal to or larger than the real result *)
	| Constraint_maybe_over -> "is an over-approximation of the actual result (or the actual result itself)"
	(* Pair of constraints: one under-approximation and one over-approximation *)
(* 	| Constraint_under_over -> "This constraint is both a possibly under-approximation and a possibly over-approximation (or the actual result itself)" *)
	(* Impossible to compare the constraint with the original result *)
	| Constraint_maybe_invalid -> "validity cannot be assessed"
let verbose_string_of_soundness s = "This constraint " ^ (verbose_string_of_soundness_suffix s)


let string_of_coverage = function
	(* Full coverage in all dimensions, including rational points *)
	| Coverage_full -> "full"

	(* No constraint computed at all *)
	| Coverage_empty -> "empty"

	(* At least all integers are covered, rationals perhaps not *)
	| Coverage_integer_complete -> "integer-complete"

	(* No indication of coverage *)
	| Coverage_unknown -> "unknown"


let verbose_string_of_coverage = function
	(* Full coverage in all dimensions, including rational points *)
	| Coverage_full -> "The entire input parameter domain has been covered by tiles."

	(* No constraint computed at all *)
	| Coverage_empty -> "None of the input parameter domain has been covered: no tiles could be computed."

	(* At least all integers are covered, rationals perhaps not *)
	| Coverage_integer_complete -> "All integer points of the input parameter domain have been covered by tiles; rational points may have been covered too (not evaluated)."

	(* No indication of coverage *)
	| Coverage_unknown -> "The coverage of the input parameter domain is probably incomplete and cannot be evaluated."


(** Convert a Result.good_or_bad_constraint into a string *)
let string_of_good_or_bad_constraint variable_names = function 
	(* Only good valuations *)
	| Good_constraint (p_nnconvex_constraint, _)
(* 	| Accepting_cycle_constraint (p_nnconvex_constraint, _) *)
	(* Only bad valuations *)
	| Bad_constraint (p_nnconvex_constraint, _)
		-> LinearConstraint.string_of_p_nnconvex_constraint variable_names p_nnconvex_constraint

	(* Both good and bad valuations *)
	| Good_bad_constraint good_and_bad_constraint
		->
		let good_p_nnconvex_constraint, _ = good_and_bad_constraint.good in
		let bad_p_nnconvex_constraint, _ = good_and_bad_constraint.bad in
		(LinearConstraint.string_of_p_nnconvex_constraint variable_names good_p_nnconvex_constraint)
		^
		"\n<good|bad>\n"
		^
		(LinearConstraint.string_of_p_nnconvex_constraint variable_names bad_p_nnconvex_constraint)

(** Convert a Result.good_or_bad_constraint into a string for the sole soundness *)
let string_soundness_of_good_or_bad_constraint = function
	(* Only good valuations *)
	| Good_constraint (_, soundness)
	(* Only bad valuations *)
	| Bad_constraint (_, soundness)
		-> string_of_soundness soundness

	(* Both good and bad valuations *)
	| Good_bad_constraint good_and_bad_constraint
		->
		let _, good_soundness = good_and_bad_constraint.good in
		let _, bad_soundness = good_and_bad_constraint.bad in
		(string_of_soundness good_soundness)
		^
		" <good|bad> "
		^
		(string_of_soundness bad_soundness)


(** Convert a Result.good_or_bad_constraint into a string for the sole statespace nature *)
let string_statespace_nature_of_good_or_bad_constraint = function
	(* Only good valuations *)
	| Good_constraint _ -> StateSpace.string_of_statespace_nature StateSpace.Good
	(* Only bad valuations *)
	| Bad_constraint _ -> StateSpace.string_of_statespace_nature StateSpace.Bad
	(* Both good and bad valuations *)
	| Good_bad_constraint _ -> (StateSpace.string_of_statespace_nature StateSpace.Good) ^ "/" ^ (StateSpace.string_of_statespace_nature StateSpace.Bad)


(** Convert a Result.good_or_bad_constraint into a verbose string for the sole soundness *)
let verbose_string_soundness_of_good_or_bad_constraint = function 
	(* Only good valuations *)
	| Good_constraint (_, soundness) -> "This positive constraint " ^ (verbose_string_of_soundness_suffix soundness)
(* 	| Accepting_cycle_constraint (_, soundness) -> "This constraint for accepting cycles " ^ (verbose_string_of_soundness_suffix soundness) *)
	(* Only bad valuations *)
	| Bad_constraint (_, soundness) -> "This negative constraint " ^ (verbose_string_of_soundness_suffix soundness)

	(* Both good and bad valuations *)
	| Good_bad_constraint good_and_bad_constraint
		->
		let _, good_soundness = good_and_bad_constraint.good in
		let _, bad_soundness = good_and_bad_constraint.bad in
		"The positive constraint " ^ (verbose_string_of_soundness_suffix good_soundness)
		^
		". "
		^
		"The negative constraint " ^ (verbose_string_of_soundness_suffix bad_soundness) ^ "."


(** Add standardised delimiters to constraints *)
let add_constraints_delimiters constraint_str =
	(* begin delimiter *)
	"\n\nBEGIN CONSTRAINT\n"
	^ constraint_str ^ ""
	(* end delimiter *)
	^ "\nEND CONSTRAINT\n"

(** Add standardised delimiters to results (e.g. runs) *)
let add_result_delimiters constraint_str =
	(* begin delimiter *)
	"\n\nBEGIN RESULT\n"
	^ constraint_str ^ ""
	(* end delimiter *)
	^ "\nEND RESULT\n"


(************************************************************)
(* I/O functions *)
(************************************************************)
(* Header for result files *)
let file_header () =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	    "(************************************************************" 
	(* Program version *)
	^ "\n * Result output by " ^ Constants.program_name ^ ""
	^ "\n * Version  : " ^ (ImitatorUtilities.program_name_and_version_and_nickname_and_build)
	^ "\n * Git      : " ^ (ImitatorUtilities.git_branch_and_hash)
	^ "\n * Model    : '" ^ options#model_file_name ^ "'"
	(* Date *)
	^ "\n * Generated: " ^ (now()) ^ ""
	(* Command *)
	^ "\n * Command  : " ^ (string_of_array_of_string_with_sep " " Sys.argv)
	^ "\n ************************************************************)\n\n"

(* Return a string made of some information concerning the input model *)
let model_statistics () =
	
	(* Retrieve the model *)
	let model = Input.get_model() in

	(* Create the statistics *)
	    "Number of IPTAs                         : " ^ (string_of_int model.nb_automata)
	^ "\nNumber of clocks                        : " ^ (string_of_int model.nb_clocks)
	^ "\nHas stopwatches?                        : " ^ (string_of_bool model.has_stopwatches)
	^ "\nL/U subclass                            : " ^ (string_of_lu_status model.lu_status)
	^ "\nHas silent actions?                     : " ^ (string_of_bool model.has_silent_actions)
	^ "\nIs strongly deterministic?              : " ^ (string_of_bool model.strongly_deterministic)
	^ "\nNumber of parameters                    : " ^ (string_of_int model.nb_parameters)
	^ "\nNumber of discrete variables            : " ^ (string_of_int model.nb_discrete)
	^ "\nNumber of actions                       : " ^ (string_of_int model.nb_actions)
	^ "\nTotal number of locations               : " ^ (string_of_int model.nb_locations)
	^ "\nAverage locations per IPTA              : " ^ (round1_float ((float_of_int model.nb_locations) /. (float_of_int model.nb_automata)))
	^ "\nTotal number of transitions             : " ^ (string_of_int model.nb_transitions)
	^ "\nAverage transitions per IPTA            : " ^ (round1_float ((float_of_int model.nb_transitions) /. (float_of_int model.nb_automata)))



(* Return a string made of some statistics for the state space *)
let statespace_statistics state_space total_time =
	(* Speed: number of states computed and still in the state space *)
	let nb_states = StateSpace.nb_states state_space in
	let states_per_second = (float_of_int nb_states) /. total_time in
	
	(* Speed: number of states computed, even if not kept (because merged, deleted…) *)
	let nb_gen_states = StateSpace.get_nb_gen_states state_space in
	let gen_states_per_second = (float_of_int nb_gen_states) /. total_time in
	
	    "Number of states                        : " ^ (string_of_int nb_states)
	^ "\nNumber of transitions                   : " ^ (string_of_int (StateSpace.nb_transitions state_space))
	^ "\nNumber of computed states               : " ^ (string_of_int nb_gen_states)
	^ "\nTotal computation time                  : " ^ (string_of_seconds total_time)
	^ "\nStates/second in state space            : " ^ (round1_float states_per_second) ^ " (" ^ (string_of_int nb_states) ^ "/" ^ (string_of_seconds total_time) ^ ")"
	^ "\nComputed states/second                  : " ^ (round1_float gen_states_per_second) ^ " (" ^ (string_of_int nb_gen_states) ^ "/" ^ (string_of_seconds total_time) ^ ")"
	^ "\nEstimated memory                        : " ^ (memory_used ())

	
(* Return a string made of some statistics for the abstract state space *)
let abstract_statespace_statistics abstract_state_space total_time =
	(* Speed: number of states computed and still in the state space *)
	let states_per_second = (float_of_int abstract_state_space.nb_states) /. total_time in
	
	    "Local number of states                  : " ^ (string_of_int abstract_state_space.nb_states)
	^ "\nLocal number of transitions             : " ^ (string_of_int abstract_state_space.nb_transitions)
	^ "\nLocal computation time                  : " ^ (string_of_seconds total_time)
	^ "\nStates/second in state space            : " ^ (round1_float states_per_second) ^ " (" ^ (string_of_int abstract_state_space.nb_states) ^ "/" ^ (string_of_seconds total_time) ^ ")"


	
(* Return a string made of some information concerning the result *)
let result_nature_statistics (soundness_str : string) termination (statespace_nature_str : string) =
	    "Constraint soundness                    : " ^ soundness_str
	^ "\nTermination                             : " ^ (string_of_bfs_algorithm_termination termination)
	^ "\nConstraint nature                       : " ^ statespace_nature_str

(* Return a string made of some information concerning the result (for multiple_synthesis_result) *)
let result_nature_statistics_bc (soundness_str : string) termination (statespace_nature_str : string) =
	    "Constraint soundness                    : " ^ soundness_str
	^ "\nTermination                             : " ^ (string_of_bc_algorithm_termination termination)
	^ "\nConstraint nature                       : " ^ statespace_nature_str


(*** TODO: would be smarter to have a generic function export_to_file_result : imitator_result -> unit () ***)

(* Write an ef_synth result to the result file *)
let export_to_file_noresult file_name =
	(*** WARNING: duplicate code concerning the counter creation ***)
	(* Create counter *)
	let counter = Statistics.create_time_counter_and_register "file generation" Graphics_counter Verbose_low in
	
	(* Start counter *)
	counter#start;
	
	(* Retrieve the model *)
(* 	let model = Input.get_model() in *)

	(* Prepare the string to write *)
	let file_content =
		(* 1) Header *)
		file_header ()
		
		(* 2) Statistics about model *)
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (model_statistics ())
		^ "\n------------------------------------------------------------"

		(* 3) General statistics *)
		^ "\n" ^ (Statistics.string_of_all_counters())
		^ "\n------------------------------------------------------------"
	in
	
	(* Write to file *)
	write_to_file file_name file_content;
	print_message Verbose_standard ("\nResult written to file '" ^ file_name ^ "'.");
	
	(* Stop counter *)
	counter#stop;
	
	(* The end *)
	()



(* Write an ef_synth result to the result file *)
let export_to_file_deprecated_efsynth_result file_name (deprecated_efsynth_result : Result.deprecated_efsynth_result) =
	(*** WARNING: duplicate code concerning the counter creation ***)
	(* Create counter *)
	let counter = Statistics.create_time_counter_and_register "file generation" Graphics_counter Verbose_low in
	
	(* Start counter *)
	counter#start;
	
	(* Retrieve the model *)
	let model = Input.get_model() in
	(* Retrieve the input options *)
(* 	let options = Input.get_options () in *)

	(* Convert the constraint to a string *)
	let result_str = string_of_list_of_string_with_sep "\n OR \n" (List.map (LinearConstraint.string_of_p_linear_constraint model.variable_names) deprecated_efsynth_result.constraints) in

	(* Prepare the string to write *)
	let file_content =
		(* 1) Header *)
		file_header ()
		
		(* 2) Statistics about model *)
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (model_statistics ())
		^ "\n------------------------------------------------------------"

		(* 3) The actual result *)
		^ (add_constraints_delimiters result_str)
		
		(* 4) Statistics about result *)
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (result_nature_statistics (string_of_soundness deprecated_efsynth_result.soundness) deprecated_efsynth_result.termination (StateSpace.string_of_statespace_nature deprecated_efsynth_result.statespace_nature))
		
		(* 5) Statistics about state space *)
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (statespace_statistics deprecated_efsynth_result.state_space deprecated_efsynth_result.computation_time)
		^ "\n------------------------------------------------------------"
		
		(* 6) General statistics *)
		^ "\n" ^ (Statistics.string_of_all_counters())
		^ "\n------------------------------------------------------------"
	in
	
	(* Write to file *)
	write_to_file file_name file_content;
	print_message Verbose_standard ("\nResult written to file '" ^ file_name ^ "'.");
	
	(* Stop counter *)
	counter#stop;
	
	(* The end *)
	()


(* Write a single_synthesis_result to the result file *)
let export_to_file_single_synthesis_result file_name (single_synthesis_result : Result.single_synthesis_result) =
	(*** WARNING: duplicate code concerning the counter creation ***)
	(* Create counter *)
	let counter = Statistics.create_time_counter_and_register "file generation" Graphics_counter Verbose_low in
	
	(* Start counter *)
	counter#start;

	(* Retrieve the model *)
	let model = Input.get_model() in
	(* Retrieve the input options *)
(* 	let options = Input.get_options () in *)

	(* Convert the resulting constraint to a string *)
	let result_str : string = string_of_good_or_bad_constraint model.variable_names single_synthesis_result.result in

	(* Handle the soundness separately *)
	let soundness_str : string = string_soundness_of_good_or_bad_constraint single_synthesis_result.result in 

	(* Handle the statespace nature separately *)
	let statespace_nature_str = string_statespace_nature_of_good_or_bad_constraint single_synthesis_result.result in

	(* Prepare the string to write *)
	let file_content =
		(* 1) Header *)
		file_header ()
		
		(* 2) Statistics about model *)
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (model_statistics ())
		^ "\n------------------------------------------------------------"

		(* 3) The actual result with delimiters *)
		^ (add_constraints_delimiters result_str)
		
		(* 4) Statistics about result *)
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (result_nature_statistics soundness_str single_synthesis_result.termination statespace_nature_str)
		
		(* 5) Statistics about state space *)
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (statespace_statistics single_synthesis_result.state_space single_synthesis_result.computation_time)
		^ "\n------------------------------------------------------------"
		
		(* 6) General statistics *)
		^ "\n" ^ (Statistics.string_of_all_counters())
		^ "\n------------------------------------------------------------"
	in
	
	(* Write to file *)
	write_to_file file_name file_content;
	print_message Verbose_standard ("\nResult written to file '" ^ file_name ^ "'.");
	
	(* Stop counter *)
	counter#stop;
	
	(* The end *)
	()


(* Write a multiple_synthesis_result to the result file *)
let export_to_file_multiple_synthesis_result file_name (multiple_synthesis_result : Result.multiple_synthesis_result) =
	(*** WARNING: duplicate code concerning the counter creation ***)
	(* Create counter *)
	let counter = Statistics.create_time_counter_and_register "file generation" Graphics_counter Verbose_low in
	
	(* Start counter *)
	counter#start;

	(* Retrieve the model *)
	let model = Input.get_model() in

	(* Convert the resulting constraint to a string *)
	let result_str : string = string_of_good_or_bad_constraint model.variable_names multiple_synthesis_result.result in

	(* Handle the soundness separately *)
	let soundness_str : string = string_soundness_of_good_or_bad_constraint multiple_synthesis_result.result in 

	(* Handle the statespace nature separately *)
	let statespace_nature_str = string_statespace_nature_of_good_or_bad_constraint multiple_synthesis_result.result in

	(* Prepare the string to write *)
	let file_content =
		(* 1) Header *)
		file_header ()
		
		(* 2) Statistics about model *)
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (model_statistics ())
		^ "\n------------------------------------------------------------"

		(* 3) The actual result with delimiters *)
		^ (add_constraints_delimiters result_str)
		
		(* 4) Statistics about result *)
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (result_nature_statistics_bc soundness_str multiple_synthesis_result.termination statespace_nature_str)
		
		(* 5) General statistics *)
		^ "\n" ^ (Statistics.string_of_all_counters())
		^ "\n------------------------------------------------------------"
	in
	
	(* Write to file *)
	write_to_file file_name file_content;
	print_message Verbose_standard ("\nResult written to file '" ^ file_name ^ "'.");
	
	(* Stop counter *)
	counter#stop;
	
	(* The end *)
	()



(* Write an ef_synth result to the result file *)
let export_to_file_point_based_result file_name (point_based_result : Result.point_based_result) =
	(*** WARNING: duplicate code concerning the counter creation ***)
	(* Create counter *)
	let counter = Statistics.create_time_counter_and_register "file generation" Graphics_counter Verbose_low in
	
	(* Start counter *)
	counter#start;

	(* Retrieve the model *)
	let model = Input.get_model() in
	(* Retrieve the input options *)
(* 	let options = Input.get_options () in *)

	(* Convert the constraint to a string *)
	let result_str = string_of_good_or_bad_constraint model.variable_names point_based_result.result in

	(* Handle the soundness separately *)
	let soundness_str = string_soundness_of_good_or_bad_constraint point_based_result.result in 
	
	(* Handle the statespace nature separately *)
	let statespace_nature_str = string_statespace_nature_of_good_or_bad_constraint point_based_result.result in

	(* Prepare the string to write *)
	let file_content =
		let pi0 = point_based_result.reference_val in
		
		(* 1) Header *)
		file_header ()

		(* 2) Statistics about model *)
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (model_statistics ())
		^ "\n------------------------------------------------------------"

		(* 3) Recall pi0 *)
		^ "\n\n------------------------------------------------------------"
		^ "\n Reference parameter valuation:"
		^ "\n" ^ (ModelPrinter.string_of_pval model pi0)
		^ "\n------------------------------------------------------------"
		
		(* 4) The actual result with delimiters *)
		^ (add_constraints_delimiters result_str)
		
		(* 5) Statistics about result *)
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (result_nature_statistics soundness_str point_based_result.termination statespace_nature_str)
(* 		^ "\nNumber of random selections             : " ^ (string_of_int point_based_result.nb_random_selections) *)
		
		(* 6) Statistics about state space *)
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (statespace_statistics point_based_result.state_space point_based_result.computation_time)
		^ "\n------------------------------------------------------------"
		
		(* 7) General statistics *)
		^ "\n" ^ (Statistics.string_of_all_counters())
		^ "\n------------------------------------------------------------"
		^ "\n"
	in
	
	(* Write to file *)
	write_to_file file_name file_content;
	print_message Verbose_standard ("\nResult written to file '" ^ file_name ^ "'.");
	
	(* Stop counter *)
	counter#stop;
	
	(* The end *)
	()


let general_bc_statistics (cartography_result : Result.cartography_result) =
	(* Store number of tiles *)
	let nb_tiles = List.length cartography_result.tiles in

	(* First, compute average number of states and transitions (for info purpose) *)
	(*** WARNING: use int, but using NumConst (unbounded) would be smarter in case of very large state spaces ***)
	let total_states, total_transitions, time_im = List.fold_left (
		fun (current_sum_states, current_sum_transitions, current_sum_time) abstract_point_based_result ->
			(
				current_sum_states + abstract_point_based_result.abstract_state_space.nb_states
				,
				current_sum_transitions +  + abstract_point_based_result.abstract_state_space.nb_transitions
				,
				current_sum_time +. abstract_point_based_result.computation_time
			)
	) (0, 0, 0.0) cartography_result.tiles
	in
	(* Compute average *)
	let average_nb_states = (float_of_int total_states) /. (float_of_int nb_tiles) in
	let average_nb_transitions = (float_of_int total_transitions) /. (float_of_int nb_tiles) in
	
       ""
	^   "Number of integers in v0                : " ^ (NumConst.string_of_numconst cartography_result.size_v0)
	^ "\nNumber of tiles computed                : " ^ (string_of_int nb_tiles)
	^ "\nCoverage                                : " ^ (string_of_coverage cartography_result.coverage)
	^ "\nTermination                             : " ^ (string_of_bc_algorithm_termination cartography_result.termination)
	^ "\nNumber of unsuccessful points           : " ^ (string_of_int cartography_result.nb_unsuccessful_points)
	^ "\nAverage number of states                : " ^ (round1_float average_nb_states)
	^ "\nAverage number of transitions           : " ^ (round1_float average_nb_transitions)
    ^ "\nTotal computation time                  : " ^ (string_of_seconds cartography_result.computation_time)
	^ "\nTotal computation time (IM)             : " ^ (string_of_seconds time_im)
(* 	^ "\nTotal computation time (find point)     : " ^ (string_of_seconds bc_result.find_point_time) *)
	^ "\nEstimated memory                        : " ^ (memory_used ())

		
(* Write result of BC to file *)
let export_to_file_cartography_result file_name (cartography_result : Result.cartography_result) =
	(*** WARNING: duplicate code concerning the counter creation ***)
	(* Create counter *)
	let counter = Statistics.create_time_counter_and_register "file generation" Graphics_counter Verbose_low in
	
	(* Start counter *)
	counter#start;

	(* Retrieve the model *)
	let model = Input.get_model() in
	(* Retrieve the input options *)
(* 	let options = Input.get_options () in *)

	(* Convert the abstract_point_based_result's to string *)
	let abstract_point_based_results_str = string_of_list_of_string_with_sep "\n" (
		List.mapi (fun index (abstract_point_based_result : abstract_point_based_result) ->
		
			(* Handle the soundness separately *)
			let soundness_str : string = string_soundness_of_good_or_bad_constraint abstract_point_based_result.result in 
			
			(* Handle the statespace nature separately *)
			let statespace_nature_str = string_statespace_nature_of_good_or_bad_constraint abstract_point_based_result.result in


			(* mapi starts counting from 0, but we like starting counting from 1 *)
			let index_from_one = index + 1 in
			"\n(************************************************************)"
			^ "\n Tile #" ^ (string_of_int index_from_one)
			(* 1) Reference valuation *)
			^ "\n\n Pi" ^ (string_of_int index_from_one) ^ ":"
			^ "\n" ^ (ModelPrinter.string_of_pval model abstract_point_based_result.reference_val)

			(* 2) Constraint *)
			^ "\n\n K" ^ (string_of_int index_from_one) ^ ":"
			^ "\n" ^ (string_of_good_or_bad_constraint model.variable_names abstract_point_based_result.result)
			
			(* 3) Statistics about result *)
			^ "\n\n------------------------------------------------------------"
			^ "\n" ^ (result_nature_statistics soundness_str abstract_point_based_result.termination statespace_nature_str)
(*			^ "\nNumber of random selections             : " ^ (string_of_int abstract_point_based_result.nb_random_selections)*)
			
			(* 4) Statistics about state space *)
			^ "\n------------------------------------------------------------"
			^ "\n" ^ (abstract_statespace_statistics abstract_point_based_result.abstract_state_space abstract_point_based_result.computation_time)
			^ "\n------------------------------------------------------------"
			^ "\n(************************************************************)\n"
		) cartography_result.tiles
	)
	in
	
	(* Prepare the string to write *)
	let file_content =
	
	
	
		(*** TODO: ***)
		raise (NotImplemented ("get_vo in result processor"))
		
		(*
		let v0 = Input.get_v0 () in

		(* 1) The file header *)
		file_header ()
		
		(* 2) Statistics about model *)
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (model_statistics ())
		^ "\n------------------------------------------------------------"

		(* 3) Recall v0 *)
		^ "\n\n------------------------------------------------------------"
		^ "\n Reference parameter domain:"
		^ "\n" ^ (ModelPrinter.string_of_v0 model v0)
		^ "\n------------------------------------------------------------"
		
		(* 4) The actual result *)
		^ "\n" ^ abstract_point_based_results_str ^ "\n"
		
		(* 5) Statistics on BC *)
		^ "\n(************************************************************)"
		^ "\nGENERAL STATISTICS"
		^ "\n(************************************************************)"
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (general_bc_statistics cartography_result)
		^ "\n------------------------------------------------------------"
		
		(* 6) General statistics *)
		^ "\n" ^ (Statistics.string_of_all_counters())
		^ "\n------------------------------------------------------------"
		^ "\n"
		*)
	in
	(* Write to file *)
	write_to_file file_name file_content;
	print_message Verbose_standard ("\nResult written to file '" ^ file_name ^ "'.");
	
	(* Stop counter *)
	counter#stop;
	
	(* The end *)
	()


(* Export result of type 'Runs_exhibition_result' *)
let export_to_file_runs_exhibition_result file_name (result : Result.runs_exhibition_result) =
	(*** WARNING: duplicate code concerning the counter creation ***)
	(* Create counter *)
	let counter = Statistics.create_time_counter_and_register "file generation" Graphics_counter Verbose_low in
	
	(* Start counter *)
	counter#start;

	(* Retrieve the model *)
	let model = Input.get_model() in
	(* Retrieve the input options *)
(* 	let options = Input.get_options () in *)

	(* Convert the valuation_and_concrete_run's to string *)
	let runs_str = string_of_list_of_string_with_sep "\n" (
		List.mapi (fun index (valuation_and_concrete_run : Result.valuation_and_concrete_run) ->
		
			(* Get the run (shortcut) *)
			let run = valuation_and_concrete_run.concrete_run in
			
			(* mapi starts counting from 0, but we like starting counting from 1 *)
			let index_from_one = index + 1 in
			"\n(************************************************************)"
			^ "\n Run #" ^ (string_of_int index_from_one)
			(* 1) Valuation for this run *)
			^ "\n\n Valuation:"
			^ "\n" ^ (ModelPrinter.string_of_pval model valuation_and_concrete_run.valuation)

			(* 2) Valuations for which an equivalent DISCRETE run exists *)
			^ "\n\n Other valuations with equivalent (discrete) run:"
			^ "\n" ^ (LinearConstraint.string_of_p_convex_or_nonconvex_constraint model.variable_names valuation_and_concrete_run.valuations)

			(* 3) Run *)
			^ "\n\n Run nature: " ^ (match run with Impossible_concrete_run _ -> "impossible run" | Concrete_run _ -> "valid run")
			^ "\n\n Run:"
			^ "\n" ^ (let str = match run with
				| Concrete_run concrete_run -> ModelPrinter.debug_string_of_concrete_run model concrete_run
				| Impossible_concrete_run impossible_concrete_run -> ModelPrinter.debug_string_of_impossible_concrete_run model impossible_concrete_run
				in str
			)
			^ "\n(************************************************************)\n"
		) result.runs
	)
	in
	

	(* Prepare the string to write *)
	let file_content =
		(* 1) Header *)
		file_header ()
		
		(* 2) Statistics about model *)
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (model_statistics ())
		^ "\n------------------------------------------------------------"

		(* 3) The actual result with delimiters *)
		^ (add_result_delimiters runs_str)
		
		(* 4) Statistics about state space *)
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (statespace_statistics result.state_space result.computation_time)
		^ "\n------------------------------------------------------------"
		
		(* 5) General statistics *)
		^ "\n" ^ (Statistics.string_of_all_counters())
		^ "\n------------------------------------------------------------"
	in
	
	(* Write to file *)
	write_to_file file_name file_content;
	print_message Verbose_standard ("\nResult written to file '" ^ file_name ^ "'.");
	
	(* Stop counter *)
	counter#stop;
	
	(* The end *)
	()



(*------------------------------------------------------------*)
(* Display statistics on the state space *)
(*------------------------------------------------------------*)
let print_state_space_statistics total_time state_space =
	(* Retrieve the input options *)
	let options = Input.get_options () in

	(*** TODO: better have an independent module (or class) 'statistics' ***)
	
	
(*	(* Speed (number of states in the graph) *)
	(*** WARNING: duplicate code from export_to_file_efsynth_result_to_file ***)
	(* Generic function for float/int conversion *)
	let string_of_average average = 
		if average < 10.0 then string_of_float average
		else string_of_int (int_of_float average)
	in
	let nb_states = StateSpace.nb_states state_space in
	let average = (float_of_int nb_states) /. total_time in
	print_message Verbose_standard ("States per second in the graph: " ^ (string_of_average average) ^ " (" ^ (string_of_int nb_states) ^ "/" ^ (string_of_seconds total_time) ^ ")");
	
	(* Speed (number of states computed, even if not kept) *)
	let nb_gen_states = StateSpace.get_nb_gen_states state_space in
	let average = (float_of_int nb_gen_states) /. total_time in
	print_message Verbose_standard ("States computed per second: " ^ (string_of_average average) ^ " (" ^ (string_of_int nb_gen_states) ^ "/" ^ (string_of_seconds total_time) ^ ")");*)
	
	if options#statistics || verbose_mode_greater Verbose_medium then(
		print_message Verbose_standard "\n------------------------------------------------------------";
		print_message Verbose_standard "Statistics: State space";
		print_message Verbose_standard "------------------------------------------------------------";
		print_message Verbose_standard (statespace_statistics state_space total_time);
	);

	
	if options#statistics || verbose_mode_greater Verbose_medium then (
		(* State space *)
(*		print_message Verbose_standard "------------------------------------------------------------";
		print_message Verbose_standard "Statistics: Graph";
		print_message Verbose_standard "------------------------------------------------------------";*)
(* 		print_message Verbose_standard (StateSpace.get_statistics ()); *)
		print_message Verbose_standard (StateSpace.get_statistics_states state_space);
		
	)


(*------------------------------------------------------------*)
(* Display statistics on the memory used (only in mode statistics or if verbose enough) *)
(*------------------------------------------------------------*)
let print_memory_statistics () =
	(* Retrieve the input options *)
	let options = Input.get_options () in

	if options#statistics || verbose_mode_greater Verbose_medium then (
		print_message Verbose_standard "------------------------------------------------------------";
		print_message Verbose_standard "Statistics on memory";
		print_message Verbose_standard "------------------------------------------------------------";
		print_message Verbose_standard (memory_used ());
		Gc.print_stat stdout;
(*		print_message Verbose_standard "------------------------------------------------------------";
		Gc.major();
		Gc.print_stat stdout;
		print_message Verbose_standard "------------------------------------------------------------";
		Gc.full_major();
		Gc.print_stat stdout;*)
	)
	

(************************************************************)
(* Print single_synthesis_result (or in fact point_based_result too) on screen *)
(************************************************************)
let print_single_synthesis_or_point_based_result result computation_time constraint_str =
	(* Print the result *)
	if verbose_mode_greater Verbose_standard then(
		(* Retrieve the model *)
		let model = Input.get_model() in
		(* Retrieve the input options *)
		let options = Input.get_options () in
		
		(* Convert result to string *)
		(*** NOTE: this conversion to string is duplicate, since it will again be converted in export_to_file_pdfc_result_to_file; but it not sure wether both operations are done, in addition they are not extremely time consuming, and they are not part of the computation time anyway *)
	
		let result_str = string_of_good_or_bad_constraint model.variable_names result in
		
		let text = 
		match result with
(* 			| Accepting_cycle_constraint _ -> "Final constraint such that there exists an accepting cycle" *)
			| Good_constraint _ -> "Final positive " ^ constraint_str ^ "" (*  (positive = guarantees the validity of the property) *)
			| Bad_constraint _  -> "Final negative " ^ constraint_str ^ ""  (*(negative = violates the property)*)
			| Good_bad_constraint _  -> "Final positive/negative " ^ constraint_str ^ " (in the form of a pair of constraints guaranteeing the validity/violation of the property)"
		in
		
		(* Print some information *)
		print_message Verbose_standard ("\n" ^ text ^ ":");
		print_highlighted_message Shell_result Verbose_standard (result_str);
	
(*			let soundness = match single_synthesis_result.result with
			| Single_constraint (_, soundness) -> soundness
			| Under_over_constraint _ -> Constraint_under_over
		in*)
		
		(* Give a comment on the validity of the result *)
		print_highlighted_message Shell_soundness Verbose_standard (verbose_string_soundness_of_good_or_bad_constraint result);
	
		print_message Verbose_low (
			"Computation time: "
			^ (string_of_seconds computation_time) ^ "."
		);

		(* Print memory information *)
		if options#statistics || verbose_mode_greater Verbose_experiments then(
			print_newline();
			print_message Verbose_standard (memory_used ());
		);
		
	);
	()


let process_single_synthesis_or_point_based_result file_prefix algorithm_name result state_space computation_time termination =
	(* Retrieve the input options *)
	let options = Input.get_options () in

	(* Print statistics *)
	print_state_space_statistics computation_time state_space;
	print_memory_statistics ();
	
	print_message Verbose_high "Drawing state space…";
	
	(* Draw state space *)
	let radical = file_prefix ^ "-statespace" in
	Graphics.draw_statespace state_space algorithm_name radical;
	
	(* Render zones in a graphical form *)
	if options#cart then (
		let zones = zones_of_good_bad_constraint result in
		Graphics.draw_cartography zones (file_prefix ^ "_cart")
	) else (
			print_message Verbose_high "Graphical cartography not asked: not drawn.";
	);
	
	(* The end *)
	()


(************************************************************)
(* Main function to process IMITATOR result *)
(************************************************************)

(** Process the result of IMITATOR. The 3rd optional argument is the file name prefix (otherwise options#files_prefix is used). *)
let process_result result algorithm_name prefix_option =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	
	(* Define the file prefix for all outputs *)
	let file_prefix = match prefix_option with
		(* Use the user-defined prefix *)
		| Some prefix -> prefix
		(* Otherwise: by default use the model file prefix *)
		| None -> options#files_prefix
	in
	
	
	match result with
	| Syntax_check_result | Translation_result ->
		(* Write to file if requested *)
		if options#output_result then(
			let file_name = file_prefix ^ Constants.result_file_extension in
			export_to_file_noresult file_name;
		)else(
			print_message Verbose_high "No result export to file requested.";
		);
		
		(* Print statistics *)
		print_memory_statistics ();
		
		(* The end *)
		()


	| State_space_computation_result state_space_computation ->
		print_message Verbose_low (
			"Computation time: "
			^ (string_of_seconds state_space_computation.computation_time) ^ "."
		);

		(* Write to file if requested *)
		if options#output_result then(
			let file_name = file_prefix ^ Constants.result_file_extension in
			export_to_file_noresult file_name;
		)else(
			print_message Verbose_high "No result export to file requested.";
		);

		(* Print statistics *)
		print_state_space_statistics state_space_computation.computation_time state_space_computation.state_space;
		print_memory_statistics ();
		
		print_message Verbose_high "Drawing state space…";
	
		(* Draw state space *)
		let radical = file_prefix ^ "-statespace" in
		Graphics.draw_statespace state_space_computation.state_space algorithm_name radical;
		
		(* The end *)
		()


	| Deprecated_efsynth_result efsynth_result ->
		
		(* Print the result *)
		if verbose_mode_greater Verbose_standard then(
			(* Retrieve the model *)
			let model = Input.get_model() in
			
			(* Convert result to string *)
			(*** NOTE: this conversion to string is duplicate, since it will again be converted in export_to_file_efsynth_result_to_file; but it not sure wether both operations are done, in addition they are not extremely time consuming, and they are not part of the computation time anyway *)
			let result_str = string_of_list_of_string_with_sep "\n OR \n" (List.map (LinearConstraint.string_of_p_linear_constraint model.variable_names) efsynth_result.constraints) in
			
			print_message Verbose_standard ("\nFinal constraint such that the property is *violated* (" ^ (string_of_int (List.length efsynth_result.constraints)) ^ " constraint" ^ (s_of_int (List.length efsynth_result.constraints)) ^ "): ");
			print_highlighted_message Shell_result Verbose_standard (result_str);

			(* Give a comment on the validity of the result *)
			print_highlighted_message Shell_soundness Verbose_standard (verbose_string_of_soundness efsynth_result.soundness);
		);
		
		print_message Verbose_low (
			"Computation time: "
			^ (string_of_seconds efsynth_result.computation_time) ^ "."
		);

		(* Print memory information *)
		if options#statistics || verbose_mode_greater Verbose_experiments then(
			print_newline();
			print_message Verbose_standard (memory_used ());
		);
		
(*		(* Print on terminal *)
		print_message Verbose_standard (
			"\nEF-synthesis successfully finished " ^ (after_seconds ()) ^ "."
		);*)

		(* Write to file if requested *)
		if options#output_result then(
			let file_name = file_prefix ^ Constants.result_file_extension in
			export_to_file_deprecated_efsynth_result file_name efsynth_result;
		)else(
			print_message Verbose_high "No result export to file requested.";
		);
		
		(* Print statistics *)
		print_state_space_statistics efsynth_result.computation_time efsynth_result.state_space;
		print_memory_statistics ();
		
		print_message Verbose_high "Drawing state space…";
	
		(* Draw state space *)
		let radical = file_prefix ^ "-statespace" in
		Graphics.draw_statespace efsynth_result.state_space algorithm_name radical;
		
		(* Render zones in a graphical form *)
		if options#cart then (
			let zones = List.map (fun p_linear_constraint -> (LinearConstraint.Convex_p_constraint p_linear_constraint, StateSpace.Bad)) efsynth_result.constraints in
			Graphics.draw_cartography zones (file_prefix ^ "_cart")
		) else (
			print_message Verbose_high "Graphical cartography not asked: not drawn.";
		);
		
		(* The end *)
		()


	| Single_synthesis_result result ->
		(* First print the result on the terminal *)
		print_single_synthesis_or_point_based_result result.result result.computation_time result.constraint_description;

		(* Write to file if requested *)
		if options#output_result then(
			let file_name = file_prefix ^ Constants.result_file_extension in
			export_to_file_single_synthesis_result file_name result;
		)else(
			print_message Verbose_high "No result export to file requested.";
		);
		
		(* Generic handling for drawing etc. *)
		process_single_synthesis_or_point_based_result file_prefix algorithm_name result.result result.state_space result.computation_time result.termination
		

	| Point_based_result result ->
		(* First print the result on the terminal *)
		print_single_synthesis_or_point_based_result result.result result.computation_time "constraint";

		(* Write to file if requested *)
		if options#output_result then(
			let file_name = file_prefix ^ Constants.result_file_extension in
			export_to_file_point_based_result file_name result;
		)else(
			print_message Verbose_high "No result export to file requested.";
		);
		
		(* Generic handling for drawing etc. *)
		process_single_synthesis_or_point_based_result file_prefix algorithm_name result.result result.state_space result.computation_time result.termination


	| Cartography_result cartography_result ->
		(* Print some information *)
		if verbose_mode_greater Verbose_standard then(
			print_message Verbose_standard ("\n**************************************************");
			print_message Verbose_standard (" END OF THE BEHAVIORAL CARTOGRAPHY ALGORITHM");
			print_message Verbose_standard ("" ^ general_bc_statistics cartography_result);
			print_message Verbose_standard ("**************************************************");
		
			print_highlighted_message Shell_soundness Verbose_standard ("\n" ^ (verbose_string_of_coverage cartography_result.coverage));
		);
		
		(* Print memory information *)
		if options#statistics || verbose_mode_greater Verbose_experiments then(
			print_newline();
			print_message Verbose_standard (memory_used ());
		);
		
		(* Write to file if requested for BC *)
		if options#output_bc_result then(
			let file_name = file_prefix ^ Constants.result_file_extension in
			export_to_file_cartography_result file_name cartography_result;
		)else(
			print_message Verbose_high "No result export to file requested.";
		);
		
		(* If cartography required for BC *)
		if options#output_bc_cart then (
			(* Print some information *)
			print_message Verbose_high "Graphical cartography asked: prepare tiles to be drawn…";

			(* Keep only valid tiles, i.e., underapproximations or exact *)
			let valid_tiles = cartography_result.tiles (*** TODO ***)(* List.filter (fun (abstract_point_based_result : abstract_point_based_result) -> abstract_point_based_result.soundness = Result.Constraint_maybe_under || abstract_point_based_result.soundness = Result.Constraint_exact) cartography_result.tiles*) in
			
			(* Render zones in a graphical form *)
			let zones = List.fold_left (fun computed_zones (abstract_point_based_result : abstract_point_based_result) ->
				(* Compute the 1 or 2 zones for this tile *)
				let zones_for_this_result = zones_of_good_bad_constraint abstract_point_based_result.result in
				(* Add it to the previously computed zones *)
				list_append computed_zones zones_for_this_result
			) [] valid_tiles
			in
			
			Graphics.draw_cartography zones (file_prefix ^ "_cart")
		) else (
			(* Print some information *)
			print_message Verbose_high "Graphical cartography not asked: not drawn.";
		);

		(* The end *)
		()
	
	
	(* Multiple synthesis (e.g., PRPC) *)
	| Multiple_synthesis_result result ->
		(* First print the result on the terminal *)
		print_single_synthesis_or_point_based_result result.result result.computation_time "constraint";

		(* Write to file if requested *)
		if options#output_bc_result then(
			let file_name = file_prefix ^ Constants.result_file_extension in
			export_to_file_multiple_synthesis_result file_name result;
			()
		)else(
			print_message Verbose_high "No result export to file requested.";
		);
		
		(* Print statistics *)
		print_memory_statistics ();

		(* Render zones in a graphical form *)
		if options#output_bc_cart then (
			let zones = zones_of_good_bad_constraint result.result in
			Graphics.draw_cartography zones (file_prefix ^ "_cart")
		) else (
			print_message Verbose_high "Graphical cartography not asked: not drawn.";
		);
		
		(* The end *)
		()


	(* Nothing to do for workers in distributed mode *)
	| Distributed_worker_result ->
		()

		
	(* Result for runs exhibition *)
	| Runs_exhibition_result result ->
		(* Write to file *)
		let file_name = file_prefix ^ Constants.result_file_extension in
		export_to_file_runs_exhibition_result file_name result;
		
		(* Print statistics *)
		print_memory_statistics ();

		(* Render signals and sets of parameters in a graphical form *)
		List.iteri (fun index valuation_and_concrete_run ->
			(* iteri starts counting from 0, but we like starting counting from 1 *)
			let index_from_one = index + 1 in
			
			(* Create prefix *)
			let prefix = options#files_prefix ^ "_ex_" ^ (string_of_int index_from_one) ^ "_" ^ (match valuation_and_concrete_run.concrete_run with Concrete_run _ -> "pos" | Impossible_concrete_run _ -> "neg") in

			(* Print signal *)
			begin
			match valuation_and_concrete_run.concrete_run with
				| Concrete_run concrete_run -> Graphics.draw_concrete_run concrete_run prefix
				| Impossible_concrete_run impossible_concrete_run -> Graphics.draw_impossible_concrete_run impossible_concrete_run prefix
			end;

			(* Print parameter zone *)
			if options#cart then (
				print_message Verbose_low "Plotting cartography of the runs' constraints…";
				(* Generate the graphics: parameters *)
				let zones = [valuation_and_concrete_run.valuations, match valuation_and_concrete_run.concrete_run with Concrete_run _ -> StateSpace.Good | Impossible_concrete_run _ -> StateSpace.Bad] in
				Graphics.draw_cartography zones prefix;
			) else (
				print_message Verbose_high "Graphical cartography not asked: not drawn.";
			);
		
		) result.runs;
		
		(* The end *)
		()




(* 	| _ -> raise (NotImplemented ("function process_result not implemented for all cases")) *)
