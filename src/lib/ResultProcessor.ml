(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: process the result of IMITATOR: print results, creates files, generates graphics, etc.
 * 
 * File contributors : Étienne André
 * Created           : 2015/12/03
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
(* Statistics *)
(************************************************************)

(* Create counter *)
let counter = Statistics.create_time_counter_and_register "file generation" Graphics_counter Verbose_low



(************************************************************)
(** Conversion of a good_bad_constraint into a list of (1 or 2) zones *)
(************************************************************)
let zones_of_good_bad_constraint = function
	(* Only good valuations: convert to GOOD convex polyhedron *)
	| Good_constraint (p_nnconvex_constraint, _) ->
		[LinearConstraint.Nonconvex_p_constraint p_nnconvex_constraint, Graphics.Good]
		
	(* Only bad valuations: convert to BAD convex polyhedron *)
	| Bad_constraint (p_nnconvex_constraint, _) ->
		[LinearConstraint.Nonconvex_p_constraint p_nnconvex_constraint, Graphics.Bad]

	(* Both good and bad valuations *)
	| Good_bad_constraint good_and_bad_constraint ->
		let good_p_nnconvex_constraint, _ = good_and_bad_constraint.good in
		let bad_p_nnconvex_constraint, _ = good_and_bad_constraint.bad in
		[
			LinearConstraint.Nonconvex_p_constraint good_p_nnconvex_constraint, Graphics.Good;
			LinearConstraint.Nonconvex_p_constraint bad_p_nnconvex_constraint, Graphics.Bad;
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

	

let string_of_state_based_algorithm_termination (state_based_algorithm_termination : Result.state_based_algorithm_termination) =
	let string_of_unexplored_successors (unexplored_successors : Result.unexplored_successors) =
		match unexplored_successors with
		| Unknown_number -> "some successor(s) unexplored"
		| Number nb_unexplored_successors -> (string_of_int nb_unexplored_successors) ^ " successor" ^ (s_of_int nb_unexplored_successors) ^ " unexplored"
	in
	match state_based_algorithm_termination with
	(* Fixpoint-like termination *)
	| Regular_termination -> "regular termination"
	(* Termination due to time limit reached *)
	| Time_limit unexplored_successors -> "time limit (" ^ (string_of_unexplored_successors unexplored_successors) ^ ")"
	(* Termination due to state space depth limit reached *)
	| Depth_limit unexplored_successors -> "depth limit (" ^ (string_of_unexplored_successors unexplored_successors) ^ ")"
	(* Termination due to a number of explored states reached *)
	| States_limit unexplored_successors -> "states limit (" ^ (string_of_unexplored_successors unexplored_successors) ^ ")"
	(* Termination due to a target state found *)
	(*** NOTE/HACK: the number of unexplored states is not known, therefore we do not add it… ***)
	| Witness_found -> "terminated after reaching a target state (some states may have been unexplored)"


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

(* let verbose_string_of_soundness s = "This constraint " ^ (verbose_string_of_soundness_suffix s) *)


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


(** Convert a Result.good_or_bad_constraint into a string for printing the constraint nature *)
let string_constraint_nature_of_good_or_bad_constraint = function
	(* Only good valuations *)
	| Good_constraint _ -> "good"
	(* Only bad valuations *)
	| Bad_constraint _ -> "bad"
	(* Both good and bad valuations *)
	| Good_bad_constraint _ -> "good" ^ "/" ^ "bad"


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
	^ constraint_str
	(* end delimiter *)
	^ "\nEND CONSTRAINT\n"

(** Add standardised delimiters to results (e.g. runs) *)
let add_result_delimiters constraint_str =
	(* begin delimiter *)
	"\n\nBEGIN RESULT\n"
	^ constraint_str
	(* end delimiter *)
	^ "\nEND RESULT\n"

(** Add standardised delimiters to results (e.g. runs) *)
let add_custom_details_delimiters str =
	(* begin delimiter *)
	"\n\nBEGIN DETAILS\n"
	^ str
	(* end delimiter *)
	^ "\nEND DETAILS\n"

(* Add custom details only if verbose mode >= low *)
let add_custom_details _ =
    if ImitatorUtilities.verbose_mode_greater ImitatorUtilities.Verbose_low then
        add_custom_details_delimiters (Logger.json_string_of_details ())
    else
        ""


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** Print warning(s) if the limit of an exploration has been reached *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
let print_warnings_of_termination_status (termination_status : Result.state_based_algorithm_termination) : unit =
	let although_explore_of_unexplored_successors = function
	| Unknown_number -> "although there were still some state(s) to explore."
	| Number nb_unexplored_successors -> "although there " ^ (waswere_of_int nb_unexplored_successors) ^ " still " ^ (string_of_int nb_unexplored_successors) ^ " state" ^ (s_of_int nb_unexplored_successors) ^ " to explore."
	in
	match termination_status with
		| Result.Regular_termination -> ()

		| (Result.Depth_limit unexplored_successors) -> print_warning (
			"The maximum depth to explore has been reached. The exploration now stops, " ^ (although_explore_of_unexplored_successors unexplored_successors)
		)

		| (Result.States_limit unexplored_successors) -> print_warning (
			"The maximum number of states to explore has been reached. The exploration now stops, " ^ (although_explore_of_unexplored_successors unexplored_successors)
		)

		| (Result.Time_limit unexplored_successors) -> print_warning (
			"The maximum execution time for " ^ (Constants.program_name) ^ " has been reached. The exploration now stops, " ^ (although_explore_of_unexplored_successors unexplored_successors)
		)

		| (Result.Witness_found) -> print_warning (
			"A target state has been found. The exploration now stops, although there are still some unexplored states."
		)


(************************************************************)
(* I/O functions *)
(************************************************************)
(** Header for result files *)
let file_header () =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	    "(************************************************************" 
	(* Program version *)
	^ "\n * Result by: " ^ (ImitatorUtilities.program_name_and_version_and_nickname_and_build)
	^ "\n * Model    : '" ^ options#model_file_name ^ "'"
	(* Date *)
	^ "\n * Generated: " ^ (now()) ^ ""
	(* Command *)
	^ "\n * Command  : " ^ (string_of_array_of_string_with_sep " " Sys.argv)
	^ "\n ************************************************************)\n\n"

(** Return a string made of some information concerning the input model *)
let model_statistics (model : AbstractModel.abstract_model) =
	(* Create the statistics *)
	    "Number of IPTAs                         : " ^ (string_of_int model.nb_automata)
	^ "\nNumber of clocks                        : " ^ (string_of_int model.nb_clocks)
	^ "\nHas invariants?                         : " ^ (string_of_bool model.has_invariants)
	^ "\nHas clocks with rate <>1?               : " ^ (string_of_bool model.has_non_1rate_clocks)
	^ "\nHas complex updates?                    : " ^ (string_of_bool model.has_complex_updates)
	^ "\nL/U subclass                            : " ^ (string_of_lu_status model.lu_status)
	^ "\nBounded parameters?                     : " ^ (string_of_bool model.bounded_parameters)
	^ "\nHas silent actions?                     : " ^ (string_of_bool model.has_silent_actions)
	^ "\nIs strongly deterministic?              : " ^ (string_of_bool model.strongly_deterministic)
	^ "\nNumber of parameters                    : " ^ (string_of_int model.nb_parameters)
	^ "\nNumber of discrete variables            : " ^ (string_of_int model.nb_discrete)
	^ "\nNumber of actions                       : " ^ (string_of_int model.nb_actions)
	^ "\nTotal number of locations               : " ^ (string_of_int model.nb_locations)
	^ "\nAverage locations per IPTA              : " ^ (round1_float ((float_of_int model.nb_locations) /. (float_of_int model.nb_automata)))
	^ "\nTotal number of transitions             : " ^ (string_of_int model.nb_transitions)
	^ "\nAverage transitions per IPTA            : " ^ (round1_float ((float_of_int model.nb_transitions) /. (float_of_int model.nb_automata)))


let property_information (property : AbstractProperty.abstract_property) (algorithm_name : string) : string =
	(* Create the statistics *)
	    "Property                                : " ^ (AlgorithmOptions.text_of_property property)
	^ "\nAlgorithm                               : " ^ algorithm_name


(* Return a string made of some statistics for the state space *)
let statespace_statistics (state_space : StateSpace.stateSpace) total_time : string =
	(* Speed: number of states computed and still in the state space *)
	let nb_states = state_space#nb_states in
	let states_per_second = (float_of_int nb_states) /. total_time in
	
	(* Speed: number of states computed, even if not kept (because merged, deleted…) *)
	let nb_gen_states = state_space#get_nb_gen_states in
	let gen_states_per_second = (float_of_int nb_gen_states) /. total_time in
	
	    "Number of states                        : " ^ (string_of_int nb_states)
	^ "\nNumber of transitions                   : " ^ (string_of_int (state_space#nb_transitions))
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
let result_nature_statistics (soundness_str : string) termination (constraint_nature_str : string) =
	    "Constraint soundness                    : " ^ soundness_str
	^ "\nTermination                             : " ^ (string_of_state_based_algorithm_termination termination)
	^ "\nConstraint nature                       : " ^ constraint_nature_str

(* Return a string made of some information concerning the result (for multiple_synthesis_result) *)
let result_nature_statistics_bc (soundness_str : string) termination (constraint_nature_str : string) =
	    "Constraint soundness                    : " ^ soundness_str
	^ "\nTermination                             : " ^ (string_of_bc_algorithm_termination termination)
	^ "\nConstraint nature                       : " ^ constraint_nature_str


(*** TODO: would be smarter to have a generic function export_to_file_result : imitator_result -> unit () ***)

(* Write an `error_result` result to the result file *)
let export_to_file_errorresult error_type file_name =
	(* Start counter *)
	counter#start;
	
	(* Error message *)
	let error_message = match error_type with
		| Division_by_zero msg			-> "division by 0 (" ^ msg ^ ")"
		| Out_of_range                  -> "index out of range"
        | Empty_collection          -> "collection empty"
		| ModelFileNotFound_error		-> "model file not found"
		| PropertyFileNotFound_error	-> "property file not found"
		| InvalidModel_error			-> "invalid model"
		| InvalidProperty_error			-> "invalid property"
		| Lexing_error msg				-> "lexing error (" ^ msg ^ ")"
		| ModelParsing_error msg		-> "model parsing error (" ^ msg ^ ")"
		| PropertyParsing_error msg		-> "property parsing error (" ^ msg ^ ")"
		| Unsatisfiable_initial_conditions	-> "unsatisfiable initial conditions"
	in

	(* Prepare the string to write *)
	let file_content =

		(* 1) Header *)
		file_header ()
		
		(* 2) Error message *)
		^ "\n------------------------------------------------------------"
		^ "\nError                                   : " ^ error_message
		^ "\n------------------------------------------------------------"

		(* 3) General statistics *)
		^ "\n" ^ (Statistics.string_of_all_counters())
		^ "\n------------------------------------------------------------"

        (*  4) More info about the model *)
        ^ add_custom_details ()
	in
	
	(* Write to file *)
	write_to_file file_name file_content;
	print_message Verbose_standard ("\nResult written to file `" ^ file_name ^ "`.");
	
	(* Stop counter *)
	counter#stop;
	
	(* The end *)
	()



(* Write an `error_result` result to the result file *)
let export_to_file_unsatisfiableinitialstate file_name =
	(* Start counter *)
	counter#start;

	(* Prepare the string to write *)
	let file_content =

		(* 1) Header *)
		file_header ()

		(* 2) Error message *)
		^ "\n------------------------------------------------------------"
		^ "\nUnsatisfiable initial state"
		^ "\n------------------------------------------------------------"

		(* 3) General statistics *)
		^ "\n" ^ (Statistics.string_of_all_counters())
		^ "\n------------------------------------------------------------"

        (*  4) More info about the model *)
        ^ add_custom_details ()
	in

	(* Write to file *)
	write_to_file file_name file_content;
	print_message Verbose_standard ("\nResult written to file `" ^ file_name ^ "`.");

	(* Stop counter *)
	counter#stop;

	(* The end *)
	()



(* Write a `no_result` result to the result file *)
let export_to_file_noresult (model : AbstractModel.abstract_model) file_name =
	(* Start counter *)
	counter#start;
	
	(* Prepare the string to write *)
	let file_content =

		(* 1) Header *)
		file_header ()

		(* 2) Statistics about model *)
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (model_statistics model)
		^ "\n------------------------------------------------------------"

		(* 3) General statistics *)
		^ "\n" ^ (Statistics.string_of_all_counters())
		^ "\n------------------------------------------------------------"

        (*  4) More info about the model *)
        ^ add_custom_details ()

	in
	
	(* Write to file *)
	write_to_file file_name file_content;
	print_message Verbose_standard ("\nResult written to file `" ^ file_name ^ "`.");
	
	(* Stop counter *)
	counter#stop;
	
	(* The end *)
	()



(* Write a single_synthesis_result to the result file *)
let export_to_file_single_synthesis_result (model : AbstractModel.abstract_model) (property : AbstractProperty.abstract_property) (algorithm_name : string) (file_name : string) (single_synthesis_result : Result.single_synthesis_result) : unit =
	(* Start counter *)
	counter#start;

	(* Convert the resulting constraint to a string *)
	let result_str : string = string_of_good_or_bad_constraint model.variable_names single_synthesis_result.result in

	(* Handle the soundness separately *)
	let soundness_str : string = string_soundness_of_good_or_bad_constraint single_synthesis_result.result in 

	(* Handle the constraint nature separately *)
	let constraint_nature_str = string_constraint_nature_of_good_or_bad_constraint single_synthesis_result.result in

	(* Prepare the string to write *)
	let file_content =
		(* 1) Header *)
		file_header ()
		
		(* 2) Statistics about model *)
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (model_statistics model)
		^ "\n------------------------------------------------------------"

		(* 3) Property *)
		^ "\n\n------------------------------------------------------------"
		^ "\n" ^ (property_information property algorithm_name)
		^ "\n------------------------------------------------------------"

		(* 4) The actual result with delimiters *)
		^ (add_constraints_delimiters result_str)

		(* 5) Statistics about result *)
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (result_nature_statistics soundness_str single_synthesis_result.termination constraint_nature_str)
		
		(* 6) Statistics about state space *)
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (statespace_statistics single_synthesis_result.state_space single_synthesis_result.computation_time)
		^ "\n------------------------------------------------------------"
		
		(* 7) General statistics *)
		^ "\n" ^ (Statistics.string_of_all_counters())
		^ "\n------------------------------------------------------------"
	in
	
	(* Write to file *)
	write_to_file file_name file_content;
	print_message Verbose_standard ("\nResult written to file `" ^ file_name ^ "`.");
	
	(* Stop counter *)
	counter#stop;
	
	(* The end *)
	()


(* Write a multiple_synthesis_result to the result file *)
let export_to_file_multiple_synthesis_result (model : AbstractModel.abstract_model) file_name (multiple_synthesis_result : Result.multiple_synthesis_result) =
	(* Start counter *)
	counter#start;

	(* Convert the resulting constraint to a string *)
	let result_str : string = string_of_good_or_bad_constraint model.variable_names multiple_synthesis_result.result in

	(* Handle the soundness separately *)
	let soundness_str : string = string_soundness_of_good_or_bad_constraint multiple_synthesis_result.result in 

	(* Handle the constraint nature separately *)
	let constraint_nature_str = string_constraint_nature_of_good_or_bad_constraint multiple_synthesis_result.result in

	(* Prepare the string to write *)
	let file_content =
		(* 1) Header *)
		file_header ()
		
		(* 2) Statistics about model *)
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (model_statistics model)
		^ "\n------------------------------------------------------------"

		(* 3) The actual result with delimiters *)
		^ (add_constraints_delimiters result_str)
		
		(* 4) Statistics about result *)
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (result_nature_statistics_bc soundness_str multiple_synthesis_result.termination constraint_nature_str)
		
		(* 5) General statistics *)
		^ "\n" ^ (Statistics.string_of_all_counters())
		^ "\n------------------------------------------------------------"
	in
	
	(* Write to file *)
	write_to_file file_name file_content;
	print_message Verbose_standard ("\nResult written to file `" ^ file_name ^ "`.");
	
	(* Stop counter *)
	counter#stop;
	
	(* The end *)
	()



(* Write an ef_synth result to the result file *)
let export_to_file_point_based_result (model : AbstractModel.abstract_model) file_name (point_based_result : Result.point_based_result) =
	(* Start counter *)
	counter#start;

	(* Convert the constraint to a string *)
	let result_str = string_of_good_or_bad_constraint model.variable_names point_based_result.result in

	(* Handle the soundness separately *)
	let soundness_str = string_soundness_of_good_or_bad_constraint point_based_result.result in 
	
	(* Handle the constraint nature separately *)
	let constraint_nature_str = string_constraint_nature_of_good_or_bad_constraint point_based_result.result in

	(* Prepare the string to write *)
	let file_content =
		let pi0 = point_based_result.reference_val in
		
		(* 1) Header *)
		file_header ()

		(* 2) Statistics about model *)
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (model_statistics model)
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
		^ "\n" ^ (result_nature_statistics soundness_str point_based_result.termination constraint_nature_str)
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
	print_message Verbose_standard ("\nResult written to file `" ^ file_name ^ "`.");
	
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
let export_to_file_cartography_result (model : AbstractModel.abstract_model) file_name (cartography_result : Result.cartography_result) =
	(* Start counter *)
	counter#start;

	(* Convert the abstract_point_based_result's to string *)
	let abstract_point_based_results_str = string_of_list_of_string_with_sep "\n" (
		List.mapi (fun index (abstract_point_based_result : abstract_point_based_result) ->
		
			(* Handle the soundness separately *)
			let soundness_str : string = string_soundness_of_good_or_bad_constraint abstract_point_based_result.result in 
			
			(* Handle the constraint nature separately *)
			let constraint_nature_str = string_constraint_nature_of_good_or_bad_constraint abstract_point_based_result.result in


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
			^ "\n" ^ (result_nature_statistics soundness_str abstract_point_based_result.termination constraint_nature_str)
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
	
		let v0 = cartography_result.parameter_domain in

		(* 1) The file header *)
		file_header ()
		
		(* 2) Statistics about model *)
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (model_statistics model)
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

	in
	(* Write to file *)
	write_to_file file_name file_content;
	print_message Verbose_standard ("\nResult written to file `" ^ file_name ^ "`.");
	
	(* Stop counter *)
	counter#stop;
	
	(* The end *)
	()


(** Export result of type 'Runs_exhibition_result' *)
let export_to_file_runs_exhibition_result (model : AbstractModel.abstract_model) file_name (result : Result.runs_exhibition_result) =
	(* Start counter *)
	counter#start;

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
(* 			^ "\n\n Run:" *)
			^ "\n" ^ (let str = match run with
				| Concrete_run concrete_run -> ModelPrinter.json_of_concrete_run model concrete_run
				| Impossible_concrete_run impossible_concrete_run -> ModelPrinter.json_of_impossible_concrete_run model impossible_concrete_run
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
		^ "\n" ^ (model_statistics model)
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
	print_message Verbose_standard ("\nResult written to file `" ^ file_name ^ "`.");
	
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
		print_message Verbose_standard (state_space#get_statistics_states);
		
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
let print_single_synthesis_or_point_based_result (model : AbstractModel.abstract_model) result computation_time constraint_str =
	(* Print the result *)
	if verbose_mode_greater Verbose_standard then(
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




let process_single_synthesis_or_point_based_result (model : AbstractModel.abstract_model) (property_option : AbstractProperty.abstract_property option) file_prefix (algorithm_name : string) result state_space computation_time =
	(* Retrieve the input options *)
	let options = Input.get_options () in

	(* Print statistics *)
	print_state_space_statistics computation_time state_space;
	print_memory_statistics ();
	
	print_message Verbose_high "Drawing state space…";
	(* Draw state space *)
	let radical = file_prefix ^ "-statespace" in
	Graphics.draw_statespace_if_requested model property_option state_space algorithm_name radical;
	
	(* Render zones in a graphical form *)
	if options#draw_cart then (
		let zones = zones_of_good_bad_constraint result in
		Graphics.draw_cartography model property_option zones (file_prefix ^ Constants.cart_file_suffix)
	) else (
			print_message Verbose_high "Graphical cartography not asked: not drawn.";
	);
	
	(* The end *)
	()


(************************************************************)
(* Main function to process IMITATOR result *)
(************************************************************)

(** Process the result of IMITATOR. The 5th optional argument is the file name prefix (otherwise options#files_prefix is used). *)
let process_result_generic (model_option : AbstractModel.abstract_model option) (property_option : AbstractProperty.abstract_property option) result algorithm_name prefix_option =
	(* Retrieve the input options *)
	let options = Input.get_options () in

	(* Useful to get the model ONLY in cases when it is defined *)
	let get_model_from_model_option = function
		| Some model -> model
		| None -> raise (InternalError "The model should have been defined in process_result_generic")
	in

	(* Useful to get the property ONLY in cases when it is defined *)
	let get_property_from_property_option = function
		| Some property -> property
		| None -> raise (InternalError "The property should have been defined in process_result_generic")
	in

	(* Define the file prefix for all outputs *)
	let file_prefix = match prefix_option with
		(* Use the user-defined prefix *)
		| Some prefix -> prefix
		(* Otherwise: by default use the model file prefix *)
		| None -> options#files_prefix
	in
	
	let draw_statespace_if_requested (model : AbstractModel.abstract_model) (property_option : AbstractProperty.abstract_property option) state_space =
		print_message Verbose_high "Drawing state space…";
		(* Draw state space *)
		let radical = file_prefix ^ "-statespace" in
		Graphics.draw_statespace_if_requested model property_option state_space algorithm_name radical
	in
		
	
	match result with
	| Error_result error_type ->
		(* Write to file if requested *)
		if options#output_result then(
			let file_name = file_prefix ^ Constants.result_file_extension in
			export_to_file_errorresult error_type file_name;
		)else(
			print_message Verbose_high "No result export to file requested.";
		);
		
		(* Print statistics *)
		print_memory_statistics ();
		
		(* The end *)
		()


	| Unsatisfiable_initial_state ->
		(* Write to file if requested *)
		if options#output_result then(
			let file_name = file_prefix ^ Constants.result_file_extension in
			export_to_file_unsatisfiableinitialstate file_name;
		)else(
			print_message Verbose_high "No result export to file requested.";
		);

		(* Print statistics *)
		print_memory_statistics ();

		(* The end *)
		()


	| Syntax_check_result | Translation_result ->
		(* The model must be defined at this point *)
		let model : AbstractModel.abstract_model = get_model_from_model_option model_option in

		(* Write to file if requested *)
		if options#output_result then(
			let file_name = file_prefix ^ Constants.result_file_extension in
			export_to_file_noresult model file_name;
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

		(* The model must be defined at this point *)
		let model : AbstractModel.abstract_model = get_model_from_model_option model_option in

		(* Write to file if requested *)
		if options#output_result then(
			let file_name = file_prefix ^ Constants.result_file_extension in
			export_to_file_noresult model file_name;
		)else(
			print_message Verbose_high "No result export to file requested.";
		);

		(* Print statistics *)
		print_state_space_statistics state_space_computation.computation_time state_space_computation.state_space;
		print_memory_statistics ();
		
		(* The model must be defined at this point *)
		let model : AbstractModel.abstract_model = get_model_from_model_option model_option in

		(* Draw state space *)
		draw_statespace_if_requested model property_option state_space_computation.state_space;
		
		(* The end *)
		()


	| Single_synthesis_result result ->
		(* The model must be defined at this point *)
		let model : AbstractModel.abstract_model = get_model_from_model_option model_option in
		let property : AbstractProperty.abstract_property = get_property_from_property_option property_option in

		(* First print the result on the terminal *)
		print_single_synthesis_or_point_based_result model result.result result.computation_time result.constraint_description;

		(* Write to file if requested *)
		if options#output_result then(
			let file_name = file_prefix ^ Constants.result_file_extension in
			export_to_file_single_synthesis_result model property algorithm_name file_name result;
		)else(
			print_message Verbose_high "No result export to file requested.";
		);
		
		(* Generic handling for drawing etc. *)
		process_single_synthesis_or_point_based_result model property_option file_prefix algorithm_name result.result result.state_space result.computation_time
		

	| Point_based_result result ->
		(* The model must be defined at this point *)
		let model : AbstractModel.abstract_model = get_model_from_model_option model_option in

		(* First print the result on the terminal *)
		print_single_synthesis_or_point_based_result model result.result result.computation_time "constraint";

		(* Write to file if requested *)
		if options#output_result then(
			let file_name = file_prefix ^ Constants.result_file_extension in
			export_to_file_point_based_result model file_name result;
		)else(
			print_message Verbose_high "No result export to file requested.";
		);
		
		(* Generic handling for drawing etc. *)
		process_single_synthesis_or_point_based_result model property_option file_prefix algorithm_name result.result result.state_space result.computation_time


	| Cartography_result cartography_result ->
		(* Print some information *)
		if verbose_mode_greater Verbose_standard then(
			print_message Verbose_standard ("\n**************************************************");
			print_message Verbose_standard (" END OF THE BEHAVIORAL CARTOGRAPHY ALGORITHM");
			print_message Verbose_standard ("" ^ general_bc_statistics cartography_result);
			print_message Verbose_standard ("**************************************************");
		
			print_highlighted_message Shell_soundness Verbose_standard ("\n" ^ (verbose_string_of_coverage cartography_result.coverage));
		);
		
		(* The model must be defined at this point *)
		let model : AbstractModel.abstract_model = get_model_from_model_option model_option in

		(* Print memory information *)
		if options#statistics || verbose_mode_greater Verbose_experiments then(
			print_newline();
			print_message Verbose_standard (memory_used ());
		);
		
		(* Write to file if requested for BC *)
		if options#output_bc_result then(
			let file_name = file_prefix ^ Constants.result_file_extension in
			export_to_file_cartography_result model file_name cartography_result;
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

			Graphics.draw_cartography model property_option zones (file_prefix ^ Constants.cart_file_suffix)
		) else (
			(* Print some information *)
			print_message Verbose_high "Graphical cartography not asked: not drawn.";
		);

		(* The end *)
		()
	
	
	(* Multiple synthesis (e.g., PRPC) *)
	| Multiple_synthesis_result result ->
		(* The model must be defined at this point *)
		let model : AbstractModel.abstract_model = get_model_from_model_option model_option in

		(* First print the result on the terminal *)
		print_single_synthesis_or_point_based_result model result.result result.computation_time "constraint";

		(* Write to file if requested *)
		if options#output_bc_result then(
			let file_name = file_prefix ^ Constants.result_file_extension in
			export_to_file_multiple_synthesis_result model file_name result;
			()
		)else(
			print_message Verbose_high "No result export to file requested.";
		);
		
		(* Print statistics *)
		print_memory_statistics ();

		(* Render zones in a graphical form *)
		if options#output_bc_cart then (
			let zones = zones_of_good_bad_constraint result.result in

			Graphics.draw_cartography model property_option zones (file_prefix ^ Constants.cart_file_suffix)
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
		(* The model must be defined at this point *)
		let model : AbstractModel.abstract_model = get_model_from_model_option model_option in

		(* Write to file *)
		let file_name = file_prefix ^ Constants.result_file_extension in
		export_to_file_runs_exhibition_result model file_name result;
		
		(* Print statistics *)
		print_memory_statistics ();

		(* Draw state space *)
		draw_statespace_if_requested model property_option result.state_space;
		
		(* Render signals and sets of parameters in a graphical form *)
		List.iteri (fun index valuation_and_concrete_run ->
			(* iteri starts counting from 0, but we like starting counting from 1 *)
			let index_from_one = index + 1 in
			
			(* Create prefix *)
			let prefix = options#files_prefix ^ "_ex_" ^ (string_of_int index_from_one) ^ "_" ^ (match valuation_and_concrete_run.concrete_run with Concrete_run _ -> "pos" | Impossible_concrete_run _ -> "neg") in

			(* Print signal *)
			begin
			match valuation_and_concrete_run.concrete_run with
				| Concrete_run concrete_run -> Graphics.draw_concrete_run model concrete_run prefix
				| Impossible_concrete_run impossible_concrete_run -> Graphics.draw_impossible_concrete_run model impossible_concrete_run prefix
			end;

			(* Print parameter zone *)
			if options#draw_cart then (
				print_message Verbose_low "Plotting cartography of the runs' constraints…";
				(* Generate the graphics: parameters *)
				let zones = [valuation_and_concrete_run.valuations, match valuation_and_concrete_run.concrete_run with Concrete_run _ -> Graphics.Good | Impossible_concrete_run _ -> Graphics.Bad] in

				Graphics.draw_cartography model property_option zones prefix;
			) else (
				print_message Verbose_high "Graphical cartography not asked: not drawn.";
			);
		
		) result.runs;
		
		(* The end *)
		()


(** Process_result can only be called with an actual defined model *)
let process_result (model : AbstractModel.abstract_model) =
	process_result_generic (Some model)



(* 	| _ -> raise (NotImplemented ("function process_result not implemented for all cases")) *)

(************************************************************)
(* Termination *)
(************************************************************)

(** Process the result of IMITATOR. The 4th optional argument is the file name prefix (otherwise options#files_prefix is used). Then terminate with success *)
let process_result_and_terminate (model : AbstractModel.abstract_model) (result : Result.imitator_result) (algorithm_name : string) prefix_option (global_counter : Statistics.timeCounter) =
	(* Process the result and create output file *)
	process_result_generic (Some model) None result algorithm_name prefix_option;

	(* Stop counter *)
	global_counter#stop;

	(* Only print counters if statistics are required, or experiments verbose mode *)
	if (try (Input.get_options())#statistics with _ -> false) || verbose_mode_greater Verbose_experiments then
		print_message Verbose_standard (string_of_all_counters());

	(* Kenavo! *)
	terminate_program()


(** Process the result of IMITATOR. The 3rd optional argument is the file name prefix (otherwise options#files_prefix is used). Then terminate with failure *)
let process_result_and_abort (error_type : Result.error_type) (algorithm_name : string) prefix_option (global_counter : Statistics.timeCounter) =
	(* Process the result and create output file *)
	process_result_generic None None (Error_result error_type) algorithm_name prefix_option;

	(* Stop counter *)
	global_counter#stop;

	(* Only print counters if statistics are required, or experiments verbose mode *)
	if (try (Input.get_options())#statistics with _ -> false) || verbose_mode_greater Verbose_experiments then
		print_message Verbose_standard (string_of_all_counters());

	(* Abort *)
	abort_program()

