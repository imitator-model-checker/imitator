(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: process the result of IMITATOR: print results, creates files, generates graphics, etc.
 * 
 * File contributors : Étienne André
 * Created           : 2015/12/03
 * Last modified     : 2016/06/08
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
(* I/O functions *)
(************************************************************)

(*** NOTE: this part should be modified with care, as external tools calling IMITATOR may use this syntax ***)


let string_of_bfs_algorithm_termination = function
	(* Fixpoint-like termination *)
	| Regular_termination -> "regular termination"
	(* Termination due to time limit reached *)
	| Time_limit nb_unexplored_successors -> "time limit (" ^ (string_of_int nb_unexplored_successors) ^ " successor" ^ (s_of_int nb_unexplored_successors) ^ " unexplored)"
	(* Termination due to state space depth limit reached *)
	| Depth_limit nb_unexplored_successors -> "depth limit (" ^ (string_of_int nb_unexplored_successors) ^ " successor" ^ (s_of_int nb_unexplored_successors) ^ " unexplored)"
	(* Termination due to a number of explored states reached *)
	| States_limit nb_unexplored_successors -> "states limit (" ^ (string_of_int nb_unexplored_successors) ^ " successor" ^ (s_of_int nb_unexplored_successors) ^ " unexplored)"


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
	| Constraint_under_over -> "both a possible under- and a possible over-approximation"
	(* Impossible to compare the constraint with the original result *)
	| Constraint_maybe_invalid -> "possibly invalid"


let verbose_string_of_soundness = function
	(* Constraint included in or equal to the real result *)
	| Constraint_maybe_under -> "This constraint is an under-approximation of the actual result (or the actual result itself)"
	(* Exact result *)
	| Constraint_exact -> "This constraint is exact (sound and complete)"
	(* Constraint equal to or larger than the real result *)
	| Constraint_maybe_over -> "This constraint is an over-approximation of the actual result (or the actual result itself)"
	(* Pair of constraints: one under-approximation and one over-approximation *)
	| Constraint_under_over -> "This constraint is both a possibly under-approximation and a possibly over-approximation (or the actual result itself)"
	(* Impossible to compare the constraint with the original result *)
	| Constraint_maybe_invalid -> "The validity of this constraint cannot be assessed"


let string_of_coverage = function
	(* Full coverage in all dimensions, including rational points *)
	| Coverage_full -> "full"

	(* At least all integers are covered, rationals perhaps not *)
	| Coverage_integer_complete -> "integer-complete"

	(* No indication of coverage *)
	| Coverage_unknown -> "unknown"


let verbose_string_of_coverage = function
	(* Full coverage in all dimensions, including rational points *)
	| Coverage_full -> "The entire input parameter domain has been covered by tiles."

	(* At least all integers are covered, rationals perhaps not *)
	| Coverage_integer_complete -> "All integer points of the input parameter domain have been covered by tiles; rational points may have been covered too (not evaluated)."

	(* No indication of coverage *)
	| Coverage_unknown -> "The coverage of the input parameter domain is probably incomplete and cannot be evaluated."


(* Header for result files *)
let file_header () =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	    "(************************************************************" 
	(* Program version *)
	^ "\n * Result output by " ^ Constants.program_name ^ ""
	^ "\n * Version  : " ^ (ImitatorUtilities.program_name_and_version_and_nickname_and_build())
	^ "\n * Git      : " ^ (ImitatorUtilities.git_branch_and_hash)
	^ "\n * Model    : '" ^ options#file ^ "'"
	(* Date *)
	^ "\n * Generated: " ^ (now()) ^ ""
	(* Command *)
	^ "\n * Command  : " ^ (string_of_array_of_string_with_sep " " Sys.argv)
	^ "\n ************************************************************)\n\n"

(* Return a string made of some information concerning the input model *)
let model_statistics () =
	(* Retrieve the model *)
	let model = Input.get_model() in
	(* Compute the number of locations *)
	let nb_total_locations = List.fold_left (fun current_sum automaton_index -> current_sum + (List.length (model.locations_per_automaton automaton_index))) 0 model.automata in
	(* Create the statistics *)
	    "Number of IPTAs                         : " ^ (string_of_int model.nb_automata)
	^ "\nNumber of clocks                        : " ^ (string_of_int model.nb_clocks)
	^ "\nHas stopwatches?                        : " ^ (string_of_bool model.has_stopwatches)
	^ "\nNumber of parameters                    : " ^ (string_of_int model.nb_parameters)
	^ "\nNumber of discrete variables            : " ^ (string_of_int model.nb_discrete)
	^ "\nNumber of actions                       : " ^ (string_of_int model.nb_actions)
	^ "\nTotal number of locations               : " ^ (string_of_int nb_total_locations)
	^ "\nAverage locations per IPTA              : " ^ (round1_float ((float_of_int nb_total_locations) /. (float_of_int model.nb_automata)))



(* Return a string made of some statistics for the state space *)
let statespace_statistics state_space total_time =
	(* Speed: number of states computed and still in the state space *)
	let nb_states = StateSpace.nb_states state_space in
	let states_per_second = (float_of_int nb_states) /. total_time in
	
	(* Speed: number of states computed, even if not kept (because merged, deleted...) *)
	let nb_gen_states = StateSpace.get_nb_gen_states state_space in
	let gen_states_per_second = (float_of_int nb_gen_states) /. total_time in
	
	    "Number of states                        : " ^ (string_of_int nb_states)
	^ "\nNumber of transitions                   : " ^ (string_of_int (StateSpace.nb_transitions state_space))
	^ "\nNumber of computed states               : " ^ (string_of_int nb_gen_states)
	^ "\nComputation time                        : " ^ (string_of_seconds total_time)
	^ "\nStates/second in state space            : " ^ (round1_float states_per_second) ^ " (" ^ (string_of_int nb_states) ^ "/" ^ (string_of_seconds total_time) ^ ")"
	^ "\nComputed states/second                  : " ^ (round1_float gen_states_per_second) ^ " (" ^ (string_of_int nb_gen_states) ^ "/" ^ (string_of_seconds total_time) ^ ")"
	^ "\nEstimated memory                        : " ^ (memory_used ())

	
(* Return a string made of some statistics for the abstract state space *)
let abstract_statespace_statistics abstract_state_space total_time =
	(* Speed: number of states computed and still in the state space *)
	let states_per_second = (float_of_int abstract_state_space.nb_states) /. total_time in
	
	    "Number of states                        : " ^ (string_of_int abstract_state_space.nb_states)
	^ "\nNumber of transitions                   : " ^ (string_of_int abstract_state_space.nb_transitions)
	^ "\nComputation time                        : " ^ (string_of_seconds total_time)
	^ "\nStates/second in state space            : " ^ (round1_float states_per_second) ^ " (" ^ (string_of_int abstract_state_space.nb_states) ^ "/" ^ (string_of_seconds total_time) ^ ")"


	
(* Return a string made of some information concerning the result *)
let result_nature_statistics soundness termination statespace_nature =
	    "Constraint soundness                    : " ^ (string_of_soundness soundness)
	^ "\nTermination                             : " ^ (string_of_bfs_algorithm_termination termination)
	^ "\nState space nature                      : " ^ (StateSpace.string_of_statespace_nature statespace_nature)



(*** TODO: would be smarter to have a generic function write_result_to_file : imitator_result -> unit () ***)

(* Write an ef_synth result to the result file *)
let write_efsynth_result_to_file file_name (efsynth_result : Result.efsynth_result) =
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
	let result_str = string_of_list_of_string_with_sep "\n OR \n" (List.map (LinearConstraint.string_of_p_linear_constraint model.variable_names) efsynth_result.constraints) in

	(* Prepare the string to write *)
	let file_content =
		(* 1) Header *)
		file_header ()
		
		(* 2) Statistics about model *)
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (model_statistics ())
		^ "\n------------------------------------------------------------"

		(* 3) The actual result *)
		(* begin delimiter *)
		^ "\n\nBEGIN CONSTRAINT\n"
		^ result_str ^ ""
		(* end delimiter *)
		^ "\nEND CONSTRAINT\n"
		
		(* 4) Statistics about result *)
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (result_nature_statistics efsynth_result.soundness efsynth_result.termination efsynth_result.statespace_nature)
		
		(* 5) Statistics about state space *)
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (statespace_statistics efsynth_result.state_space efsynth_result.computation_time)
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


(* Write a pdfc_result to the result file *)
let write_pdfc_result_to_file file_name (pdfc_result : Result.pdfc_result) =
	(*** WARNING: duplicate code concerning the counter creation ***)
	(* Create counter *)
	let counter = Statistics.create_time_counter_and_register "file generation" Graphics_counter Verbose_low in
	
	(* Start counter *)
	counter#start;

	(* Retrieve the model *)
	let model = Input.get_model() in
	(* Retrieve the input options *)
(* 	let options = Input.get_options () in *)
	let result_str, soundness = match pdfc_result.result with
		| Single_constraint (result, soundness) ->
			(
			(* Convert the constraint to a string *)
			let result_str = LinearConstraint.string_of_p_nnconvex_constraint model.variable_names result in

			(* begin delimiter *)
			"\n\nBEGIN CONSTRAINT\n"
			^ result_str ^ ""
			(* end delimiter *)
			^ "\nEND CONSTRAINT\n"
			)
			, soundness
			
		| Under_over_constraint (under, over) ->
			(
			(* Convert the constraints to a string *)
			let under_str = LinearConstraint.string_of_p_nnconvex_constraint model.variable_names under in
			let over_str = LinearConstraint.string_of_p_nnconvex_constraint model.variable_names over in

			(* begin delimiter *)
			"\n\nBEGIN UNDER CONSTRAINT\n"
			^ under_str ^ ""
			(* end delimiter *)
			^ "\nEND UNDER CONSTRAINT\n"
			(* begin delimiter *)
			^ "\n\nBEGIN OVER CONSTRAINT\n"
			^ over_str ^ ""
			(* end delimiter *)
			^ "\nEND OVER CONSTRAINT\n"
			)
			, Constraint_under_over
	in

	(* Prepare the string to write *)
	let file_content =
		(* 1) Header *)
		file_header ()
		
		(* 2) Statistics about model *)
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (model_statistics ())
		^ "\n------------------------------------------------------------"

		(* 3) The actual result *)
		^ result_str
		
		(* 4) Statistics about result *)
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (result_nature_statistics soundness pdfc_result.termination pdfc_result.statespace_nature)
		
		(* 5) Statistics about state space *)
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (statespace_statistics pdfc_result.state_space pdfc_result.computation_time)
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



(* Write an ef_synth result to the result file *)
let write_im_result_to_file file_name (im_result : Result.im_result) =
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
	let result_str = LinearConstraint.string_of_p_convex_or_nonconvex_constraint model.variable_names im_result.result in

	(* Prepare the string to write *)
	let file_content =
		let pi0 = Input.get_pi0 () in
		
		(* 1) Header *)
		file_header ()

		(* 2) Statistics about model *)
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (model_statistics ())
		^ "\n------------------------------------------------------------"

		(* 3) Recall pi0 *)
		^ "\n\n------------------------------------------------------------"
		^ "\n Reference parameter valuation:"
		^ "\n" ^ (ModelPrinter.string_of_pi0 model pi0)
		^ "\n------------------------------------------------------------"
		
		(* 4) The actual result *)
		(* begin delimiter *)
		^ "\n\nBEGIN CONSTRAINT\n"
		^ result_str ^ ""
		(* end delimiter *)
		^ "\nEND CONSTRAINT\n"
		
		(* 5) Statistics about result *)
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (result_nature_statistics im_result.soundness im_result.termination im_result.statespace_nature)
		^ "\nNumber of random selections             : " ^ (string_of_int im_result.nb_random_selections)
		
		(* 6) Statistics about state space *)
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (statespace_statistics im_result.state_space im_result.computation_time)
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


(*
	(* Write constraint to file (from im_result) *)
(*** TODO: remove ***)
let write_constraint_to_file file_name constraint_str =
	(* Retrieve the input options *)
(* 	let options = Input.get_options () in *)
	(* Prepare the string to write *)
	let file_content =
		file_header ()
			(*** TODO: other statistics (number of states, transitions, etc.) ***)
		(* The actual result *)
		^ constraint_str ^ "\n"
	in
	(* Write to file *)
	write_to_file file_name file_content;
	print_message Verbose_standard ("\nResult written to file '" ^ file_name ^ "'.")
*)


let general_bc_statistics bc_result =
	(* Store number of tiles *)
	let nb_tiles = List.length bc_result.tiles in

	(* First, compute average number of states and transitions (for info purpose) *)
	(*** WARNING: use int, but using NumConst (unbounded) would be smarter in case of very large state spaces ***)
	let total_states, total_transitions = List.fold_left (
		fun (current_sum_states, current_sum_transitions) abstract_im_result ->
			(current_sum_states + abstract_im_result.abstract_state_space.nb_states, current_sum_transitions +  + abstract_im_result.abstract_state_space.nb_transitions)
	) (0,0) bc_result.tiles
	in
	(* Compute average *)
	let average_nb_states = (float_of_int total_states) /. (float_of_int nb_tiles) in
	let average_nb_transitions = (float_of_int total_transitions) /. (float_of_int nb_tiles) in
	
	(* Then: compute the total time to run IM *)
	let time_im = List.fold_left (fun current_sum (abstract_im_result : Result.abstract_im_result) -> current_sum +. abstract_im_result.computation_time) 0. bc_result.tiles in

       ""
	^   "Number of integers in v0                : " ^ (NumConst.string_of_numconst bc_result.size_v0)
	^ "\nNumber of tiles computed                : " ^ (string_of_int nb_tiles)
	^ "\nCoverage                                : " ^ (string_of_coverage bc_result.coverage)
	^ "\nTermination                             : " ^ (string_of_bc_algorithm_termination bc_result.termination)
	^ "\nNumber of unsuccessful points           : " ^ (string_of_int bc_result.nb_unsuccessful_points)
	^ "\nAverage number of states                : " ^ (round1_float average_nb_states)
	^ "\nAverage number of transitions           : " ^ (round1_float average_nb_transitions)
    ^ "\nComputation time                        : " ^ (string_of_seconds bc_result.computation_time)
	^ "\nComputation time (IM)                   : " ^ (string_of_seconds time_im)
	^ "\nComputation time (find point)           : " ^ (string_of_seconds bc_result.find_point_time)
	^ "\nEstimated memory                        : " ^ (memory_used ())

		
(* Write result of BC to file *)
let write_bc_result_to_file file_name bc_result =
	(*** WARNING: duplicate code concerning the counter creation ***)
	(* Create counter *)
	let counter = Statistics.create_time_counter_and_register "file generation" Graphics_counter Verbose_low in
	
	(* Start counter *)
	counter#start;

	(* Retrieve the model *)
	let model = Input.get_model() in
	(* Retrieve the input options *)
(* 	let options = Input.get_options () in *)
	
	(* Convert the im_result's to string *)
	let im_results_str = string_of_list_of_string_with_sep "\n" (
		List.mapi (fun index abstract_im_result ->
			(* mapi starts counting from 0, but we like starting counting from 1 *)
			let index_from_one = index + 1 in
			"\n(************************************************************)"
			^ "\n Tile #" ^ (string_of_int index_from_one)
			(* 1) Reference valuation *)
			^ "\n\n Pi" ^ (string_of_int index_from_one) ^ ":"
			^ "\n" ^ (ModelPrinter.string_of_pi0 model abstract_im_result.reference_val)

			(* 2) Constraint *)
			^ "\n\n K" ^ (string_of_int index_from_one) ^ ":"
			^ "\n" ^ (LinearConstraint.string_of_p_convex_or_nonconvex_constraint model.variable_names abstract_im_result.result)
			
			(* 3) Statistics about result *)
			^ "\n\n------------------------------------------------------------"
			^ "\n" ^ (result_nature_statistics abstract_im_result.soundness abstract_im_result.termination abstract_im_result.statespace_nature)
			^ "\nNumber of random selections             : " ^ (string_of_int abstract_im_result.nb_random_selections)
			
			(* 4) Statistics about state space *)
			^ "\n------------------------------------------------------------"
			^ "\n" ^ (abstract_statespace_statistics abstract_im_result.abstract_state_space abstract_im_result.computation_time)
			^ "\n------------------------------------------------------------"
			^ "\n(************************************************************)\n"
		) bc_result.tiles
	)
	in
	
	(* Prepare the string to write *)
	let file_content =
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
		^ "\n" ^ im_results_str ^ "\n"
		
		(* 5) Statistics on BC *)
		^ "\n(************************************************************)"
		^ "\nGENERAL STATISTICS"
		^ "\n(************************************************************)"
		^ "\n------------------------------------------------------------"
		^ "\n" ^ (general_bc_statistics bc_result)
		^ "\n------------------------------------------------------------"
		
		(* 6) General statistics *)
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



(*------------------------------------------------------------*)
(* Performances *)
(*------------------------------------------------------------*)
let print_statistics total_time state_space =
	(* Retrieve the model *)
(* 	let model = Input.get_model() in *)
	(* Retrieve the input options *)
	let options = Input.get_options () in

	(*** TODO: better have an independent module (or class) 'statistics' ***)
	
	
(*	(* Speed (number of states in the graph) *)
	(*** WARNING: duplicate code from write_efsynth_result_to_file ***)
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
	
	if verbose_mode_greater Verbose_standard then(
		print_message Verbose_standard "\n------------------------------------------------------------";
		print_message Verbose_standard "Statistics: State space";
		print_message Verbose_standard "------------------------------------------------------------";
		print_message Verbose_standard (statespace_statistics state_space total_time);
	);

	
	if options#statistics then (
		(* State space *)
(*		print_message Verbose_standard "------------------------------------------------------------";
		print_message Verbose_standard "Statistics: Graph";
		print_message Verbose_standard "------------------------------------------------------------";*)
		print_message Verbose_standard (StateSpace.get_statistics ());
		print_message Verbose_standard (StateSpace.get_statistics_states state_space);
		
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
(* Main function to process IMITATOR result *)
(************************************************************)

(** Process the result of IMITATOR. The 3rd optional argument is the file name prefix (otherwise options#files_prefix is used). *)
let process_result result algorithm_name prefix_option =
	(* Retrieve the model *)
	let model = Input.get_model() in
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
	| PostStar_result poststar_result ->
		print_message Verbose_low (
			"Computation time: "
			^ (string_of_seconds poststar_result.computation_time) ^ "."
		);

		(* Print statistics *)
		print_statistics poststar_result.computation_time poststar_result.state_space;
		
		(* Draw state space *)
		let radical = file_prefix ^ "-statespace" in
		Graphics.draw_statespace poststar_result.state_space algorithm_name radical;
		
		(* The end *)
		()

		
		
		
	| EFsynth_result efsynth_result ->
		

		(* Print the result *)
		if verbose_mode_greater Verbose_standard then(
			(* Convert result to string *)
			(*** NOTE: this conversion to string is duplicate, since it will again be converted in write_efsynth_result_to_file; but it not sure wether both operations are done, in addition they are not extremely time consuming, and they are not part of the computation time anyway *)
			let result_str = string_of_list_of_string_with_sep "\n OR \n" (List.map (LinearConstraint.string_of_p_linear_constraint model.variable_names) efsynth_result.constraints) in
			
			print_message Verbose_standard ("\nFinal constraint such that the property is *violated* (" ^ (string_of_int (List.length efsynth_result.constraints)) ^ " constraint" ^ (s_of_int (List.length efsynth_result.constraints)) ^ "): ");
			print_message Verbose_standard (result_str);

			(* Give a comment on the validity of the result *)
			print_message Verbose_standard (verbose_string_of_soundness efsynth_result.soundness);
		);
		
		print_message Verbose_low (
			"Computation time: "
			^ (string_of_seconds efsynth_result.computation_time) ^ "."
		);

		(* Print memory information *)
		if verbose_mode_greater Verbose_standard then(
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
			write_efsynth_result_to_file file_name efsynth_result;
		);
		
		(* Print statistics *)
		print_statistics efsynth_result.computation_time efsynth_result.state_space;
		
		(* Draw state space *)
		let radical = file_prefix ^ "-statespace" in
		Graphics.draw_statespace efsynth_result.state_space algorithm_name radical;
		
		(* Render zones in a graphical form *)
		if options#cart then (
			let zones = List.map (fun p_linear_constraint -> (LinearConstraint.Convex_p_constraint p_linear_constraint, StateSpace.Bad)) efsynth_result.constraints in
			Graphics.draw_cartography zones (file_prefix ^ "_cart_ef")
		) else (
				print_message Verbose_high "Graphical cartography not asked: not drawn.";
		);
		
		(* The end *)
		()



	(*** TODO: merge with efsynth when efsynth becomes non convex, with possible under/over-approximation? ***)
	| PDFC_result pdfc_result ->
		
		(* Print the result *)
		if verbose_mode_greater Verbose_standard then(
			(* Convert result to string *)
			(*** NOTE: this conversion to string is duplicate, since it will again be converted in write_pdfc_result_to_file; but it not sure wether both operations are done, in addition they are not extremely time consuming, and they are not part of the computation time anyway *)
		
			begin
			match pdfc_result.result with
				| Single_constraint (result, _) ->
					let result_str = LinearConstraint.string_of_p_nnconvex_constraint model.variable_names result in
					
					print_message Verbose_standard ("\nFinal constraint such that the system is deadlock-free:");
					print_message Verbose_standard (result_str);

				| Under_over_constraint (under, over) ->
					(* Convert the constraints to a string *)
					let under_str = LinearConstraint.string_of_p_nnconvex_constraint model.variable_names under in
					let over_str = LinearConstraint.string_of_p_nnconvex_constraint model.variable_names over in

					print_message Verbose_standard ("\nFinal possibly under-approximated constraint such that the system is deadlock-free:");
					print_message Verbose_standard (under_str);
					print_message Verbose_standard ("\nFinal possibly over-approximated constraint such that the system is deadlock-free:");
					print_message Verbose_standard (over_str);
			
			end;

			let soundness = match pdfc_result.result with
				| Single_constraint (_, soundness) -> soundness
				| Under_over_constraint _ -> Constraint_under_over
			in
			
			(* Give a comment on the validity of the result *)
			print_message Verbose_standard (verbose_string_of_soundness soundness);
		);
		
		print_message Verbose_low (
			"Computation time: "
			^ (string_of_seconds pdfc_result.computation_time) ^ "."
		);

		(* Print memory information *)
		if verbose_mode_greater Verbose_standard then(
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
			write_pdfc_result_to_file file_name pdfc_result;
		);
		
		(* Print statistics *)
		print_statistics pdfc_result.computation_time pdfc_result.state_space;
		
		(* Draw state space *)
		let radical = file_prefix ^ "-statespace" in
		Graphics.draw_statespace pdfc_result.state_space algorithm_name radical;
		
		(* Render zones in a graphical form *)
		if options#cart then (
			let result = match pdfc_result.result with
				| Single_constraint (result, _) -> result
				| _ -> raise (InternalError("not implemented"))
			in

			let zones = List.map (fun p_linear_constraint -> (LinearConstraint.Convex_p_constraint p_linear_constraint, StateSpace.Bad (*** TODO ? ***))) (LinearConstraint.p_linear_constraint_list_of_p_nnconvex_constraint result) in
			Graphics.draw_cartography zones (file_prefix ^ "_cart_pdfc")
		) else (
				print_message Verbose_high "Graphical cartography not asked: not drawn.";
		);
		
		(* The end *)
		()

	

	| IM_result im_result ->

		(* Print on terminal *)
		if verbose_mode_greater Verbose_standard then(
			(* Convert result to string *)
			let result_str = LinearConstraint.string_of_p_convex_or_nonconvex_constraint model.variable_names im_result.result in
			print_message Verbose_standard ("\nResult:\n" ^ result_str);
			
			(* Give a comment on the validity of the result *)
			print_message Verbose_standard (verbose_string_of_soundness im_result.soundness);
		);
		
		(* Print memory information *)
		if verbose_mode_greater Verbose_standard then(
			print_newline();
			print_message Verbose_standard (memory_used ());
		);
		
		(* Write to file if requested *)
		if options#output_result then(
			let file_name = file_prefix ^ Constants.result_file_extension in
			write_im_result_to_file file_name im_result;
		);

		print_message Verbose_low (
			"Computation time for IM only: "
			^ (string_of_seconds im_result.computation_time) ^ "."
		);
		
		(* Print statistics *)
		print_statistics im_result.computation_time im_result.state_space;

		(* Draw state space *)
		(*** TODO: move inside inverse_method_gen ***)
		let radical = file_prefix ^ "-statespace" in
		Graphics.draw_statespace im_result.state_space algorithm_name radical;
		
		if options#cart then (
			(* Render zones in a graphical form *)
			let zones =
			[im_result.result, im_result.statespace_nature]
(*				match im_result.result with
				| LinearConstraint.Convex_p_constraint p_linear_constraint -> [Convex_constraint (p_linear_constraint, AbstractModel.Unknown (*** TODO ***))]
				
				(*** A bit a HACk here ***)
				| LinearConstraint.Nonconvex_p_constraint p_nnconvex_constraint ->
					let convex_constraints = LinearConstraint.p_linear_constraint_list_of_p_nnconvex_constraint p_nnconvex_constraint in
					[Union_of_constraints (convex_constraints, AbstractModel.Unknown (*** TODO ***))]*)
			in
			Graphics.draw_cartography zones (file_prefix ^ "_cart_im")
		) else (
				print_message Verbose_high "Graphical cartography not asked: not drawn.";
		);

		(* The end *)
		()



	| BC_result bc_result ->
		(* Print some information *)
		if verbose_mode_greater Verbose_standard then(
			print_message Verbose_standard ("\n**************************************************");
			print_message Verbose_standard (" END OF THE BEHAVIORAL CARTOGRAPHY ALGORITHM");
			print_message Verbose_standard ("" ^ general_bc_statistics bc_result);
			print_message Verbose_standard ("**************************************************");
		
			print_message Verbose_standard ("\n" ^ (verbose_string_of_coverage bc_result.coverage));
		);
		
		(* Print memory information *)
		if verbose_mode_greater Verbose_standard then(
			print_newline();
			print_message Verbose_standard (memory_used ());
		);
		
		(* Write to file if requested for BC *)
		if options#output_bc_result then(
			let file_name = file_prefix ^ Constants.result_file_extension in
			write_bc_result_to_file file_name bc_result;
		);
		
		(* If cartography required for BC *)
		if options#output_bc_cart then (
			(* Print some information *)
			print_message Verbose_high "Graphical cartography asked: prepare tiles to be drawn...";

			(* Keep only valid tiles, i.e., underapproximations or exact *)
			let valid_tiles = List.filter (fun abstract_im_result -> abstract_im_result.soundness = Result.Constraint_maybe_under || abstract_im_result.soundness = Result.Constraint_exact) bc_result.tiles in
			
			(* Render zones in a graphical form *)
			let zones = List.map (fun abstract_im_result -> (abstract_im_result.result, abstract_im_result.statespace_nature)) valid_tiles in
			Graphics.draw_cartography zones (file_prefix ^ "_cart_bc")
		) else (
			(* Print some information *)
			print_message Verbose_high "Graphical cartography not asked: not drawn.";
		);

		(* The end *)
		()
	
	(* Nothing to do for workers in distributed mode *)
	| Distributed_worker_result ->
		()
		
		
		
(* 	| _ -> raise (InternalError ("function process_result not implemented for all cases")) *)
