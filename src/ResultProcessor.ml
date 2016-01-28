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
 * Last modified     : 2016/01/28
 *
 ************************************************************)

 
 
(************************************************************)
(* Modules *)
(************************************************************)
open OCamlUtilities
open ImitatorUtilities
open Exceptions
open AbstractModel
open Result




(************************************************************)
(* I/O functions *)
(************************************************************)

(* Header for result files *)
let file_header () =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	"(*" 
	(* Program version *)
	^ "\n  Result output by " ^ Constants.program_name ^ ""
	^ "\n  Version  : " ^ (ImitatorUtilities.program_name_and_version_and_nickname_and_build())
	^ "\n  Model    : '" ^ options#file ^ "'"
	(* Date *)
	^ "\n  Generated: " ^ (now()) ^ ""
	(* Command *)
	^ "\n  Command  : " ^ (string_of_array_of_string_with_sep " " Sys.argv)
	^ "\n*)\n\n"


(* Write constraint to file (from im_result) *)
let write_constraint_to_file constraint_str =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	(* Prepare the string to write *)
	let file_content =
		file_header ()
		(* The actual result *)
		^ constraint_str ^ "\n"
	in
	(* Write to file *)
	let file_name = options#files_prefix ^ Constants.result_file_extension in
	write_to_file file_name file_content;
	print_message Verbose_standard ("\nResult written to file '" ^ file_name ^ "'.")



(* Write result of BC to file *)
let write_bc_result_to_file bc_result =
	(* Retrieve the model *)
	let model = Input.get_model() in
	(* Retrieve the input options *)
	let options = Input.get_options () in
	
	(* Convert the im_result's to string *)
	let im_results_str = string_of_list_of_string_with_sep "\n" (
		List.mapi (fun index abstract_im_result ->
			(* mapi starts counting from 0, but we like starting counting from 1 *)
			let real_index = index + 1 in
			"\n(************************************************************)"
			^ "\n Tile #" ^ (string_of_int real_index)
			^ "\n\n Pi" ^ (string_of_int real_index) ^ ":"
			^ (ModelPrinter.string_of_pi0 model abstract_im_result.reference_val)
			^ "\n\n K" ^ (string_of_int real_index) ^ ":"
			^ (LinearConstraint.string_of_p_convex_or_nonconvex_constraint model.variable_names abstract_im_result.result)
			
			(*** TODO: other statistics (number of states, transitions, etc.) ***)
			
			^ "\n(************************************************************)\n"
		) bc_result.tiles
	)
	in
	
	(* Prepare the string to write *)
	let file_content =
		file_header ()
		(* The actual result *)
		^ im_results_str ^ "\n"
	in
	(* Write to file *)
	let file_name = options#files_prefix ^ Constants.result_file_extension in
	write_to_file file_name file_content;
	print_message Verbose_standard ("\nResult written to file '" ^ file_name ^ "'.")



(*------------------------------------------------------------*)
(* Performances *)
(*------------------------------------------------------------*)
let print_statespace_statistics total_time state_space =
	(* Retrieve the model *)
(* 	let model = Input.get_model() in *)
	(* Retrieve the input options *)
	let options = Input.get_options () in

	(*** TODO: better have an independent module (or class) 'statistics' ***)
	
	(* Generic function for float/int conversion *)
	let string_of_average average = 
		if average < 10.0 then string_of_float average
		else string_of_int (int_of_float average)
	in
	
	(* Speed (number of states in the graph) *)
	let nb_states = StateSpace.nb_states state_space in
	let average = (float_of_int nb_states) /. total_time in
	print_message Verbose_standard ("States per second in the graph: " ^ (string_of_average average) ^ " (" ^ (string_of_int nb_states) ^ "/" ^ (string_of_seconds total_time) ^ ")");
	
	(* Speed (number of states computed, even if not kept) *)
	let nb_gen_states = StateSpace.get_nb_gen_states state_space in
	let average = (float_of_int nb_gen_states) /. total_time in
	print_message Verbose_standard ("States computed per second: " ^ (string_of_average average) ^ " (" ^ (string_of_int nb_gen_states) ^ "/" ^ (string_of_seconds total_time) ^ ")");

	
	if options#statistics then (
		(* PPL *)
		print_message Verbose_standard "--------------------";
		print_message Verbose_standard "Statistics on PPL";
		print_message Verbose_standard ("--------------------" ^ (LinearConstraint.get_statistics total_time));
		
		(* Graph *)
		print_message Verbose_standard "--------------------";
		print_message Verbose_standard "Statistics on Graph";
		print_message Verbose_standard "--------------------";
		print_message Verbose_standard (StateSpace.get_statistics ());
		print_message Verbose_standard (StateSpace.get_statistics_states state_space);
		
		print_message Verbose_standard "--------------------";
		print_message Verbose_standard "Statistics on Cache";
		print_message Verbose_standard "--------------------";
		
		(*** TODO: re-enable later (using counters or something like that ??? ***)
(* 		print_stats (); *)
		
		print_message Verbose_standard "--------------------";
		print_message Verbose_standard "Statistics on Reachability";
		print_message Verbose_standard "--------------------";
		
		(*** TODO: re-enable later (using counters) ***)
(*		print_message Verbose_standard ("Number of early skips because of unsatisfiable guards: " ^ (string_of_int !nb_early_unsatisfiable));
		print_message Verbose_standard ("Number of early skips because no actions: " ^ (string_of_int !nb_early_skip));
		print_message Verbose_standard ("Number of unsatisfiable constraints: " ^ (string_of_int !nb_unsatisfiable));
		print_message Verbose_standard ("Number of unsat1: " ^ (string_of_int !nb_unsat1));
		print_message Verbose_standard ("Number of unsat2: " ^ (string_of_int !nb_unsat2));
		print_message Verbose_standard ("Number of combinations considered: " ^ (string_of_int !nb_combinations));*)
		
		
		print_message Verbose_standard "--------------------";
		print_message Verbose_standard "Statistics on memory";
		print_message Verbose_standard "--------------------";
		print_memory_used Verbose_standard;
		Gc.print_stat stdout;
(*		print_message Verbose_standard "--------------------";
		Gc.major();
		Gc.print_stat stdout;
		print_message Verbose_standard "--------------------";
		Gc.full_major();
		Gc.print_stat stdout;*)
	)

	

(************************************************************)
(* Main function to process IMITATOR result *)
(************************************************************)

let process_result result =
	(* Retrieve the model *)
	let model = Input.get_model() in
	(* Retrieve the input options *)
	let options = Input.get_options () in
	
	match result with
	| PostStar_result poststar_result ->
		print_message Verbose_low (
			"Computation time: "
			^ (string_of_seconds poststar_result.computation_time) ^ "."
		);

		(* Print statistics *)
		print_statespace_statistics poststar_result.computation_time poststar_result.state_space;
		
		(* Generate graphics *)
		let radical = options#files_prefix in
		Graphics.generate_graph poststar_result.state_space radical;
		
		(* The end *)
		()

		
		
		
	| EFsynth_result efsynth_result ->
		
		(* Convert result to string *)
		let result_str = string_of_list_of_string_with_sep "\n OR \n" (List.map (LinearConstraint.string_of_p_linear_constraint model.variable_names) efsynth_result.constraints) in

		(* Print the result *)
		if verbose_mode_greater Verbose_standard then(
			print_message Verbose_standard ("\nFinal constraint such that the property is *violated* (" ^ (string_of_int (List.length efsynth_result.constraints)) ^ " constraint" ^ (s_of_int (List.length efsynth_result.constraints)) ^ "): ");
			print_message Verbose_standard (result_str);
		);
		
		print_message Verbose_low (
			"Computation time: "
			^ (string_of_seconds efsynth_result.computation_time) ^ "."
		);

(*		(* Print on terminal *)
		print_message Verbose_standard (
			"\nEF-synthesis successfully finished " ^ (after_seconds ()) ^ "."
		);*)

		(* Write to file if requested *)
		if options#output_result then(
			write_constraint_to_file result_str;
		);
		
		(* Print statistics *)
		print_statespace_statistics efsynth_result.computation_time efsynth_result.state_space;
		
		(* Generate graphics *)
		let radical = options#files_prefix in
		Graphics.generate_graph efsynth_result.state_space radical;
		
		(* Render zones in a graphical form *)
		if options#cart then (
			let zones = List.map (fun p_linear_constraint -> (LinearConstraint.Convex_p_constraint p_linear_constraint, StateSpace.Bad)) efsynth_result.constraints in
			Graphics.draw_cartography zones (options#files_prefix ^ "_cart_ef")
		) else (
				print_message Verbose_high "Graphical cartography not asked: not drawn.";
		);
		
		(* The end *)
		()

		
	| IM_result im_result ->
		(* Convert result to string *)
		let result_str = LinearConstraint.string_of_p_convex_or_nonconvex_constraint model.variable_names im_result.result in

		(* Print on terminal *)
		print_message Verbose_standard ("\nResult:\n" ^ result_str);
		
		(* Write to file if requested *)
		if options#output_result then(
			write_constraint_to_file result_str;
		);
		
		(* Print memory information *)
		if verbose_mode_greater Verbose_standard then(
			print_newline();
			print_memory_used Verbose_standard;
		);
		
		print_message Verbose_low (
			"Computation time for IM only: "
			^ (string_of_seconds im_result.computation_time) ^ "."
		);
		
		(* Print statistics *)
		print_statespace_statistics im_result.computation_time im_result.state_space;

		(* Generate graphics *)
		(*** TODO: move inside inverse_method_gen ***)
		let radical = options#files_prefix in
		Graphics.generate_graph im_result.state_space radical;
		
		if options#cart then (
			(* Render zones in a graphical form *)
			let zones =
			[im_result.result, StateSpace.Unknown] (*** TODO ***)
(*				match im_result.result with
				| LinearConstraint.Convex_p_constraint p_linear_constraint -> [Convex_constraint (p_linear_constraint, AbstractModel.Unknown (*** TODO ***))]
				
				(*** A bit a HACk here ***)
				| LinearConstraint.Nonconvex_p_constraint p_nnconvex_constraint ->
					let convex_constraints = LinearConstraint.p_linear_constraint_list_of_p_nnconvex_constraint p_nnconvex_constraint in
					[Union_of_constraints (convex_constraints, AbstractModel.Unknown (*** TODO ***))]*)
			in
			Graphics.draw_cartography zones (options#files_prefix ^ "_cart_im")
		) else (
				print_message Verbose_high "Graphical cartography not asked: not drawn.";
		);

		(* The end *)
		()



	| BC_result bc_result ->
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

		
		(* Print some information *)
		print_message Verbose_standard ("\n**************************************************");
		print_message Verbose_standard (" END OF THE BEHAVIORAL CARTOGRAPHY ALGORITHM");
		
		(*** TODO! need to access the algorithm variable... (or add info to bc_result) ***)
(* 		print_message Verbose_standard ("Size of V0: " ^ (NumConst.string_of_numconst nb_points) ^ ""); *)

		(*** TODO! need to access the algorithm variable... (or add info to bc_result) ***)
(* 		print_message Verbose_standard ("Unsuccessful points: " ^ (string_of_int !nb_useless_points) ^ ""); *)

		print_message Verbose_standard ("Number of tiles                 : " ^ (string_of_int nb_tiles) ^ "");
		print_message Verbose_standard ("Average number of states        : " ^ (round1_float average_nb_states) ^ "");
		print_message Verbose_standard ("Average number of transitions   : " ^ (round1_float average_nb_transitions) ^ "");

		print_message Verbose_standard ("Global time spent               : " ^ (string_of_seconds bc_result.computation_time) ^ "");
		(*** TODO: require a new counter, and a new field in bc_result ***)
(* 		print_message Verbose_standard ("Time spent on IM                : " ^ (string_of_float (!time_spent_on_IM)) ^ " s"); *)
	(* 	print_message Verbose_standard ("Time spent on BC only: " ^ (string_of_float (time_spent_on_BC)) ^ " s"); *)
		(*** TODO: require a new counter, and a new field in bc_result ***)
(* 		print_message Verbose_standard ("Time spent to compute next point: " ^ (string_of_float (counter_next_point#value)) ^ " s"); *)
		print_message Verbose_standard ("**************************************************");
			
		
		(* Write to file if requested *)
		if options#output_result then(
			write_bc_result_to_file bc_result;
		);
		
		(* Print memory information *)
		if verbose_mode_greater Verbose_standard then(
			print_newline();
			print_memory_used Verbose_standard;
		);
		
		(*** TODO: BC ***)
(*		print_message Verbose_low (
			"Computation time for IM only: "
			^ (string_of_seconds im_result.computation_time) ^ "."
		);*)
		

		if options#cart then (
			(* Render zones in a graphical form *)
			let zones = List.map (fun abstract_im_result -> (abstract_im_result.result , StateSpace.Unknown (*** TODO ***))) bc_result.tiles in
			Graphics.draw_cartography zones (options#files_prefix ^ "_cart_bc")
		) else (
				print_message Verbose_high "Graphical cartography not asked: not drawn.";
		);

		(* The end *)
		()
		
		
		
(* 	| _ -> raise (InternalError ("function process_result not implemented for all cases yet")) *)
