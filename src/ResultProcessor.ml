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
 * Last modified     : 2016/01/13
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

let write_result_to_file constraint_str =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	(* Prepare the string to write *)
	let file_content =
		(*** WARNING: duplicate code (Cartography.ml) ***)
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
		(* The actual result *)
		^ constraint_str ^ "\n"
	in
	(* Write to file *)
	let file_name = options#files_prefix ^ Constants.result_file_extension in
	write_to_file file_name file_content;
	print_message Verbose_standard ("Result written to file '" ^ file_name ^ "'.")





(*------------------------------------------------------------*)
(* Performances *)
(*------------------------------------------------------------*)
let print_statistics total_time reachability_graph =
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
	let nb_states = StateSpace.nb_states reachability_graph in
	let average = (float_of_int nb_states) /. total_time in
	print_message Verbose_standard ("\nStates per second in the graph: " ^ (string_of_average average) ^ " (" ^ (string_of_int nb_states) ^ "/" ^ (string_of_seconds total_time) ^ ")");
	
	(* Speed (number of states computed, even if not kept) *)
	let nb_gen_states = StateSpace.get_nb_gen_states reachability_graph in
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
		print_message Verbose_standard (StateSpace.get_statistics_states reachability_graph);
		
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

	


let process_result result =
	(* Retrieve the model *)
	let model = Input.get_model() in
	(* Retrieve the input options *)
	let options = Input.get_options () in
	
	match result with
	| PostStar_result poststar_result ->
		print_message Verbose_standard (
			"\nState space exploration completed " ^ (after_seconds ()) ^ "."
		);
			
		print_message Verbose_low (
			"Computation time: "
			^ (string_of_seconds poststar_result.computation_time) ^ "."
		);

		(* Print statistics *)
		print_statistics poststar_result.computation_time poststar_result.state_space;
		
		(* Generate graphics *)
		let radical = options#files_prefix in
		Graphics.generate_graph model poststar_result.state_space radical;
		
		(* The end *)
		()

		
		
		
	| EFsynth_result efsynth_result ->

		print_message Verbose_standard (
			"\nState space exploration completed " ^ (after_seconds ()) ^ "."
		);
		
		
		(*** TODO: compute as well *good* zones, depending whether the analysis was exact, or early termination occurred ***)
		
		
		(* Convert result to string *)
		let result_str = string_of_list_of_string_with_sep "\n OR \n" (List.map (LinearConstraint.string_of_p_linear_constraint model.variable_names) efsynth_result.constraints) in

		(* Print the result *)
		print_message Verbose_standard ("\nFinal constraint such that the property is *violated* (" ^ (string_of_int (List.length efsynth_result.constraints)) ^ " constraint" ^ (s_of_int (List.length efsynth_result.constraints)) ^ "): ");
		print_message Verbose_standard (result_str);
		
		print_message Verbose_low (
			"Computation time: "
			^ (string_of_seconds efsynth_result.computation_time) ^ "."
		);

		(* Print on terminal *)
		print_message Verbose_standard (
			"\nEF-synthesis successfully finished " ^ (after_seconds ()) ^ "."
		);

		(* Write to file if requested *)
		if options#output_result then(
			write_result_to_file result_str;
		);
		
		(* Print statistics *)
		print_statistics efsynth_result.computation_time efsynth_result.state_space;
		
		(* Generate graphics *)
		let radical = options#files_prefix in
		Graphics.generate_graph model efsynth_result.state_space radical;
		
		(* Render zones in a graphical form *)
		let zones = [Union_of_constraints (efsynth_result.constraints, AbstractModel.Bad)] in
		if options#cart then (
			Graphics.cartography model (Input.get_v0()) zones (options#files_prefix ^ "_cart_ef")
		) else (
				print_message Verbose_high "Graphical cartography not asked: graph not generated.";
		);
		
		(* The end *)
		()

		
	| IMConvex_result im_result ->
		print_message Verbose_standard (
			"\nInverse method successfully finished " ^ (after_seconds ()) ^ "."
		);

		(* Convert result to string *)
		let result_str = LinearConstraint.string_of_p_linear_constraint model.variable_names im_result.convex_constraint in

		(* Print on terminal *)
		print_message Verbose_standard result_str;
		
		(* Write to file if requested *)
		if options#output_result then(
			write_result_to_file result_str;
		);
		
		(* Print memory information *)
		if verbose_mode_greater Verbose_standard then
			print_memory_used Verbose_standard;
		
		print_message Verbose_low (
			"Computation time for IM only: "
			^ (string_of_seconds im_result.computation_time) ^ "."
		);
		
		(* Print statistics *)
		print_statistics im_result.computation_time im_result.state_space;

		(* Generate graphics *)
		(*** TODO: move inside inverse_method_gen ***)
		let radical = options#files_prefix in
		Graphics.generate_graph model im_result.state_space radical;
		
		(* Render zones in a graphical form *)
		(*** TODO ***)

		(* The end *)
		()
		
		
	| IMNonconvex_result im_result ->
		print_message Verbose_standard (
			"\nInverse method successfully finished " ^ (after_seconds ()) ^ "."
		);

		(* Convert result to string *)
		let result_str = LinearConstraint.string_of_p_nnconvex_constraint model.variable_names im_result.nonconvex_constraint in

		(* Print on terminal *)
		print_message Verbose_standard result_str;
		
		(* Write to file if requested *)
		if options#output_result then(
			write_result_to_file result_str;
		);
		
		(* Print memory information *)
		if verbose_mode_greater Verbose_standard then
			print_memory_used Verbose_standard;
		
		print_message Verbose_low (
			"Computation time for IM only: "
			^ (string_of_seconds im_result.computation_time) ^ "."
		);
		
		(* Print statistics *)
		print_statistics im_result.computation_time im_result.state_space;

		(* Generate graphics *)
		(*** TODO: move inside inverse_method_gen ***)
		let radical = options#files_prefix in
		Graphics.generate_graph model im_result.state_space radical;
		
		(* Render zones in a graphical form *)
		(*** TODO ***)

		(* The end *)
		()



(* 	| _ -> raise (InternalError ("function process_result not implemented for all cases yet")) *)
