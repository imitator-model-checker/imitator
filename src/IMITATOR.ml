(***************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2009/09/07
 * Last modified: 2012/06/07
 *
 **************************************************)


(**************************************************)
(* Modules *)
(**************************************************)
open Global
open AbstractImitatorFile
open Arg
open ImitatorPrinter
open Graph
open Options
open Reachability
open Graphics
open Gc


(**************************************************

A FAIRE
[ ] eviter les etats degeneres (avec "faux") : arrive dans le cas ou aucun etat n'est genere (init deja pas satisfiable) --> bouger le test de satisfiabilite dans le demarrage de InverseMethod ?

 OPTIMISATIONS A FAIRE POUR L'EXECUTION

[ ] METTRE DES TABLES DE HASH et non des tableaux pour transitions, gardes, invariants, etc. Avantage : (beaucoup) moins de choses en memoire, execution a peine plus lente.



TAGS POUR CHOSES A FAIRE
- (**** TO DO ****)
- (**** BAD PROG ****)
- (**** TO OPTIMIZE ****)
- (**** OPTIMIZED ****)

**************************************************)

(**************************************************)
(**************************************************)
(* GLOBAL CONSTANTS *)
(**************************************************)
(**************************************************)

let dot_command = "dot"
let dot_extension = "jpg"



(**************************************************)
(**************************************************)
(* GLOBAL VARIABLES *)
(**************************************************)
(**************************************************)

(* object with command line options *)
let options = new imitator_options




(**************************************************)
(**************************************************)
(* FUNCTIONS *)
(**************************************************)
(**************************************************)
(* String functions *)

(** Convert a 'returned_constraint' to a 'string'*)

let string_of_returned_constraint variable_names = function 
	| Convex_constraint linear_constraint -> LinearConstraint.string_of_linear_constraint variable_names linear_constraint
	(** Disjunction of constraints *)
	| Union_of_constraints k_list -> string_of_list_of_string_with_sep "\n OR \n" (List.map (LinearConstraint.string_of_linear_constraint variable_names) k_list)



(**************************************************)
(* Parsing function *)
(**************************************************)

(* Parse a file and return the abstract structure *)
let parser_lexer the_parser the_lexer file =
	(* Open file *)
	let in_channel = try (open_in file) with
		| Sys_error e -> print_error ("The file " ^ file ^ " could not be opened.\n" ^ e); abort_program (); exit(0)
	in
	
	(* Lexing *)
	let lexbuf = try (Lexing.from_channel in_channel) with
		| Failure f -> print_error ("Lexing error in file " ^ file ^ ": " ^ f); abort_program (); exit(0)
	in

	(* Parsing *)
	let parsing_structure = try(
		the_parser the_lexer lexbuf
	) with
		| ParsingError (symbol_start, symbol_end) ->
			(* Convert the file into a string *)
			let extlib_input = IO.input_channel (open_in file) in
			let file_string = IO.read_all extlib_input in
			(* Create the error message *)
			let error_message =
				if symbol_start >= 0 && symbol_end >= symbol_start then (
					(* Get the symbol *)
					let error_symbol = (String.sub file_string symbol_start (symbol_end - symbol_start)) in
					(* Resize it if too big *)
					let error_symbol =
						if (String.length error_symbol > 15) then
							"..." ^ (String.sub error_symbol (String.length error_symbol - 15) 15)
						else error_symbol
					in
					(* Get the line *)
					let beginning_of_the_file = String.sub file_string 0 symbol_end in
					let lines = Str.split (Str.regexp "\n") beginning_of_the_file in
					let line = List.length lines in
					(* Make the message *)
					"next to '" ^ error_symbol ^ "' at line " ^ (string_of_int line) ^ ".")
				else "somewhere in the file, most probably in the very beginning."
			in
			(* Print the error message *)
			print_error ("Parsing error in file " ^ file ^ " " ^ error_message); abort_program (); exit(0)
		| Failure f -> print_error ("Parsing error in file " ^ file ^ ": " ^ f); abort_program (); exit(0)
	in
	parsing_structure


(**************************************************)
(* System functions *)
(**************************************************)

let write_to_file file_name file_content =
	let oc = open_out file_name in
	(* Write file *)
	output_string oc file_content;
	(* Close channel *)
	close_out oc;
	()



(**************************************************)
(* Pi0 function *)
(**************************************************)
(* Generate a random pi0 in a given interval for each parameter (array view!) *)
let random_pi0 program pi0 =
	(* Create the pi0 *)
	let random_pi0 = Array.make program.nb_parameters NumConst.zero in
	(* Fill it *)
	for i = 0 to program.nb_parameters - 1 do
		let a, b = pi0.(i) in
		(* Generate a random value in the interval *)
		Random.self_init();
		let random_value = Random.int (b - a + 1) + a in
		(* Debug *)
		print_message Debug_medium ("Generating randomly value '" ^ (string_of_int random_value) ^ "' for parameter '" ^ (program.variable_names i) ^ "'.");
		(* Convert to a num *)
		random_pi0.(i) <- NumConst.numconst_of_int random_value;
	done;
	(* Return the result as an array *)
	random_pi0



(**************************************************)
(* Functions to interact with Dot *)
(**************************************************)
(* Create a gif graph using dot *)
let generate_graph program pi0 reachability_graph dot_file_name states_file_name gif_file_name =
	(* Do not write if no dot AND no log *)
	if not (options#no_dot && options#no_log) then (
		
		(* Create the input file *)
		print_message Debug_medium ("Creating input file for dot...");
		let dot_program, states = Graph.dot_of_graph program pi0 reachability_graph ~fancy:options#fancy in

		if not options#no_dot then (
			(* Write dot file *)
			print_message Debug_medium ("Writing to dot file...");
			write_to_file dot_file_name dot_program;

			(* Generate gif file using dot *)
			print_message Debug_medium ("Calling dot...");
			let command_result = Sys.command (dot_command ^ " -T" ^ dot_extension ^ " " ^ dot_file_name ^ " -o " ^ gif_file_name ^ "") in
			print_message Debug_medium ("Result of the 'dot' command: " ^ (string_of_int command_result));
			(* Removing dot file *)
			print_message Debug_medium ("Removing dot file...");
			Sys.remove dot_file_name;
		);
			
		(* Write states file *)
		if not options#no_log then (
			print_message Debug_medium ("Writing to file for file description...");
			write_to_file states_file_name states;
		);
	)




(**************************************************)
(* BEHAVIORAL CARTOGRAPHY ALGORITHM functions *)
(**************************************************)
(** Check if a pi_0 belongs to a 'returned_constraint'*)
let pi0_in_returned_constraint pi0 = function
	| Convex_constraint k -> LinearConstraint.is_pi0_compatible pi0 k
	(** Disjunction of constraints *)
	| Union_of_constraints k_list -> List.exists (LinearConstraint.is_pi0_compatible pi0) k_list


(** Behavioral cartography algorithm with full coverage of V0 *)
let cover_behavioral_cartography program pi0cube init_state =
	(* Dimension of the system *)
	let dimension = Array.length pi0cube in
	(* Min & max bounds for the parameters *)
	let min_bounds = Array.map (fun (low, high) -> NumConst.numconst_of_int low) pi0cube in
	let max_bounds = Array.map (fun (low, high) -> NumConst.numconst_of_int high) pi0cube in
	
	(* Initial constraint of the program *)
	let _, init_constraint = init_state in
	(* Hide non parameters *)
	let init_constraint = LinearConstraint.hide (List.rev_append program.discrete program.clocks) init_constraint in

	(* Current pi0, instantiated with the lower bounds *)
	let current_pi0 = Array.copy min_bounds in
	(* (Dynamic) Array for the pi0 *)
	let pi0_computed = DynArray.create() in
	(* (Dynamic) Array for the results *)
	let results = DynArray.create() in
	(* Current iteration *)
	let current_iteration = ref 0 in
	(* Sum of number of states *)
	let nb_states = ref 0 in
	(* Sum of number of transitions *)
	let nb_transitions = ref 0 in

	(* Debug mode *)
	let global_debug_mode = get_debug_mode() in
	
	(* Iterate on all the possible pi0 *)
	let more_pi0 = ref true in
	let limit_reached = ref false in
	while !more_pi0 && not !limit_reached do
		(* Copy the array current_pi0*)
		let pi0_array = Array.copy current_pi0 in
		let pi0 = fun parameter -> pi0_array.(parameter) in
		
		(* Check that it does not belong to any constraint *)
		if dynArray_exists (pi0_in_returned_constraint pi0) results then (
			if debug_mode_greater Debug_medium then (
				print_message Debug_medium "The following pi0 is already included in a constraint.";
				print_message Debug_medium (string_of_pi0 program pi0);
			);
		(* Check that it satisfies the initial constraint *)
		) else if not (LinearConstraint.is_pi0_compatible pi0 init_constraint) then (
			if debug_mode_greater Debug_medium then (
				print_message Debug_medium "The following pi0 does not satisfy the initial constraint of the program.";
				print_message Debug_medium (string_of_pi0 program pi0);
			);
		) else (
			(* Iterate *)
			current_iteration := !current_iteration + 1;

			(* Debug messages *)
			print_message Debug_standard ("\n**************************************************");
			print_message Debug_standard ("BEHAVIORAL CARTOGRAPHY ALGORITHM: " ^ (string_of_int !current_iteration) ^ "");
			print_message Debug_standard ("Considering the following pi" ^ (string_of_int !current_iteration));
			print_message Debug_standard (string_of_pi0 program pi0);
			
			(* Prevent the debug messages *)
			if not (debug_mode_greater Debug_medium) then
				set_debug_mode Debug_nodebug;
			(* Compute the post and the constraint *)
			let returned_constraint, graph, nb_iterations, counter = Reachability.post_star program pi0 init_state in
			(* Get the debug mode back *)
			set_debug_mode global_debug_mode;
			(* Update the counters *)
			nb_states := !nb_states + (Graph.nb_states graph);
			nb_transitions := !nb_transitions + (Hashtbl.length (graph.transitions_table));
			(* Print message *)
			print_message Debug_standard (
				"\nK" ^ (string_of_int (!current_iteration)) ^ " computed using algorithm InverseMethod after "
				^ (string_of_int nb_iterations) ^ " iteration" ^ (s_of_int nb_iterations) ^ ""
				^ " in " ^ (string_of_seconds (time_from counter)) ^ ": "
				^ (string_of_int (Graph.nb_states graph)) ^ " reachable state" ^ (s_of_int (Graph.nb_states graph))
				^ " with "
				^ (string_of_int (Hashtbl.length (graph.transitions_table))) ^ " transition" ^ (s_of_int (Hashtbl.length (graph.transitions_table))) ^ ".");
			
			(* Generate the dot graph *)			
			let radical = options#program_prefix ^ "_" ^ (string_of_int !current_iteration) in
			let dot_file_name = (radical ^ ".dot") in
			let states_file_name = (radical ^ ".states") in
			let gif_file_name = (radical ^ "." ^ dot_extension) in
			generate_graph program pi0 graph dot_file_name states_file_name gif_file_name;
			
			(* compute k0 *)

(*			let k0 =  if options#dynamic || options#union then ( returned_constraint )
				else  match returned_constraint with 
					| Convex_constraint _ -> Convex_constraint (Graph.compute_k0_destructive program graph)
					| _ -> print_error ("Internal error when getting the result of post_star in cover: 'options#dynamic' is activated but the constraint returned is not convex (type 'Convex_constraint')."); abort_program (); exit(0)
			in*)
			let k0 = returned_constraint in
			
			(* Add the pi0 and the computed constraint *)
			DynArray.add pi0_computed pi0;
			DynArray.add results k0;
			
			(* Print the constraint *)
(* 			let bad_string = if Graph.is_bad program graph then "BAD." else "GOOD." in			 *)
			print_message Debug_low ("Constraint K0 computed:");
			print_message Debug_standard (string_of_returned_constraint program.variable_names k0);
(* 			print_message Debug_standard ("This zone is " ^ bad_string); *)


		); (* else if new pi0 *)

		(* Find the next pi0 *)
		let not_is_max = ref true in
		let local_index = ref 0 in
		while !not_is_max do
			(* Try to increment the local index *)
			if current_pi0.(!local_index) < max_bounds.(!local_index) then(
				(* Increment this index *)
				current_pi0.(!local_index) <- NumConst.add current_pi0.(!local_index) options#step;
				(* Reset the smaller indexes to the low bound *)
				for i = 0 to !local_index - 1 do
					current_pi0.(i) <- min_bounds.(i);
				done;
				(* Stop the loop *)
				not_is_max := false;
			)
			(* Else: try the next index *)
			else ( 
				local_index := !local_index + 1;
				(* If last index: the end! *)
				if !local_index >= dimension then(
					more_pi0 := false;
					not_is_max := false;
				)
			);
		done; (* while not is max *)
		
		(* Stop if the time limit has been reached *)
		match options#time_limit with
			| None -> ()
			| Some limit -> if (get_time()) > (float_of_int limit) then limit_reached := true;

	done; (* while more pi0 *)

	if !limit_reached && !more_pi0 then (
		match options#time_limit with
			| None -> ()
			| Some limit -> if (get_time()) > (float_of_int limit) then print_warning (
				"The time limit (" ^ (string_of_int limit) ^ " second" ^ (s_of_int limit) ^ ") has been reached. The behavioral cartography algorithm now stops, although the cartography has not been covered yet."
			);
	);
	
	let nb_tiles = DynArray.length results in
	let nb_states = (float_of_int (!nb_states)) /. (float_of_int nb_tiles) in
	let nb_transitions = (float_of_int (!nb_transitions)) /. (float_of_int nb_tiles) in
	
	(* Print the result *)
	print_message Debug_standard ("\n**************************************************");
	print_message Debug_standard ("" ^ (string_of_int nb_tiles) ^ " different constraints were computed.");
	print_message Debug_standard ("Average number of states     : " ^ (string_of_float nb_states) ^ "");
	print_message Debug_standard ("Average number of transitions: " ^ (string_of_float nb_transitions) ^ "");
	print_message Debug_standard ("**************************************************");

	(* Return a list of the generated zones *)
	let zones = DynArray.to_list results in
	zones




(** Behavioral cartography algorithm with random selection of a pi0 *)
let random_behavioral_cartography program pi0cube init_state nb =
	(* Array for the pi0 *)
	(***** TO OPTIMIZE: why create such a big array?! *****)
	let pi0_computed = Array.make nb (random_pi0 program pi0cube) in
	(* Array for the results *)
	(***** TO OPTIMIZE: why create such a big array?! *****)
	let results = Array.make nb (Convex_constraint (LinearConstraint.false_constraint ())) in
	(* Index of the iterations where we really found different constraints *)
	let interesting_interations = ref [] in
	(* Debug mode *)
	let global_debug_mode = get_debug_mode() in
	(* Prevent the printing of messages in algorithm Inverse Method *)
	let cut_messages = not (debug_mode_greater Debug_low) in
	(* Initial constraint of the program *)
	let _, init_constraint = init_state in
	(* Hide non parameters *)
	let init_constraint = LinearConstraint.hide (List.rev_append program.discrete program.clocks) init_constraint in

	(* Current iteration *)
	let i = ref 1 in
	let limit_reached = ref false in
	while !i <= nb && not !limit_reached do
		let pi0 = random_pi0 program pi0cube in

		(* Print messages *)
		print_message Debug_standard ("\n**************************************************");
		print_message Debug_standard ("BEHAVIORAL CARTOGRAPHY ALGORITHM: " ^ (string_of_int !i) ^ " / " ^ (string_of_int nb) ^ "");
		
		(* First check that it was not computed before *)
		let already_computed, index = 
			try true, index_of pi0 pi0_computed
			with Not_found -> false, 0
		in

		(* If already computed: message *)
		if already_computed then (
			print_message Debug_standard ("This pi" ^ (string_of_int !i) ^ " is equal to pi" ^ (string_of_int (index + 1)) ^ ".");
		(* Only consider new pi0 *)
		) else (
			(* Convert the pi0 to a functional representation *)
			let pi0_functional = fun parameter -> pi0.(parameter) in

			(* Check that it does not belong to any constraint *)
			if array_exists (pi0_in_returned_constraint pi0_functional) results then (
				print_message Debug_standard ("This pi" ^ (string_of_int !i) ^ " is already included in a constraint.");
				print_message Debug_standard (string_of_pi0 program pi0_functional);
				
			(* Check that it satisfies the initial constraint *)
			) else if not (LinearConstraint.is_pi0_compatible pi0_functional init_constraint) then (
				print_message Debug_standard ("This pi" ^ (string_of_int !i) ^ " does not satisfy the initial constraint of the program.");
				print_message Debug_standard (string_of_pi0 program pi0_functional);
				
			) else (
				(* Consider from here a brand new and correct pi0 *)
				print_message Debug_standard ("Considering pi" ^ (string_of_int !i) ^ " :=");
				print_message Debug_standard (string_of_pi0 program pi0_functional);

				(* Prevent the messages if needed *)
				if cut_messages then (
					set_debug_mode Debug_nodebug;
				);
				(* Compute the post *)
				let returned_constraint, graph, nb_iterations, counter = Reachability.post_star program pi0_functional init_state in
				(* Get the debug mode back *)
				set_debug_mode global_debug_mode;
				print_message Debug_standard (
					"\nK" ^ (string_of_int !i) ^ " computed using algorithm InverseMethod after "
					^ (string_of_int nb_iterations) ^ " iteration" ^ (s_of_int nb_iterations) ^ ""
					^ " in " ^ (string_of_seconds (time_from counter)) ^ ": "
					^ (string_of_int (Graph.nb_states graph)) ^ " reachable state" ^ (s_of_int (Graph.nb_states graph))
					^ " with "
					^ (string_of_int (Hashtbl.length (graph.transitions_table))) ^ " transition" ^ (s_of_int (Hashtbl.length (graph.transitions_table))) ^ ".");

				(* Add the pi0 *)
				pi0_computed.(!i - 1) <- pi0;

				(* Generate the dot graph *)
				let radical = options#program_prefix ^ "_" ^ (string_of_int !i) in
				let dot_file_name = (radical ^ ".dot") in
				let states_file_name = (radical ^ ".states") in
				let gif_file_name = (radical ^ "." ^ dot_extension) in
				generate_graph program pi0_functional graph dot_file_name states_file_name gif_file_name;
				(* Add the index to the interesting list *)
				interesting_interations := !i :: !interesting_interations;

				(* compute k0 *)
(*				let k0 =  if options#dynamic || options#union then ( returned_constraint )
				else  match returned_constraint with 
					| Convex_constraint _ -> Convex_constraint (Graph.compute_k0_destructive program graph)
					| _ -> print_error ("Internal error when getting the result of post_star in cover: 'options#dynamic' is activated but the constraint returned is not convex (type 'Convex_constraint')."); abort_program (); exit(0)

				in*)
				let k0 = returned_constraint in
												
				(* Print the constraint *)
				print_message Debug_low ("Constraint K0 computed:");
				print_message Debug_standard (string_of_returned_constraint program.variable_names k0);

				(* Add the result *)
				results.(!i - 1) <- k0;
			);
		);
		(* Stop if the time limit has been reached *)
		let _ =
		match options#time_limit with
			| None -> ()
			| Some limit -> if (get_time()) > (float_of_int limit) then limit_reached := true;
		in

		(* Increment the iteration *)
		i := !i + 1;
	done;

	if !limit_reached && !i <= nb then (
		match options#time_limit with
			| None -> ()
			| Some limit -> if (get_time()) > (float_of_int limit) then print_warning (
				"The time limit (" ^ (string_of_int limit) ^ " second" ^ (s_of_int limit) ^ ") has been reached. The behavioral cartography algorithm now stops, although the cartography has not been covered yet."
			);
	);
	
	(* Print the result *)
	print_message Debug_standard ("\n**************************************************");
	print_message Debug_standard ("" ^ (string_of_int (List.length !interesting_interations)) ^ " different constraint" ^ (s_of_int (List.length !interesting_interations)) ^ " were computed at the following iteration" ^ (s_of_int (List.length !interesting_interations)) ^ " :");
	print_message Debug_standard (string_of_list_of_string_with_sep ", " (List.map string_of_int (List.rev !interesting_interations)));
	print_message Debug_standard ("**************************************************");

	(* Return a list of generated zones *)
	let zones = List.map (fun index -> results.(index)) !interesting_interations in
	zones

;;


(**************************************************)
(**************************************************)
(* STARTING PROGRAM *)
(**************************************************)
(**************************************************)



(**************************************************)
(* Get the arguments *)
(**************************************************)

options#parse;



(**************************************************)
(**************************************************)
(* Print startup message *)
(**************************************************)
(**************************************************)

print_message Debug_standard
	( "**************************************************");
Printf.printf " *  IMITATOR %-36s *\n" version_string;
print_message Debug_standard
	( "*                                                *\n"
	^ "*   Etienne ANDRE, Ulrich KUEHNE, Romain SOULAT  *\n"
	^ "*                                   2009 - 2012  *\n"
	^ "*     Laboratoire Specification et Verification  *\n"
	^ "*                  ENS de Cachan & CNRS, France  *\n"
	^ "**************************************************");


(**************************************************)
(* Recall the arguments *)
(**************************************************)

(* File *)
print_message Debug_standard ("Model: " ^ options#file);

(* Global mode *)
let message = match options#imitator_mode with
	| Translation -> "translation"
	| Reachability_analysis -> "parametric reachability analysis"
	| Inverse_method -> "inverse method"
	| Cover_cartography -> "behavioral cartography algorithm with full coverage and step " ^ (NumConst.string_of_numconst options#step)
	| Random_cartography nb -> "behavioral cartography algorithm with " ^ (string_of_int nb) ^ " random iterations and step " ^ (NumConst.string_of_numconst options#step)
in print_message Debug_standard ("Mode: " ^ message ^ ".");


(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(* Check compatibility between options *) 
(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
if options#nb_args = 2 then(
	if options#imitator_mode = Reachability_analysis then
		print_warning ("The pi0 file " ^ options#pi0file ^ " will be ignored since this is a reachability analysis.")
	;
	if options#imitator_mode = Translation then
		print_warning ("The pi0 file " ^ options#pi0file ^ " will be ignored since this is a translation.")
	;
);

if options#acyclic && options#tree then (
	options#acyclic_unset;
	print_warning ("Ayclic mode is set although tree mode is already set. Only tree mode will be considered");
);


(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(* Recall modes *) 
(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(* Variant of the inverse method *)
if options#inclusion then
	print_message Debug_standard ("Considering fixpoint variant with inclusion.")
else
	print_message Debug_medium ("No fixpoint variant (default).");

if options#union then
	print_message Debug_standard ("Considering return variant IMunion.")
else
	print_message Debug_medium ("No IMunion return variant (default).");

if options#pi_compatible then
	print_message Debug_standard ("Considering return variant IMoriginal.")
else
	print_message Debug_medium ("No IMoriginal return variant (default).");

(* Should add a warning in case of incompatible mode (IMoriginal incompatible with IMunion) + VARIANT ROMAIN *)



(* Syntax *)
if options#fromGML then
	print_warning ("GML syntax used (experimental!).");


(* OPTIONS *)

if options#no_merging then
	print_message Debug_standard ("Merging technique of [AFS12] disabled.")
else
	print_message Debug_medium ("Merging technique of [AFS12] enabled (default).");

if options#dynamic then
	print_message Debug_standard ("Dynamic mode (optimization by RS).")
else
	print_message Debug_medium ("No dynamic mode (default).");

if options#sync_auto_detection then
	print_message Debug_standard ("Auto-detection mode for sync actions.")
else
	print_message Debug_medium ("No auto-detection mode for sync actions (default).");

if options#no_random then
	print_message Debug_standard ("No random selection for pi0-incompatible inequalities.")
else
	print_message Debug_medium ("Standard random selection for pi0-incompatible inequalities (default).");

if options#acyclic then
	print_message Debug_standard ("Acyclic mode: will only check inclusion or equality of a new state into a former state of the same iteration (graph depth).")
else
	print_message Debug_medium ("No acyclic mode (default).");

if options#tree then
	print_message Debug_standard ("Tree mode: will never check inclusion or equality of a new state into a former state.")
else
	print_message Debug_medium ("No tree mode (default).");


(* Output *)

if options#no_dot then
	print_message Debug_standard ("No graphical output.");

if options#no_log then
	print_message Debug_standard ("No log mode.");


(* LIMIT OF POST *)
let _ =
match options#post_limit with
	| None -> print_message Debug_medium "Considering no limit for the depth of the Post operation (default)."
	| Some limit -> print_warning ("Considering a limit of " ^ (string_of_int limit) ^ " for the depth of the Post operation.")
in ();

(* TIME LIMIT *)
let _ =
match options#time_limit with
	| None -> print_message Debug_medium "Considering no time limit (default)."
	| Some limit -> print_warning ("The program will try to stop after " ^ (string_of_int limit) ^ " seconds.")
in ();


(* Verification of incompatibilities between options *)

if (options#imitator_mode = Reachability_analysis || options#imitator_mode = Translation) && (options#union || options#pi_compatible) then
	print_warning ("The program will be launched in reachability mode; options regarding to the variant of the inverse method will thus be ignored.");

if (options#imitator_mode = Reachability_analysis || options#imitator_mode = Translation || options#imitator_mode = Inverse_method) && (NumConst.neq options#step NumConst.one) then
	print_warning ("The program will be launched in reachability mode; option regarding to the step of the cartography algorithm will thus be ignored.");




(**************************************************)
(* Timed mode *)
(**************************************************)
if options#timed_mode then (
	(* Debug *)
	print_message Debug_standard ("Timed mode is on.");
	(* Set the timed mode *)
	set_timed_mode ();
) else (
	print_message Debug_medium ("Timed mode is off (default).");
);


(**************************************************)
(* Parsing *)
(**************************************************)

(* Parsing the main program *)
print_message Debug_low ("Considering file " ^ options#file ^ ".");
let parsing_structure = 
	(* Branching between 2 input syntaxes *)
	if options#fromGML then
		try parser_lexer GMLParser.main GMLLexer.token options#file
		with InvalidProgram -> (print_error ("GML input contains error. Please check it again."); abort_program (); exit 0)
	else parser_lexer ImitatorParser.main ImitatorLexer.token options#file
in 

print_message Debug_medium ("Considering program prefix " ^ options#program_prefix ^ ".");

if options#imitator_mode != Reachability_analysis && options#imitator_mode != Translation then
	print_message Debug_low ("Considering reference valuation in file " ^ options#pi0file ^ ".");

(* Pi0 Parsing *)
let pi0_parsed, pi0cube_parsed =
	(* Depending on which operation we are performing *)
	match options#imitator_mode with
		(* If translation: no pi0 *)
		| Translation -> [], []
		(* If reachability: no pi0 *)
		| Reachability_analysis -> [], []
		(* Inverse method : pi0 *)
		| Inverse_method -> parser_lexer Pi0Parser.main Pi0Lexer.token options#pi0file, []
		(* Cartography : pi0cube *)
		| _ -> [], parser_lexer Pi0CubeParser.main Pi0CubeLexer.token options#pi0file
in

print_message Debug_standard ("\nParsing done " ^ (after_seconds ()) ^ ".");
(** USELESS, even increases memory x-( **)
(* Gc.major (); *)

(**************************************************)
(* Conversion to an abstract program *)
(**************************************************)

let program, pi0, pi0cube = 
try (
	ProgramConverter.abstract_program_of_parsing_structure
		parsing_structure pi0_parsed pi0cube_parsed options
) with 
	| InvalidProgram -> (print_error ("The input program contains errors. Please check it again."); abort_program (); exit 0)
	| ProgramConverter.InvalidPi0 -> (print_error ("The input pi_0 file contains errors. Please check it again."); abort_program (); exit 0)
	| InternalError e -> (print_error ("Internal error: " ^ e ^ "\nPlease insult the developers."); abort_program (); exit 0)
	in

let gc_stat = Gc.stat () in
let nb_words = gc_stat.minor_words +. gc_stat.major_words -. gc_stat.promoted_words in
let nb_ko = nb_words *. 4.0 /. 1024.0 in
print_message Debug_standard ("Memory for abstract program: " ^ (string_of_float nb_ko) ^ " KB (i.e., " ^ (string_of_float nb_words) ^ " words)");

(* With or without stopwatches *)
if program.has_stopwatches then
	print_message Debug_standard ("The model contains stopwatches.\n")
else
	print_message Debug_standard ("The model is purely timed (no stopwatches).\n");


	
(**************************************************)
(* Debug print: program *)
(**************************************************)
if debug_mode_greater Debug_total then
	print_message Debug_total ("\nProgram:\n" ^ (ImitatorPrinter.string_of_program program) ^ "\n");


(**************************************************)
(* Case translation *)
(**************************************************)

(* Translation to CLP (work in progress) *)
if options#pta2clp then(
	print_message Debug_standard ("Translating program to CLP.");
	print_warning ("Work in progress!!!!");
	print_message Debug_standard ("\nProgram in CLP:\n" ^ (PTA2CLP.string_of_program program) ^ "\n");
	terminate_program()
	);

(* Translation to GML (experimental) *)
if options#pta2gml then(
	print_message Debug_standard ("Translating program to GML.");
	print_warning ("Experimental translation!");
	let translated_model = PTA2GML.string_of_program program in
	let gml_file = program.options#file ^ ".gml" in
	print_message Debug_total ("\n" ^ translated_model ^ "\n");
	(* Write *)
	write_to_file gml_file translated_model;
	terminate_program()
	);



(**************************************************)
(* Initial state *)
(**************************************************)

(* Print the initial state *)
print_message Debug_medium ("\nInitial state:\n" ^ (ImitatorPrinter.string_of_state program (program.initial_location, program.initial_constraint)) ^ "\n");

(* Check the satisfiability *)
if not (LinearConstraint.is_satisfiable program.initial_constraint) then (
	print_warning "The initial constraint of the program is not satisfiable.";
	terminate_program();
)else(
	print_message Debug_total ("\nThe initial constraint of the program is satisfiable.");
);

(* Get the initial state after time elapsing *)
let init_state_after_time_elapsing = Reachability.create_initial_state program in
let _, initial_constraint_after_time_elapsing = init_state_after_time_elapsing in

(*(* COMPARISON *)
let init_state_after_time_elapsing2 = Reachability.create_initial_state2 program in
let _, initial_constraint_after_time_elapsing2 = init_state_after_time_elapsing2 in

if(LinearConstraint.is_equal initial_constraint_after_time_elapsing initial_constraint_after_time_elapsing2) then(
	print_message Debug_standard ("\n INITIAL STATE OK :o)");
	terminate_program ();
)else (
	print_error ("\n INITIAL STATES DIFFERENT.");
	print_message Debug_standard ("\n 1) \n" ^ (LinearConstraint.string_of_linear_constraint program.variable_names initial_constraint_after_time_elapsing));
	print_message Debug_standard ("\n 2) \n" ^ (LinearConstraint.string_of_linear_constraint program.variable_names initial_constraint_after_time_elapsing2));
	abort_program ();
);*)


(* Check the satisfiability *)
if not (LinearConstraint.is_satisfiable initial_constraint_after_time_elapsing) then (
	print_warning "The initial constraint of the program after time elapsing is not satisfiable.";
	terminate_program();
)else(
	print_message Debug_total ("\nThe initial constraint of the program after time elapsing is satisfiable.");
);
(* Print the initial state after time elapsing *)
print_message Debug_medium ("\nInitial state after time-elapsing:\n" ^ (ImitatorPrinter.string_of_state program init_state_after_time_elapsing) ^ "\n");



(**************************************************)
(* EXPERIMENTAL: branch and bound *)
(**************************************************)

if options#imitator_mode = Inverse_method && options#branch_and_bound then(
	Reachability.branch_and_bound program pi0 init_state_after_time_elapsing;
	terminate_program();
);




(**************************************************)
(* Execute IMITATOR II *)
(**************************************************)

let zones =
match options#imitator_mode with
	| Translation -> raise (InternalError "Translation can't be executed; program should have terminated before.");
	(* Perform reachability analysis or inverse Method *)
	| Reachability_analysis | Inverse_method ->
		let returned_constraint, reachability_graph, _, _ =
			Reachability.post_star program pi0 init_state_after_time_elapsing
		in
		(* Generate the DOT graph *)
		print_message Debug_high "Generating the dot graph";
		let dot_file_name = (options#program_prefix ^ ".dot") in
		let states_file_name = (options#program_prefix ^ ".states") in
		let gif_file_name = (options#program_prefix ^ "." ^ dot_extension) in
		generate_graph program pi0 reachability_graph dot_file_name states_file_name gif_file_name;
		
		(* MODE INVERSE METHOD *)
		if options#imitator_mode = Inverse_method then (
			(* If convex constraint (i.e., if no union mode) *)
			if not options#union then(
(*				(* compute k0 *)	
				let k0 =  if options#dynamic then ( if options#pi_compatible then ( 
							let (_ , k_constraint) = get_state reachability_graph 0 in
							(LinearConstraint.hide program.clocks_and_discrete k_constraint);
						) else(
					match returned_constraint with
					| Convex_constraint k_prime -> k_prime
					| _ -> print_error ("Internal error when getting the result of post_star: 'options#dynamic' is activated but the constraint returned is not convex (type 'Convex_constraint')."); abort_program (); exit(0)
				))
				else ( 
						if options#pi_compatible then ( 
							let (_ , k_constraint) = get_state reachability_graph 0 in
							(LinearConstraint.hide program.clocks_and_discrete k_constraint);
						) else(Graph.compute_k0_destructive program reachability_graph)
					)
				in*)
				(* print it *)
				print_message Debug_standard ("\nFinal constraint K0 :");
(* 				print_message Debug_standard (LinearConstraint.string_of_linear_constraint program.variable_names k0);            		 *)
				print_message Debug_standard (string_of_returned_constraint program.variable_names returned_constraint);
			) else (
			(* Else (i.e., if union mode) *)

				(* print it *)
				print_message Debug_standard ("\nFinal constraint K0 under disjunctive form :");
				print_message Debug_standard (string_of_returned_constraint program.variable_names returned_constraint);
				
			);
		);
		[ ]


	| Random_cartography nb ->
	(* Behavioral cartography algorithm with random iterations *)
		random_behavioral_cartography program pi0cube init_state_after_time_elapsing nb;

	| Cover_cartography ->
	(* Behavioral cartography algorithm with full coverage *)
		cover_behavioral_cartography program pi0cube init_state_after_time_elapsing
		
in ();

let _ =
if options#cart then ( 
		print_message Debug_standard ("Cartography started " ^ (after_seconds ()) ^ "\n");
 		cartography program pi0cube zones (options#program_prefix ^ "_cart")
	) else (
		print_message Debug_medium "No graph for the cartography."
	)
in ();

(**************************************************)
(* Bye bye! *)
(**************************************************)

(* Reachability.print_stats (); *)

terminate_program()
