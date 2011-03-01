(***************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2009/09/07
 * Last modified: 2010/04/28
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

open Ppl_ocaml

exception NotFound
exception Found of int

(**************************************************

A FAIRE
[ ] rétablir l'appel à IMITATOR pour graphe d'accessibilité seulement
[ ] rétablir l'appel à IMITATOR pour graphe d'accessibilité non temporisé seulement
[ ] eviter les etats degeneres (avec "faux") : arrive dans le cas ou aucun etat n'est genere (init deja pas satisfiable) --> bouger le test de satisfiabilite dans le demarrage de InverseMethod ?
[ ] probleme avec la variable D, non ?? (pas vraiment, juste pour l'affichage)

 OPTIMISATIONS A FAIRE POUR L'EXECUTION

[ ] garder en memoire les p-contraintes. Avantage : plus rapide lors de l'intersection finale. Inconvenient : memoire en + ! Astuce : fichier temporaire ?
[ ] METTRE DES TABLES DE HASH et non des tableaux pour transitions, gardes, invariants, etc. Avantage : (beaucoup) moins de choses en memoire, execution a peine plus lente.
[ ] separer les elements de AbstractImitatorFile en (au moins) 'automata', 'variables' et 'env'


 OPTIMISATIONS A FAIRE POUR LE POST

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
	Random.self_init();
	for i = 0 to program.nb_parameters - 1 do
		let a, b, _ = pi0.(i) in
		(* Generate a random value in the interval *)
		let random_value = NumConst.random a b in
		(* Debug *)
		print_message Debug_medium ("Generating randomly value '" ^ (NumConst.string_of_numconst random_value) ^ "' for parameter '" ^ (program.variable_names i) ^ "'.");
		(* Convert to a num *)
		random_pi0.(i) <- random_value;
	done;
	(* Return the result as an array *)
	random_pi0



(**************************************************)
(* Functions to interact with Dot *)
(**************************************************)
(* Create a gif graph using dot *)
let generate_graph pi0 reachability_graph dottyfier dot_file_name states_file_name gif_file_name =
	if not options#no_dot then (
		(* Create the input file *)
		print_message Debug_total ("Creating input file for dot...");
		let dot_program, states = dottyfier reachability_graph in
		(* Write dot file *)
		print_message Debug_total ("Writing to dot file...");
		write_to_file dot_file_name dot_program;
	
		(* Generate gif file using dot *)
		print_message Debug_total ("Calling dot...");
		let command_result = Sys.command (dot_command ^ " -T" ^ dot_extension ^ " " ^ dot_file_name ^ " -o " ^ gif_file_name ^ "") in
		print_message Debug_total ("Result of the 'dot' command: " ^ (string_of_int command_result));
		(* Removing dot file *)
		print_message Debug_total ("Removing dot file...");
		Sys.remove dot_file_name;
	
		(* Write states file *)
		print_message Debug_total ("Writing to file for file description...");
		write_to_file states_file_name states;
		()
  )



(**************************************************)
(* BEHAVIORAL CARTOGRAPHY ALGORITHM functions *)
(**************************************************)

(** Behavioral cartography algorithm with full coverage of V0 *)
let cover_behavioral_cartography program pi0cube init_state =
	(* Dimension of the system *)
	let dimension = Array.length pi0cube in
	(* Min & max bounds for the parameters *)
	let min_bounds = Array.map (fun (low, high, step) -> low) pi0cube in
	let max_bounds = Array.map (fun (low, high, step) -> high) pi0cube in
	let step_size  = Array.map (fun (low, high, step) -> step) pi0cube in
	
	(* Initial constraint of the program *)
	let _, init_constraint = program.init in
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
	
	let badlist = ref [] in
	
	(* Iterate on all the possible pi0 *)
	let more_pi0 = ref true in
	let limit_reached = ref false in
	while !more_pi0 && not !limit_reached do
		(* functional version of current pi0 *)
		let pi0 = fun parameter -> current_pi0.(parameter) in
		(* store in global table *)
		Program.set_pi0 pi0;
		
		(* Check that it does not belong to any constraint *)
		if dynArray_exists (LinearConstraint.is_pi0_compatible pi0) results then (
			if debug_mode_greater Debug_medium then (
				print_message Debug_medium "The following pi0 is already included in a constraint.";
				print_message Debug_medium (string_of_pi0 pi0);
			);
		(* Check that it satisfies the initial constraint *)
		) else if not (LinearConstraint.is_pi0_compatible pi0 init_constraint) then (
			if debug_mode_greater Debug_medium then (
				print_message Debug_medium "The following pi0 does not satisfy the initial constraint of the program.";
				print_message Debug_medium (string_of_pi0 pi0);
			);
		) else (
			(* Iterate *)
			current_iteration := !current_iteration + 1;

			(* Debug messages *)
			print_message Debug_standard ("\n**************************************************");
			print_message Debug_standard ("BEHAVIORAL CARTOGRAPHY ALGORITHM: " ^ (string_of_int !current_iteration) ^ "");
			print_message Debug_standard ("Considering the following pi" ^ (string_of_int !current_iteration));
			print_message Debug_standard (string_of_pi0 pi0);
			
			(* Prevent the debug messages *)
			if not (debug_mode_greater Debug_medium) then
				set_debug_mode Debug_nodebug;
			(* Compute the post and the constraint *)
			let k, nb_iterations, counter = Reachability.post_star init_state in
			let graph = Program.get_reachability_graph () in
			(* Get the debug mode back *)
			set_debug_mode global_debug_mode;
			(* Update the counters *)
			nb_states := !nb_states + (Graph.nb_states graph);
			nb_transitions := !nb_transitions + (Graph.nb_transitions graph);
			(* Print message *)
			print_message Debug_standard (
				"\nK" ^ (string_of_int (!current_iteration)) ^ " computed using algorithm InverseMethod after "
				^ (string_of_int nb_iterations) ^ " iteration" ^ (s_of_int nb_iterations) ^ ""
				^ " in " ^ (string_of_seconds (time_from counter)) ^ ": "
				^ (string_of_int (Graph.nb_states graph)) ^ " reachable state" ^ (s_of_int (Graph.nb_states graph))
				^ " with "
				^ (string_of_int (Graph.nb_transitions graph)) ^ " transition" ^ (s_of_int (Graph.nb_transitions graph)) ^ ".");
			
			let k0 = Graph.compute_k0_destructive graph in
			LinearConstraint.intersection_assign k0 [k];
			let k0 = [k0] in
			
			(* Add the pi0 and the constraint *)
			DynArray.add pi0_computed pi0;
			List.iter (DynArray.add results) k0;

			(* Print the constraint *)
			let bad = Graph.bad_states_reachable graph in
			if bad then badlist := (DynArray.length results - 1) :: !badlist;
			let bad_string = if bad then "bad" else "good" in			
			print_message Debug_low ("Constraint K0 computed:");
			print_message Debug_standard (bad_string ^ " zone");
			List.iter (fun k0 -> print_message Debug_standard (LinearConstraint.string_of_linear_constraint program.variable_names k0)) k0;

			(* Generate the dot graph (only if K0 <> false) *)
			if LinearConstraint.is_satisfiable (List.hd k0) then (
				let radical = options#program_prefix ^ "_" ^ (string_of_int !current_iteration) in
				let dot_file_name = (radical ^ ".dot") in
				let states_file_name = (radical ^ ".states") in
				let gif_file_name = (radical ^ "." ^ dot_extension) in
				generate_graph pi0 graph Graph.dot_of_graph dot_file_name states_file_name gif_file_name;
			);
		); (* else if new pi0 *)
				
		(* Find the next pi0 *)
		let not_is_max = ref true in
		let local_index = ref 0 in
		while !not_is_max do
			(* Try to increment the local index *)
			if current_pi0.(!local_index) < max_bounds.(!local_index) then(
				(* Increment this index *)
				current_pi0.(!local_index) <- NumConst.add current_pi0.(!local_index) step_size.(!local_index);
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

		Gc.major ()
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
	
	(* plot the cartography *)
	let zones = DynArray.to_list results in
	Graphics.cartography zones !badlist 2 options#program_prefix;
	
	(* compute coverage of v0 *)
	let cov = 100.0 *. Graphics.coverage zones in
	print_message Debug_standard ("coverage of v0 rectangle: " ^ (string_of_float cov) ^ " %")



(** Behavioral cartography algorithm with random selection of a pi0 *)
let random_behavioral_cartography program pi0cube init_state nb =
	(* Array for the pi0 *)
	(***** TO OPTIMIZE: why create such a big array?! *****)
	let pi0_computed = Array.make nb (random_pi0 program pi0cube) in
	(* Array for the results *)
	(***** TO OPTIMIZE: why create such a big array?! *****)
	let results = Array.make nb (LinearConstraint.false_constraint ()) in
	(* Index of the iterations where we really found different constraints *)
	let interesting_interations = ref [] in
	(* Debug mode *)
	let global_debug_mode = get_debug_mode() in
	(* Prevent the printing of messages in algorithm Inverse Method *)
	let cut_messages = not (debug_mode_greater Debug_low) in
	(* Initial constraint of the program *)
	let _, init_constraint = program.init in
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
			(* store in global table *)
			Program.set_pi0 pi0_functional;

			(* Check that it does not belong to any constraint *)
			if array_exists (LinearConstraint.is_pi0_compatible pi0_functional) results then (
				print_message Debug_standard ("This pi" ^ (string_of_int !i) ^ " is already included in a constraint.");
				print_message Debug_standard (string_of_pi0 pi0_functional);
				
			(* Check that it satisfies the initial constraint *)
			) else if not (LinearConstraint.is_pi0_compatible pi0_functional init_constraint) then (
				print_message Debug_standard ("This pi" ^ (string_of_int !i) ^ " does not satisfy the initial constraint of the program.");
				print_message Debug_standard (string_of_pi0 pi0_functional);
				
			) else (
				(* Consider from here a brand new and correct pi0 *)
				print_message Debug_standard ("Considering pi" ^ (string_of_int !i) ^ " :=");
				print_message Debug_standard (string_of_pi0 pi0_functional);

				(* Prevent the messages if needed *)
				if cut_messages then (
					set_debug_mode Debug_nodebug;
				);
				(* Compute the post *)
				let k, nb_iterations, counter = Reachability.post_star init_state in
				let graph = Program.get_reachability_graph () in
				(* Get the debug mode back *)
				set_debug_mode global_debug_mode;
				print_message Debug_standard (
					"\nK" ^ (string_of_int !i) ^ " computed using algorithm InverseMethod after "
					^ (string_of_int nb_iterations) ^ " iteration" ^ (s_of_int nb_iterations) ^ ""
					^ " in " ^ (string_of_seconds (time_from counter)) ^ ": "
					^ (string_of_int (Graph.nb_states graph)) ^ " reachable state" ^ (s_of_int (Graph.nb_states graph))
					^ " with "
					^ (string_of_int (Graph.nb_transitions graph)) ^ " transition" ^ (s_of_int (Graph.nb_transitions graph)) ^ ".");

				let k0 = Graph.compute_k0_destructive graph in
				LinearConstraint.intersection_assign k0 [k];
				let k0 = [k0] in

				(* Add the pi0 *)
				pi0_computed.(!i - 1) <- pi0;

				(* Print the constraint *)
				print_message Debug_low ("Constraint K0 computed:");
				List.iter (fun k0 -> print_message Debug_standard (LinearConstraint.string_of_linear_constraint program.variable_names k0)) k0;
				(* Generate the dot graph *)
				let radical = options#program_prefix ^ "_" ^ (string_of_int !i) in
				let dot_file_name = (radical ^ ".dot") in
				let states_file_name = (radical ^ ".states") in
				let gif_file_name = (radical ^ "." ^ dot_extension) in
				generate_graph pi0_functional graph Graph.dot_of_graph dot_file_name states_file_name gif_file_name;
				(* Add the index to the interesting list *)
				interesting_interations := !i :: !interesting_interations;
				(* Add the result *)
				results.(!i - 1) <- List.hd k0;
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

	()


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
(* Hello world! *)
(**************************************************)
(**************************************************)

print_message Debug_standard
	( "**************************************************");
Printf.printf " *  IMITATOR %-36s *\n" version_string;	
print_message Debug_standard
	( "*                                 Etienne ANDRE  *\n"
	^ "*                                   2009 - 2010  *\n"
	^ "*     Laboratoire Specification et Verification  *\n"
	^ "*                  ENS de Cachan & CNRS, France  *\n"
	^ "**************************************************");


(**************************************************)
(* Recall the arguments *)
(**************************************************)

if options#inclusion then
	print_message Debug_medium ("Considering inclusion mode.");

if options#pre then
	print_message Debug_medium ("Invert automaton for backward reachability");

if options#sync_auto_detection then
	print_message Debug_medium ("Auto-detection mode for sync actions.");

if options#no_random then
	print_message Debug_medium ("No random selection for pi0-incompatible inequalities.");

if options#no_dot then
	print_message Debug_medium ("No graphical output.");

if options#no_log then
	print_message Debug_medium ("No log mode.");

if options#plot_vars <> [] then
	List.iter (fun (x, y) ->
		print_message Debug_standard ("Plot variables " ^ x ^ ", " ^ y); 
	) options#plot_vars;
	

(* Print the mode *)
let message = match options#imitator_mode with
	| Reachability_analysis -> "parametric reachability analysis"
	| AbstractReachability -> "abstract reachability analysis"
	| Inverse_method -> "inverse method"
	| Cover_cartography -> "behavioral cartography algorithm with full coverage"
	| Random_cartography nb -> "behavioral cartography algorithm with " ^ (string_of_int nb) ^ " random iterations"
in print_message Debug_standard ("Mode: " ^ message ^ ".");

(* LIMIT OF POST *)
let _ =
match options#post_limit with
	| None -> print_message Debug_medium "Considering no limit for the depth of the Post operation."
	| Some limit -> print_warning ("Considering a limit of " ^ (string_of_int limit) ^ " for the depth of the Post operation.")
in ();

(* TIME LIMIT *)
let _ =
match options#time_limit with
	| None -> print_message Debug_medium "Considering no time limit."
	| Some limit -> print_warning ("The program will try to stop after " ^ (string_of_int limit) ^ " seconds.")
in ();


(**************************************************)
(* Timed mode *)
(**************************************************)
if options#timed_mode then (
	(* Debug *)
	print_message Debug_standard ("Timed mode is on.");
	(* Set the timed mode *)
	set_timed_mode ();
) else (
	print_message Debug_low ("Timed mode is off.");
);


(**************************************************)
(* Parsing *)
(**************************************************)

(* Parsing the main program *)
print_message Debug_low ("Considering file " ^ options#file ^ ".");
let parsing_structure = parser_lexer ImitatorParser.main ImitatorLexer.token options#file in

print_message Debug_medium ("Considering program prefix " ^ options#program_prefix ^ ".");

if options#imitator_mode != Reachability_analysis && options#imitator_mode != AbstractReachability then
	print_message Debug_low ("Considering reference valuation in file " ^ options#pi0file ^ ".");

(* Pi0 Parsing *)
let pi0_parsed, pi0cube_parsed =
	(* Depending on which operation we are performing *)
	match options#imitator_mode with
		(* If reachability: no pi0 *)
		| AbstractReachability
		| Reachability_analysis -> [], []
		(* Inverse method : pi0 *)
		| Inverse_method -> parser_lexer Pi0Parser.main Pi0Lexer.token options#pi0file, []
		(* Cartography : pi0cube *)
		| _ -> [], parser_lexer Pi0CubeParser.main Pi0CubeLexer.token options#pi0file
in

Gc.major ();
print_message Debug_standard ("\nParsing done " ^ (after_seconds ()) ^ ".");


(**************************************************)
(* Conversion to an abstract program *)
(**************************************************)

let program, pi0, pi0cube = 
try (
	ProgramConverter.abstract_program_of_parsing_structure
		parsing_structure pi0_parsed pi0cube_parsed 
			~acyclic:options#acyclic 
			~sync_auto_detection:options#sync_auto_detection
			~inclusion_mode:options#inclusion
			~union_mode:options#union
			~no_random:options#no_random
			~with_parametric_log:options#with_parametric_log			
			options#imitator_mode options#file
) with 
	| ProgramConverter.InvalidProgram -> (print_error ("The input program contains errors. Please check it again."); abort_program (); exit 0)
	| ProgramConverter.InvalidPi0 -> (print_error ("The input pi_0 file contains errors. Please check it again."); abort_program (); exit 0)
	| InternalError e -> (print_error ("Internal error: " ^ e ^ "\nPlease insult the developers."); abort_program (); exit 0)
	in

Gc.major ();
print_message Debug_standard ("Program checked and converted " ^ (after_seconds ()) ^ ".\n");

let program = if not options#pre then program else (
	try (	
		ProgramInverter.invert program
	) with InternalError e -> (print_error e; abort_program (); exit 0) 
) in 

if options#pre then (
	Gc.major ();
	print_message Debug_standard ("Automata reversed " ^ (after_seconds ()) ^ ".\n");
	if debug_mode_greater Debug_high then print_message Debug_high ("\nProgram:\n" ^ (ImitatorPrinter.string_of_program program) ^ "\n")				
);

(* store global references *)
Program.set_program program;
Program.set_pi0 pi0;
Program.set_pi0cube pi0cube;
Program.set_options options;


(**************************************************)
(* Debug print: program *)
(**************************************************)
if debug_mode_greater Debug_total then (
	print_message Debug_total ("\nProgram:\n" ^ (ImitatorPrinter.string_of_program program) ^ "\n")
 );

(**************************************************)
(* Initial state *)
(**************************************************)

let (init_loc, init_constraint) = program.init in
(* Print the initial state *)
print_message Debug_medium ("\nInitial state:\n" ^ (ImitatorPrinter.string_of_state program.init) ^ "\n");

(* Check the satisfiability *)
if not (LinearConstraint.is_satisfiable init_constraint) then (
	print_warning "The initial constraint of the program is not satisfiable.";
	terminate_program();
)else(
	print_message Debug_total ("\nThe initial constraint of the program is satisfiable.");
);

(* Get the initial state after time elapsing *)
let init_state_after_time_elapsing = Reachability.create_initial_state () in
let _, initial_constraint_after_time_elapsing = init_state_after_time_elapsing in
(* Check the satisfiability *)
if not (LinearConstraint.is_satisfiable initial_constraint_after_time_elapsing) then (
	print_warning "The initial constraint of the program after time elapsing is not satisfiable.";
	terminate_program();
)else(
	print_message Debug_total ("\nThe initial constraint of the program after time elapsing is satisfiable.");
);
(* Print the initial state after time elapsing *)
print_message Debug_medium ("\nInitial state after time-elapsing:\n" ^ (ImitatorPrinter.string_of_state init_state_after_time_elapsing) ^ "\n");



(* function for plotting reachable states *)	
let make_plot reachability_graph plotter =
		(* reverse lookup function *) 
		let index_of x =
			try (
				for i = 0 to program.nb_variables do 
				  if (program.variable_names i) = x then raise (Found i)
				done;
				raise NotFound				
			) with
				| Found i -> i in 
		(* construct pairs of plot variables *)
		let plot_pairs = List.fold_left (fun ps (x,y) -> 
			try (
				let xi = index_of x in
				let yi = index_of y in
				(xi, yi) :: ps
			)	with NotFound -> ps
		) [] options#plot_vars in
		(* do the plotting *)
		let i = ref 0 in
		List.iter (fun (x,y) -> 
			let x_name = program.variable_names x in
			let y_name = program.variable_names y in
			print_message Debug_standard (
				"Plotting reachable states projected on variables " ^ x_name ^ ", " ^ y_name);
			let plot_file_name = (options#program_prefix ^ ".plot_" ^ x_name ^ "_" ^ y_name) in
			let plot = plotter x y reachability_graph in
			write_to_file plot_file_name plot;
			let img_file_name = plot_file_name ^ ".png" in
			let cmd = "graph -Tpng --bitmap-size 1600x1600 -B -C -q0.2"
				^ " -m " ^ (string_of_int (!i + 1)) 
				^ " -X \"" ^ x_name ^ "\""
			  ^ " -Y \"" ^ y_name ^ "\" " 
				^ (try (
						let min, max = Hashtbl.find options#plot_limits x_name in
						" -x " ^ min ^ " " ^ max
						) with Not_found -> "")
				^ (try (
						let min, max = Hashtbl.find options#plot_limits y_name in
						" -y " ^ min ^ " " ^ max
						) with Not_found -> "")
				^ " " ^ plot_file_name 
				^ " > " ^ img_file_name in				
			let result = Sys.command cmd in
			print_message Debug_medium ("return value of system call: " ^ (string_of_int result));
			i := (!i + 1) mod 5 
		) plot_pairs in


(**************************************************)
(* Execute IMITATOR II *)
(**************************************************)

if debug_mode_greater Debug_medium then
	List.iter (fun ineq -> 
		print_message Debug_medium ("pred: " ^ (LinearConstraint.string_of_linear_inequality program.variable_names ineq))
	) program.predicates;



let _ =
match options#imitator_mode with
	(* Perform reachability analysis or inverse Method *)
	| AbstractReachability ->
		
		let stop = ref false in
		let nb_refinements = ref 0 in
		while not !stop do 
			let _, _, _ =
				Reachability.post_star init_state_after_time_elapsing
			in 
			let reachability_graph = Program.get_abstract_reachability_graph () in
			let path = Graph.get_counterexample reachability_graph in
							
			begin 
				match path with
				| None -> begin
						(* no more counterexamples *)
						print_message Debug_standard "System verified SUCCESSFULLY!\n";
						stop := true
					end
				| Some path -> begin
						let new_pred = Reachability.verify_path init_state_after_time_elapsing path in
						match new_pred with
							| None -> begin
									(* true counterexample *)
									print_message Debug_standard "System verification FAILED!\n";
									stop := true						
								end
							| Some pred -> begin
									(* next refinement step *)
									nb_refinements := !nb_refinements + 1;
									(* reached limit? *)
									if match options#cegar_limit with
										| None -> false
										| Some limit -> limit < !nb_refinements
									then begin
										stop := true;
										print_warning ("The limit number of refinements has been reached. CEGAR now stops. The analysis might be incomplete.")
									end else begin 
										let new_program = { program with predicates = pred :: (Program.get_program ()).predicates } in
										Program.set_program new_program;
									end
								end;
					end;
			end;			
		done;
	
		print_message Debug_standard "Final set of predicates:";
		let program = Program.get_program () in
		let i = ref 0 in
		List.iter (fun p -> 
			print_message Debug_standard ("p" ^ (string_of_int !i) ^ ": " ^ (LinearConstraint.string_of_linear_inequality program.variable_names p));
			i := !i + 1		
		) program.predicates;
		
		if program.nb_parameters > 0 then begin
			let ainits = PredicateAbstraction.abstract program.predicates init_state_after_time_elapsing in
			let cinits = List.map (PredicateAbstraction.concretize program.predicates) ainits in
			List.iter (fun (_, constr) -> 
				let p_constr = LinearConstraint.hide program.clocks constr in
				print_message Debug_standard "Abstract initial state projected on parameters: ";
				print_message Debug_standard (LinearConstraint.string_of_linear_constraint program.variable_names p_constr);
			) cinits;
		end;

		(* Generate the DOT graph *)
		print_message Debug_high "Generating the dot graph";
		let dot_file_name = (options#program_prefix ^ ".dot") in
		let states_file_name = (options#program_prefix ^ ".states") in
		let gif_file_name = (options#program_prefix ^ "." ^ dot_extension) in
		let reachability_graph = Program.get_abstract_reachability_graph () in
		generate_graph pi0 reachability_graph Graph.dot_of_abstract_graph dot_file_name states_file_name gif_file_name;

		(* Plot all reachable states projected on the selected variables *)
		make_plot reachability_graph Graph.plot_abstract_graph;
 		
	| Reachability_analysis | Inverse_method -> 
		let k, _, _ =
			Reachability.post_star init_state_after_time_elapsing
		in
				
		(* Plot all reachable states projected on the selected variables *)
		let reachability_graph = Program.get_reachability_graph () in
		make_plot reachability_graph Graph.plot_graph;
	
		(* Generate the DOT graph *)
		print_message Debug_high "Generating the dot graph";
		let dot_file_name = (options#program_prefix ^ ".dot") in
		let states_file_name = (options#program_prefix ^ ".states") in
		let gif_file_name = (options#program_prefix ^ "." ^ dot_extension) in
		generate_graph pi0 reachability_graph Graph.dot_of_graph dot_file_name states_file_name gif_file_name;
		
		if options#imitator_mode = Inverse_method then (
			let k0 = Graph.compute_k0_destructive reachability_graph in
			LinearConstraint.intersection_assign k0 [k];
			print_message Debug_standard ("\nFinal constraint K0 :");
			print_message Debug_standard (LinearConstraint.string_of_linear_constraint program.variable_names k0);
		)
		
	| Random_cartography nb ->
	(* Behavioral cartography algorithm with random iterations *)
		random_behavioral_cartography program pi0cube init_state_after_time_elapsing nb;

	| Cover_cartography ->
	(* Behavioral cartography algorithm with full coverage *)
		cover_behavioral_cartography program pi0cube init_state_after_time_elapsing;

in ();		

(**************************************************)
(* Bye bye! *)
(**************************************************)

terminate_program()
