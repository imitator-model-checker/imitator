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

(* No log at all *)
let no_log = ref false

(* No graphical output using dot *)
let no_dot = ref false

(* Program prefix for log files *)
let program_prefix = ref ""

(* Limit for the iterations in the post operation *)
let post_limit = ref None

(* Time limit for the program *)
let time_limit = ref None



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
(*	(* Return the result as a function *)
	fun parameter -> random_pi0.(parameter)*)
	(* Return the result as an array *)
	random_pi0



(**************************************************)
(* Functions to interact with Dot *)
(**************************************************)
(* Create a gif graph using dot *)
let generate_graph program pi0 reachability_graph dot_file_name states_file_name gif_file_name =
	(* Create the input file *)
	print_message Debug_total ("Creating input file for dot...");
	let dot_program, states = Graph.dot_of_graph program pi0 reachability_graph in
	(* Write dot file *)
	if not !no_dot then (
		print_message Debug_total ("Writing to dot file...");
		write_to_file dot_file_name dot_program;

	(* Generate gif file using dot *)
		print_message Debug_total ("Calling dot...");
		let command_result = Sys.command (dot_command ^ " -T" ^ dot_extension ^ " " ^ dot_file_name ^ " -o " ^ gif_file_name ^ "") in
		print_message Debug_total ("Result of the 'dot' command: " ^ (string_of_int command_result));
		(* Removing dot file *)
		print_message Debug_total ("Removing dot file...");
		Sys.remove dot_file_name;
	);
	(* Write states file *)
	if not !no_log then (
		print_message Debug_total ("Writing to file for file description...");
		write_to_file states_file_name states;
	);
	()



(**************************************************)
(* Post functions *)
(**************************************************)

(*--------------------------------------------------*)
(* Compute the invariant associated to a location *)
(*--------------------------------------------------*)
let compute_invariant program location =
	let invariants = List.map (fun automaton_index ->
		(* Get the current location *)
		let location_index = Automaton.get_location location automaton_index in
		(* Compute the invariant *)
		program.invariants automaton_index location_index
	) program.automata in
	(* Perform the intersection *)
	LinearConstraint.intersection invariants


(*--------------------------------------------------*)
(* Create a fresh constraint of the form 'D = d' for any discrete variable D with value d *)
(*--------------------------------------------------*)
let instantiate_discrete discrete_values =
	let inequalities = List.map (fun (discrete_index, discrete_value) ->
		(* Create a linear term 'D - d' *)
		let linear_term = LinearConstraint.make_linear_term
			[(NumConst.one, discrete_index)]
			(NumConst.neg discrete_value)
		in
		(* Create a linear equality *)
		LinearConstraint.make_linear_inequality linear_term LinearConstraint.Op_eq
	) discrete_values in
	(* Create the linear constraint *)
	LinearConstraint.make inequalities


(*--------------------------------------------------*)
(* Compute the initial state with the initial invariants and time elapsing *)
(*--------------------------------------------------*)
let create_initial_state program =
	(* Get the declared init state with initial constraint C_0(X) *)
	let initial_location, init_constraint = program.init in
	
	(* Compute the invariants I_q0(X) for the initial locations *)
	print_message Debug_high ("\nComputing initial invariant I_q0(X)");
	(* Create the invariant *)
	let invariant = compute_invariant program initial_location in
	(* Debug *)
	print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names invariant);
	
	(* Compute the invariant after time elapsing I_q0(X') *)
	print_message Debug_high ("Computing initial invariant after time-elapsing I_q0(X')");
	let invariant_after_time_elapsing =
		LinearConstraint.rename_variables program.renamed_clocks_couples invariant in
	(* Debug *)
	print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names invariant_after_time_elapsing);
	
	(* Compute constraint for assigning a (constant) value to discrete variables *)
	print_message Debug_high ("Computing constraint for discrete variables");
	let discrete_values = List.map (fun discrete_index -> discrete_index, (Automaton.get_discrete_value initial_location discrete_index)) program.discrete in
	let discrete_constraint = instantiate_discrete discrete_values in
	(* Debug *)
	print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names discrete_constraint);
	
	(* Compute the equalities X' = X + d *)
	let time_elapsing_inequalities =
		List.map (fun clock_index -> 
			(* X + d - X' = 0 *)
			LinearConstraint.make_linear_inequality
				(LinearConstraint.make_linear_term [
					(NumConst.one, clock_index);
					(NumConst.minus_one, program.prime_of_variable clock_index);
					(NumConst.one, program.d);
					] NumConst.zero
				)
				LinearConstraint.Op_eq
		) program.clocks
	in
	(* Time elapsing constraint *)
	let time_elapsing_constraint = LinearConstraint.make time_elapsing_inequalities in
	
	(* Perform intersection of all those constraints *)
	print_message Debug_high ("Performing intersection of C0(X) and I_q0(X) and X' = X + d and I_q0(X')");
	let full_constraint = LinearConstraint.intersection [init_constraint ; invariant ; time_elapsing_constraint ; program.positive_d ; invariant_after_time_elapsing ; discrete_constraint] in
	(* Debug *)
	print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names full_constraint);
	
	(* Hide 'X', 'Discrete' and 'd' *)
	print_message Debug_high ("Hide clocks, discrete and 'd' in C0(X) ^ I_q0(X) ^ X' = X + d ^ I_q0(X')");
	let full_constraint_hidden = LinearConstraint.hide (program.d :: program.clocks_and_discrete) full_constraint in
	(* Debug *)
	print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names full_constraint_hidden);
	(* Rename X' -> X *)
	print_message Debug_high ("Renaming X' into X in C0(X) ^ I_q0(X) ^ X' = X + d ^ I_q0(X')");
	let final_constraint =
		LinearConstraint.rename_variables program.unrenamed_clocks_couples full_constraint_hidden in
	(* Debug *)
	print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names final_constraint);
	(* Return the initial state *)
	initial_location, final_constraint



(*--------------------------------------------------*)
(* Compute a list of possible actions for a state   *)
(*--------------------------------------------------*)
let compute_possible_actions program original_location = 
	(* Create a boolean array for the possible actions *)
	let possible_actions = Array.make program.nb_actions false in
	(* Fill it with all the possible actions per location *)
	for automaton_index = 0 to program.nb_automata - 1 do
		(* Get the current location for automaton_index *)
		let location_index = Automaton.get_location original_location automaton_index in
		(* Debug print *)
		print_message Debug_total ("Considering automaton " ^ (program.automata_names automaton_index) ^ " with location " ^ (program.location_names automaton_index location_index) ^ ".");
		(* Get the possible actions for this location *)
		let possible_actions_for_this_automaton =
			program.actions_per_location automaton_index location_index
		in
		(* Add all the actions to our array *)
		List.iter (fun action_index ->
			(* Add the action *)
			possible_actions.(action_index) <- true;
			(* Debug print *)
			print_message Debug_total ("Adding action " ^ (program.action_names action_index) ^ " for automaton " ^ (program.automata_names automaton_index) ^ " with location " ^ (program.location_names automaton_index location_index) ^ ".");
		) possible_actions_for_this_automaton;
	done;
	(* Debug print *)
	if debug_mode_greater Debug_total then (
		print_message Debug_total ("Possible actions for all the locations are:");
		Array.iteri (fun action_index possible ->
			if possible then (print_message Debug_total (" - " ^ (program.action_names action_index)));
		) possible_actions;
	);
	(* Remove every action where an automaton can not take this action in its *)
	(*  location although the action was declared for this automaton          *)
	let possible_actions = Array.mapi (fun action_index possible ->
		(* If this action is not possible, then false *)
		if not possible then false else(
		let automata_for_this_action = program.automata_per_action action_index in
		(* Check if the action is possible for all the automata for which it is defined *)
		(**** TO OPTIMIZE: no need to keep searching if not "still_possible" anymore ****)
		let action_possible =
			List.fold_left (fun still_possible automaton_index -> 
				still_possible
				&& (List.mem action_index (program.actions_per_location automaton_index (Automaton.get_location original_location automaton_index)))
			) possible automata_for_this_action in
		(* Debug print *)
		if not action_possible && (debug_mode_greater Debug_total) then (
			print_message Debug_total ("But action '" ^ (program.action_names action_index) ^ "' is not possible for all declared automata.")
		);
		(* Replace the previous value by the new one *)
		action_possible
		)
	) possible_actions in
	(* Make a list *)
	true_indexes possible_actions



(*------------------------------------------------------------------*)
(* Compute a new location for a given set of transitions            *)
(* aut_table         : indices of involved automata                 *)
(* trans_table       : indices of examined transition per automaton *)
(* action_index      : index of current action                      *)
(* original_location : the source location                          *)
(*------------------------------------------------------------------*)
(* returns the new location, the guards, the updates                *)
(*------------------------------------------------------------------*)
let compute_new_location program aut_table trans_table action_index original_location =
		(* make a copy of the location *)		
		let location = Automaton.copy_location original_location in
		(* Create a temporary hashtbl for discrete values *)
		let updated_discrete = Hashtbl.create program.nb_discrete in
		(* Update the location for the automata synchronized with 'action_index'; return the list of guards and updates *)
		let guards_and_updates = Array.to_list (Array.mapi (fun local_index real_index ->
			(* Get the current location for this automaton *)
			let location_index = Automaton.get_location original_location real_index in
			(* Find the transitions for this automaton *)
			let transitions = program.transitions real_index location_index action_index in
  		(* Get the index of the examined transition for this automaton *)
			let current_index = trans_table.(local_index) in
			(* Keep the 'current_index'th transition *)
			let transition = List.nth transitions current_index in
			(* Keep only the dest location *)
			let guard, clock_updates, discrete_updates, dest_index = transition in			
			(* Update discrete *)
			List.iter (fun (discrete_index, linear_term) ->
				(* Compute its new value *)
				let new_value = LinearConstraint.evaluate_linear_term (Automaton.get_discrete_value original_location) linear_term in
				(* Check if already updated *)
				if Hashtbl.mem updated_discrete discrete_index then (
					(* Find its value *)
					let previous_new_value = Hashtbl.find updated_discrete discrete_index in
					(* Compare with the new one *)
					if NumConst.neq previous_new_value new_value then (
					(* If different: warning *)
						print_warning ("The discrete variable '" ^ (program.variable_names discrete_index) ^ "' is updated several times with different values for the same synchronized action '" ^ (program.action_names action_index) ^ "'. The behavior of the system is now unspecified.");
					);
				) else (
					(* Else keep it in memory for update *)
					Hashtbl.add updated_discrete discrete_index new_value;
				);
			) discrete_updates;
			(* Update the global location *)
			Automaton.update_location_with [real_index, dest_index] [] location;
			(* Keep the guard and updates *)
			guard, clock_updates;
		) aut_table) in
		(* Split the list of guards and updates *)
		let guards, clock_updates = List.split guards_and_updates in
		(* Compute couples to update the discrete variables *)
		let updated_discrete_couples = ref [] in
		Hashtbl.iter (fun discrete_index discrete_value ->
			updated_discrete_couples := (discrete_index, discrete_value) :: !updated_discrete_couples;
		) updated_discrete;
		(* Update the global location *)
		Automaton.update_location_with [] !updated_discrete_couples location;
	  (* return the new location, the guards, and the clock updates *)
		(location, guards, clock_updates)
	
	
	
(*------------------------------------------------------------*)
(* Compute the clock updates for a given transition           *)
(* clock_updates: list of variables to be reset               *)
(*------------------------------------------------------------*)
(* returns a couple of constraints, representing the updated  *)
(* and the non-updated clocks                                 *)
(*------------------------------------------------------------*)
let compute_updates program clock_updates = 
		(* keep track of variables that are not updated *)
		let not_appearing_in_updates = Array.make program.nb_variables true in
		(* Compute X' = rho(X) + d for the variables appearing in updates *)
		let updates = List.fold_left (fun list_of_updates current_updates ->			
			List.rev_append	list_of_updates
			(List.map (fun (variable_index, linear_term) ->
				(* 'variable_index' appears in an updates *)
				not_appearing_in_updates.(variable_index) <- false;			
				(* Consider cases for clocks *)				
				match program.type_of_variables variable_index with
				(* Clocks: Build rho(X) + d - X' = 0 *)
				| Var_type_analog | Var_type_clock -> 
				let xprime_lt = LinearConstraint.make_linear_term [
					NumConst.minus_one, program.prime_of_variable variable_index;
					NumConst.one, program.d
				] NumConst.zero in
				LinearConstraint.make_linear_inequality
					(LinearConstraint.add_linear_terms xprime_lt linear_term)
					LinearConstraint.Op_eq
				| _ -> raise (InternalError "Only clocks can be updated.")
			) current_updates
		)) [] clock_updates in
		(* Create the constraint *)
		let updates = LinearConstraint.make updates in
		(* Debug print *)
		if debug_mode_greater Debug_total then(
			print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names updates);
		);
		(* Compute X' = X + d for the clocks NOT appearing in updates *)
		print_message Debug_total ("\nComputing X' = X + d for non-updated clocks");
		(**** BAD PROG ****)
		(* List of inequalities *)
		let non_updated = ref [] in
		Array.iteri (fun variable_index not_appearing -> 
			(**** TO OPTIMIZE : there are only clocks, here ! ****)
			let is_clock = program.is_clock variable_index || program.is_analog variable_index in
			(* Only consider clocks *)
			if not_appearing && is_clock then(
				(* Useful names *)
				let x = NumConst.one, variable_index in
				let x_prime = NumConst.minus_one, program.prime_of_variable variable_index in
				(* Build the inequality *)
				let members =
					(* Build the inequality X + d - X' = 0 *)
					[x ; (NumConst.one, program.d) ; x_prime]
				in 
				let new_inequality = LinearConstraint.make_linear_inequality
					(LinearConstraint.make_linear_term members NumConst.zero)
					LinearConstraint.Op_eq
				in
				(* Add it to the list *)
				non_updated := new_inequality :: !non_updated;
			);
		) not_appearing_in_updates;
		(* Create the constraint *)
		let non_updated = LinearConstraint.make !non_updated in
		(* Debug print *)
		if debug_mode_greater Debug_total then(
			print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names non_updated);
		);
		(* return both constraints *)
		(updates, non_updated)	
	
	
exception Unsat_exception
	
(*--------------------------------------------------*)	
(* Compute the new constraint for a transition      *)
(* orig_constraint : contraint in source location   *)
(* orig_location   : source location                *)
(* dest_location   : target location                *)
(* guards          : guard constraints per automaton*)
(* clock_updates   : updated clock variables        *)
(*--------------------------------------------------*)
let compute_new_constraint program orig_constraint orig_location dest_location guards clock_updates =
		(* the constraint is checked on the fly for satisfyability -> exception mechanism *)
		try ( 
			print_message Debug_total ("\nComputing equalities for discrete variables (previous values)");
			(* Compute discrete values using the current (new) location *)
			let discrete_values = List.map (fun discrete_index -> discrete_index, (Automaton.get_discrete_value orig_location discrete_index)) program.discrete in
			(* Convert to a constraint *)
			let previous_discrete_constraint = instantiate_discrete discrete_values in
			(* Debug *)
			print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names previous_discrete_constraint);
		
			(* Debug *)
			if debug_mode_greater Debug_total then(
				print_message Debug_total ("\nComputing guards g(X)");
				List.iter (fun guard -> 
					print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names guard);
				) guards;
			);
			print_message Debug_total ("\nComputing the guards and discrete");
			(* Add the (old) value for discrete to the guards *)
			let guards_and_discrete = LinearConstraint.intersection (previous_discrete_constraint :: guards) in
			(* Debug print *)
			if debug_mode_greater Debug_total then(
				print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names guards_and_discrete);
			);
			(* check here for unsatisfiability *)
			if not (LinearConstraint.is_satisfiable guards_and_discrete) then (
				print_message Debug_high "skip transition";
				raise Unsat_exception
			);
			
			print_message Debug_total ("\nEliminate the discrete variables in g(X)");
			(* Remove the discrete variables *)
			let guards_without_discrete = LinearConstraint.hide program.discrete guards_and_discrete in
			(* Debug print *)
			if debug_mode_greater Debug_total then(
				print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names guards_without_discrete);
			);
	
			print_message Debug_total ("\nComputing clock updates X' = rho(X) + d");
			let updates, non_updated = compute_updates program clock_updates in
	
			(* Compute the invariant in the destination location *)
			print_message Debug_total ("\nComputing invariant I_q(X) ");
			let invariant = compute_invariant program dest_location in
			(* Debug print *)
			if debug_mode_greater Debug_total then(
				print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names invariant);
				if not (LinearConstraint.is_satisfiable invariant) then
					print_message Debug_total ("This constraint is NOT satisfiable.");
			);
			
			(* Compute the invariant after time elapsing *)
			print_message Debug_total ("Computing invariant I_q(X') ");
			let renamed_invariant = LinearConstraint.rename_variables program.renamed_clocks_couples invariant in
			(* Debug print *)
			if debug_mode_greater Debug_total then(
				print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names renamed_invariant);
				if not (LinearConstraint.is_satisfiable renamed_invariant) then
					print_message Debug_total ("This constraint is NOT satisfiable.");
			);
			
			(* Compute the invariant before time elapsing *)
			print_message Debug_total ("\nComputing invariant I_q(X' - d) ");
			let renamed_invariant_before_time_elapsing = LinearConstraint.add_d program.d NumConst.minus_one program.renamed_clocks renamed_invariant in
			(* Debug print *)
			if debug_mode_greater Debug_total then(
				print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names renamed_invariant_before_time_elapsing);
				if not (LinearConstraint.is_satisfiable renamed_invariant_before_time_elapsing) then
					print_message Debug_total ("This constraint is NOT satisfiable.");
			);
	
			(* Compute the equalities for the discrete variables *)
			print_message Debug_total ("\nComputing equalities for discrete variables");
			(* Compute discrete values using the current (new) location *)
			let discrete_values = List.map (fun discrete_index -> discrete_index, (Automaton.get_discrete_value dest_location discrete_index)) program.discrete in
			(* Convert to a constraint *)
			let discrete_constraint = instantiate_discrete discrete_values in
			(* Debug *)
			print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names discrete_constraint);
		
			(* Perform the intersection *)
			print_message Debug_total ("\nPerforming intersection of C(X) and g(X) and X' = rho(X) + d and I_q(X' - d) and I_q(X') ");
			let new_full_constraint = LinearConstraint.intersection
				[
					orig_constraint ();
					updates;
					non_updated;
					renamed_invariant_before_time_elapsing;
					renamed_invariant;
					program.positive_d;
					discrete_constraint;
					guards_without_discrete
				] in
			(* Debug print *)
			if debug_mode_greater Debug_total then(
				print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names new_full_constraint);
				if not (LinearConstraint.is_satisfiable new_full_constraint) then
					print_message Debug_total ("This constraint is NOT satisfiable.");
			);
	
			(* Hide 'X' and 'd' *)
			print_message Debug_total ("\nHide 'X', discrete and 'd' in C(X) ^ g(X) ^ X' = rho(X) + d ^ I_q(X' - d) ^ I_q(X')");
			let full_constraint_hidden = LinearConstraint.hide (program.d :: program.clocks_and_discrete) new_full_constraint in
			(* Debug print *)
			if debug_mode_greater Debug_total then(
				print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names full_constraint_hidden);
				if not (LinearConstraint.is_satisfiable full_constraint_hidden) then
					print_message Debug_total ("This constraint is NOT satisfiable.");
			);
	
			(* Rename X' -> X *)
			print_message Debug_total ("\nRenaming X' into X:");
			let final_constraint =
				LinearConstraint.rename_variables program.unrenamed_clocks_couples full_constraint_hidden in
			(* Debug print *)
			if debug_mode_greater Debug_total then(
				print_message Debug_total (LinearConstraint.string_of_linear_constraint program.variable_names final_constraint);
				if not (LinearConstraint.is_satisfiable final_constraint) then
					print_message Debug_total ("This constraint is NOT satisfiable.");
			);
			(* return the final constraint *)
			final_constraint
		) with Unsat_exception -> LinearConstraint.false_constraint ()
		
						

(*--------------------------------------------------*)
(* Compute the list of possible destination states w.r.t. to a reachability_graph, and update this graph; return the (really) new states *)
(*--------------------------------------------------*)
let post program pi0 reachability_graph orig_state_index =
	(* Original location: static *)
	let original_location, _ = Graph.get_state reachability_graph orig_state_index in
	(* Dynamic version of the orig_constraint (can change!) *)
	let orig_constraint () =
		let _, orig_constraint = Graph.get_state reachability_graph orig_state_index in
		orig_constraint
	in

	(* Debug prints *)
	if debug_mode_greater Debug_high then(
		let orig_state = Graph.get_state reachability_graph orig_state_index in
		let _, orig_constraint = orig_state in
		let orig_constraint_projection = LinearConstraint.hide program.clocks_and_discrete orig_constraint in
		print_message Debug_high ("Performing post from state:");
		print_message Debug_high (string_of_state program orig_state);
		print_message Debug_high ("\nThe projection of this constraint onto the parameters is:");
		print_message Debug_high (LinearConstraint.string_of_linear_constraint program.variable_names orig_constraint_projection);
	);

	(* get possible actions originating from current state *)
	let list_of_possible_actions = compute_possible_actions program original_location in

	(* Debug print *)
	if debug_mode_greater Debug_high then (
		let actions_string = List.map (fun action_index -> program.action_names action_index) list_of_possible_actions in
		print_message Debug_high ("Possible synchronized actions are: " ^ (string_of_list_of_string_with_sep ", " actions_string));
	);

	(* build the list of new states *)
	let new_states = ref [] in

	(* FOR ALL ACTION DO: *)
	List.iter (fun action_index ->

	print_message Debug_medium ("\nComputing destination states for action '" ^ (program.action_names action_index) ^ "'");
	(* Get the automata declaring the action *)
	let automata_for_this_action = program.automata_per_action action_index in
	let nb_automata_for_this_action = List.length automata_for_this_action in

	(*------------------------------------------------*)
	(* Compute the reachable states on the fly: i.e., all the possible transitions for all the automata belonging to 'automata' *)
	(*------------------------------------------------*)
	
	(* add discrete values to constraint *)
	let discrete_values = List.map (fun discrete_index -> discrete_index, (Automaton.get_discrete_value original_location discrete_index)) program.discrete in
	(* Convert to a constraint *)
	let discrete_constr = instantiate_discrete discrete_values in
	(* compute conjunction with current constraint *)
	let orig_plus_discrete = LinearConstraint.intersection [orig_constraint (); discrete_constr] in
	
	(* Give a new index to those automata *)
	let real_indexes = Array.make nb_automata_for_this_action 0 in
	(* Keep an array of possible transition indices for each automaton *)
	let possible_transitions = Array.make nb_automata_for_this_action [] in
	(* Use an array of transition indices for the search (start with 0),*)
	(* indicating the current index within the possible transitions for*)
	(* each automaton *)
	let current_indexes = Array.make nb_automata_for_this_action 0 in
	(* Keep the maximum index of possible transitions for each automaton *)
	let max_indexes = Array.make nb_automata_for_this_action 0 in
	(* Array for the currently selected transition indices *)
	let current_transitions = Array.make nb_automata_for_this_action 0 in
	
	(* Compute all possible transitions for the involved automata.*)
	(* Stop computation as soon as one automaton has no legal transition left. *)
	let current_index = ref 0 in
	let legal_transitions_exist = 
	try (
		List.iter (fun automaton_index ->
			(* Tabulate the real index *)
			real_indexes.(!current_index) <- automaton_index;
			(* Get the current location for this automaton *)
			let location_index = Automaton.get_location original_location automaton_index in
			(* Get transitions for this automaton *)
			let transitions = program.transitions automaton_index location_index action_index in
			(* Keep only possible transitions *)
			let is_possible = fun trans -> (
				let guard, _, _, _ = trans in
				let constr_and_guard = LinearConstraint.intersection [orig_plus_discrete; guard] in
				let is_possible = LinearConstraint.is_satisfiable constr_and_guard in
				if not is_possible then (
					print_message Debug_medium "** early skip transition **"
				);
				is_possible
			) in
			let legal_transitions = ref [] in
			let trans_index = ref 0 in
			List.iter (fun trans -> 
				if is_possible trans then(
					legal_transitions := !trans_index :: !legal_transitions
				);
				trans_index := !trans_index + 1
			) transitions;
			(* Stop computation if no legal transition exists *)
			if !legal_transitions = [] then (
				print_message Debug_medium "*** early skip action ***";
				raise Unsat_exception
			);
			(* Store possible transitions *)
			possible_transitions.(!current_index) <- !legal_transitions;
			(* Tabulate the number of transitions for this location *)
			max_indexes.(!current_index) <-	List.length !legal_transitions - 1;
			(* Increment the index *)
			current_index := !current_index + 1;
		) automata_for_this_action;
		(* arrived here, so each automaton must have at least one legal transition *)
		true		
	) with Unsat_exception -> false in

	(* Debug: compute the number of combinations *)
	if debug_mode_greater Debug_medium then(
		let nb_combinations = Array.fold_left (fun sum max -> sum * (max + 1)) 1 max_indexes in
		print_message Debug_medium ("I will consider " ^ (string_of_int nb_combinations) ^ " combination" ^ (s_of_int nb_combinations) ^ " for this state and this action\n");
	);

	(* Loop on all the transition combinations *)
	let more_combinations = ref legal_transitions_exist in
	while !more_combinations do
		(* Debug *)
		if debug_mode_greater Debug_total then (
		let local_indexes = string_of_array_of_string_with_sep "\n\t" (
		Array.mapi (fun local_index real_index ->
			(string_of_int local_index) ^ " -> " ^ (string_of_int real_index) ^ " : " ^ (string_of_int current_indexes.(local_index)) ^ "; ";
		) real_indexes) in
		print_message Debug_total ("--- Consider the combination \n\t" ^ local_indexes);
		);

		(* build the current combination of transitions *)
		for i=0 to Array.length current_transitions -1 do
			current_transitions.(i) <- List.nth (possible_transitions.(i)) (current_indexes.(i))
		done; 

		(* Compute the new location for the current combination of transitions *)
		let location, guards, clock_updates = compute_new_location program real_indexes current_transitions action_index original_location in
		
		(* Compute the new constraint for the current transition *)
		let final_constraint = compute_new_constraint program orig_constraint original_location location guards clock_updates in

		(* Check the satisfiability *)
		if not (LinearConstraint.is_satisfiable final_constraint) then(
			print_message Debug_high ("\nThis constraint is not satisfiable.");
		) else (

		(* Branching between 2 algorithms here *)
		let add_new_state =
			if program.imitator_mode = Reachability_analysis then true
			else (
			(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
			(* Algorithm InverseMethod: here! *)
			(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
			
			(* Hide non parameters (X) *)
			print_message Debug_high ("\nHiding non parameters:");
			let p_constraint = LinearConstraint.hide program.clocks_and_discrete final_constraint in
			(* Debug print *)
			if debug_mode_greater Debug_high then(
				print_message Debug_high (LinearConstraint.string_of_linear_constraint program.variable_names p_constraint);
			);
			(* Check the pi0-compatibility *)
			let compatible, incompatible = LinearConstraint.partition_pi0_compatible pi0 p_constraint in
			let is_pi0_incompatible = incompatible != [] in
			
			(* If pi0-incompatible: select an inequality *)
			if is_pi0_incompatible then (
				print_message Debug_low ("\nFound a pi0-incompatible state.");
				(* Debug print *)
				if debug_mode_greater Debug_medium then(
					print_message Debug_medium ("\nThe following inequalities are pi0-incompatible:");
					List.iter (fun inequality -> print_message Debug_medium (LinearConstraint.string_of_linear_inequality program.variable_names inequality)) incompatible;
				);

				let inequality =
					(* If random selection: pick up a random inequality *)
					if program.random then random_element incompatible
					(* Else select the first one *)
					else List.nth incompatible 0
				in
				(* Debug print *)
				if debug_mode_greater  Debug_medium then(
					print_message Debug_medium ("\nSelecting the following pi0-incompatible inequality:");
					print_message Debug_medium (LinearConstraint.string_of_linear_inequality program.variable_names inequality);
				);

				(* Negate the inequality *)
				let negated_inequality = LinearConstraint.negate_wrt_pi0 pi0 inequality in
				(* Debug print *)
				let randomly = if program.random then "randomly " else "" in
				let among = if List.length incompatible > 1 then (" (" ^ randomly ^ "selected among " ^ (string_of_int (List.length incompatible)) ^ " inequalities)") else "" in
				print_message Debug_standard ("  Adding the following inequality" ^ among ^ ":");
				print_message Debug_standard ("  " ^ (LinearConstraint.string_of_linear_inequality program.variable_names negated_inequality));

				(* Update the previous states (including the 'new_states' and the 'orig_state') *)
				print_message Debug_medium ("\nUpdating all the previous states.\n");
				Graph.add_inequality_to_states reachability_graph negated_inequality;
				
				(* If pi-incompatible *)
				false
				(* If pi-compatible *)
				) else true
			(*------------------------------------------------*)
			(* End of algorithm inverse method *)
			(*------------------------------------------------*)
			) in
			
			if add_new_state then (
				(*------------------------------------------------*)
				(* Create the state *)
				(*------------------------------------------------*)
				let new_state = location, final_constraint in

				(* Debug print *)
				if debug_mode_greater Debug_total then(
					print_message Debug_total ("Consider the state \n" ^ (string_of_state program new_state));
				);

				(*------------------------------------------------*)
				(* Add this new state *)
				(*------------------------------------------------*)
				(* Try to add the state to the graph // with the p-constraint ????? *)
				let new_state_index, added = Graph.add_state program reachability_graph new_state in
				(* If this is really a new state *)
				if added then (
					(* Add the state_index to the list of new states *)
					new_states := new_state_index :: !new_states;
				);
				(* Update the transitions *)
				Graph.add_transition reachability_graph (orig_state_index, action_index, new_state_index);
				(* Debug print *)
				if debug_mode_greater Debug_high then (
					let beginning_message = (if added then "NEW STATE" else "Old state") in
					print_message Debug_high ("\n" ^ beginning_message ^ " reachable through action '" ^ (program.action_names action_index) ^ "': ");
					print_message Debug_high (string_of_state program new_state);
				);
			); (* end if pi0 incompatible *)
		); (* end if satisfiable *)

		(*------------------------------------------------*)
		(* Update the index *)
		(*------------------------------------------------*)
		let not_is_max = ref true in
		let local_index = ref 0 in
		while !not_is_max do
			(* Look for the first automaton local index to be incremented *)
			if current_indexes.(!local_index) < max_indexes.(!local_index) then(
				(* Increment this index *)
				current_indexes.(!local_index) <- current_indexes.(!local_index) + 1;
				(* Reset the smaller indexes to 0 *)
				for i = 0 to !local_index - 1 do
					current_indexes.(i) <- 0;
				done;
				(* Stop the loop *)
				not_is_max := false;
			) else (
				local_index := !local_index + 1;
				if !local_index >= nb_automata_for_this_action then(
					more_combinations := false;
					not_is_max := false;
				)
			)
		done; (* end while *)
	done; (* while more new states *)
	) list_of_possible_actions;
	
	(* Return the list of (really) new states *)
	List.rev (!new_states)


(*--------------------------------------------------*)
(* Compute the reachability graph from a given state *)
(*--------------------------------------------------*)
let post_star program pi0 init_state =
	(* Time counter *)
	let counter = ref (Unix.gettimeofday()) in
	(* Get some variables *)
	let nb_actions = program.nb_actions in
	let nb_variables = program.nb_variables in
	let nb_automata = program.nb_automata in
	(* Debut prints *)
	print_message Debug_low ("Performing reachability analysis from state:");
	print_message Debug_low (string_of_state program init_state);
	(* Guess the number of reachable states *)
	let guessed_nb_states = 10 * (nb_actions + nb_automata + nb_variables) in 
	let guessed_nb_transitions = guessed_nb_states * nb_actions in 
	print_message Debug_total ("I guess I will reach about " ^ (string_of_int guessed_nb_states) ^ " states with " ^ (string_of_int guessed_nb_transitions) ^ " transitions.");
	(* Create the reachability graph *)
	let reachability_graph = Graph.make guessed_nb_transitions in
	
	(* Add the initial state to the reachable states *)
	let init_state_index, _ = Graph.add_state program reachability_graph init_state in

	(*--------------------------------------------------*)
	(* Perform the post^* *)
	(*--------------------------------------------------*)
	let newly_found_new_states = ref [init_state_index] in
	let nb_iterations = ref 1 in
	let limit_reached = ref false in

	(* Check if the list of new states is empty *)
	while not (!limit_reached  || list_empty !newly_found_new_states) do
		if debug_mode_greater Debug_standard then (
			print_message Debug_low ("\n");
			print_message Debug_standard ("Computing post^" ^ (string_of_int (!nb_iterations)) ^ "");
			print_message Debug_low ("Number of recently found state" ^ (s_of_int (List.length !newly_found_new_states)) ^ ": " ^ (string_of_int (List.length !newly_found_new_states)) ^ ".");
		);
		(* Count the states for debug purpose: *)
		let num_state = ref 0 in
		(* Length of 'newly_found_new_states' for debug purpose *)
		let nb_states = List.length !newly_found_new_states in

		let new_newly_found_new_states =
		(* For each newly found state: *)
		List.fold_left (fun new_newly_found_new_states orig_state_index ->
			(* Count the states for debug purpose: *)
			num_state := !num_state + 1;
			(* Perform the post *)
			let post = post program pi0 reachability_graph orig_state_index in
			let new_states = post in
			(* Debug *)
			if debug_mode_greater Debug_medium then (
				let beginning_message = if list_empty new_states then "Found no new state" else ("Found " ^ (string_of_int (List.length new_states)) ^ " new state" ^ (s_of_int (List.length new_states)) ^ "") in
				print_message Debug_medium (beginning_message ^ " for the post of state " ^ (string_of_int !num_state) ^ " / " ^ (string_of_int nb_states) ^ " in post^" ^ (string_of_int (!nb_iterations)) ^ ".\n");
			);

			(* Return the concatenation of the new states *)
			(**** OPTIMIZED: do not care about order (else shoud consider 'list_append new_newly_found_new_states (List.rev new_states)') *)
			List.rev_append new_newly_found_new_states new_states
		) [] !newly_found_new_states in
		(* Update the newly_found_new_states *)
		newly_found_new_states := new_newly_found_new_states;
		(* Debug *)
		if debug_mode_greater Debug_medium then (
			let beginning_message = if list_empty !newly_found_new_states then "\nFound no new state" else ("\nFound " ^ (string_of_int (List.length !newly_found_new_states)) ^ " new state" ^ (s_of_int (List.length !newly_found_new_states)) ^ "") in
			print_message Debug_medium (beginning_message ^ " for post^" ^ (string_of_int (!nb_iterations)) ^ ".\n");
		);
		
		(* Clean up a little *)
		Gc.major ();
		
		(* Iterate *)
		nb_iterations := !nb_iterations + 1;
		(* Check if the limit has been reached *)
		match !post_limit with
			| None -> ()
			| Some limit -> if !nb_iterations > limit then limit_reached := true;
		(* No time limit in cartography (because would not stop the global program *)
(* 		if program.imitator_mode = Reachability_analysis || program.imitator_mode = Inverse_method then (  *)
		match !time_limit with
			| None -> ()
			| Some limit -> if (get_time()) > (float_of_int limit) then limit_reached := true;
(* 		) *)
	done;

	if !limit_reached && not (list_empty !newly_found_new_states) then(
		match !post_limit with
			| None -> ()
			| Some limit -> if !nb_iterations > limit then print_warning (
				"The limit number of iterations (" ^ (string_of_int limit) ^ ") has been reached. Post^* now stops, although there were still " ^ (string_of_int (List.length !newly_found_new_states)) ^ " state" ^ (s_of_int (List.length !newly_found_new_states)) ^ " to explore at this iteration."
			);
		match !time_limit with
			| None -> ()
			| Some limit -> if (get_time()) > (float_of_int limit) then print_warning (
				"The time limit (" ^ (string_of_int limit) ^ " second" ^ (s_of_int limit) ^ ") has been reached. Post^* now stops, although there were still " ^ (string_of_int (List.length !newly_found_new_states)) ^ " state" ^ (s_of_int (List.length !newly_found_new_states)) ^ " to explore at this iteration."
			);
	);

	(*--------------------------------------------------*)
	(* Debug *)
	(*--------------------------------------------------*)
	print_message Debug_standard (
		"\nFixpoint reached after "
		^ (string_of_int (!nb_iterations)) ^ " iteration" ^ (s_of_int (!nb_iterations)) ^ ""
		^ " in " ^ (string_of_seconds (time_from !counter)) ^ ": "
		^ (string_of_int (Graph.nb_states reachability_graph)) ^ " reachable state" ^ (s_of_int (Graph.nb_states reachability_graph))
		^ " with "
		^ (string_of_int (Hashtbl.length (reachability_graph.transitions_table))) ^ " transition" ^ (s_of_int (Hashtbl.length (reachability_graph.transitions_table))) ^ ".");

	(*--------------------------------------------------*)
	(* Perform the intersection of all inequalities *)
	(*--------------------------------------------------*)
	let final_k0 =
		if program.imitator_mode = Reachability_analysis
		then (LinearConstraint.true_constraint ())
		else(
		(* Get all the constraints *)
		let all_constraints = Graph.all_p_constraints program reachability_graph in
		(* Perform the intersection *)
		let intersection = LinearConstraint.intersection all_constraints in
		(* Print the result :-) *)
		print_message Debug_standard ("\nFinal constraint K0 :");
		print_message Debug_standard (LinearConstraint.string_of_linear_constraint program.variable_names intersection);
		print_message Debug_standard ("\nAlgorithm InverseMethod finished after " ^ (string_of_seconds (time_from !counter)) ^ ".");
		(* Return the result *)
		intersection
	) in

	(*--------------------------------------------------*)
	(* Return the result *)
	(*--------------------------------------------------*)
	reachability_graph, final_k0, !nb_iterations, !counter


(**************************************************)
(* BEHAVIORAL CARTOGRAPHY ALGORITHM functions *)
(**************************************************)

(** Behavioral cartography algorithm with full coverage of V0 *)
let cover_behavioral_cartography program pi0cube init_state =
	(* Dimension of the system *)
	let dimension = Array.length pi0cube in
	(* Min & max bounds for the parameters *)
	let min_bounds = Array.map (fun (low, high) -> low) pi0cube in
	let max_bounds = Array.map (fun (low, high) -> high) pi0cube in
	
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
	
	(* Iterate on all the possible pi0 *)
	let more_pi0 = ref true in
	let limit_reached = ref false in
	while !more_pi0 && not !limit_reached do
		(* Convert the int array into a well-typed pi0 *)
		let pi0_array = Array.map NumConst.numconst_of_int current_pi0 in
		let pi0 = fun parameter -> pi0_array.(parameter) in
		
		(* Check that it does not belong to any constraint *)
		if dynArray_exists (LinearConstraint.is_pi0_compatible pi0) results then (
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
			let graph, k0, nb_iterations, counter = post_star program pi0 init_state in
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
			
			(* Add the pi0 and the constraint *)
			DynArray.add pi0_computed pi0;
			DynArray.add results k0;

			(* Print the constraint *)
			print_message Debug_low ("Constraint K0 computed:");
			print_message Debug_standard (LinearConstraint.string_of_linear_constraint program.variable_names k0);

			(* Generate the dot graph (only if K0 <> false) *)
			if LinearConstraint.is_satisfiable k0 then (
				let radical = !program_prefix ^ "_" ^ (string_of_int !current_iteration) in
				let dot_file_name = (radical ^ ".dot") in
				let states_file_name = (radical ^ ".states") in
				let gif_file_name = (radical ^ "." ^ dot_extension) in
				generate_graph program pi0 graph dot_file_name states_file_name gif_file_name;
			);
		); (* else if new pi0 *)

		(* Find the next pi0 *)
		let not_is_max = ref true in
		let local_index = ref 0 in
		while !not_is_max do
			(* Try to increment the local index *)
			if current_pi0.(!local_index) < max_bounds.(!local_index) then(
				(* Increment this index *)
				current_pi0.(!local_index) <- current_pi0.(!local_index) + 1;
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
		match !time_limit with
			| None -> ()
			| Some limit -> if (get_time()) > (float_of_int limit) then limit_reached := true;

	done; (* while more pi0 *)

	if !limit_reached && !more_pi0 then (
		match !time_limit with
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
	()


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

			(* Check that it does not belong to any constraint *)
			if array_exists (LinearConstraint.is_pi0_compatible pi0_functional) results then (
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
				let graph, k0, nb_iterations, counter = post_star program pi0_functional init_state in
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

				(* Print the constraint *)
				print_message Debug_low ("Constraint K0 computed:");
				print_message Debug_standard (LinearConstraint.string_of_linear_constraint program.variable_names k0);
				(* Generate the dot graph *)
				let radical = !program_prefix ^ "_" ^ (string_of_int !i) in
				let dot_file_name = (radical ^ ".dot") in
				let states_file_name = (radical ^ ".states") in
				let gif_file_name = (radical ^ "." ^ dot_extension) in
				generate_graph program pi0_functional graph dot_file_name states_file_name gif_file_name;
				(* Add the index to the interesting list *)
				interesting_interations := !i :: !interesting_interations;
				(* Add the result *)
				results.(!i - 1) <- k0;
			);
		);
		(* Stop if the time limit has been reached *)
		let _ =
		match !time_limit with
			| None -> ()
			| Some limit -> if (get_time()) > (float_of_int limit) then limit_reached := true;
		in

		(* Increment the iteration *)
		i := !i + 1;
	done;

	if !limit_reached && !i <= nb then (
		match !time_limit with
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
(* Count the number of arguments *)
let nb_args = ref 0 in

(* Name of the main file *)
let file = ref "" in
(* Name of the pi0 file *)
let pi0file = ref "" in
(* Name of the pi0 cube file *)
(* let pi0cubefile = ref "" in *)

(* Auto-detection of the sync actions *)
let sync_auto_detection = ref false in
(* No random selection of the pi0-incompatible inequality *)
let no_random = ref false in
(* Default debug mode: standard *)
let global_debug_mode = ref Debug_standard in
(* Consider the inclusion of region; default: false *)
let inclusion = ref false in
(* Mode for IMITATOR *)
let imitator_mode = ref Inverse_method in
(* Timed mode *)
let timed_mode = ref false in
(* Acyclic mode *)
let acyclic = ref false in
(* Mode with parametric constraints (clock elimination) in the log file *)
let with_parametric_log = ref false in

(* Usage message *)
let usage_msg = "Usage: IMITATOR program_file [pi0_file] [options]" in

(* Get the debug mode *)
let rec set_debug_mode_ref debug_mode =
	global_debug_mode := try debug_mode_of_string debug_mode
		with Not_found ->
			print_error ("The debug mode '" ^ debug_mode ^ "' is not valid.");
			Arg.usage speclist usage_msg;
			abort_program ();
			exit(0);

(* Get the mode *)
and set_mode mode =
	(* Case: 'reachability' *)
	if mode = "reachability" then 
		imitator_mode := Reachability_analysis
	(* Case: inverse method *)
	else if mode = "inversemethod" then 
		imitator_mode := Inverse_method
	(* Case: cover *)
	else if mode = "cover" then 
		imitator_mode := Cover_cartography
	(* Case: number of iterations *)
	else try (
		(* Find the 'random' string *)
		if not (String.sub mode 0 6 = "random") then raise (Failure "toto");
		(* Find the number *)
		let number = String.sub mode 6 (String.length mode - 6) in
		imitator_mode := (Random_cartography (int_of_string number))
	) with _ -> (
		print_error ("The mode '" ^ mode ^ "' is not valid.");
		Arg.usage speclist usage_msg;
		abort_program ();
		exit(0);
	);

(* Options *)
and speclist = [
	("-acyclic", Set acyclic, " Does not test if a new state was already encountered. To be set ONLY if the system is acyclic. Default: 'false'");

	("-debug", String set_debug_mode_ref, " Print more or less debug information. Can be set to 'nodebug', 'standard', 'low', 'medium', 'high', 'total'. Default: 'standard'");
	
	("-inclusion", Set inclusion, " Consider an inclusion of region instead of the equality when performing the Post operation. Default: 'false'");

	("-log-prefix", Set_string program_prefix, " Sets the prefix for log files. Default: [program_file].");

	("-mode", String set_mode, " Mode for IMITATOR II. Use 'reachability' for a parametric reachability analysis (no pi0 needed). Use 'inversemethod' for the inverse method. For the behavioral cartography algorithm, use 'cover' to cover all the points within V0, or 'randomXX' where XX is a number to iterate randomly algorithm. Default: 'inversemethod'.");
	
	("-no-dot", Set no_dot, " No graphical output using 'dot'. Default: false.");

	("-no-log", Set no_log, " No generation of log files. Default: false.");

	("-no-random", Set no_random, " No random selection of the pi0-incompatible inequality (select the first found). Default: false.");
	
	("-post-limit", Int (fun i -> post_limit := Some i), " Limits the depth of the Post exploration. Default: no limit.");

	("-sync-auto-detect", Set sync_auto_detection, " Detect automatically the synchronized actions in each automaton. Default: false (consider the actions declared by the user)");
	
	("-time-limit", Int (fun i -> time_limit := Some i), " Time limit in seconds. Warning: no guarantee that the program will stop exactly after the given amount of time. Default: no limit.");

	("-timed", Set timed_mode, " Adds a timing information to each output of the program. Default: none.");

	("-with-parametric-log", Set with_parametric_log, " Adds the elimination of the clock variables in the constraints in the log files. Default: false.");

	] in

(* A function on any argument *)
let anon_fun = (fun arg ->
	(* If 1st argument: main file *)
	if !nb_args = 0 then(
		nb_args := !nb_args + 1;
		file := arg;
	)
	(* If 2nd argument: pi0 file *)
	else if !nb_args = 1 then(
		nb_args := !nb_args + 1;
		pi0file := arg;
	)
	(* If more than one argument : warns *)
	else (
		print_warning ("The program argument '" ^ arg ^ "' will be ignored.");
	)
) in

(* Parse the arguments *)
Arg.parse speclist anon_fun usage_msg;

(* Case no file *)
if !nb_args < 1 then(
	print_error ("Please give a source file name.");
	Arg.usage speclist usage_msg;
	abort_program (); exit(0)
);
(* Case no pi0 file *)
if !nb_args = 1 && (!imitator_mode != Reachability_analysis) then(
	print_error ("Please give a reference valuation file name.");
	Arg.usage speclist usage_msg;
	abort_program (); exit(0)
);



(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(* Debug mode *) 
(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
set_debug_mode !global_debug_mode;


(**************************************************)
(**************************************************)
(* Hello world! *)
(**************************************************)
(**************************************************)
print_message Debug_standard
	( "**************************************************\n"
	^ "*                   IMITATOR II                  *\n"
	^ "*                                 Etienne ANDRE  *\n"
	^ "*                                   2009 - 2010  *\n"
	^ "*     Laboratoire Specification et Verification  *\n"
	^ "*                  ENS de Cachan & CNRS, France  *\n"
	^ "**************************************************");


(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(* Check compatibility between options *) 
(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
if !imitator_mode = Reachability_analysis && !nb_args = 2 then
	print_warning ("The pi0 file " ^ !pi0file ^ " will be ignored since this is a reachability analysis.");

(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(* Update the program prefix if not set *)
(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
if !program_prefix = "" then
	program_prefix := !file;


(**************************************************)
(* Recall the arguments *)
(**************************************************)


if !inclusion then
	print_message Debug_medium ("Considering inclusion mode.");

if !sync_auto_detection then
	print_message Debug_medium ("Auto-detection mode for sync actions.");

if !no_random then
	print_message Debug_medium ("No random selection for pi0-incompatible inequalities.");

if !no_dot then
	print_message Debug_medium ("No graphical output.");

if !no_log then
	print_message Debug_medium ("No log mode.");


(* Print the mode *)
let message = match !imitator_mode with
	| Reachability_analysis -> "parametric reachability analysis"
	| Inverse_method -> "inverse method"
	| Cover_cartography -> "behavioral cartography algorithm with full coverage"
	| Random_cartography nb -> "behavioral cartography algorithm with " ^ (string_of_int nb) ^ " random iterations"
in print_message Debug_standard ("Mode: " ^ message ^ ".");

(* LIMIT OF POST *)
let _ =
match !post_limit with
	| None -> print_message Debug_medium "Considering no limit for the depth of the Post operation."
	| Some limit -> print_warning ("Considering a limit of " ^ (string_of_int limit) ^ " for the depth of the Post operation.")
in ();

(* TIME LIMIT *)
let _ =
match !time_limit with
	| None -> print_message Debug_medium "Considering no time limit."
	| Some limit -> print_warning ("The program will try to stop after " ^ (string_of_int limit) ^ " seconds.")
in ();


(**************************************************)
(* Timed mode *)
(**************************************************)
if !timed_mode then (
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
print_message Debug_low ("Considering file " ^ !file ^ ".");
let parsing_structure = parser_lexer ImitatorParser.main ImitatorLexer.token !file in 

print_message Debug_medium ("Considering program prefix " ^ !program_prefix ^ ".");

if !imitator_mode != Reachability_analysis then
	print_message Debug_low ("Considering reference valuation in file " ^ !pi0file ^ ".");

(* Pi0 Parsing *)
let pi0_parsed, pi0cube_parsed =
	(* Depending on which operation we are performing *)
	match !imitator_mode with
		(* If reachability: no pi0 *)
		| Reachability_analysis -> [], []
		(* Inverse method : pi0 *)
		| Inverse_method -> parser_lexer Pi0Parser.main Pi0Lexer.token !pi0file, []
		(* Cartography : pi0cube *)
		| _ -> [], parser_lexer Pi0CubeParser.main Pi0CubeLexer.token !pi0file
in

print_message Debug_standard ("\nParsing done " ^ (after_seconds ()) ^ ".");
Gc.major ();

(**************************************************)
(* Conversion to an abstract program *)
(**************************************************)

let program, pi0, pi0cube = 
try (
	ProgramConverter.abstract_program_of_parsing_structure
		parsing_structure pi0_parsed pi0cube_parsed !acyclic !sync_auto_detection !inclusion !no_random  !with_parametric_log !imitator_mode !file
) with 
	| ProgramConverter.InvalidProgram -> (print_error ("The input program contains errors. Please check it again."); abort_program (); exit 0)
	| ProgramConverter.InvalidPi0 -> (print_error ("The input pi_0 file contains errors. Please check it again."); abort_program (); exit 0)
	| InternalError e -> (print_error ("Internal error: " ^ e ^ "\nPlease insult the developers."); abort_program (); exit 0)
	in

print_message Debug_standard ("Program checked and converted " ^ (after_seconds ()) ^ ".\n");
Gc.major ();


(**************************************************)
(* Debug print: program *)
(**************************************************)
(* print_message Debug_total ("\nProgram:\n" ^ (ImitatorPrinter.string_of_program program) ^ "\n"); *)


(**************************************************)
(* Initial state *)
(**************************************************)

let (init_loc, init_constraint) = program.init in
(* Print the initial state *)
print_message Debug_medium ("\nInitial state:\n" ^ (ImitatorPrinter.string_of_state program program.init) ^ "\n");

(* Check the satisfiability *)
if not (LinearConstraint.is_satisfiable init_constraint) then (
	print_warning "The initial constraint of the program is not satisfiable.";
	terminate_program();
)else(
	print_message Debug_total ("\nThe initial constraint of the program is satisfiable.");
);

(* Get the initial state after time elapsing *)
let init_state_after_time_elapsing = create_initial_state program in
let _, initial_constraint_after_time_elapsing = init_state_after_time_elapsing in
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
(* Execute IMITATOR II *)
(**************************************************)

let _ =
match !imitator_mode with
	(* Perform reachability analysis or inverse Method *)
	| Reachability_analysis | Inverse_method ->
		let reachability_graph, k0, _, _ =
			post_star program pi0 init_state_after_time_elapsing
		in
		(* Generate the DOT graph *)
		print_message Debug_high "Generating the dot graph";
		let dot_file_name = (!program_prefix ^ ".dot") in
		let states_file_name = (!program_prefix ^ ".states") in
		let gif_file_name = (!program_prefix ^ "." ^ dot_extension) in
		generate_graph program pi0 reachability_graph dot_file_name states_file_name gif_file_name;

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
(* flush stdout; *)

terminate_program()
