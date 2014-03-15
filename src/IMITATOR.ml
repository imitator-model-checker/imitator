(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        Ulrich Kuehne, Etienne Andre
 * 
 * Created:       2009/09/07
 * Last modified: 2014/03/15
 *
 ****************************************************************)


(**************************************************)
(* Modules *)
(**************************************************)
open Global
open AbstractModel
open Arg
open ModelPrinter
(* open Graph *)
open Options
open Reachability
open Gc


(**************************************************

TAGS POUR CHOSES A FAIRE
- (**** TO DO ****)
- (**** BAD PROG ****)
- (**** TO OPTIMIZE ****)
- (**** OPTIMIZED ****)

<>

**************************************************)





(**************************************************)
(**************************************************)
(* GLOBAL VARIABLES *)
(**************************************************)
(**************************************************)



(**************************************************)
(* Parsing function *)
(**************************************************)
(* Generic parser that returns the abstract structure *)
let parser_lexer_gen the_parser the_lexer lexbuf string_of_input file_name =
	(* Parsing *)
	let parsing_structure = try(
		the_parser the_lexer lexbuf
	) with
		| ParsingError (symbol_start, symbol_end) ->
			(* Convert the in_channel into a string *)
			let file_string = string_of_input () in
			(* Create the error message *)
			let error_message =
				if symbol_start >= 0 && symbol_end >= symbol_start then (
					(* Get the symbol *)
					let error_symbol = (String.sub file_string symbol_start (symbol_end - symbol_start)) in
					(* Resize it if too big *)
					let error_symbol =
						if (String.length error_symbol > 25) then
							"..." ^ (String.sub error_symbol (String.length error_symbol - 25) 25)
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
			print_error ("Parsing error in file " ^ file_name ^ " " ^ error_message); abort_program (); exit(1)
		| UnexpectedToken c -> print_error ("Parsing error in file " ^ file_name ^ ": unexpected token '" ^ (Char.escaped c) ^ "'."); abort_program (); exit(1)
		| Failure f -> print_error ("Parsing error ('failure') in file " ^ file_name ^ ": " ^ f); abort_program (); exit(1)
	in
	parsing_structure


(* Parse a file and return the abstract structure *)
let parser_lexer_from_file the_parser the_lexer file_name =
	(* Open file *)
	let in_channel = try (open_in file_name) with
		| Sys_error e -> print_error ("The file " ^ file_name ^ " could not be opened.\n" ^ e); abort_program (); exit(1)
	in
	(* Lexing *)
	let lexbuf = try (Lexing.from_channel in_channel) with
		| Failure f -> print_error ("Lexing error in file " ^ file_name ^ ": " ^ f); abort_program (); exit(1)
	in
	(* Function to convert a in_channel to a string (in case of parsing error) *)
	let string_of_input () =
		(* Convert the file into a string *)
		let extlib_input = IO.input_channel (open_in file_name) in
			IO.read_all extlib_input
	in
	(* Generic function *)
	parser_lexer_gen the_parser the_lexer lexbuf string_of_input file_name


(* Parse a string and return the abstract structure *)
let parser_lexer_from_string the_parser the_lexer the_string =
	(* Lexing *)
	let lexbuf = try (Lexing.from_string the_string) with
		| Failure f -> print_error ("Lexing error: " ^ f ^ "\n The string was: \n" ^ the_string ^ ""); abort_program (); exit(1)
(* 		| Parsing.Parse_error -> print_error ("Parsing error\n The string was: \n" ^ the_string ^ ""); abort_program (); exit(1) *)
	in
	(* Function to convert a in_channel to a string (in case of parsing error) *)
	let string_of_input () = the_string in
	(* Generic function *)
	parser_lexer_gen the_parser the_lexer lexbuf string_of_input the_string




;;


(**************************************************)
(**************************************************)
(* STARTING PROGRAM *)
(**************************************************)
(**************************************************)


(* TEST !! *)
(*LinearConstraint.test_PDBMs();
terminate_program();*)


(**************************************************)
(**************************************************)
(* Print startup message *)
(**************************************************)
(**************************************************)

print_message Debug_standard header_string;


(* Print date *)
print_message Debug_standard ("Analysis time: " ^ (now()) ^ "\n");




(**************************************************)
(* Get the arguments *)
(**************************************************)
(* object with command line options *)
let options = new imitator_options in

options#parse;

(* Set the options (for other modules) *)
Input.set_options options;



(**************************************************)
(* Recall the arguments *)
(**************************************************)
options#recall();




(**************************************************)
(* Parsing *)
(**************************************************)

(* Parsing the main model *)
print_message Debug_low ("Considering file " ^ options#file ^ ".");
let parsing_structure = 
	(* Branching between 2 input syntaxes *)
	if options#fromGML then
		try parser_lexer_from_file GrMLParser.main GrMLLexer.token options#file
		with InvalidModel -> (print_error ("GrML input contains error. Please check it again."); abort_program (); exit 1)
	else parser_lexer_from_file ModelParser.main ModelLexer.token options#file
in 


if options#imitator_mode != State_space_exploration && options#imitator_mode != Translation then
	print_message Debug_low ("Considering reference valuation in file " ^ options#pi0file ^ ".");

(* Pi0 Parsing *)
let pi0_parsed, v0_parsed =
	(* Depending on which operation we are performing *)
	match options#imitator_mode with
		(* If translation, reachability, synthesis: no pi0 *)
		| Translation
		| State_space_exploration
		| EF_synthesis
			-> [], []
		
		(* Inverse method : pi0 *)
		| Inverse_method ->
(*			(* Case forcePi0 *)
			if options#forcePi0 then  parser_lexer_from_string Pi0Parser.main Pi0Lexer.token "p1 = 1 & p2 = 2 & p3 = 3 & p4 = 4 & p5 = 5", []
			(* Normal case *)
			else*) parser_lexer_from_file Pi0Parser.main Pi0Lexer.token options#pi0file, []
		(* Cartography : v0 *)
		| _ -> [], parser_lexer_from_file V0Parser.main V0Lexer.token options#pi0file
in

print_message Debug_standard ("\nParsing completed " ^ (after_seconds ()) ^ ".");
(** USELESS, even increases memory x-( **)
(* Gc.major (); *)


(**************************************************)
(* Conversion to an abstract model *)
(**************************************************)

let model, pi0, v0 = 
try (
	ModelConverter.abstract_model_of_parsing_structure
		parsing_structure pi0_parsed v0_parsed options
) with 
	| InvalidModel -> (print_error ("The input model contains errors. Please check it again."); abort_program (); exit 1)
	| ModelConverter.InvalidPi0 -> (print_error ("The input pi_0 file contains errors. Please check it again."); abort_program (); exit 1)
	| InternalError e -> (print_error ("Internal error: " ^ e ^ "\nPlease kindly insult the developers."); abort_program (); exit 1)
	in

let gc_stat = Gc.stat () in
let nb_words = gc_stat.minor_words +. gc_stat.major_words -. gc_stat.promoted_words in
let nb_ko = nb_words *. 4.0 /. 1024.0 in
print_message Debug_standard ("Memory for abstract model: " ^ (round3_float nb_ko) ^ " KiB (i.e., " ^ (string_of_int (int_of_float nb_words)) ^ " words)");

(* With or without stopwatches *)
if model.has_stopwatches then
	print_message Debug_standard ("The model contains stopwatches.")
else
	print_message Debug_low ("The model is purely timed (no stopwatches).");

(* Ugly line break *)
print_message Debug_standard "";




(**************************************************)
(* Set model and pi0 *)
(**************************************************)
Input.set_model model;
Input.set_pi0 pi0;


(**************************************************)
(* Debug print: model *)
(**************************************************)
if debug_mode_greater Debug_total then
	print_message Debug_total ("\nModel:\n" ^ (ModelPrinter.string_of_model model) ^ "\n");


(**************************************************)
(* Case translation *)
(**************************************************)

(* Translation to CLP (work in progress) *)
if options#pta2clp then(
	print_message Debug_standard ("Translating model to CLP.");
	print_warning ("Work in progress!!!!");
	print_message Debug_standard ("\nmodel in CLP:\n" ^ (PTA2CLP.string_of_model model) ^ "\n");
	terminate_program()
);

(* Translation to GrML (experimental) *)
if options#pta2gml then(
	print_message Debug_standard ("Translating model to GrML.");
	let translated_model = PTA2GrML.string_of_model model in
	let gml_file = options#files_prefix ^ ".grml" in
	print_message Debug_total ("\n" ^ translated_model ^ "\n");
	(* Write *)
	write_to_file gml_file translated_model;
	terminate_program()
);

(* Translation to JPG *)
if options#pta2jpg then(
	print_message Debug_standard ("Translating model to a graphics.");
	let translated_model = PTA2JPG.string_of_model model in
	print_message Debug_high ("\n" ^ translated_model ^ "\n");
	Graphics.dot model options#files_prefix translated_model;
	terminate_program()
);

(* Direct cartography output *)
if options#cartonly then(
	print_message Debug_standard ("Direct output of a cartography (no analysis will be run).");
	(* Get the parameters *)
	let constraints , (p1_min , p1_max) , (p2_min , p2_max) = model.carto in
	(* Transform the constraint for cartography *)
	let constraints = List.map (fun (linear_constraint , tile_nature) ->
		Convex_constraint (linear_constraint , tile_nature)
	) constraints in
	(* Call the cartography *)
	Graphics.cartography model [| (p1_min , p1_max); (p2_min , p2_max) |] constraints options#files_prefix;
	(* The end *)
	terminate_program()
);
(* 		| End_of_file -> print_error ("Parsing error in file " ^ file_name ^ ": unexpected end of file."); abort_program (); exit(1) *)



(**************************************************)
(* Preliminary checks *)
(**************************************************)

if options#imitator_mode = EF_synthesis then(
	match model.correctness_condition with
		(* Synthesis only works w.r.t. (un)reachability *)
		| Some (Unreachable _) -> ()
		| _ -> print_error ("EF-synthesis can only be run if an unreachability property is defined in the model.");
			abort_program();
);


if (options#imitator_mode = Border_cartography && model.correctness_condition = None) then(
	print_error ("In border cartography mode, a correctness property must be defined.");
	abort_program();
);



(**************************************************)
(* EXPERIMENTAL: dynamic clock elimination *)
(**************************************************)
(* Need to be called before initial state is created! *)
if options#dynamic_clock_elimination then (
	Reachability.prepare_clocks_elimination model
);


(**************************************************)
(* Initial state *)
(**************************************************)

(* Print the initial state *)
print_message Debug_medium ("\nInitial state:\n" ^ (ModelPrinter.string_of_state model (model.initial_location, model.initial_constraint)) ^ "\n");

(* Check the satisfiability *)
if not (LinearConstraint.px_is_satisfiable model.initial_constraint) then (
	print_warning "The initial constraint of the model is not satisfiable.";
	terminate_program();
)else(
	print_message Debug_total ("\nThe initial constraint of the model is satisfiable.");
);

(* Get the initial state after time elapsing *)
let init_state_after_time_elapsing = Reachability.create_initial_state model in
let _, initial_constraint_after_time_elapsing = init_state_after_time_elapsing in


(* Check the satisfiability *)
if not (LinearConstraint.px_is_satisfiable initial_constraint_after_time_elapsing) then (
	print_warning "The initial constraint of the model after time elapsing is not satisfiable.";
	terminate_program();
)else(
	print_message Debug_total ("\nThe initial constraint of the model after time elapsing is satisfiable.");
);
(* Print the initial state after time elapsing *)
print_message Debug_medium ("\nInitial state after time-elapsing:\n" ^ (ModelPrinter.string_of_state model init_state_after_time_elapsing) ^ "\n");






(*(* TESTS *) 
print_message Debug_standard ("\nInitial constraint:\n" ^ (LinearConstraint.string_of_linear_constraint model.variable_names initial_constraint_after_time_elapsing) ^ "\n");

(*let n = ref 1 in

List.iter (fun parameter_id ->
	LinearConstraint.time_elapse_assign [parameter_id] (list_diff model.parameters [parameter_id]) initial_constraint_after_time_elapsing;
	
	print_message Debug_standard ("\nAfter time elapsing #" ^ (string_of_int !n) ^ " on parameter '" ^ (model.variable_names parameter_id) ^ "' :\n" ^ (LinearConstraint.string_of_linear_constraint model.variable_names initial_constraint_after_time_elapsing) ^ "\n");
	
	Graphics.cartography model v0 [Convex_constraint initial_constraint_after_time_elapsing] (options#file ^ "-carto" ^ (string_of_int !n));

	n := !n + 1;

) model.parameters;
(* Graphics.cartography model v0 [Convex_constraint initial_constraint_after_time_elapsing] (options#file ^ "-carto"); *)
terminate_program();*)


LinearConstraint.grow_to_zero_assign model.parameters model.clocks_and_discrete initial_constraint_after_time_elapsing;
print_message Debug_standard ("\nFinal constraint:\n" ^ (LinearConstraint.string_of_linear_constraint model.variable_names initial_constraint_after_time_elapsing) ^ "\n");
Graphics.cartography model v0 [Convex_constraint initial_constraint_after_time_elapsing] (options#file ^ "-cartoz");
terminate_program();*)



(*(**************************************************)
(* EXPERIMENTAL: branch and bound *)
(**************************************************)

if options#imitator_mode = Inverse_method && options#branch_and_bound then(
	Reachability.branch_and_bound model pi0 init_state_after_time_elapsing;
	terminate_program();
);*)




(**************************************************)
(* Execute IMITATOR *)
(**************************************************)

begin
try(
	let zones =
	match options#imitator_mode with
		| Translation -> raise (InternalError "Translation cannot be executed here; program should already have terminated at this point.");

		
		(* Exploration *)
		| State_space_exploration
			-> Reachability.full_state_space_exploration model init_state_after_time_elapsing;
			[]
			
		(* Synthesis *)
		| EF_synthesis 
			->
			[Reachability.ef_synthesis model init_state_after_time_elapsing]

			
		(* Inverse Method *)
		| Inverse_method ->
				Reachability.inverse_method model init_state_after_time_elapsing;
			[]


		| Cover_cartography | Border_cartography ->
		(* Behavioral cartography algorithm with full coverage *)
			Cartography.cover_behavioral_cartography model v0 init_state_after_time_elapsing
			
			
		| Random_cartography nb ->
		(* Behavioral cartography algorithm with random iterations *)
			Cartography.random_behavioral_cartography model v0 init_state_after_time_elapsing nb;

			
	in

	(* Computation of the cartography *)
	if options#cart then (
			(* No cartography if no zone *)
			if zones = [] then(
				print_message Debug_standard ("\nNo cartography can be generated since the list of constraints is empty.\n");
			)else(
				print_message Debug_standard ("\nGeneration of the graphical cartography...\n");
				Graphics.cartography model v0 zones (options#files_prefix ^ "_cart")
			)
		) else (
			print_message Debug_high "Graphical cartography not asked: graph not generated."
		)
	;
) with
	| InternalError e -> (print_error ("Internal error: " ^ e ^ "\nPlease (kindly) insult the developers."); abort_program (); exit 1);
end;


(**************************************************)
(* Bye bye! *)
(**************************************************)

(* Reachability.print_stats (); *)

terminate_program()
