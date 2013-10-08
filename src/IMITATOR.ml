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
 * Last modified: 2013/03/20
 *
 ****************************************************************)


(**************************************************)
(* Modules *)
(**************************************************)
open Global
open AbstractModel
open Arg
open ModelPrinter
open Graph
open Options
open Reachability
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


(**************************************************)
(**************************************************)
(* Print startup message *)
(**************************************************)
(**************************************************)

print_message Debug_standard header_string;



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

(* Print date *)
print_message Debug_standard ("Analysis time: " ^ (now()) ^ "\n");

(* File *)
print_message Debug_standard ("Model: " ^ options#file);
(* File prefix *)
print_message Debug_low ("Prefix for output files: " ^ options#files_prefix);

(* Global mode *)
let message = match options#imitator_mode with
	| Translation -> "translation"
	| Reachability_analysis -> "parametric reachability analysis"
	| Inverse_method -> "inverse method"
	| Cover_cartography -> "behavioral cartography algorithm with full coverage and step " ^ (NumConst.string_of_numconst options#step)
	| Border_cartography -> "behavioral cartography algorithm with border detection (experimental) and step " ^ (NumConst.string_of_numconst options#step)
	| Random_cartography nb -> "behavioral cartography algorithm with " ^ (string_of_int nb) ^ " random iterations and step " ^ (NumConst.string_of_numconst options#step)
in print_message Debug_standard ("Mode: " ^ message ^ ".");


(* TODO : print the user-defined correctness condition, if any *)


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
	if options#forcePi0 then
		print_warning ("The pi0 file " ^ options#pi0file ^ " will be ignored since this the pi0 file is automatically generated.")
	;
);

if options#acyclic && options#tree then (
	options#acyclic_unset;
	print_warning ("Ayclic mode is set although tree mode is already set. Only tree mode will be considered.");
);

if options#with_parametric_log && not options#with_log then (
	print_warning ("Parametric log was asked, but log was not asked. No log will be output.");
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

if options#branch_and_bound then
	print_message Debug_standard ("Considering branch and bound (experimental!).")
else
	print_message Debug_medium ("No branch and bound mode (default).");



(* Syntax *)
if options#fromGML then
	print_message Debug_standard ("GrML syntax used.");

(* Syntax *)
if options#forcePi0 then
	print_warning ("Pi0 is automatically generated.");


(* OPTIONS *)

if options#merge then (
	print_message Debug_standard ("Merging technique of [AFS12] enabled.");
) else
	print_message Debug_medium ("Merging technique of [AFS12] disabled (default).")
;
if options#merge_before then
	print_message Debug_standard ("Variant of the merging technique of [AFS12] enabled. States will be merged before pi0-compatibility test (EXPERIMENTAL).")
else
	print_message Debug_medium ("Variant of the merging technique of [AFS12] disabled.")
;

(*if options#dynamic then
	print_message Debug_standard ("Dynamic mode (optimization by RS).")
else
	print_message Debug_medium ("No dynamic mode (default).");*)

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

if options#dynamic_clock_elimination then
	print_message Debug_standard ("Dynamic clock elimination activated.")
else
	print_message Debug_medium ("No dynamic clock elimination (default).");


(* Output *)

if options#with_dot then
	print_message Debug_standard ("Graphical output will be generated.")
else
	print_message Debug_medium ("No graphical output (default).");
	
if options#with_log then
	print_message Debug_standard ("Log (description of states) will be generated.")
else
	print_message Debug_medium ("No state description (default).");

(* LIMIT OF POST *)
let _ =
match options#post_limit with
	| None -> print_message Debug_medium "Considering no limit for the depth of the Post operation (default)."
	| Some limit -> print_warning ("Considering a limit of " ^ (string_of_int limit) ^ " for the depth of the Post operation.")
in ();

(* LIMIT OF POST *)
begin
match options#states_limit with
	| None -> print_message Debug_medium "Considering no limit for the number of states (default)."
	| Some limit -> print_warning ("Considering a limit of " ^ (string_of_int limit) ^ " for the number of states.")
end;

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

(* Parsing the main model *)
print_message Debug_low ("Considering file " ^ options#file ^ ".");
let parsing_structure = 
	(* Branching between 2 input syntaxes *)
	if options#fromGML then
		try parser_lexer_from_file GrMLParser.main GrMLLexer.token options#file
		with InvalidModel -> (print_error ("GrML input contains error. Please check it again."); abort_program (); exit 1)
	else parser_lexer_from_file ModelParser.main ModelLexer.token options#file
in 


if options#imitator_mode != Reachability_analysis && options#imitator_mode != Translation then
	print_message Debug_low ("Considering reference valuation in file " ^ options#pi0file ^ ".");

(* Pi0 Parsing *)
let pi0_parsed, v0_parsed =
	(* Depending on which operation we are performing *)
	match options#imitator_mode with
		(* If translation: no pi0 *)
		| Translation -> [], []
		(* If reachability: no pi0 *)
		| Reachability_analysis -> [], []
		(* Inverse method : pi0 *)
		| Inverse_method ->
			(* Case forcePi0 *)
			(* HACK !! *)
			if options#forcePi0 then  parser_lexer_from_string Pi0Parser.main Pi0Lexer.token "p1 = 1 & p2 = 2 & p3 = 3 & p4 = 4 & p5 = 5", []
			(* Normal case *)
			else parser_lexer_from_file Pi0Parser.main Pi0Lexer.token options#pi0file, []
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
print_message Debug_standard ("Memory for abstract model: " ^ (round3_float nb_ko) ^ " KB (i.e., " ^ (string_of_int (int_of_float nb_words)) ^ " words)");

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

(*(* COMPARISON *)
let init_state_after_time_elapsing2 = Reachability.create_initial_state2 model in
let _, initial_constraint_after_time_elapsing2 = init_state_after_time_elapsing2 in

if(LinearConstraint.is_equal initial_constraint_after_time_elapsing initial_constraint_after_time_elapsing2) then(
	print_message Debug_standard ("\n INITIAL STATE OK :o)");
	terminate_program ();
)else (
	print_error ("\n INITIAL STATES DIFFERENT.");
	print_message Debug_standard ("\n 1) \n" ^ (LinearConstraint.string_of_linear_constraint model.variable_names initial_constraint_after_time_elapsing));
	print_message Debug_standard ("\n 2) \n" ^ (LinearConstraint.string_of_linear_constraint model.variable_names initial_constraint_after_time_elapsing2));
	abort_program ();
);*)


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
		| Translation -> raise (InternalError "Translation can't be executed; program should have terminated before.");

		| Reachability_analysis ->
			Reachability.full_reachability model init_state_after_time_elapsing;
			[]
		
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
			print_message Debug_standard ("Generation of the graphical cartography...\n");
			Graphics.cartography model v0 zones (options#files_prefix ^ "_cart")
		) else (
			print_message Debug_total "Not in cartography mode: no graph for the cartography."
		)
	;
) with
| InternalError e -> (print_error ("Internal error: " ^ e ^ "\nPlease kindly insult the developers."); abort_program (); exit 1);
end;


(**************************************************)
(* Bye bye! *)
(**************************************************)

(* Reachability.print_stats (); *)

terminate_program()
