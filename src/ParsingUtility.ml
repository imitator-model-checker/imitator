(*****************************************************************
 *
 *                     IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2014/03/15
 * Last modified: 2015/03/30
 *
 ****************************************************************)


(**************************************************)
(* External modules *)
(**************************************************)
open Gc


(**************************************************)
(* Internal modules *)
(**************************************************)
open Exceptions
open AbstractModel
open CamlUtilities
open ImitatorUtilities



(************************************************************)
(* Local parsing function *)
(************************************************************)

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



(************************************************************)
(** Compile the different files and set the models *)
(************************************************************)
let compile options =
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Parsing *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(* Parsing the main model *)
	print_message Verbose_low ("Parsing file " ^ options#file ^ "...");
	let parsing_structure = 
		(* Branching between 2 input syntaxes *)
		if options#fromGML then
			try parser_lexer_from_file GrMLParser.main GrMLLexer.token options#file
			with InvalidModel -> (print_error ("GrML input contains error. Please check it again."); abort_program (); exit 1)
		else parser_lexer_from_file ModelParser.main ModelLexer.token options#file
	in 


	if options#imitator_mode != State_space_exploration && options#imitator_mode != Translation then
		print_message Verbose_low ("Parsing reference valuation in file " ^ options#pi0file ^ "...");

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

	print_message Verbose_standard ("\nParsing completed " ^ (after_seconds ()) ^ ".");
	(** USELESS, even increases memory x-( **)
	(* Gc.major (); *)


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Conversion to an abstract model *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

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
	print_message Verbose_standard ("Memory for abstract model: " ^ (round3_float nb_ko) ^ " KiB (i.e., " ^ (string_of_int (int_of_float nb_words)) ^ " words)");

	(* With or without stopwatches *)
	if model.has_stopwatches then
		print_message Verbose_standard ("The model contains stopwatches.")
	else
		print_message Verbose_low ("The model is purely timed (no stopwatches).");

	(* Ugly line break *)
	print_message Verbose_standard "";
	

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* return *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	model, pi0, v0

