(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Generic lexing/parsing driver for input files.
 *
 * File contributors : Ulrich Kühne, Étienne André, Jaime Arias
 * Created           : 2014/03/15
 *
 ************************************************************)


(************************************************************)
(* Internal modules *)
(************************************************************)
open Exceptions
open ImitatorUtilities

(* let force_included_file_terminator = ref false

let set_force_included_file_terminator enabled =
	force_included_file_terminator := enabled

(* redundant again ?? delete all of these 2 methods*)
let file_needs_terminal_end file_name =
	if not !force_included_file_terminator then
		false
	else
		try
			let ic = open_in file_name in
			let file_contents = really_input_string ic (in_channel_length ic) in
			close_in ic;
			let trimmed = String.trim file_contents in
			if trimmed = "" then
				true
			else
				not (Str.string_match (Str.regexp ".*\\bend\\s*$") trimmed 0)
		with _ ->
			false

let model_token_stream_adapter token_fn file_name =
	let synthetic_end_pending = ref false in
	fun lexbuf ->
		if !synthetic_end_pending then (
			synthetic_end_pending := false;
			ModelParser.EOF
		) else
			match token_fn lexbuf with
			| ModelParser.EOF when file_needs_terminal_end file_name ->
				if !force_included_file_terminator then
					print_message Verbose_low ("extra end is added");
				synthetic_end_pending := true;
				ModelParser.CT_END
			| token ->
				token *)

(************************************************************)
(* Exceptions *)
(************************************************************)

(** Exception raised when the input file cannot be opened *)
exception InputFileNotFound of string

(** Exception raised when a lexing or parsing error occurs *)
exception ParsingFailure of string


(************************************************************)
(* Local parsing function *)
(************************************************************)

(* Generic parser that returns the abstract structure *)
let parser_lexer_gen the_parser the_lexer lexbuf string_of_input file_name =
	(* Parsing *)
	print_message Verbose_total ("Preparing actual parsing…");
	let parsing_structure = try (
		let absolute_filename = FilePath.make_absolute (FileUtil.pwd ()) file_name in
		print_message Verbose_total ("Created absolute file name `" ^ absolute_filename ^ "`.");

		print_message Verbose_total ("Assigning lex_curr_p…");
		lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = absolute_filename };

		print_message Verbose_total ("Assigning lex_start_p…");
		lexbuf.Lexing.lex_start_p <- { lexbuf.Lexing.lex_start_p with Lexing.pos_fname = absolute_filename };

		print_message Verbose_total ("Starting actual parsing of `" ^ absolute_filename ^ "`…");

		let parsing_structure = the_parser the_lexer lexbuf in
		print_message Verbose_total ("Parsing structure created");

		(* let debug_string_of_value v =
			Marshal.to_string v [] in
		print_message Verbose_total (debug_string_of_value parsing_structure); *)

		parsing_structure
	) with
		| ParsingError (symbol_start, symbol_end) ->
			print_message Verbose_total ("Parsing error detected. Processing…");

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
							"…" ^ (String.sub error_symbol (String.length error_symbol - 25) 25)
						else error_symbol
					in
					(* Get the line *)
					let beginning_of_the_file = String.sub file_string 0 symbol_end in
					let lines = Str.split (Str.regexp "\n") beginning_of_the_file in
					let line = List.length lines in
					(* Make the message *)
					"near `" ^ error_symbol ^ "` at line " ^ (string_of_int line) ^ ".")
				else "somewhere in the file, most probably in the very beginning."
			in
			(* Abort properly *)
			raise (ParsingFailure ("Parsing error in file `" ^ file_name ^ "` " ^ error_message))

		| UnexpectedToken c ->
			(* Print some information *)
			print_message Verbose_total ("Parsing error detected `UnexpectedToken`. Processing…");
			(* Abort properly *)
			raise (ParsingFailure ("Parsing error in file `" ^ file_name ^ "`: unexpected token `" ^ (Char.escaped c) ^ "`."))


		(*** HACK: added because of some mysterious exception raised during parsing (2020/04/16) ***)
		| Invalid_argument (*"index out of bounds"*)_ ->
			(* Print some information *)
			print_message Verbose_total ("Parsing error detected `index out of bounds`. Processing…");
			(* Abort properly *)
			raise (ParsingFailure ("Mysterious parsing error in file `" ^ file_name ^ "`, maybe at the very beginning."))

		| Failure f ->
			(* Print some information *)
			print_message Verbose_total ("Parsing error detected `Failure`. Processing…");
			(* Abort properly *)
			raise (ParsingFailure ("Parsing error (`failure`) in file `" ^ file_name ^ "`: " ^ f))

		(* Static division by 0 *)
		| Static_division_by_0 error_message ->
			(* Abort properly *)
			raise (ParsingFailure ("Division by 0 (" ^ error_message ^ ") spotted during the parsing!"))

		(* Problem with an included file *)
		| IncludeFileNotFound included_file ->
			(* Abort properly *)
			raise (ParsingFailure ("File `" ^ included_file ^ "` (included by `" ^ file_name ^ "`) not found."))

		(* April 1st *)
		| April1st ->
			print_message Verbose_standard Constants.fish;
			terminate_program();
			(* Necessary to make the program compile (even though this line won't be executed) *)
			exit(0)
	in
	parsing_structure


(* Parse a file and return the abstract structure *)
let parser_lexer_from_file the_parser the_lexer file_name =
	(* Open file *)
	print_message Verbose_total ("Opening in_channel…");
	let in_channel = try (open_in file_name) with
		| Sys_error e ->
			(* Abort properly *)
			raise (InputFileNotFound ("The file `" ^ file_name ^ "` could not be opened.\n" ^ e))
	in
	(* Lexing *)
	print_message Verbose_total ("Lexing…");
	let lexbuf = try (Lexing.from_channel in_channel) with
		(* Failure during parsing *)
		| Failure f ->
			(* Abort properly *)
			raise (ParsingFailure ("Lexing error in file `" ^ file_name ^ "`: " ^ f))
	in
	(* Function to convert a in_channel to a string (in case of parsing error) *)
	let string_of_input () =
		(* Convert the file into a string *)
		let extlib_input = IO.input_channel (open_in file_name) in
			IO.read_all extlib_input
	in
	(* Generic function *)
	print_message Verbose_total ("Calling parser lexer…");
	parser_lexer_gen the_parser the_lexer lexbuf string_of_input file_name


(*(* Parse a string and return the abstract structure *)
let parser_lexer_from_string the_parser the_lexer the_string =
	(* Lexing *)
	let lexbuf = try (Lexing.from_string the_string) with
		| Failure f -> print_error ("Lexing error: " ^ f ^ "\n The string was: \n" ^ the_string ^ ""); abort_program (); exit(1)
(* 		| Parsing.Parse_error -> print_error ("Parsing error\n The string was: \n" ^ the_string ^ ""); abort_program (); exit(1) *)
	in
	(* Function to convert a in_channel to a string (in case of parsing error) *)
	let string_of_input () = the_string in
	(* Generic function *)
	parser_lexer_gen the_parser the_lexer lexbuf string_of_input the_string*)


(************************************************************)
(* High-level parsing functions *)
(************************************************************)

(** Parse a model file and return its (unexpanded) parsing structure *)
let parse_model_from_file (file_name : string) : ParsingStructure.unexpanded_parsed_model =
	(* let lexer_output =
		if !force_included_file_terminator then
			model_token_stream_adapter ModelLexer.token file_name
		else
			ModelLexer.token
	in *)
	parser_lexer_from_file ModelParser.main ModelLexer.token  file_name

(** Parse a property file and return its (unexpanded) parsing structure *)
let parse_property_from_file (file_name : string) : ParsingStructure.unexpanded_parsed_property =
	parser_lexer_from_file PropertyParser.main PropertyLexer.token file_name

let parse_update_from_file (file_name : string) :
	ParsingStructure.unexpanded_parsed_location list =
	let channel = open_in file_name in
	Fun.protect
			~finally:(fun () -> close_in_noerr channel)
			(fun () ->
					let lexbuf = Lexing.from_channel channel in
					ModelParser.update_locations ModelLexer.token lexbuf
			)