	(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Parsing functions for input elements
 *
 * File contributors : Ulrich Kühne, Étienne André
 * Created           : 2014/03/15
 * Last modified     : 2021/01/22
 *
 ************************************************************)


(************************************************************)
(* External modules *)
(************************************************************)
open Gc


(************************************************************)
(* Internal modules *)
(************************************************************)
open Exceptions
open AbstractModel
open AbstractAlgorithm
open OCamlUtilities
open ImitatorUtilities
open Result
open Statistics


let parsing_counter = create_time_counter_and_register "model parsing" Parsing_counter Verbose_experiments

let converting_counter = create_time_counter_and_register "model converting" Parsing_counter Verbose_experiments

(************************************************************)
(* Parsing errors management *)
(************************************************************)
let print_error_and_abort (options : Options.imitator_options) (error_message : string) (error_type : Result.error_type) =
	(* Print the error *)
	print_error error_message;
	
	(* Force output result if not set *)
	if not options#is_set_output_result then(
		options#set_output_result true;
	);

	(* Process result (including file export, if possible) and fail *)
	ResultProcessor.process_result_and_abort error_type "unset algorithm" None ((*** HACK ***)converting_counter);
	
	(* Safety *)
	exit(1)


(* Defining types for errors *)
type model_or_property =
	| Model
	| Property

let parsing_error_of model_or_property error_message = match model_or_property with
	| Model -> Result.ModelParsing_error error_message
	| Property -> Result.ModelParsing_error error_message

let filenotfound_error_of model_or_property = match model_or_property with
	| Model -> Result.ModelFileNotFound_error
	| Property -> Result.PropertyFileNotFound_error


(************************************************************)
(* Local parsing function *)
(************************************************************)

(* Generic parser that returns the abstract structure *)
let parser_lexer_gen (model_or_property : model_or_property) (options : Options.imitator_options) the_parser the_lexer lexbuf string_of_input file_name =
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
			let failure_message = "Parsing error in file `" ^ file_name ^ "` " ^ error_message in
			print_error_and_abort options failure_message (parsing_error_of model_or_property failure_message)

		| UnexpectedToken c ->
			(* Print some information *)
			print_message Verbose_total ("Parsing error detected `UnexpectedToken`. Processing…");
			(* Abort properly *)
			let failure_message = "Parsing error in file `" ^ file_name ^ "`: unexpected token `" ^ (Char.escaped c) ^ "`." in
			print_error_and_abort options failure_message (parsing_error_of model_or_property failure_message)

		
		(*** HACK: added because of some mysterious exception raised during parsing (2020/04/16) ***)
		| Invalid_argument (*"index out of bounds"*)_ ->
			(* Print some information *)
			print_message Verbose_total ("Parsing error detected `index out of bounds`. Processing…");
			(* Abort properly *)
			let failure_message = "Mysterious parsing error in file `" ^ file_name ^ "`, maybe at the very beginning." in
			print_error_and_abort options failure_message (parsing_error_of model_or_property failure_message)
		
		| Failure f ->
			(* Print some information *)
			print_message Verbose_total ("Parsing error detected `Failure`. Processing…");
			(* Abort properly *)
			let failure_message = "Parsing error (`failure`) in file `" ^ file_name ^ "`: " ^ f in
			print_error_and_abort options failure_message (parsing_error_of model_or_property failure_message)

	in
	parsing_structure


(* Parse a file and return the abstract structure *)
let parser_lexer_from_file (model_or_property : model_or_property) (options : Options.imitator_options) the_parser the_lexer file_name =
	(* Open file *)
	print_message Verbose_total ("Opening in_channel…");
	let in_channel = try (open_in file_name) with
		| Sys_error e ->
			(* Abort properly *)
			let failure_message = "The file `" ^ file_name ^ "` could not be opened.\n" ^ e in
			print_error_and_abort options failure_message (filenotfound_error_of model_or_property)
	in
	(* Lexing *)
	print_message Verbose_total ("Lexing…");
	let lexbuf = try (Lexing.from_channel in_channel) with
		| Failure f ->
			(* Abort properly *)
			let failure_message = "Lexing error in file `" ^ file_name ^ "`: " ^ f in
			print_error_and_abort options failure_message (parsing_error_of model_or_property failure_message)
	in
	(* Function to convert a in_channel to a string (in case of parsing error) *)
	let string_of_input () =
		(* Convert the file into a string *)
		let extlib_input = IO.input_channel (open_in file_name) in
			IO.read_all extlib_input
	in
	(* Generic function *)
	print_message Verbose_total ("Calling parser lexer…");
	parser_lexer_gen model_or_property options the_parser the_lexer lexbuf string_of_input file_name


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
(** Compile the concrete model and convert it into an abstract model *)
(************************************************************)
let compile_model_and_property (options : Options.imitator_options) =

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Parsing the model *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(* Statistics *)
	parsing_counter#start;

	(* Parsing the main model *)
	print_message Verbose_low ("Parsing model file " ^ options#model_file_name ^ "…");
	let parsed_model : ParsingStructure.parsed_model = parser_lexer_from_file Model options ModelParser.main ModelLexer.token options#model_file_name in

	(* Statistics *)
	parsing_counter#stop;

	print_message Verbose_low ("\nModel parsing completed " ^ (after_seconds ()) ^ ".");
	
	(** USELESS, even increases memory x-( **)
	(* Gc.major (); *)


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Parsing the property *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	
	(* We parse a property file if 1) the algorithm requires a property OR 2) the algorithm has an optional property and there is indeed a property *)
	let property_parsing =
		AbstractAlgorithm.property_needed options#imitator_mode = Second_file_required
		||
		(AbstractAlgorithm.property_needed options#imitator_mode = Second_file_optional && options#property_file_name <> None)
	in

	let parsed_property_option =
	if property_parsing then(
		(* Statistics *)
		parsing_counter#start;

		(* Get the file name *)
		let property_file_name = match options#property_file_name with
			| Some property_file_name -> property_file_name
			| None -> raise (InternalError "No property file name found in `compile_model_and_property` although it was expected.")
		in
		
		print_message Verbose_low ("Parsing property file `" ^ property_file_name ^ "`…");
		
		(* Parsing the property *)
		let parsed_property : ParsingStructure.parsed_property = parser_lexer_from_file Property options PropertyParser.main PropertyLexer.token property_file_name in

		(* Statistics *)
		parsing_counter#stop;

		print_message Verbose_low ("\nProperty parsing completed " ^ (after_seconds ()) ^ ".");
		
		Some parsed_property
	)else(
		None
	)
	in
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Conversion to abstract structures *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(* Statistics *)
	converting_counter#start;

	let model, property_option =
	try (
		ModelConverter.abstract_structures_of_parsing_structures options parsed_model parsed_property_option
	) with
		| ModelConverter.InvalidModel ->
			(* Abort properly *)
			let failure_message = "The input model contains errors. Please check it again." in
			print_error_and_abort options failure_message (Result.InvalidModel_error)

		| ModelConverter.InvalidProperty ->
			(* Abort properly *)
			let failure_message = "The property contains errors. Please check it again." in
			print_error_and_abort options failure_message (Result.InvalidModel_error)

        | ModelConverter.InvalidExpression message	->
            (* Abort properly *)
            let failure_message =  "An expression contains errors. Please check it again.\nDetails : " ^ message in
            print_error_and_abort options failure_message (Result.InvalidModel_error)

        | ModelConverter.TypeError message
        | TypeChecker.TypeError message	->
            (* Abort properly *)
            let failure_message =  "Type error: " ^ message in
            print_error_and_abort options failure_message (Result.InvalidModel_error)
		| InternalError e ->
			(print_error ("Internal error while parsing the input model and the property: " ^ e ^ "\nPlease kindly insult the developers."); abort_program (); exit 1)
		in

	(* Statistics *)
	converting_counter#stop;

	(* Print some information *)
	print_message Verbose_experiments ("\nAbstract model built " ^ (after_seconds ()) ^ ".");
	let gc_stat = Gc.stat () in
	let nb_words = gc_stat.minor_words +. gc_stat.major_words -. gc_stat.promoted_words in
	let nb_ko = nb_words *. 4.0 /. 1024.0 in
	print_message Verbose_experiments ("Memory for abstract model: " ^ (round3_float nb_ko) ^ " KiB (i.e., " ^ (string_of_int (int_of_float nb_words)) ^ " words)");

	(* Ugly line break *)
	print_message Verbose_experiments "";


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* return *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	model, property_option

