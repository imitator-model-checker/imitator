	(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Parsing functions for input elements.
 * Note: this module is a wrapper around the `parsing` library. It is in charge
 * of error reporting (options, result processing) and of the conversion of the
 * parsing structures into abstract structures.
 *
 * File contributors : Ulrich Kühne, Étienne André
 * Created           : 2014/03/15
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
open AbstractAlgorithm
open OCamlUtilities
open ImitatorUtilities
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
type parsed_structure_type =
	| Model
	| OnTheFlyModification
	| Property

let parsing_error_of parsed_structure_type error_message = match parsed_structure_type with
	| Model -> Result.ModelParsing_error error_message
	| OnTheFlyModification -> Result.ModelParsing_error error_message (*** TODO ***)
	| Property -> Result.ModelParsing_error error_message

let filenotfound_error_of parsed_structure_type = match parsed_structure_type with
	| Model -> Result.ModelFileNotFound_error
	| OnTheFlyModification -> Result.UpdateFileNotFound_error 
	| Property -> Result.PropertyFileNotFound_error


(************************************************************)
(* Local parsing function *)
(************************************************************)

(* Call a parsing function of the `parsing` library, and abort properly (using the options) in case of error *)
let parse_or_abort (model_or_property : model_or_property) (options : Options.imitator_options) (parsing_function : string -> 'parsing_structure) (file_name : string) : 'parsing_structure =
	try(
		parsing_function file_name
	) with
		| ParsingDriver.ParsingFailure failure_message ->
			(* Abort properly *)
			print_error_and_abort options failure_message (parsing_error_of model_or_property failure_message)

		| ParsingDriver.InputFileNotFound failure_message ->
			(* Abort properly *)
			print_error_and_abort options failure_message (filenotfound_error_of model_or_property)


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
	let parsed_model : ParsingStructure.unexpanded_parsed_model = parse_or_abort Model options ParsingDriver.parse_model_from_file options#model_file_name in

	(* Statistics *)
	parsing_counter#stop;

	print_message Verbose_low ("\nModel parsing completed " ^ (after_seconds ()) ^ ".");

	(*** USELESS, even increases memory x-( ***)
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
		let parsed_property : ParsingStructure.unexpanded_parsed_property = parse_or_abort Property options ParsingDriver.parse_property_from_file property_file_name in

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
		| InvalidModel ->
			(* Abort properly *)
			let failure_message = "The input model contains errors. Please check it again." in
			print_error_and_abort options failure_message (Result.InvalidModel_error)

		| ModelConverter.InvalidProperty ->
			(* Abort properly *)
			let failure_message = "The property contains errors. Please check it again." in
			print_error_and_abort options failure_message (Result.InvalidModel_error)

        | InvalidExpression message	->
            (* Abort properly *)
            let failure_message =  "An expression contains errors. Please check it again.\nDetails : " ^ message in
            print_error_and_abort options failure_message (Result.InvalidModel_error)

        | TypeError message	->
            (* Abort properly *)
            let failure_message =  "Type error: " ^ message in
            print_error_and_abort options failure_message (Result.InvalidModel_error)
        | UndefinedFunction function_name ->
            (* Abort properly *)
            let failure_message =  "Function `" ^ function_name ^ "` is undefined." in
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


(************************************************************)
(* Compile an on-the-fly modification of the model *)
(************************************************************)
let parsing_structure_of_ontheflycommand (options : Options.imitator_options) =

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Parsing the model *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)


	(* Parsing the main model *)
	(* print_message Verbose_low ("Parsing model file " ^ options#model_file_name ^ "…");
	let parsed_ontheflyupdate : ParsingStructure.on_the_fly_update = parser_lexer_from_file Model options ModelUpdateParser.main ModelUpdateLexer.token options#model_file_name in *)


    (*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
    (* Parsing the on-the-fly update file *)
    (*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

    print_message Verbose_low
        ("Parsing on-the-fly update file "
         ^ options#update_file_name ^ "…");

    let parsed_ontheflyupdate : ParsingStructure.on_the_fly_update =
        parser_lexer_from_file
            OnTheFlyModification
            options
            ModelUpdateParser.main
            ModelUpdateLexer.token
            options#update_file_name
    in

	parsed_ontheflyupdate


