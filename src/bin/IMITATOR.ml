(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: Main file for IMITATOR
 *
 * File contributors : Ulrich Kühne, Étienne André, Laure Petrucci, Dylan Marinho
 * Created           : 2009/09/07
 *
 ************************************************************)



(************************************************************)
(* Internal modules *)
(************************************************************)
open Lib
open Exporter
open Driver
open Exceptions
open OCamlUtilities

open ImitatorUtilities
open AbstractAlgorithm
open Result
open Options
open Statistics


(**************************************************
TAGS USED THROUGHOUT THIS PROJECT
- (*** BADPROG ***)
- (*** NOTE ***)
- (*** OPTIMIZED ***)
- (*** QUESTION ***)
- (*** TO OPTIMIZE ***)
- (*** TODO ***)
- (*** WARNING ***)
**************************************************)

(* Output handling for a synthesised PTG controller. *)
let export_ptg_controller (options : Options.imitator_options) (controller_model : AbstractModel.abstract_model) : unit =
	(* Write controller to file *)
	let imi_file_name = options#files_prefix ^ "-controller.imi" in
	write_to_file imi_file_name (ModelPrinter.string_of_model controller_model);
	print_highlighted_message Shell_result Verbose_standard ("Controller model `" ^ imi_file_name ^ "` succesfully created.");

	(* Generate graphical representation of controller *)
	if options#ptg_controller_mode == AbstractAlgorithm.Draw then begin
		print_message Verbose_medium ("Translating generated controller model to a graphics…");
		let translated_model = PTA2dot.string_of_model options controller_model in
		let dot_created_file_option = Graphics.dot "pdf" (options#files_prefix ^ "-controller") translated_model in
		match dot_created_file_option with
		| None -> print_error "Oops…! Something went wrong with dot."
		| Some created_file -> print_highlighted_message Shell_result Verbose_standard ("Graphical representation of controller `" ^ created_file ^ "` successfully created.")
	end
;;


(************************************************************)
(************************************************************)
(* STARTING PROGRAM *)
(************************************************************)
(************************************************************)


(************************************************************)
(* Start the global counters *)
(************************************************************)
(* Counter counting everything from beginning to the end *)
let global_counter = create_time_counter_and_register "total" Global_counter Verbose_standard in
global_counter#start;

(* Counter counting everything except the final processing (graphics, external files generation, etc.) *)
let counter_algorithm_and_parsing = create_time_counter_and_register "main algorithm + parsing" Algorithm_counter Verbose_standard in
counter_algorithm_and_parsing#start;

(* Counter counting everything except the final processing (graphics, external files generation, etc.) and the parsing *)
let counter_main_algorithm = create_time_counter_and_register "main algorithm" Algorithm_counter Verbose_standard in



(************************************************************)
(* BEGIN EXCEPTION MECHANISM *)
(************************************************************)

begin
try(

(************************************************************)
(************************************************************)
(* Print startup message *)
(************************************************************)
(************************************************************)

(* Print header *)
print_header_string();


(************************************************************)
(* Get the arguments *)
(************************************************************)

(* let options_parsing_counter = create_time_counter_and_register "options parsing" Parsing_counter Verbose_low in
options_parsing_counter#start;*)

(* Object with command line options *)
let options = new imitator_options in

options#parse;

(* Set the options (for other modules) *)
Input.set_options options;

(*** BUG: verbose mode has been set before the start and the stop; this particular counter may never stop! ***)
(* options_parsing_counter#stop; *)


(* Print date *)
print_message Verbose_experiments ("Analysis time: " ^ (now()) ^ "\n");


(************************************************************)
(* Record backtrace if verbose > standard *)
(************************************************************)
if verbose_mode_greater Verbose_low then(
	Printexc.record_backtrace true;
);



(************************************************************)
(* Get input *)
(************************************************************)
let parsing_counter = create_time_counter_and_register "model parsing and converting" Parsing_counter Verbose_standard in
parsing_counter#start;



(*------------------------------------------------------------*)
(* Parse the model and the property *)
(*------------------------------------------------------------*)

let model, property_option = ParsingUtility.compile_model_and_property options in

(*------------------------------------------------------------*)
(* End of parsing *)
(*------------------------------------------------------------*)
(* End of parsing *)
parsing_counter#stop;


(************************************************************)
(* Set some (default) options *)
(************************************************************)
AlgorithmDriver.set_default_options options model property_option;


(************************************************************)
(* Debug print: model *)
(************************************************************)
if verbose_mode_greater Verbose_high then(
	print_message Verbose_total ("\nPreparing to print the model…");
	let model_string = ModelPrinter.string_of_model model in
	print_message Verbose_high ("\nThe input model is the following one:\n" ^ model_string ^ "\n");
);


(************************************************************)
(* Debug print: property *)
(************************************************************)
begin
match options#property_file_name with
	| Some _ ->
		(*** NOTE: at this stage, we are sure to have defined a property ***)
		let property = match property_option with
			| Some property -> property
			| None -> raise (InternalError "A property should have been set when printing the property after checking Some options#property_file_name")
		in

		print_message Verbose_total ("\nPreparing to print the property…");
		let property_string = ModelPrinter.string_of_abstract_property model property in

		print_message Verbose_low ("\nThe property is the following one:\n" ^ property_string ^ "\n");

	| None ->
		()

end;




(* Statistics *)
counter_main_algorithm#start;


(************************************************************)
(* Prepare clock elimination and extrapolation *)
(************************************************************)
AlgorithmDriver.prepare_model options model;


(************************************************************)
(************************************************************)
(* Start branching depending on the algorithm *)
(************************************************************)
(************************************************************)

begin
match options#imitator_mode with
	| Syntax_check ->

	(************************************************************)
	(* Case no analysis *)
	(************************************************************)
	(* If arrived here, syntax is correct *)
	print_message Verbose_standard "Syntax is correct. Have fun!";

	(* Generate directly the "empty" result for syntax check *)
	ResultProcessor.process_result_and_terminate model Syntax_check_result "syntax check" None global_counter


	(************************************************************)
	(* Case translation *)
	(************************************************************)
	(*------------------------------------------------------------*)
	(* Translation to text language (IMITATOR, other model checkers, TikZ…) *)
	(*------------------------------------------------------------*)
	| Translation IMI | Translation HyTech | Translation TikZ | Translation Uppaal | Translation JaniSpec | Translation DOT ->

		let target = match options#imitator_mode with
			| Translation translation -> translation
			| _ -> raise (InternalError ("Impossible situation: No target for translation was found, although it should have been"))
		in

		(* Select the exporter, render the model and write the output file.
		   All of selection/naming/writing now lives in ExportDriver. *)
		let _target_language_file = ExportDriver.export_and_write options model target in

		(* Create a file with some statistics on the original model if requested *)
		ResultProcessor.process_result model None Translation_result ("translation to " ^ (AbstractAlgorithm.string_of_translation target)) None;

		terminate_program()

	(*------------------------------------------------------------*)
	(* Translation of property *)
	(*------------------------------------------------------------*)
	| Translation ImiProp ->

		(*** TODO! check ***)
		(*** NOTE: at this stage, we are sure to have defined a property ***)
		let property = match property_option with
			| Some property -> property
			| None -> raise (InternalError "A property should have been set when regenerating the property")
		in

		print_message Verbose_standard ("Regenerating the property to a new file.");

		let suffix = "-imiprop.imiprop" in
		let target_language_file = options#files_prefix ^ suffix in
		let regenerated_property = ModelPrinter.string_of_abstract_property model property in
		if verbose_mode_greater Verbose_total then(
			print_message Verbose_total ("\n" ^ regenerated_property ^ "\n");
		);

		(* Write *)
		write_to_file target_language_file regenerated_property;
		print_message Verbose_standard ("File '" ^ target_language_file ^ "' successfully created.");

		(* Create a file with some statistics on the original model if requested *)
		ResultProcessor.process_result model None Translation_result ("translation to " ^ (AbstractAlgorithm.string_of_translation
			(match options#imitator_mode with Translation translation -> translation | _ -> raise (InternalError ("Impossible situation: No target for translation was found, although it should have been"))
			)) ) None;

		terminate_program()


	(*------------------------------------------------------------*)
	(* Translation of the model to a graphics *)
	(*------------------------------------------------------------*)
	| Translation JPG | Translation PDF | Translation PNG ->
		print_message Verbose_medium ("Translating model to a graphics…");
		let translated_model = PTA2dot.string_of_model options model in
		if verbose_mode_greater Verbose_high then(
			print_message Verbose_high ("\n" ^ translated_model ^ "\n");
		);
		(*** NOTE: not so nice… ***)
		let extension = match options#imitator_mode with
			| Translation JPG	-> "jpg"
			| Translation PDF	-> "pdf"
			| Translation PNG	-> "png"
			| _					-> raise (InternalError ("Impossible situation: No graphic extension found although JPG/PDF/PNG was expected"))
		in
		let dot_created_file_option = Graphics.dot extension (options#files_prefix ^ "-pta") translated_model in
		begin
		match dot_created_file_option with
		| None -> print_error "Oops…! Something went wrong with dot."
		| Some created_file -> print_message Verbose_standard ("File `" ^ created_file ^ "` successfully created.");
		end;

		(* Create a file with some statistics on the original model if requested *)
		ResultProcessor.process_result_and_terminate model Translation_result "translation to graphics" None global_counter


	(************************************************************)
	(* Computation of the whole state space *)
	(************************************************************)
	| State_space_computation ->

		(*** NOTE: this is static subclass coercition; see https://ocaml.org/learn/tutorials/objects.html ***)
		let concrete_algorithm :> AlgoGeneric.algoGeneric = new AlgoPostStar.algoPostStar model options in

		(*** NOTE: duplicate code with what follows ***)

		let result = concrete_algorithm#run in

		(* Stop the main algorithm counters *)
		counter_algorithm_and_parsing#stop;
		counter_main_algorithm#stop;

		(* Process and terminate *)
		ResultProcessor.process_result_and_terminate model result concrete_algorithm#algorithm_name None global_counter


	(************************************************************)
	(* Some algorithm *)
	(************************************************************)
	(* Synthesis algorithm *)
	| Algorithm ->
	begin
		(* Retrieve the algorithm *)
		(*** NOTE: at this stage, we are sure to have defined a property ***)
		let property = match property_option with
			| Some property -> property
			| None -> raise (InternalError "A property should have been set when starting to enumerate Algorithm cases")
		in

		(* Find the correct concrete algorithm to execute *)
		let concrete_algorithm = AlgorithmDriver.algorithm_of_property options model property (export_ptg_controller options) in



		(* Run! *)
		let result = concrete_algorithm#run in

		(* Stop the main algorithm counters *)
		counter_algorithm_and_parsing#stop;
		counter_main_algorithm#stop;

		(* Process *)
		ResultProcessor.process_result model property_option result concrete_algorithm#algorithm_name None;

	end; (* match type of abstract algorithm *)

(************************************************************)
(************************************************************)
(* End branching depending on the algorithm *)
(************************************************************)
(************************************************************)
end;



(************************************************************)
(* END EXCEPTION MECHANISM *)
(************************************************************)
) with
	e ->(

	(* Actions to perform for "bad" exceptions, i.e., that should NOT happen *)
	let abort_with_bad_exception (error_message : string) =
		(* Stop the main algorithm counters *)
		counter_algorithm_and_parsing#stop;
		counter_main_algorithm#stop;

		print_error (error_message ^ "\nPlease (politely) insult the developers.");
		Printexc.print_backtrace stderr;

		abort_program ();
		(* Safety *)
		exit 1
	in

	(* Actions to perform for "good" exceptions, i.e., that depend on the user model *)
	let abort_with_good_exception error_type error_message =
		(* Stop the main algorithm counters *)
		counter_algorithm_and_parsing#stop;
		counter_main_algorithm#stop;

		(* Print error *)
		print_error (error_message);

		(* Force output result if not set *)
		(*** NOTE: probably useless check ***)
		let options = Input.get_options () in
		if not options#is_set_output_result then(
			options#set_output_result true;
		);

		(* Process result (including file export, if possible) and fail *)
		ResultProcessor.process_result_and_abort error_type "unknown algorithm" (* because we lost this information somewhere… *) None global_counter;
	in

	begin match e with
		(* "Good" (at least not bad) exceptions *)

		| Division_by_0 msg -> abort_with_good_exception (Result.Division_by_zero msg) msg
        | Out_of_range msg -> abort_with_good_exception (Result.Out_of_range) msg
        | Empty_collection msg -> abort_with_good_exception (Result.Empty_collection) msg
		| UnsatisfiableInitialConditions -> abort_with_good_exception (Result.Unsatisfiable_initial_conditions) "Unsatisfiable initial conditions"



		(* "Bad" exceptions *)

		| Failure msg -> abort_with_bad_exception ("`Failure` exception: `" ^ msg ^ "`")

		| InterfacingError msg -> abort_with_bad_exception ("Interfacing error: " ^ msg ^ "")

		| InternalError msg -> abort_with_bad_exception ("Fatal internal error: " ^ msg ^ "")

		| Invalid_argument msg -> abort_with_bad_exception ("`Invalid_argument` exception: `" ^ msg ^ "`")

		| Not_found -> abort_with_bad_exception ("`Not_found` exception!")

		| NotImplemented msg -> abort_with_bad_exception ("A non-implemented feature has been called: " ^ msg ^ "")

		| Random_generator_initialization_exception-> abort_with_bad_exception("A fatal error occurred during the random generator initialization.")

		| SerializationError msg -> abort_with_bad_exception ("Serialization error: " ^ msg ^ "")

		| Out_of_memory -> abort_with_bad_exception ("Oops! Out of memory")

		| Stack_overflow -> abort_with_bad_exception ("Oops! Stack overflow")

		| _ -> abort_with_bad_exception ("Fatal exception `" ^ (Printexc.to_string e) ^ "`.")
	end;

	(* Safety *)
	exit 1

	)
end; (* try *)




(************************************************************)
(* Handling statistics *)
(************************************************************)
global_counter#stop;

(* Only print counters if statistics are required, or experiments verbose mode *)
if (try (Input.get_options())#statistics with _ -> false) || verbose_mode_greater Verbose_experiments then
	print_message Verbose_standard (string_of_all_counters());


(************************************************************)
(* Bye bye! *)
(************************************************************)

terminate_program()
