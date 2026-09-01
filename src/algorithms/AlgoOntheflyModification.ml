(************************************************************
 *
 *                       IMITATOR
 *
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: Temporary on-the-fly model modification
 *
 * File contributors : Étienne André
 * Created           : 2025/05/14
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open ImitatorUtilities
open Exceptions
open Result
open AlgoGeneric
open ModelProvider



(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoOntheflyModification (model : AbstractModel.abstract_model) 
								(provider : model_provider) 
								(options : Options.imitator_options) =
	object (self) inherit algoGeneric model options (*as super*)
	
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "On-the-fly modification"
	
	method process_current_model =
      print_message Verbose_standard "hello"
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)
	

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Main method *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method run =
			(* let parsed_update = ParsingUtility.parsing_structure_of_ontheflycommand options in *)
				(* match parsed_update with
				| Parsed_new_location location ->
					print_message Verbose_standard
						("New location: " ^ location); *)


	print_message Verbose_standard
        "Successfully parsed the initial model.";

	(* print_message Verbose_standard
        "Successfully parsed on-the-fly update."; *)

	print_message Verbose_standard
		"ABOUT TO ENTER WAIT LOOP.";

	let finished = ref false in

	while not !finished do
        (* `model` is the same mutable object.
           Provider has potentially updated it. *)
        (* self#process_current_model; *)
		print_message Verbose_standard
			"WAITING FOR KEY (q = quit)...";

		match provider#wait_for_update with

			| Updated content->
				print_message Verbose_standard
					("Received an update (algo on the fly): " ^ content);
				self#process_current_model

			| Finished ->
				print_message Verbose_standard
					"Received q.";
				finished := true

      done;

      (* self#build_result *)

    	print_message Verbose_standard
            "On-the-fly modification terminated.";

    OnTheFly_Update_result


end;;
(************************************************************)
(************************************************************)
