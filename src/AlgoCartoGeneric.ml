(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Generic class for cartography-style algorithms
 * 
 * File contributors : Étienne André
 * Created           : 2016/01/19
 * Last modified     : 2016/01/19
 *
 ************************************************************)


(************************************************************)
(************************************************************)
(* Modules *)
(************************************************************)
(************************************************************)
open OCamlUtilities
open ImitatorUtilities
open Exceptions
open AbstractModel
open Result
open AlgoGeneric




(************************************************************)
(************************************************************)
(* Types *)
(************************************************************)
(************************************************************)
(*** NOTE: no use of the "option" type, as we may want to add more values later (e.g., "Maybe more points but could not find any" for the random cartography) ***)
type more_points =
	(* No more uncovered parameter valuations *)
	| No_more
	(* Some more uncovered parameter valuations *)
	| Some_pval of PVal.pval


(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class virtual algoCartoGeneric =
	object (self) inherit algoGeneric as super
	
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	(* Current point *)
	val mutable current_point = No_more
	
	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(* 	method algorithm_name = "IM" *)

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(*	method initialize_variables =
(* 		super#initialize_variables; *)
		(* The end *)
		()*)
	
	(* Create the initial point for the analysis *)
	method virtual get_initial_point : more_points

	(* Find the next point *)
	method virtual find_next_point : more_points


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Main method running the algorithm: implements here a generic cartography, and calls other functions that may be modified in subclasses *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method run () =
		(* Retrieve the model *)
		let model = Input.get_model () in
		
		(* Retrieve the input options *)
		let options = Input.get_options () in
		
		(* Retrieve the first point *)
		current_point <- self#get_initial_point;
		
		(* While there is another point to explore *)
		while (match current_point with Some_pval _ -> true | _ -> false) do
		
			(* Get the point *)
			let pi0 = match current_point with
				| Some_pval pval -> pval
				| _ -> raise (InternalError("Impossible situation in AlgoCartoGeneric: the point should have been tested before"))
			in
			
			(* Print some messages *)
			(*** HACK: only print if non-distributed ***)
			if options#distribution_mode = Options.Non_distributed then(
				print_message Verbose_standard ("\n**************************************************");
				(*** TODO: add back ***)
(* 				print_message Verbose_standard ("BEHAVIORAL CARTOGRAPHY ALGORITHM: " ^ (string_of_int !current_iteration) ^ ""); *)
(* 				print_message Verbose_standard ("Considering the following pi" ^ (string_of_int !current_iteration)); *)
				print_message Verbose_standard (ModelPrinter.string_of_pi0 model pi0);
			);
			
			(* Save the verbose mode as it may be modified *)
			let global_verbose_mode = get_verbose_mode() in
			
			(* Prevent the verbose messages (except in verbose medium, high or total) *)
			if not (verbose_mode_greater Verbose_medium) then
				set_verbose_mode Verbose_mute;
			
			(* Set the new pi0 *)
			Input.set_pi0 (pi0);
			
			(* Call the inverse method *)
			(*** TODO !!! ***)
			let algo = new AlgoIM.algoIM in
			let result = algo#run() in
			(*** WARNING: what is this going to do, exactly? ***)
			ResultProcessor.process_result result;
			
			(* Get the verbose mode back *)
			set_verbose_mode global_verbose_mode;
			
			(* Process the result by IM *)
			(*** TODO ***)
(* 			let _ = bc_process_im_result im_result in (); *)
			
			(* Get to the next point *)
			current_point <- self#find_next_point;
			
		done; (* end while more points *)
		
		
		(* Terminate *)
		(*** TODO ***)
		
		(* The end *)
		raise (InternalError("Not implemented"))
		

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(*	method compute_result =
		raise (InternalError("not implemented"))*)

	
(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
