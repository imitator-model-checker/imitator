(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Random Behavioral Cartography with a maximum number of consecutive failed attempts to find a non-integer point not covered by any tile [AF10]
 * Note: the algorithm does NOT track points already computed randomly but not kept because covered by some tile.
 * 
 * File contributors : Étienne André
 * Created           : 2016/02/02
 * Last modified     : 2016/03/10
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
open AlgoCartoGeneric


(************************************************************)
(************************************************************)
(* Internal exceptions *)
(************************************************************)
(************************************************************)
(* To stop a loop when a point is found *)
exception Found_point of PVal.pval

(* To stop a loop when a point is found or there is no more point *)
(* exception Stop_loop of more_points *)



(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
(*** NOTE: this function cannot have max_tries as a parameter, as it it inherits algoCartoGeneric which has none ***)
class algoBCRandom (*max_tries*) =
	object (self) inherit algoCartoGeneric as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	(* Current number of failed attempts to find an integer point not covered by any tile *)
(* 	val mutable nb_failed_attempts = 0 *)

	(* Variable to be initialized *)
	val mutable max_tries : int option = None
	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "BC (random coverage)"

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Set the maximum number of tries (must be done right after creating the algorithm object!) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method set_max_tries m =
		max_tries <- Some m
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Getting max_tries *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private get_max_tries : int =
		match max_tries with
			| Some m -> m
			| None -> raise (InternalError ("In algoBCRandom.get_max_tries, the number of maximum tries should have been already initialized."))

		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
		super#initialize_variables;
		
(* 		nb_failed_attempts <- 0; *)
		
		(* The end *)
		()

		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Global method on pi0 *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(* Return the current_point; raises InternalError if current_point was not initialized *)
	(*** WARNING: duplicate code (see AlgoBCCover) ***)
	method private get_current_point_option =
		match current_point with
		| No_more -> 
			raise (InternalError("current_point has not been initialized yet, altough it should have at this point."))
		| Some_pval current_point -> current_point


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Methods on random generation of a pi0 *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	method private one_random_pi0 : PVal.pval =
	(* Get the model *)
	let model = Input.get_model() in
	(* Get the v0 *)
	let v0 = Input.get_v0() in
	(* Retrieve the input options *)
	let options = Input.get_options () in
	
	(*** WARNING! Does not work with step <> 1 !!!! ***)
	if options#step <> NumConst.one then
		raise (InternalError("Random pi0 not implemented with steps <> 1."));
	
	(* Create the pi0 *)
	let random_pi0 = new PVal.pval in
	(* Fill it *)
	for i = 0 to model.nb_parameters - 1 do
		let min = v0#get_min i in
		let max = v0#get_max i in
		(* Generate a random value in the interval *)
		let random_value = NumConst.random_integer min max in
		
		(* Print some information *)
		if verbose_mode_greater Verbose_medium then(
			self#print_algo_message Verbose_medium ("Generating randomly value '" ^ (NumConst.string_of_numconst random_value) ^ "' for parameter '" ^ (model.variable_names i) ^ "'.");
		);
 		
		(* Add to the array *)
		random_pi0#set_value i random_value;
	done;
	
	(* Return the result *)
	random_pi0

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Return a new instance of the algorithm to be iteratively called (typically IM or PRP) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_instance =
		(* Create a new instance of IM *)
		new AlgoIM.algoIM

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Create the initial point for the analysis *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method get_initial_point =
		(* Retrieve the model *)
(* 		let model = Input.get_model() in *)

		(* Return a random point *)
		Some_pval (self#one_random_pi0)

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Find the next point *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method find_next_point =
		(* Get the model *)
(* 		let model = Input.get_model() in *)
		(* Retrieve the input options *)
(* 		let options = Input.get_options () in *)
		
		(* Get the max number of tries of BC *)
		let max_tries = (*match options#imitator_mode with
			 | Random_cartography i -> i
			 | _ -> raise (InternalError ("Calling algoBCRandom.find_next_point should be done using the random cartography only"))*)
			 self#get_max_tries
		in

		(* Print some information *)
		self#print_algo_message Verbose_low ("Trying to randomly find a fresh pi0 with " ^ (string_of_int max_tries) ^ " tries.");

		(* Counter *)
		let nb_tries = ref 0 in
		
		try(
			(* Loop until exceed the number of tries *)
			while !nb_tries < max_tries do
				(* Generate a random pi0 *)
				let tentative_pi0 = self#one_random_pi0 in
				(* Try to see if valid (and updates found_pi0) *)
				if self#test_pi0_uncovered tentative_pi0 then(
					(* Print some information *)
					self#print_algo_message  Verbose_low ("Try " ^ (string_of_int (!nb_tries + 1)) ^ " successful!");

					raise (Found_point tentative_pi0);
					
				(* Otherwise: go further *)
				)else(
					(* Print some information *)
					self#print_algo_message  Verbose_low ("Try " ^ (string_of_int (!nb_tries + 1)) ^ " unsuccessful.");
					
					(* Increment local counter *)
					nb_tries := !nb_tries + 1;
					
					(* Increment global counter *)
					nb_unsuccessful_points <- nb_unsuccessful_points + 1;
				);
			done;

			(* Print some information *)
			self#print_algo_message  Verbose_low ("Could not find a pi0 within " ^ (string_of_int max_tries) ^ " tries.");
			
			(* Could not find point *)
			No_more
			
		(* Handle the Found_point exception *)
		) with Found_point tentative_pi0 -> Some_pval tentative_pi0



	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_bc_result =
		self#print_algo_message_newline Verbose_standard (
			"Successfully terminated " ^ (after_seconds ()) ^ "."
		);

		
		(* Get the termination status *)
		 let termination_status = match termination_status with
			| None -> raise (InternalError "Termination status not set in BCRandom.compute_bc_result")
			| Some status -> status
		in

		(* Coverage is... *)
		(*** TODO: could perhaps check manually (!) if the coverage is integer complete, or even complete ***)
		let coverage = Coverage_unknown
		in
		
		(* Return result *)
		BC_result {
			(* Number of points in V0 *)
			size_v0				= nb_points;
			
			(* List of tiles *)
			(*** NOTE: reverse as each im_result was added as first element ***)
			tiles				= List.rev im_results;
			
			(* Total computation time of the algorithm *)
			computation_time	= time_from start_time;
			
			(* Computation time to look for points *)
			find_point_time		= find_next_point_counter#value;
			
			(* Number of points on which IM could not be called because already covered *)
			nb_unsuccessful_points = nb_unsuccessful_points;
			
			(* Evaluation of the coverage of V0 by tiles computed by the cartography *)
			coverage			= coverage;
			
			(* Termination *)
			termination			= termination_status;
		}


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
