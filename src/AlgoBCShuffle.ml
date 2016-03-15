(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Classical Behavioral Cartography with exhaustive coverage of integer points [AF10]. Shuffled version, used for the distributed cartography. [ACN15]
 * 
 * File contributors : Étienne André
 * Created           : 2016/03/14
 * Last modified     : 2016/03/15
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
open AlgoBCCover


(************************************************************)
(************************************************************)
(* Internal exceptions *)
(************************************************************)
(************************************************************)
(* To stop a loop when a point is found *)
exception Found_point of PVal.pval

(* To stop a loop when a point is found or there is no more point *)
exception Stop_loop of more_points



(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoBCShuffle =
	object (self) inherit algoBCCover as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	(* The points array to be shuffled *)
	val mutable all_points_array : PVal.pval array option = None
	
	(* The index in the array *)
	val mutable next_point_index = 0
	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Return the all_points_array; raises InternalError if all_pi0_array was not initialized *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	
	method private get_all_points_array_option =
		match all_points_array with
		| None -> 
			raise (InternalError("all_points_array has not been initialized yet, altough it should have at this point."))
		| Some all_points_array -> all_points_array

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Compute an array made of *all* points in V0 *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private compute_all_points =
	(*	(* Get the model *)
		let model = Input.get_model() in
		(* Get the v0 *)
		let v0 = Input.get_v0() in*)
		(* Retrieve the input options *)
		let options = Input.get_options () in

		(*** WARNING: step not implemented here! ***)
		if NumConst.neq options#step NumConst.one then(
			raise (InternalError("The step must be equal to 1 to compute all pi0 in V0 (for now)."));
		);
		
		(* Check that nb_points has been computed (it should have) *)
		if NumConst.equal nb_points NumConst.zero then(
			raise (InternalError("The number of points in V0 has not been computed (but it should)."));
		);
		
		(* First check that the number of points can be represented as an int *)
		if not (NumConst.is_int nb_points) then(
			raise (InternalError("The number of points in V0 is too big to be represented as an int."));
		);
		
		(* Convert to int *)
		let int_nb_points = NumConst.to_int nb_points in
		
		(* Create a array for all the pi0, initially containing a useless object everywhere *)
		let useless_pi0 = new PVal.pval in
		self#print_algo_message Verbose_medium ("[compute_all_pi0] Creating an array of " ^ (string_of_int int_nb_points) ^ " points");
		let all_points = Array.make int_nb_points useless_pi0 in
		
		self#print_algo_message Verbose_medium ("[compute_all_pi0] Computing the initial pi0");
		(* Set the first point *)
		let first_point = self#compute_smallest_point in
		self#print_algo_message Verbose_medium ("[compute_all_pi0] Done computing the initial pi0");
		
		self#print_algo_message Verbose_medium ("[compute_all_pi0] Setting pi0 to the first point");
		(* Fill the first point with the initial pi0 *)
		all_points.(0) <- first_point;
		
		self#print_algo_message Verbose_medium ("[compute_all_pi0] Computing the other points");
		self#print_algo_message Verbose_medium ("[compute_all_pi0] Computing the other points");
		(* Fill it for the other points *)
		let current_point = ref first_point in
		for pi0_index = 1 to int_nb_points - 1 do
			(* Compute the next pi0 *)
			let next_pi0_option = self#compute_next_sequential_pi0 !current_point in
			
			let next_pi0 = match next_pi0_option with
				(* If no more pi0: problem! *)
				| None -> raise (InternalError("No more pi0 before completing to fill the static array of all pi0."))
				| Some next_pi0 -> next_pi0
			in

			(* Update the current point *)
			current_point := next_pi0;

			(* Get the current pi0 and COPY it! Very important *)
			(*** NOTE (2016/03/14): why?? ***)
(* 			let current_pi0_copy = next_pi0#copy in *)
			(* Fill the array *)
			all_points.(pi0_index) <- !current_point;
		done;
		
		self#print_algo_message Verbose_medium ("[compute_all_pi0] Done computing the other points");
		
		(* Print some information *)
		if verbose_mode_greater Verbose_total then(
			(*** BEGIN DEBUG ***)
			(* Print all pi0 *)
			let model = Input.get_model() in
			for pi0_index = 0 to Array.length all_points - 1 do
				print_message Verbose_standard ((string_of_int pi0_index) ^ ":");
				print_message Verbose_standard (ModelPrinter.string_of_pi0 model all_points.(pi0_index));
			done;
			(*** END DEBUG ***)
		);
		
		(* Return the array *)
		all_points

(*		(* Set the first pi0 *)
		current_pi0 := Some (all_points.(0));
		all_pi0_array_current := 0;*)
		
		
		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "BC (full coverage, shuffle)"

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
		super#initialize_variables;
		
		(* Initialize the shuffled array *)
		(* 1. Compute all points *)
		let all_points = self#compute_all_points in
		
		(* 2. shuffle *)
		(*** TODO: add counter ***)
		(*** NOTE: applied two times, because once is quite deterministic (see warning in the array_shuffle code) ***)
		array_shuffle all_points;
		array_shuffle all_points;
		(*** TODO: add counter ***)
		
		(* 3. Set it *)
		all_points_array <- Some all_points;
		
		(* 4. Initialize the index *)
		next_point_index <- 0;
		
		(* Print some information *)
		if verbose_mode_greater Verbose_high then(
			(*** BEGIN DEBUG ***)
			(* Print all pi0 *)
			let model = Input.get_model() in
			for pi0_index = 0 to Array.length all_points - 1 do
				print_message Verbose_standard ((string_of_int pi0_index) ^ ":");
				print_message Verbose_standard (ModelPrinter.string_of_pi0 model all_points.(pi0_index));
			done;
			(*** END DEBUG ***)
		);

		(* The end *)
		()

		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Global method on pi0 *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Compute the sequential successor of a point. Returns Some next_pi0 if there is indeed one, or None if no more point is available. *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

      
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
		(* Retrieve the array *)
		let all_points = self#get_all_points_array_option in
	
		(* Get the point *)
		let initial_point = all_points.(0) in
		
		(* Increment the next point index *)
		next_point_index <- next_point_index + 1;
		
		(* Return the point *)
		Some_pval initial_point

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Find the immediatly next point in the shuffled array *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private next_point_in_array =
		(* Retrieve the array *)
		let all_points = self#get_all_points_array_option in
	
		(* Check that the array limit is not reached *)
		if next_point_index >= Array.length all_points then None
		else(
		
			(* Get the point *)
			let the_point = all_points.(next_point_index) in
			
			(* Increment the next point index *)
			next_point_index <- next_point_index + 1;
			
			(* Return the point *)
			Some the_point
		)
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Find the next point *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** BADPROG: code mostly copied from AlgoBCCover ***)
	method find_next_point =
		(* Get the model *)
	(* 	let model = Input.get_model() in *)

		(* Retrieve the current pi0 (that must have been initialized before) *)
		let current_pi0 = ref (self#get_current_point_option) in
		
		try(
		while true do
			
			(* 1) Compute the next pi0 (if any left) in a sequential manner *)
			let tentative_next_point =
			match self#next_point_in_array with
			| Some point -> point
			| None -> raise (Stop_loop No_more)
			in
			
			(* 2) Update our local current_pi0 *)
			current_pi0 := tentative_next_point;
			
			(* 3) Check that this pi0 is not covered by any tile *)
			self#print_algo_message Verbose_high ("Check whether pi0 is covered");
			(* If uncovered: stop loop and return *)
			if self#test_pi0_uncovered !current_pi0 then
				raise (Stop_loop (Some_pval !current_pi0))
			(* Else: keep running the loop *)
			
		done; (* while more pi0 and so on *)
		
		(* This point is unreachable *)
		raise (InternalError("This part of the code should be unreachable in find_next_point"))
		
		(* Return the point *)
		) with Stop_loop sl -> sl
		
		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_bc_result =
		self#print_algo_message_newline Verbose_standard (
			"Successfully terminated " ^ (after_seconds ()) ^ "."
		);

		
		(* Get the termination status *)
		 let termination_status = match termination_status with
			| None -> raise (InternalError "Termination status not set in BCShuffle.compute_bc_result")
			| Some status -> status
		in

		(* Coverage is... *)
		(*** NOTE: this is only true for the original behavioral cartography; for variants this may not hold ***)
		let coverage =
			(* INTEGER COMPLETE if termination is regular and all tiles are exact or under-approximations *)
			if termination_status = BC_Regular_termination && (List.for_all (fun abstract_im_result -> match abstract_im_result.soundness with
					| Constraint_exact | Constraint_maybe_under -> true
					| Constraint_maybe_over | Constraint_maybe_invalid -> false
				) im_results)
				then Coverage_integer_complete
			(* UNKNOWN otherwise *)
			else Coverage_unknown
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
