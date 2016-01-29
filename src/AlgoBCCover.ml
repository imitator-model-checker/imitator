(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Classical Behavioral Cartography with exhaustive coverage of integer points [AF10]
 * 
 * File contributors : Étienne André
 * Created           : 2016/01/19
 * Last modified     : 2016/01/29
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
exception Stop_loop of more_points



(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoBCCover =
	object (self) inherit algoCartoGeneric as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)

	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "BC (full coverage)"

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
		super#initialize_variables;
		
		(* The end *)
		()

		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Global method on pi0 *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(* Return the current_point; raises InternalError if current_point was not initialized *)
	method private get_current_point_option =
		match current_point with
		| No_more -> 
			raise (InternalError("current_point has not been initialized yet, altough it should have at this point."))
		| Some_pval current_point -> current_point



	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Compute the sequential successor of a point. Returns Some next_pi0 if there is indeed one, or None if no more point is available. *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private compute_next_sequential_pi0 current_pi0 =
		(* Retrieve the model *)
		let model = Input.get_model() in
		(* Retrieve the input options *)
		let options = Input.get_options () in
		
		let nb_dimensions = model.nb_parameters in

		(* Retrieve the current pi0 (that must have been initialized before) *)
(* 		let current_pi0 = self#get_current_point_option in *)

		(* Start with the first dimension *)
		let current_dimension = ref 0 in (*** WARNING: should be sure that 0 is the first parameter dimension ***)
		(* The current dimension is not yet the maximum *)
		let reached_max_dimension = ref false in
		
		try(
		while not !reached_max_dimension do
			(* Try to increment the local dimension *)
			let current_dimension_incremented = NumConst.add (current_pi0#get_value !current_dimension) options#step in
			if current_dimension_incremented <= max_bounds.(!current_dimension) then (
				(* Copy the current point *)
				let new_point = current_pi0#copy in
				
				(* Increment this dimension *)
				new_point#set_value (!current_dimension) current_dimension_incremented;
				(* Reset the smaller dimensions to the low bound *)
				for i = 0 to !current_dimension - 1 do
					new_point#set_value i min_bounds.(i);
				done;
				
				(* Stop the loop *)
				raise (Found_point new_point)
			)
			(* Else: try the next dimension *)
			else ( 
				current_dimension := !current_dimension + 1;
				(* If last dimension: the end! *)
				if !current_dimension >= nb_dimensions then(
					reached_max_dimension := true;
				)
			);
		done; (* while not is max *)
		
		(* Found no point *)
		None
		
		(* If exception: found a point! *)
		) with Found_point point -> Some point

      
      
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
		let model = Input.get_model() in

		let pi0 = new PVal.pval in
		(* Copy min bounds *)
		for parameter_index = 0 to model.nb_parameters - 1 do
			pi0#set_value parameter_index min_bounds.(parameter_index);
		done;
		
		(* Return the point *)
		Some_pval pi0

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Find the next point *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method find_next_point =
		(* Get the model *)
	(* 	let model = Input.get_model() in *)

		(* Retrieve the current pi0 (that must have been initialized before) *)
		let current_pi0 = ref (self#get_current_point_option) in
		
		try(
		while true do
			
			(* 1) Compute the next pi0 (if any left) in a sequential manner *)
			let tentative_next_point =
			match self#compute_next_sequential_pi0 !current_pi0 with
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
	(* Processing the result of IM *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_result im_result reference_val =
		(* Compute the abstraction *)
		let abstract_im_result = abstract_im_result_of_im_result im_result reference_val in
		
		(* Add to the list of tiles *)
		im_results <- abstract_im_result :: im_results;
	
		(* The end *)
		()

		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_result =
		self#print_algo_message_newline Verbose_standard (
			"Successfully terminated " ^ (after_seconds ()) ^ "."
		);

		
		(* Get the termination status *)
		 let termination_status = match termination_status with
			| None -> raise (InternalError "Termination status not set in BCCover.compute_result")
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
