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
 * Last modified     : 2016/01/28
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
(* Internal exception *)
(************************************************************)
(************************************************************)
(* To stop a loop when a point is found *)
exception Found_point of PVal.pval

(* To stop a loop when a point is found or there is no more point *)
exception Stop_loop of more_points



(************************************************************)
(************************************************************)
(* Class-independent functions *)
(************************************************************)
(************************************************************)


(*------------------------------------------------------------*)
(** Check if a parameter valuation belongs to the constraint of an im_result *)
(*------------------------------------------------------------*)
let pi0_in_tiles pval (abstract_im_result : abstract_im_result) =
	match abstract_im_result.result with
	| LinearConstraint.Convex_p_constraint p_linear_constraint -> LinearConstraint.is_pi0_compatible pval#get_value p_linear_constraint
	| LinearConstraint.Nonconvex_p_constraint p_nnconvex_constraint -> LinearConstraint.p_nnconvex_constraint_is_pi0_compatible pval#get_value p_nnconvex_constraint


	

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
	(* List of im_results *)
	val mutable im_results : abstract_im_result list = []
	
	(* Initial p-constraint (needed to check whether points satisfy it) *)
	val mutable init_p_constraint = LinearConstraint.p_true_constraint ()

	(* Counts the points actually member of an existing constraint (hence useless) for information purpose *)
	val mutable nb_useless_points = 0

	
	
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
		
		(* Retrieve the model *)
(* 		let model = Input.get_model() in *)
		(* Retrieve the input options *)
(* 		let options = Input.get_options () in *)

		(* Print some information *)
(* 		print_message Verbose_medium ("Starting preprocessing for the behavioral cartography"); *)

		(*** TODO ***)
		(* Time counter for recording the globl time spent on BC *)
(* 		time_spent_on_IM := 0.; *)

		(* Time counter for the algorithm *)
		start_time <- Unix.gettimeofday();

		(* Print some information *)
		self#print_algo_message Verbose_medium ("Number of dimensions: " ^ (string_of_int nb_dimensions));

		(* Check that the cartography is not applied to 0 dimension! *)
		if nb_dimensions = 0 then(
			print_error "The cartography has 0 dimension in V0, and cannot be applied.";
			abort_program();
		);

		(* Compute the initial state *)
		let init_state = AlgoStateBased.compute_initial_state_or_abort() in
		
		(* Set the counter of useless points to 0 *)
		nb_useless_points <- 0;

		(* Initial constraint of the model *)
		let _, init_px_constraint = init_state in
		(* Hide non parameters *)
		init_p_constraint <- LinearConstraint.px_hide_nonparameters_and_collapse init_px_constraint;

		(* List of im_results *)
		im_results <- [];

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
	(* Generic function returning true if a computed pi0 belongs to none of the tiles, and satisfies the init constraint. *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private test_pi0_uncovered tentative_pi0 =
		(* Get the model *)
		let model = Input.get_model() in
		(* Retrieve the input options *)
(* 		let options = Input.get_options () in *)

		(* Check that the point does not belong to any constraint *)
		if List.exists (pi0_in_tiles tentative_pi0) im_results then (
			(* Update the number of unsuccessful points *)
			nb_useless_points <- nb_useless_points + 1;
			if verbose_mode_greater Verbose_medium then (
				self#print_algo_message Verbose_medium "The following pi0 is already included in a constraint.";
				print_message Verbose_medium (ModelPrinter.string_of_pi0 model tentative_pi0);
			);
			(*** TODO: could be optimized by finding the nearest multiple of tile next to the border, and directly switching to that one ***)
			false
			
		(* Check that it satisfies the initial constraint *)
		) else if not (LinearConstraint.is_pi0_compatible tentative_pi0#get_value init_p_constraint) then (
			(* Update the number of unsuccessful points *)
			nb_useless_points <- nb_useless_points + 1;
			if verbose_mode_greater Verbose_medium then (
				self#print_algo_message Verbose_medium "The following pi0 does not satisfy the initial constraint of the model.";
				print_message Verbose_medium (ModelPrinter.string_of_pi0 model tentative_pi0);
			);
			false
		(* If both checks passed, then pi0 found *)
		)else(
			true
		)(*;*)
		
		(*** TODO: add back ***)
(*		(*** BADPROG: this check has nothing to do in this function! put back to the BC algorithm ***)
		(* If pi0 still not found, check time limit *)
		if not !found_pi0 then(
			(* Stop if the time limit has been reached *)
			match options#time_limit with
			| None -> ()
			| Some limit ->
				if (get_time()) > (float_of_int limit)
					then time_limit_reached := true;
		);*)
		(*()*)


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
		
	(*		(* Counts the points actually member of an existing constraint (hence useless) for information purpose *)
		let nb_useless_points = ref 0 in*)
		
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

		(* Return result *)
		BC_result {
			(* List of tiles *)
			(*** NOTE: reverse as each im_result was added as first element ***)
			tiles				= List.rev im_results;
			
			(* Total computation time of the algorithm *)
			computation_time	= time_from start_time;
			
			(* Termination *)
			termination			= (*** TODO ***) BC_Regular_termination;
		}


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
