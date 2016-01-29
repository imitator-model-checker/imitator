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
(* Limit type and exception *)
(************************************************************)
(************************************************************)

type limit_reached =
	(* No limit *)
	| Keep_going

	(* Termination due to time limit reached *)
	| Time_limit_reached
	
	(* Termination due to state space depth limit reached *)
	| Tiles_limit_reached
	

exception Limit_detected of limit_reached



(************************************************************)
(************************************************************)
(* Class-independent functions *)
(************************************************************)
(************************************************************)

(*------------------------------------------------------------*)
(* Convert an 'im_result' into an 'abstract_im_result' *)
(*------------------------------------------------------------*)
let abstract_im_result_of_im_result (im_result : im_result) reference_val : abstract_im_result =
	(* First, abstract state space *)
	let abstract_state_space = {
		nb_states		= StateSpace.nb_states im_result.state_space;
		nb_transitions	= StateSpace.nb_transitions im_result.state_space;
(* 		depth		 	= im_result.state_space; *)
	}
	in
	
	(* Construct the abstraction *)
	{
	(* Reference valuation *)
	reference_val		= reference_val;

	(* Convex constraint *)
	result				= im_result.result;
	
	(* Abstracted version of the explored state space *)
	abstract_state_space	= abstract_state_space;
	
	(* Nature of the state space *)
	statespace_nature		= im_result.statespace_nature;
	
	(* Number of random selections of pi-incompatible inequalities performed *)
	nb_random_selections= im_result.nb_random_selections;
	
	(* Total computation time of the algorithm *)
	computation_time	= im_result.computation_time;
		
	(* Soundness of the result *)
	soundness			= im_result.soundness;
	
	(* Termination *)
	termination			= im_result.termination;
}


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
class virtual algoCartoGeneric =
	object (self) inherit algoGeneric as super
	
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	(* Number of dimensions *)
	val mutable nb_dimensions = 0
	
	(* Number of points in V0 (slightly approximated) *)
	val mutable nb_points = NumConst.zero
	
	(* Min & max bounds for the parameters *)
	val mutable min_bounds = Array.make 0 NumConst.zero
	val mutable max_bounds = Array.make 0 NumConst.zero
	
	(* Current point *)
	val mutable current_point = No_more
	
	(* Current iteration (number of times IM is called); used for printing only *)
	val mutable current_iteration = 0
	
	(* List of im_results *)
	val mutable im_results : abstract_im_result list = []
	
	(* Initial p-constraint (needed to check whether points satisfy it) *)
	val mutable init_p_constraint = LinearConstraint.p_true_constraint ()

	(* Counts the points actually member of an existing constraint for information purpose *)
	val mutable nb_unsuccessful_points = 0
	
	(* Counter tracking the computation time to look for points *)
	val find_next_point_counter = new Counter.counter

	(* Status of the analysis *)
	val mutable termination_status = None
	
	
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
	method initialize_variables =
		(* Time counter for the algorithm *)
		start_time <- Unix.gettimeofday();

		(* Retrieve the model *)
		let model = Input.get_model () in
		(* Retrieve the v0 *)
		let v0 = Input.get_v0() in
		(* Retrieve the input options *)
		let options = Input.get_options () in

		(* 		super#initialize_variables; *)

		(* Set the number of dimensions in the system *)
		nb_dimensions <- model.nb_parameters;
		
		(*** BADPROG (to improve ***)
		(* Initialize *)
		min_bounds <- Array.make nb_dimensions NumConst.zero;
		max_bounds <- Array.make nb_dimensions NumConst.zero;
		(* Fill *)
		for parameter_index = 0 to nb_dimensions - 1 do
			min_bounds.(parameter_index) <- v0#get_min parameter_index;
			max_bounds.(parameter_index) <- v0#get_max parameter_index;
		done;
	
		(* Start counting from 1 *)
		current_iteration <- 1;
		
		(* Compute the (actually slightly approximate) number of points in V0 (for information purpose) *)
		nb_points <- NumConst.one;
		for parameter_index = 0 to nb_dimensions - 1 do
			nb_points <-
			let low = v0#get_min parameter_index in
			let high = v0#get_max parameter_index in
			(* Multiply current number of points by the interval + 1, itself divided by the step *)
			NumConst.mul
				nb_points
				(NumConst.div
					(NumConst.add
						(NumConst.sub high low)
						NumConst.one
					)
					options#step
				)
			;
		done;

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
		nb_unsuccessful_points <- 0;
		
		(* Initialize the counter *)
		find_next_point_counter#init;

		(* Initial constraint of the model *)
		let _, init_px_constraint = init_state in
		(* Hide non parameters *)
		init_p_constraint <- LinearConstraint.px_hide_nonparameters_and_collapse init_px_constraint;

		(* List of tiles *)
		im_results <- [];

		(* Status of the analysis *)
		termination_status <- None;

		(* The end *)
		()
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Return a new instance of the algorithm to be iteratively called (typically IM or PRP) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual algorithm_instance : AlgoIMK.algoIMK

		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Create the initial point for the analysis *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual get_initial_point : more_points

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Find the next point *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual find_next_point : more_points


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check whether the limit of the cartography has been reached, according to the analysis options *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private check_bc_limit =
		(* Retrieve the input options *)
		let options = Input.get_options () in
		
		(* Check all limits *)
		
		(* Depth limit *)
		try(
		begin
		match options#carto_tiles_limit with
			| None -> ()
			| Some limit -> if List.length im_results >= limit then(
				raise (Limit_detected Tiles_limit_reached)
			)
		end
		;
		(* Time limit *)
		begin
		match options#carto_time_limit with
			| None -> ()
			| Some limit -> if (time_from start_time) > (float_of_int limit) then(
				raise (Limit_detected Time_limit_reached)
			)
		end
		;
		(* If reached here, then everything is fine: keep going *)
		Keep_going
		)
		(* If exception caught, then update termination status, and return the reason *)
		with Limit_detected reason -> reason

		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Generic function returning true if a computed pi0 belongs to none of the tiles, and satisfies the init constraint. *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method test_pi0_uncovered tentative_pi0 =
		(* Get the model *)
		let model = Input.get_model() in
		(* Retrieve the input options *)
(* 		let options = Input.get_options () in *)

		(* Check that the point does not belong to any constraint *)
		if List.exists (pi0_in_tiles tentative_pi0) im_results then (
			(* Update the number of unsuccessful points *)
			nb_unsuccessful_points <- nb_unsuccessful_points + 1;
			if verbose_mode_greater Verbose_medium then (
				self#print_algo_message Verbose_medium "The following pi0 is already included in a constraint.";
				print_message Verbose_medium (ModelPrinter.string_of_pi0 model tentative_pi0);
			);
			(*** TODO: could be optimized by finding the nearest multiple of tile next to the border, and directly switching to that one ***)
			false
			
		(* Check that it satisfies the initial constraint *)
		) else if not (LinearConstraint.is_pi0_compatible tentative_pi0#get_value init_p_constraint) then (
			(* Update the number of unsuccessful points *)
			nb_unsuccessful_points <- nb_unsuccessful_points + 1;
			if verbose_mode_greater Verbose_medium then (
				self#print_algo_message Verbose_medium "The following pi0 does not satisfy the initial constraint of the model.";
				print_message Verbose_medium (ModelPrinter.string_of_pi0 model tentative_pi0);
			);
			false
		(* If both checks passed, then pi0 found *)
		)else(
			true
		)(*;*)
		
		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Print warning(s) if the limit of an exploration has been reached, according to the analysis options *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private print_warnings_limit () =
		match termination_status with
			| Some BC_Regular_termination -> ()

			| Some BC_Tiles_limit -> print_warning (
				"The limit number of tiles has been computed. The exploration now stops although there may be some more points to cover."
					(*** TODO (one day): really say whether some points are still uncovered ***)
			)

			| Some BC_Time_limit -> print_warning (
				"The time limit for the cartography has been reached. The exploration now stops although there may be some more points to cover."
					(*** TODO (one day): really say whether some points are still uncovered ***)
			)
			
			| None -> raise (InternalError "The termination status should be set when displaying warnings concerning early termination.")


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Main method running the algorithm: implements here a generic cartography, and calls other functions that may be modified in subclasses *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method run () =
		(* Retrieve the model *)
		let model = Input.get_model () in
		(* Retrieve the v0 *)
		let v0 = Input.get_v0() in
		(* Retrieve the input options *)
		let options = Input.get_options () in
		
		(* Print some information *)
		self#print_algo_message Verbose_standard ("Starting running cartography...\n"); (* " ^ self#algorithm_name ^ " *)
		
		(* Print some information *)
		print_message Verbose_standard ("\n**************************************************");
		print_message Verbose_standard (" START THE BEHAVIORAL CARTOGRAPHY ALGORITHM");
		print_message Verbose_standard ("**************************************************");
		print_message Verbose_standard (" Parametric rectangle V0: ");
		print_message Verbose_standard (ModelPrinter.string_of_v0 model v0);
		print_message Verbose_standard (" Number of points inside V0: " ^ (NumConst.string_of_numconst nb_points));

		(* Variable initialization *)
		self#print_algo_message Verbose_low ("Initializing the algorithm local variables...");
		self#initialize_variables;

		(* Retrieve the first point *)
		self#print_algo_message Verbose_low ("Retrieving initial point...");
		(* Count! *)
		find_next_point_counter#start;
		current_point <- self#get_initial_point;
		(* Count! *)
		find_next_point_counter#stop;
		
		
		(*** TODO : check that initial pi0 is suitable!! (could be incompatible with initial constraint) ***)
		
		
		let limit_reached = ref Keep_going in

		(* While there is another point to explore *)
		while !limit_reached = Keep_going && (match current_point with Some_pval _ -> true | _ -> false) do
		
			(* Get the point *)
			let pi0 = match current_point with
				| Some_pval pval -> pval
				| _ -> raise (InternalError("Impossible situation in AlgoCartoGeneric: the point should have been tested before"))
			in
			
			(* Print some messages *)
			(*** HACK: only print if non-distributed ***)
			if options#distribution_mode = Options.Non_distributed then(
				print_message Verbose_standard ("\n**************************************************");
				print_message Verbose_standard ("BEHAVIORAL CARTOGRAPHY ALGORITHM: " ^ (string_of_int current_iteration) ^ "");
				print_message Verbose_standard ("Considering the following pi" ^ (string_of_int current_iteration));
				print_message Verbose_standard (ModelPrinter.string_of_pi0 model pi0);
			);
			
			
			(* Print some information *)
			self#print_algo_message Verbose_low ("Setting new pi0...");

			(* Set the new pi0 *)
			Input.set_pi0 (pi0);
			
			(* Save the verbose mode as it may be modified *)
			let global_verbose_mode = get_verbose_mode() in
			
			(* Prevent the verbose messages (except in verbose medium, high or total) *)
			(*------------------------------------------------------------*)
			if not (verbose_mode_greater Verbose_medium) then
				set_verbose_mode Verbose_mute;
						
			(* Call the algorithm to be iterated on (typically IM or PRP) *)
			(*** NOTE: the bc time limit is NOT checked inside one execution of the algorithm to be iterated (but also note that the max execution time of the algorithm to be iterated is set to that of BC, in the Options pre-processing) ***)
			let algo = self#algorithm_instance in
			let imitator_result : imitator_result = algo#run() in

			(** Create auxiliary files with the proper file prefix, if requested *)
			let file_prefix = options#files_prefix ^ "-" ^ (string_of_int current_iteration) in
			ResultProcessor.process_result imitator_result (Some file_prefix);
			
			(* Get the verbose mode back *)
			set_verbose_mode global_verbose_mode;
			(*------------------------------------------------------------*)

			(* Print some information *)
			self#print_algo_message Verbose_low ("Processing the result by IM...");

			(* Get the result *)
			let im_result =  match imitator_result with
				| IM_result im_result -> im_result
				| _ -> raise (InternalError("The result of IM must be an im_result."))
			in

			(* Process the result by IM *)
			self#process_result im_result pi0;
			
			
			(*** TODO: check validity of result! ***)
			(* may not be valid if early termination for PRP without bad state, for example *)
			
			
			(* Print some information *)
			let nb_states = StateSpace.nb_states im_result.state_space in
			let nb_transitions = StateSpace.nb_transitions im_result.state_space in
			print_message Verbose_standard (
				"\nK" ^ (string_of_int current_iteration) ^ " computed by " ^ algo#algorithm_name
(* 					^ " after " ^ (string_of_int im_result.nb_iterations) ^ " iteration" ^ (s_of_int im_result.nb_iterations) ^ "" *)
				^ " in " ^ (string_of_seconds im_result.computation_time) ^ ": "
				^ (string_of_int nb_states) ^ " state" ^ (s_of_int nb_states)
				^ " with "
				^ (string_of_int nb_transitions) ^ " transition" ^ (s_of_int nb_transitions) ^ " explored.");

			print_message Verbose_low ("Constraint K0 computed:");
			print_message Verbose_standard (LinearConstraint.string_of_p_convex_or_nonconvex_constraint model.variable_names im_result.result);
			
			(*** TODO ***)
(*			if model.correctness_condition <> None then(
				print_message Verbose_medium ("This tile is " ^ (StateSpace.string_of_tile_nature im_result.tile_nature) ^ ".");
			);*)


			(* Check if the limit has been reached *)
			(*** NOTE: the bc time limit is NOT checked inside one execution of IM ***)
			limit_reached := self#check_bc_limit;

			(* Only compute next point if limit not reached *)
			if !limit_reached = Keep_going then(
				(* Print some information *)
				self#print_algo_message Verbose_low ("Computing the next point...");

				(* Count! *)
				find_next_point_counter#start;
				(* Get to the next point *)
				current_point <- self#find_next_point;
				(* Count! *)
				find_next_point_counter#stop;
				
				(* Iterate *)
				current_iteration <- current_iteration + 1;
				
				(* Check again the limit (especially for time, that may have elapsed) *)
				limit_reached := self#check_bc_limit;
			);
			
		done; (* end while more points *)

		(* Update termination condition *)
		begin
		match !limit_reached with
			(* No limit: regular termination *)
			| Keep_going -> termination_status <- Some BC_Regular_termination
			
			(* Termination due to the number of tiles reached *)
			| Tiles_limit_reached -> termination_status <- Some BC_Tiles_limit
			
			(* Termination due to time limit reached *)
			| Time_limit_reached -> termination_status <- Some BC_Time_limit
		end
		;
	
		(* Print some information *)
		(*** NOTE: must be done after setting the limit (above) ***)
		self#print_warnings_limit ();
		
		(* Return the algorithm-dependent result *)
		self#compute_result



	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Processing the result of IM *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual process_result : Result.im_result -> PVal.pval -> unit

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
