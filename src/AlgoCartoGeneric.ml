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
 * Last modified     : 2016/08/26
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
open Statistics
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


(*** NOTE: this should be a class parameter (but would require much work in other classes… so TODO later) ***)
type tiles_storage =
	(* List of constraints, as in the legacy cartography [AF10] *)
	| Tiles_list
	(* A good/bad representation of the tiles, when only the parameter valuations are of interest *)
	| Tiles_good_bad_constraint


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
(* Internal exceptions *)
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
(** Convert a 'single_synthesis_result' into an 'abstract_point_based_result' *)
(*------------------------------------------------------------*)
let abstract_point_based_result_of_single_synthesis_result (single_synthesis_result : single_synthesis_result) reference_val : abstract_point_based_result =
	(* First, abstract state space *)
	let abstract_state_space = {
		nb_states		= StateSpace.nb_states single_synthesis_result.state_space;
		nb_transitions	= StateSpace.nb_transitions single_synthesis_result.state_space;
(* 		depth		 	= single_synthesis_result.state_space; *)
	}
	in
	
	(* Construct the abstraction *)
	{
	(* Reference valuation *)
	reference_val		= reference_val;

	(* Convex constraint *)
	result				= single_synthesis_result.result;
	
	(* Abstracted version of the explored state space *)
	abstract_state_space	= abstract_state_space;
	
	(* Nature of the state space *)
(* 	statespace_nature		= single_synthesis_result.statespace_nature; *)
	
	(* Number of random selections of pi-incompatible inequalities performed *)
(* 	nb_random_selections= im_result.nb_random_selections; *)
	
	(* Total computation time of the algorithm *)
	computation_time	= single_synthesis_result.computation_time;
		
	(* Soundness of the result *)
(* 	soundness			= single_synthesis_result.soundness; *)
	
	(* Termination *)
	termination			= single_synthesis_result.termination;
}


(*------------------------------------------------------------*)
(** Convert a 'point_based_result' into an 'abstract_point_based_result' *)
(*------------------------------------------------------------*)
let abstract_point_based_result_of_point_based_result (point_based_result : point_based_result) reference_val : abstract_point_based_result =
	(* First, abstract state space *)
	let abstract_state_space = {
		nb_states		= StateSpace.nb_states point_based_result.state_space;
		nb_transitions	= StateSpace.nb_transitions point_based_result.state_space;
(* 		depth		 	= point_based_result.state_space; *)
	}
	in
	
	(* Construct the abstraction *)
	{
	(* Reference valuation *)
	reference_val		= reference_val;

	(* Convex constraint *)
	result				= point_based_result.result;
	
	(* Abstracted version of the explored state space *)
	abstract_state_space	= abstract_state_space;
	
	(* Nature of the state space *)
(* 	statespace_nature		= point_based_result.statespace_nature; *)
	
	(* Number of random selections of pi-incompatible inequalities performed *)
(* 	nb_random_selections= im_result.nb_random_selections; *)
	
	(* Total computation time of the algorithm *)
	computation_time	= point_based_result.computation_time;
		
	(* Soundness of the result *)
(* 	soundness			= point_based_result.soundness; *)
	
	(* Termination *)
	termination			= point_based_result.termination;
}


(*------------------------------------------------------------*)
(* Print warning(s) depending on a Result.bc_algorithm_termination *)
(*------------------------------------------------------------*)
let print_warnings_limit_for = function
	| BC_Regular_termination -> ()

	| BC_Tiles_limit -> print_warning (
		"The limit number of tiles has been computed. The exploration now stops although there may be some more points to cover."
			(*** TODO (one day): really say whether some points are still uncovered ***)
	)

	| BC_Time_limit -> print_warning (
		"The time limit for the cartography has been reached. The exploration now stops although there may be some more points to cover."
			(*** TODO (one day): really say whether some points are still uncovered ***)
	)
	
	| BC_Mixed_limit -> raise (InternalError "The termination status 'BC_Mixed_limit' should not be called in a single non-distributed instance of BC.")



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
	
	(*** BADPROG: code shared with AlgoBCCoverDistributed ***)
	(* The function creating a new instance of the algorithm to call (typically IM or PRP). Initially None, to be updated immediatly after the object creation. *)
	(*** NOTE: this should be a parameter of the class; but cannot due to inheritance from AlgoGeneric ***)
	val mutable algo_instance_function = None
	
	(*** BADPROG: code shared with AlgoBCCoverDistributed ***)
	(* The type of the tiles manager *)
	(*** NOTE: must be initialized first (and should be in the future passed as a class paramater) ***)
	val mutable tiles_manager_type : tiles_storage option = None
	

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Given a cartography termination and a list of abstract_point_based_result, evalutes the coverage of the cartography *)
	(*** NOTE: this should be a parameter of the class; but cannot due to inheritance from AlgoGeneric ***)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(* 	val mutable coverage_evaluation_function = None *)

	
	(* Number of dimensions (shortcut) *)
	val mutable nb_dimensions = 0
	
	(* Number of points in V0 (slightly approximated) *)
	(*** NOTE: why slightly approximated? Because of step? ***)
	val mutable nb_points = NumConst.zero
	
	(* Min & max bounds for the parameters *)
	val mutable min_bounds = Array.make 0 NumConst.zero
	val mutable max_bounds = Array.make 0 NumConst.zero
	
	(* Current point *)
	val mutable current_point = No_more
	
	(* Current iteration (number of times IM is called); used for printing only *)
	val mutable current_iteration = 0
	
	(* The current algorithm instance *)
	(*** NOTE: this initialiation is useless (and time consuming?), as a new instance will be overwritten when needed ***)
	val mutable current_algo_instance : AlgoBFS.algoBFS =
		let myalgo :> AlgoBFS.algoBFS = new AlgoIMK.algoIMK in myalgo
	
(*	(* List of im_results *)
	val mutable im_results : abstract_point_based_result list = []*)
	
	(* Manager for the tiles, the class of which depends on the tiles_storage type *)
	(*** NOTE: arbitrarily set to TilesManagerList, but will be initialized later anyway ***)
	(*** TODO: when the class is parameterized, will be directly set to the correct manager, without mutable ***)
	val mutable tiles_manager = new TilesManagerList.tilesManagerList
	
	(* Initial p-constraint (needed to check whether points satisfy it) *)
	val mutable init_p_constraint = LinearConstraint.p_true_constraint ()

	(* Counts the points actually member of an existing constraint for information purpose *)
	val mutable nb_unsuccessful_points = 0
	
	(* Counter tracking the computation time to look for points *)
	val find_next_point_counter = create_hybrid_counter_and_register "find next point" Algorithm_counter Verbose_low

	(* Is the time/state/etc. limit reached? *)
	val mutable limit_reached = Keep_going
	
	(* Status of the analysis *)
	val mutable termination_status = None
	
	
	
	(************************************************************)
	(* Class methods to simulate class parameters *)
	(************************************************************)
	
	(*** BADPROG: code shared with AlgoBCCoverDistributed ***)
	(* Sets the function creating a new instance of the algorithm to call (typically IM or PRP) *)
	method set_algo_instance_function (f : unit -> AlgoBFS.algoBFS) : unit =
		match algo_instance_function with
		| Some _ -> 
			raise (InternalError("algo_instance_function was already set in AlgoCartoGeneric."))
		| None ->
			algo_instance_function <- Some f
	
	(*** BADPROG: code shared with AlgoBCCoverDistributed ***)
	(* Get the function creating a new instance of the algorithm to call (typically IM or PRP) *)
	method get_algo_instance_function =
		match algo_instance_function with
		| Some f -> f
		| None ->
			raise (InternalError("algo_instance_function not yet set in AlgoCartoGeneric."))
		
(*	(* Sets the coverage evaluation function *)
	method set_coverage_evaluation_function : (f : Result.bc_algorithm_termination -> Result.abstract_point_based_result list -> Result.bc_coverage) =
		match coverage_evaluation_function with
		| Some _ -> 
			raise (InternalError("coverage_evaluation_function was already set in AlgoCartoGeneric."))
		| None ->
			coverage_evaluation_function <- f*)
	
	(*** BADPROG: code shared with AlgoBCCoverDistributed ***)
	(* Set the tiles_manager type *)
	method set_tiles_manager_type new_tiles_manager_type =
		tiles_manager_type <- Some new_tiles_manager_type

	(*** BADPROG: code shared with AlgoBCCoverDistributed ***)
	(* Get the tiles_manager type *)
	method private get_tiles_manager_type =
	match tiles_manager_type with
		| Some t -> t
		| None -> raise (InternalError("tiles_manager_type not yet set in AlgoCartoGeneric."))

	
	(************************************************************)
	(* Class methods: methods used in subclasses as building blocks *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Return the current_point; raises InternalError if current_point was not initialized *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method get_current_point_option =
		match current_point with
		| No_more -> 
			raise (InternalError("current_point has not been initialized yet, altough it should have at this point."))
		| Some_pval current_point -> current_point


	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Compute the smallest point (according to the min bounds) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_smallest_point =
		(* Retrieve the model *)
		let model = Input.get_model() in

		let point = new PVal.pval in
		(* Copy min bounds *)
		for parameter_index = 0 to model.nb_parameters - 1 do
			point#set_value parameter_index min_bounds.(parameter_index);
		done;
		
		(* Return the point *)
		point
		



	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Compute the sequential successor of a given point. *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_next_sequential_pi0 (current_pi0 : PVal.pval) : more_points =
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
		No_more
		
		(* If exception: found a point! *)
		) with Found_point point -> Some_pval point

     
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Compute the sequential uncovered successor of a given point. *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_next_sequential_uncovered_pi0_from point =
		(* Retrieve the current pi0 (that must have been initialized before) *)
		let current_pi0 = ref point in
		
		try(
		while true do
			
			(* 1) Compute the next pi0 (if any left) in a sequential manner *)
			let tentative_next_point =
			match self#compute_next_sequential_pi0 !current_pi0 with
			| Some_pval point -> point
			| No_more -> raise (Stop_loop No_more)
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
		raise (InternalError("This part of the code should be unreachable in compute_next_sequential_uncovered_pi0"))
		
		(* Return the point *)
		) with Stop_loop sl -> sl
		
		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Compute the sequential uncovered successor of the current point. *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_next_sequential_uncovered_pi0 =
		(* Retrieve the current pi0 (that must have been initialized before) *)
		self#compute_next_sequential_uncovered_pi0_from (self#get_current_point_option)
		
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Methods on random generation of a pi0 *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	method one_random_pi0 : PVal.pval =
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
	(* Try to generate a random pi0; after unsuccessful max_tries (because the randomly generated point was always covered by a tile), return No_more *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method try_random_pi0 max_tries =
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
	
	
	
	
	(************************************************************)
	(* Class methods: regular methods *)
	(************************************************************)


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
		
		nb_points <- v0#get_nb_points options#step;

		(* Print some information *)
		self#print_algo_message Verbose_medium ("Number of dimensions: " ^ (string_of_int nb_dimensions));

		(* Check that the cartography is not applied to 0 dimension! *)
		if nb_dimensions = 0 then(
			print_error "The cartography has 0 dimension in V0, and cannot be applied.";
			abort_program();
		);
		
		(*** TODO: check that the V0 is not empty ***)

		(* Compute the initial state *)
		let init_state = AlgoStateBased.compute_initial_state_or_abort() in
		
		(* Set the counter of useless points to 0 *)
		nb_unsuccessful_points <- 0;
		
		(* Initialize the counter *)
		find_next_point_counter#reset;

		(* Initial constraint of the model *)
		let _, init_px_constraint = init_state in
		(* Hide non parameters *)
		init_p_constraint <- LinearConstraint.px_hide_nonparameters_and_collapse init_px_constraint;

(*		(* List of tiles *)
		im_results <- [];*)

		(* First create the tiles manager *)
		(*** NOTE: the get function takes care of the Some/None cases (and may raise an exception if not properly initialized) ***)
		begin
		match self#get_tiles_manager_type with
			| Tiles_list -> tiles_manager <- new TilesManagerList.tilesManagerList
			| Tiles_good_bad_constraint -> tiles_manager <- new TilesManagerConstraint.tilesManagerConstraint
		end;
		(* Now initialize the tiles manager *)
		tiles_manager#initialize;

		(* Status of the analysis *)
		termination_status <- None;

		(* The end *)
		()
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Return a new instance of the algorithm to be iteratively called (typically IM or PRP) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(* 	method virtual algorithm_instance : AlgoIMK.algoIMK *)

		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Create the initial point for the analysis *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual get_initial_point : more_points

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Find the next uncovered point *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual find_next_point : more_points


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Shortcut to get the current_point when it is known that it is different from No_more *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private get_current_point_instance =
		match current_point with
			| Some_pval pval -> pval
			| _ -> raise (InternalError("Impossible situation in AlgoCartoGeneric: the point should have been tested before"))
			
			
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check whether the limit of the cartography has been reached, according to the analysis options *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private check_bc_limit =
		(* Retrieve the input options *)
		let options = Input.get_options () in
		
		(* Check all limits *)
		
		(* Tiles limit *)
		try(
		begin
		match options#carto_tiles_limit with
			| None -> ()
			| Some limit -> if (*List.length im_results*)tiles_manager#get_nb_results >= limit then(
				raise (Limit_detected Tiles_limit_reached)
			)
		end
		;
		(* Time limit *)
		begin
		match options#carto_time_limit with
			| None -> ()
			| Some limit -> if (time_from start_time) >= (float_of_int limit) then(
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
		if (*List.exists (pi0_in_tiles tentative_pi0) im_results*) tiles_manager#pval_in_tiles tentative_pi0 then (
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
	(* Initializing cartography algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_cartography =
		(* Retrieve the model *)
		let model = Input.get_model () in
		(* Retrieve the v0 *)
		let v0 = Input.get_v0() in

		(* Print some information *)
		self#print_algo_message Verbose_standard ("Starting running cartography...\n"); (* " ^ self#algorithm_name ^ " *)
		
		(* Variable initialization *)
		(*** NOTE: done before printing, since the number of points is needed just below ***)
		self#print_algo_message Verbose_low ("Initializing the algorithm local variables...");
		self#initialize_variables;

		(* Print some information *)
		print_message Verbose_standard ("\n**************************************************");
		print_message Verbose_standard (" START THE BEHAVIORAL CARTOGRAPHY ALGORITHM");
		print_message Verbose_standard ("**************************************************");
		print_message Verbose_standard (" Parametric rectangle V0: ");
		print_message Verbose_standard (ModelPrinter.string_of_v0 model v0);
		print_message Verbose_standard (" Number of points inside V0: " ^ (NumConst.string_of_numconst nb_points));

		(* Retrieve the first point *)
		self#print_algo_message Verbose_low ("Retrieving initial point...");
		(* Count *)
		find_next_point_counter#start;
		current_point <- self#get_initial_point;
		(* Count *)
		find_next_point_counter#stop;
		
		(* Limit not yet reached *)
		limit_reached <- Keep_going;
		
		(* The end *)
		()
	
	
	
(*	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Get all tiles *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method get_abstract_im_result_list =
		im_results*)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Get the tiles manager *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method get_tiles_manager =
		tiles_manager
	
	
(*	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Set all tiles, i.e., replace the list of abstract_im_result by that given in argument (used when the collaborator creates a new AlgoCartoGeneric, and wants to add the previously computed tiles) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method set_abstract_im_result_list abstract_im_result_list =
		im_results <- abstract_im_result_list*)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Set the tiles manager, i.e., replace the list of tiles as managed by the manager with that given in argument (used when the collaborator creates a new AlgoCartoGeneric, and wants to add the previously computed tiles) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method set_tiles_manager new_tiles_manager =
		tiles_manager <- new_tiles_manager

		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Iteration condition to keep computing new tiles *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method check_iteration_condition =
		limit_reached = Keep_going && (match current_point with Some_pval _ -> true | _ -> false)
	
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Create auxiliary files generated by one instance of IM *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method create_auxiliary_files imitator_result =
		(* Retrieve the input options *)
		let options = Input.get_options () in

		let file_prefix = options#files_prefix ^ "-" ^ (string_of_int current_iteration) in
		ResultProcessor.process_result imitator_result current_algo_instance#algorithm_name (Some file_prefix);
		
		(* The end *)
		()
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Abstract the result of IM *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private abstract_result imitator_result reference_val =
		(* Get the result and compute the abstraction *)
		match imitator_result with
			(* Result for most regular algorithms (EF, PDFC) *)
			| Single_synthesis_result single_synthesis_result -> abstract_point_based_result_of_single_synthesis_result single_synthesis_result reference_val
			
			(* Result for point-based algorithms (IM, PRP…) *)
			| Point_based_result point_based_result -> abstract_point_based_result_of_point_based_result point_based_result reference_val
			
			| _ -> raise (InternalError("The expected result must be a Single_synthesis_result or a Point_based_result (in function AlgoCartoGeneric.abstract_result)."))


	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Process one result of an abstract version of an instance of IM *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_result abstract_result =
		(* Retrieve the model *)
		let model = Input.get_model () in

		(* Print some information *)
		let nb_states = abstract_result.abstract_state_space.nb_states (*StateSpace.nb_states im_result.state_space*) in
		let nb_transitions = abstract_result.abstract_state_space.nb_transitions (*StateSpace.nb_transitions im_result.state_space*) in
		print_message Verbose_standard (
			"\nK" ^ (string_of_int current_iteration) ^ " computed by " ^ current_algo_instance#algorithm_name
(* 					^ " after " ^ (string_of_int im_result.nb_iterations) ^ " iteration" ^ (s_of_int im_result.nb_iterations) ^ "" *)
			^ " in " ^ (string_of_seconds abstract_result.computation_time) ^ ": "
			^ (string_of_int nb_states) ^ " state" ^ (s_of_int nb_states)
			^ " with "
			^ (string_of_int nb_transitions) ^ " transition" ^ (s_of_int nb_transitions) ^ " explored.");

		print_message Verbose_low ("Constraint K0 computed:");
		print_message Verbose_standard (ResultProcessor.string_of_good_or_bad_constraint model.variable_names abstract_result.result);
		
		
		(* Print some information *)
		self#print_algo_message Verbose_low ("Processing the result by IM...");

(*		(* Get the point *)
		let pi0 = self#get_current_point_instance in*)
		
		(* Add to the list of tiles *)
(* 		im_results <- abstract_result :: im_results; *)
		tiles_manager#process_tile abstract_result;
	
		
		
		(*** TODO: check validity of result! ***)
		(* may not be valid if early termination for PRP without bad state, for example *)
		
		

		
		(*** TODO ***)
(*			if model.correctness_condition <> None then(
			print_message Verbose_medium ("This tile is " ^ (StateSpace.string_of_tile_nature im_result.tile_nature) ^ ".");
		);*)
		
		(* The end *)
		()



	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Compute the next point and store it; update limits before and after computing next point *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private compute_next_point_and_update_limit =
	(* Check if the limit has been reached *)
		(*** NOTE: the bc time limit is NOT checked inside one execution of IM ***)
		limit_reached <- self#check_bc_limit;

		(* Only compute next point if limit not reached *)
		if limit_reached = Keep_going then(
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
			limit_reached <- self#check_bc_limit;
		);
		
		(* The end *)
		()
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Compute the next point, store it; return it if new point exists and limits not reached *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_and_return_next_point =
		(* Actual computation *)
		self#compute_next_point_and_update_limit;
		
		(* Only return if valid *)
		if current_point <> No_more && limit_reached = Keep_going then current_point
		else No_more


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Call the algorithm on the current point (typically call IM or PRP) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method call_point =
		(* Save the verbose mode as it may be modified *)
		let global_verbose_mode = get_verbose_mode() in

		(* Prevent the verbose messages (except in verbose medium, high or total) *)
		(*------------------------------------------------------------*)
		if not (verbose_mode_greater Verbose_medium) then
			set_verbose_mode Verbose_mute;
					
		(* Call the algorithm to be iterated on (typically IM or PRP) *)
		(*** NOTE: the bc time limit is NOT checked inside one execution of the algorithm to be iterated (but also note that the max execution time of the algorithm to be iterated is set to that of BC, in the Options pre-processing) ***)
		current_algo_instance <- self#get_algo_instance_function ();
		let imitator_result : imitator_result = current_algo_instance#run() in

		(** Create auxiliary files with the proper file prefix, if requested *)
		self#create_auxiliary_files imitator_result;

		(* Get the verbose mode back *)
		set_verbose_mode global_verbose_mode;
		(*------------------------------------------------------------*)
		
		(* Return result *)
		imitator_result
		

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Update termination condition, depending on the limit reached *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method update_termination_condition =
		match limit_reached with
			(* No limit: regular termination *)
			| Keep_going -> termination_status <- Some BC_Regular_termination
			
			(* Termination due to the number of tiles reached *)
			| Tiles_limit_reached -> termination_status <- Some BC_Tiles_limit
			
			(* Termination due to time limit reached *)
			| Time_limit_reached -> termination_status <- Some BC_Time_limit
		;
		(* The end *)
		()
		

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Print warning(s) if the limit of an exploration has been reached, according to the analysis options *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method print_warnings_limit =
		match termination_status with
			| Some status -> print_warnings_limit_for status
			| None -> raise (InternalError "The termination status should be set when displaying warnings concerning early termination.")

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Main method running the algorithm: implements here a generic cartography, and calls other functions that may be modified in subclasses *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method run () =
		(* Retrieve the model *)
		let model = Input.get_model () in
		(* Retrieve the input options *)
(* 		let options = Input.get_options () in *)

		(* Factoring initialization *)
		self#initialize_cartography;
		

		
		(*** TODO : check that initial pi0 is suitable!! (could be incompatible with initial constraint) ***)
		
		

		(* While there is another point to explore *)
		while self#check_iteration_condition do
		
			(* Get the point *)
			let pi0 = self#get_current_point_instance in
			
			(* Print some messages *)
			(*** HACK: only print if non-distributed ***)
(* 			if options#distribution_mode = Options.Non_distributed then( *)
			print_message Verbose_standard ("\n**************************************************");
			print_message Verbose_standard ("BEHAVIORAL CARTOGRAPHY ALGORITHM: " ^ (string_of_int current_iteration) ^ "");
			print_message Verbose_standard ("Considering the following pi" ^ (string_of_int current_iteration));
			print_message Verbose_standard (ModelPrinter.string_of_pi0 model pi0);
(* 			); *)
			
			
			(* Print some information *)
			self#print_algo_message Verbose_low ("Setting new pi0...");

			(* Set the new pi0 *)
			Input.set_pi0 (pi0);
			
			(* Actual call to IM/PRP (or whatever) *)
			let imitator_result = self#call_point in
			
			(* Create the abstraction of the result *)
			let abstract_result = self#abstract_result imitator_result pi0 in

			(* Process result *)
			self#process_result abstract_result;
			
			(* Compute the next point and store it; update limits before and after computing next point *)
			self#compute_next_point_and_update_limit;

		done; (* end while more points *)

		(* Update termination condition *)
		self#update_termination_condition;
	
		(* Print some information *)
		(*** NOTE: must be done after setting the limit (above) ***)
		self#print_warnings_limit;
		
		(* Return the algorithm-dependent result *)
		self#compute_bc_result





	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Packaging the result at the end of the exploration (to be defined in subclasses) *)
	method virtual compute_bc_result : Result.imitator_result

	
(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
