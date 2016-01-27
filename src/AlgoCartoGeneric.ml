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
 * Last modified     : 2016/01/27
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
(* Class-independent functions *)
(************************************************************)
(************************************************************)
(* Convert an 'im_result' into an 'abstract_im_result' *)
let abstract_im_result_of_im_result (im_result : im_result) : abstract_im_result =
	(* First, abstract state space *)
	let abstract_state_space = {
		nb_states		= StateSpace.nb_states im_result.state_space;
		nb_transitions	= StateSpace.nb_transitions im_result.state_space;
(* 		depth		 	= im_result.state_space; *)
	}
	in
	
	(* Construct the abstraction *)
	{
	(* Convex constraint *)
	result				= im_result.result;
	
	(* Explored state space *)
	state_space			= abstract_state_space;
	
	(* Nature of the state space (needed??) *)
(* 	tile_nature			: AbstractModel.tile_nature; *)
	
	(* Number of random selections of pi-incompatible inequalities performed *)
	nb_random_selections= im_result.nb_random_selections;
	
	(* Total computation time of the algorithm *)
	computation_time	= im_result.computation_time;
	
	(* Termination *)
	termination			= im_result.termination;
}




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

		(* The end *)
		()
	
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
		current_point <- self#get_initial_point;
		
		(*** TODO : check that initial pi0 is suitable!! (could be incompatible with initial constraint) ***)

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
			if not (verbose_mode_greater Verbose_medium) then
				set_verbose_mode Verbose_mute;
			
			(* Call the inverse method *)
			let algo = new AlgoIM.algoIM in
			let result = algo#run() in
			(*** WARNING: what is this going to do, exactly? ***)
			ResultProcessor.process_result result;
			
			(* Get the verbose mode back *)
			set_verbose_mode global_verbose_mode;
			
			(* Print some information *)
			self#print_algo_message Verbose_low ("Processing the result by IM...");

			(* Process the result by IM *)
			begin
			match result with
			| IM_result im_result -> self#process_result im_result;
			| _ -> raise (InternalError("The result of IM must be an im_result."));
			end;

			(*** TODO: check time/etc. limits ***)
			
			(* Print some information *)
			self#print_algo_message Verbose_low ("Computing the next point...");

			(* Get to the next point *)
			current_point <- self#find_next_point;
			
			current_iteration <- current_iteration + 1;
			
		done; (* end while more points *)
		
		
		(* Return the algorithm-dependent result *)
		self#compute_result



	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Processing the result of IM *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual process_result : Result.im_result -> unit

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
