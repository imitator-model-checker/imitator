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
 * Last modified     : 2016/01/20
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
(* Class-independent functions *)
(************************************************************)
(************************************************************)


(*------------------------------------------------------------*)
(** Check if a pi_0 belongs to a 'returned_constraint'*)
(*------------------------------------------------------------*)
let pi0_in_returned_constraint pi0 = function
	| Convex_constraint (k,_) -> LinearConstraint.is_pi0_compatible pi0#get_value k
	(** Disjunction of constraints *)
	| Union_of_constraints (k_list , _) -> List.exists (LinearConstraint.is_pi0_compatible pi0#get_value) k_list
	| NNCConstraint _ -> raise (InternalError ("NNCCs are not available everywhere yet."))

	
	

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
	(* Number of dimensions *)
	val mutable nb_dimensions = 0
	
	(* Min & max bounds for the parameters *)
	val mutable min_bounds = Array.make 0 NumConst.zero
	val mutable max_bounds = Array.make 0 NumConst.zero
	
	(* (Dynamic) Array for the results *)
	(*** TODO: change the structure kept in memory ***)
	val mutable computed_constraints : Result.returned_constraint DynArray.t = DynArray.create()
	
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
(* 		super#initialize_variables; *)
		(* The end *)
		
		(* Retrieve the model *)
		let model = Input.get_model() in
		(* Retrieve the v0 *)
		let v0 = Input.get_v0() in
		(* Retrieve the input options *)
(* 		let options = Input.get_options () in *)

		(* Print some information *)
(* 		print_message Verbose_medium ("Starting preprocessing for the behavioral cartography"); *)

		(* Time counter for recording the globl time spent on BC *)
(* 		time_spent_on_IM := 0.; *)
		(* Record start time to compute the time spent only on calling IM *)
(* 		start_time := Unix.gettimeofday(); *)

		(* Set the number of dimensions in the system *)
		nb_dimensions <- model.nb_parameters;
		
		(* Print some information *)
		self#print_algo_message Verbose_medium ("Number of dimensions: " ^ (string_of_int nb_dimensions));

		(* Check that the cartography is not applied to 0 dimension! *)
		if nb_dimensions = 0 then(
			print_error "The cartography has 0 dimension in V0, and cannot be applied.";
			abort_program();
		);

		(*** BADPROG (to improve ***)
		(* Initialize *)
		min_bounds <- Array.make nb_dimensions NumConst.zero;
		max_bounds <- Array.make nb_dimensions NumConst.zero;
		(* Fill *)
		for parameter_index = 0 to nb_dimensions - 1 do
			min_bounds.(parameter_index) <- v0#get_min parameter_index;
			max_bounds.(parameter_index) <- v0#get_max parameter_index;
		done;
	
		(* Compute the initial state *)
		let init_state = AlgoStateBased.compute_initial_state_or_abort() in
		
		(* Set the counter of useless points to 0 *)
		nb_useless_points <- 0;

		(* Initial constraint of the model *)
		let _, init_px_constraint = init_state in
		(* Hide non parameters *)
		init_p_constraint <- LinearConstraint.px_hide_nonparameters_and_collapse init_px_constraint;

		(* (Dynamic) Array for the results *)
		computed_constraints <- DynArray.create();

		(* The end *)
		()

		
		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Generic function checking if a computed pi0 belongs to some constraint and satisfies the init constraint; sets the reference "found_pi0" to true if indeed uncovered *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private test_pi0_uncovered tentative_pi0 =
		(* Get the model *)
		let model = Input.get_model() in
		(* Retrieve the input options *)
(* 		let options = Input.get_options () in *)

		(* Check that the current pi0 does not belong to any constraint *)
		if dynArray_exists (pi0_in_returned_constraint tentative_pi0) computed_constraints then (
			(* Update the number of unsuccessful points *)
			nb_useless_points <- nb_useless_points + 1;
			if verbose_mode_greater Verbose_medium then (
				print_message Verbose_medium "[Cartography.test_pi0_uncovered] The following pi0 is already included in a constraint.";
				print_message Verbose_medium (ModelPrinter.string_of_pi0 model tentative_pi0);
			);
			(*** TODO: could be optimized by finding the nearest multiple of tile next to the border, and directly switching to that one ***)
			false
			
		(* Check that it satisfies the initial constraint *)
		) else if not (LinearConstraint.is_pi0_compatible tentative_pi0#get_value init_p_constraint) then (
			(* Update the number of unsuccessful points *)
			nb_useless_points <- nb_useless_points + 1;
			if verbose_mode_greater Verbose_medium then (
				print_message Verbose_medium "[Cartography.test_pi0_uncovered] The following pi0 does not satisfy the initial constraint of the model.";
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


		
		

	(* Create the initial point for the analysis *)
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

	
	(* Find the next point *)
	method find_next_point =
		raise (InternalError("not implemented"))


		
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Main method running the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method run () =
		raise (InternalError("not implemented"))

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_result =
		raise (InternalError("not implemented"))

	
(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
