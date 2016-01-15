(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Convert a parsing structure into an abstract model
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        Ulrich Kuehne, Etienne Andre
 * 
 * Created:       2012/06/18
 * Last modified: 2016/01/15
 *
 ****************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)

open Exceptions
open OCamlUtilities
open ImitatorUtilities
open Automaton
open Options
open AbstractModel
open Result
open StateSpace
open Reachability






(************************************************************)
(* Types *)
(************************************************************)

(* List version of pi0 for PaTATOR *)
type pi0_list = (variable_index * NumConst.t) list


(************************************************************)
(* Global variable used for BC *)
(************************************************************)

(*** NOTE: all this could be turned in a structure, or a .mli file ***)


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(* Constants *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(* Verbose mode *)
let global_verbose_mode = ref (Verbose_standard)

(* Number of dimensions in the system *)
let nb_dimensions = ref 0

(* Min & max bounds for the parameters *)
let min_bounds = ref (Array.make 0 NumConst.zero)
let max_bounds = ref (Array.make 0 NumConst.zero)
	
(* Compute the (actually slightly approximate) number of points in V0 (for information purpose) *)
let nb_points = ref NumConst.zero



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(* Variables *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(* Current iteration (for information purpose) *)
let current_iteration = ref 0
(* Sum of number of states (for information purpose) *)
let nb_states = ref 0
(* Sum of number of transitions (for information purpose) *)
let nb_transitions = ref 0

(* Time limit reached? *)
let time_limit_reached = ref false

(* Time counter for recording the globl time spent on BC *)
let time_spent_on_IM = ref 0.

(* Counts the points actually member of an existing constraint (hence useless) for information purpose *)
let nb_useless_points = ref 0

(* Compute the initial state (TOTALLY RANDOM VALUE) *)
let init_state = ref (Location.make_location [] [], LinearConstraint.px_true_constraint())

(* Initial constraint of the model *)
let init_constraint = ref (LinearConstraint.p_true_constraint())

(* (Dynamic) Array for the results *)
let computed_constraints : returned_constraint DynArray.t ref = ref (DynArray.create())

(* Compute some variables for the border cartography only *)
(* Current_intervals_min and Current_intervals_max represent, for each dimension, the interval (multiple of step) in which the points have not been classified as good or bad *)
let current_intervals_min = ref (Array.make 0 NumConst.zero)
let current_intervals_max = ref (Array.make 0 NumConst.zero)

let current_pi0 : PVal.pval option ref = ref None

(* For PaTATOR: array of all pi0 contained in V0 *)
let all_pi0_array = ref None
(* Pointer to current pi0 in all_pi0_array *)
let all_pi0_array_current = ref 0



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(* Counters *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(* Record start time to compute the time spent only on calling IM *)
let start_time = ref (Unix.gettimeofday())





(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(* Global functions on pi0 *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(* Return the current_pi0; raises InternalError if current_pi0 was not initialized *)
let get_current_pi0_option () =
	match !current_pi0 with
	| None -> 
		raise (InternalError("Current_pi0 has not been initialized yet, altough it should have at this point."))
	| Some current_pi0 -> current_pi0


(* Return the all_pi0_array; raises InternalError if all_pi0_array was not initialized *)
let get_all_pi0_array_option () =
	match !all_pi0_array with
	| None -> 
		raise (InternalError("all_pi0_array has not been initialized yet, altough it should have at this point."))
	| Some all_pi0_array -> all_pi0_array




let counter_next_point = new Counter.counter



(**************************************************************)
(* I/O functions *)
(**************************************************************)
(** Write all constraints to a file *)
let write_result_to_file total_time =
	(* Get the model *)
	let model = Input.get_model() in
	(* Retrieve the input options *)
	let options = Input.get_options () in
	(* Retrieve all constraints in the form of a string *)
	let index = ref 1 in
	let constraints_str =
		DynArray.fold_left (fun current_str returned_constraint ->
			(* Get the constraint and convert to string *)
			let constraint_str = ModelPrinter.string_of_returned_constraint model.variable_names returned_constraint in
			let index_str = string_of_int !index in
			(* Increment *)
			index := !index + 1;
			(* Add the constraint to the string *)
			(*** TODO: add other information from im_result if needed (tile nature, computation time, etc.) ***)
			current_str
			^ "\n (***** Constraint " ^ index_str ^ "*****)"
			^ "\n" ^ constraint_str
			^ "\n\n"
		) "" !computed_constraints
	in
	(* Prepare the string to write *)
	let file_content =
		(*** WARNING: duplicate code (Reachability.ml) ***)
		"(*" 
		(* Program version *)
		^ "\n  Result output by " ^ Constants.program_name ^ ""
		^ "\n  Version  : " ^ (ImitatorUtilities.program_name_and_version_and_nickname_and_build())
		^ "\n  Model    : '" ^ options#file ^ "'"
		(* Date *)
		^ "\n  Generated: " ^ (now()) ^ ""
		(* Command *)
		^ "\n  Command  : " ^ (string_of_array_of_string_with_sep " " Sys.argv)
		(* Predefined format statistics for the cartography *)
		^ "\n  Stats    : "
			(* Number of PTA in parallel *)
			^ " " ^ (string_of_int model.nb_automata)
			(* Number of clocks *)
			^ " " ^ (string_of_int model.nb_clocks)
			(* Number of parameters *)
			^ " " ^ (string_of_int model.nb_parameters)
			(* Number of discrete variables *)
			^ " " ^ (string_of_int model.nb_discrete)
			(* Number of points in V0 *)
			^ " " ^ (NumConst.string_of_numconst !nb_points)
			(* Time *)
			^ " " ^ (string_of_float total_time)
(*				(* Number of nodes *)
				^ " " ^ (string_of_int (DistributedUtilities.get_nb_nodes()))*)
				(*** NOTE: cannot add easily the number of nodes, because in non-distributed compiling, DistributedUtilities is not accessible! ***)
			(* Number of tiles *)
			^ " " ^ (string_of_int (DynArray.length !computed_constraints))
		^ "\n*)\n\n"
		(* The actual result *)
		^ constraints_str ^ "\n"
	in
	(* Write to file *)
	let file_name = options#files_prefix ^ "_cart" ^ Constants.result_file_extension in
	write_to_file file_name file_content;
	print_message Verbose_standard ("Result written to file '" ^ file_name ^ "'.")



(************************************************************)
(* Functions on tile nature *)
(************************************************************)

let tile_nature_of_returned_constraint = function
	| Convex_constraint (_ , tn) -> tn
	| Union_of_constraints (_ , tn) -> tn
	| NNCConstraint _ -> raise (InternalError ("NNCCs are not available everywhere yet."))


(************************************************************)
(* Functions on NumConst (that may not need to be defined here) *)
(************************************************************)
(*------------------------------------------------------------*)
(* Check if number is a multiple of step since base_number *)
(* That is: does there exist an integer k such that number = base_number + k * step ? *)
(*------------------------------------------------------------*)
let is_multiple_with_step base_number step number =
	(* Substract base_number *)
	let number_minus_base = NumConst.sub number base_number in
	(* Divide by step *)
	let number_minus_base_divided = NumConst.div number_minus_base step in

	(* Print some information *)
(* 	print_message Verbose_standard ("(" ^ (NumConst.string_of_numconst number) ^ " - " ^ (NumConst.string_of_numconst base_number) ^ ") / " ^ (NumConst.string_of_numconst step) ^ " = " ^ (NumConst.string_of_numconst number_minus_base_divided) ^ ""); *)
	
	(* Print some information again *)
(* 	print_message Verbose_standard ("  Is it positive? " ^ (string_of_bool (NumConst.ge number_minus_base_divided NumConst.zero))); *)
(* 	print_message Verbose_standard ("  Is it an integer? " ^ (string_of_bool (NumConst.is_integer number_minus_base_divided))); *)
	
	(* Check if positive integer *)
	(NumConst.ge number_minus_base_divided NumConst.zero)
	&&
	(NumConst.is_integer number_minus_base_divided)




(*------------------------------------------------------------*)
(* Find a point n more or less in the middle of min and max, and that is a multiple of step
	(viz., n = min + k * step , and max = l * step + n, with l "close" to k *)
(* Raise Not_found if no such n exists *)
(*------------------------------------------------------------*)
let find_multiple_in_between min max step =
	(* First check that min <= max *)
	if NumConst.g min max then raise Not_found;
	
	(* Compute the average *)
	let average = NumConst.div
		(NumConst.add min max)
		(NumConst.numconst_of_int 2)
	in
	(* Check if the average is a valid point *)
	if is_multiple_with_step min step average then (
(* 				print_message Verbose_standard ((NumConst.string_of_numconst average) ^ " is multiple of " ^ (NumConst.string_of_numconst step) ^ "."); *)
		average
		
		(* Otherwise try below *)
		) else(
(* 				print_message Verbose_standard ((NumConst.string_of_numconst average) ^ " is NOT multiple of " ^ (NumConst.string_of_numconst step) ^ ""); *)
		let below = NumConst.find_multiple_below min step average in
		if NumConst.ge below min then below else(
			(* Otherwise try above *)
			let above = NumConst.find_multiple_above min step average in
			if NumConst.le above max then above else(
				(* Otherwise: not found *)
				raise Not_found
			) (* end if above valid *)
		) (* end if below valid *)
	) (* end if average valid *)


(*------------------------------------------------------------*)
(* Find a point n more or less in the middle of min and max, and that is a multiple of step starting from min_bound
	(viz., n = min_bound + k * step , with n >= min , and max = l * step + n, with l "close" to k *)
(* Raise Not_found if no such n exists *)
(*------------------------------------------------------------*)
let find_multiple_in_between_and_from min_bound min max step =
	(** TODO: could certainly be optimized *)
	
	(* 1) Find the first multiple m of step from min_bound that is above min *)
	let m = NumConst.find_multiple_above min_bound step min in

	(* 2) Apply the 'find_multiple_in_between' function on [m, max] *)
	find_multiple_in_between m max step


(************************************************************)
(* General functions *)
(************************************************************)
(*------------------------------------------------------------*)
(** Check if a pi_0 belongs to a 'returned_constraint'*)
(*------------------------------------------------------------*)
let pi0_in_returned_constraint pi0 = function
	| Convex_constraint (k,_) -> LinearConstraint.is_pi0_compatible pi0#get_value k
	(** Disjunction of constraints *)
	| Union_of_constraints (k_list , _) -> List.exists (LinearConstraint.is_pi0_compatible pi0#get_value) k_list
	| NNCConstraint _ -> raise (InternalError ("NNCCs are not available everywhere yet."))


(*------------------------------------------------------------*)
(** Try to check whether a returned_constraint is included in another constraint *)
(*------------------------------------------------------------*)
let leq_returned_constraint rc1 rc2 =
	match (rc1, rc2) with
	| Convex_constraint (k1,_) , Convex_constraint (k2,_) -> LinearConstraint.p_is_leq k1 k2
	| Convex_constraint (k1,_) , Union_of_constraints (k_list,_) -> List.exists (LinearConstraint.p_is_leq k1) k_list
	(** Otherwise, don't bother for now *)
	(*** TODO: improve ***)
	| _ -> false



(************************************************************)
(* Initial pi0 functions *)
(************************************************************)
(*------------------------------------------------------------*)
(* Set pi0 to the initial (smallest) point in V0 *)
(*** WARNING / TODO: technically, we should check that pi0 models the initial constraint ***)
(*------------------------------------------------------------*)
let compute_initial_pi0 () =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	(* Get the model *)
	let model = Input.get_model() in
	
	(*** WARNING: should be sure that 0 is the first parameter dimension!! ***)
(* 	let first_dimension = 0 in *)
	
(* 	let step = options#step in *)
	
	(* Case by case *)
	begin
	match options#imitator_mode with
	
	(* Instantiate with the lower bounds *)
	| Cover_cartography ->
		let pi0 = new PVal.pval in
		(* Copy min bounds *)
		for parameter_index = 0 to model.nb_parameters - 1 do
			pi0#set_value parameter_index !min_bounds.(parameter_index);
		done;
		
		current_pi0 := Some pi0;
		(*
		(*** BEGIN DEBUG ***)
		;
		let pi0_fun = pi0_fun_of_current_pi0 () in
			print_message Verbose_standard ("=======");
			print_message Verbose_standard (ModelPrinter.string_of_pi0 (Input.get_model()) pi0_fun)
		(*** END DEBUG ***)
		*)

	(* Instantiate with the point in the middle of V0 *)
	| Border_cartography ->
		(*** NOTE: to rewrite and retest ***)
		raise (InternalError ("Border cartography is not implemented"));
(*		(* Start with the min bounds everywhere *)
		let initial_pi0 = new PVal.pval in
(** 		for i = 0 to (Array.length initial_pi0) - 1 do *)
			(* Get some variables *)
			let min_bound = !min_bounds.(first_dimension) in
			let max_bound = !max_bounds.(first_dimension) in
			(* Compute the middle *)
			let local_point = try
				find_multiple_in_between min_bound max_bound step
				(* If not found: cannot start (but this should not happen if max_bound >= min_bound, for whatever step) *)
				with Not_found -> 
					raise (Failure("V0 does not contain any point multiple of step '" ^ (NumConst.string_of_numconst step) ^"' in some direction."))
			in
			(* Assign it to the array *)
			initial_pi0.(first_dimension) <- local_point;
(* 		done; *)
		(* Set the initial_pi0 *)
		current_pi0 := Some (initial_pi0)
		*)
		
	| _ -> raise (InternalError("In function 'initial_pi0', the mode should be a cover / border cartography only."))
	end;
	()


(************************************************************)
(* Next pi0 functions *)
(************************************************************)

(* Generic function checking if a computed pi0 belongs to some constraint and satisfies the init constraint; sets the reference "found_pi0" to true if indeed uncovered *)
let test_pi0_uncovered current_pi0 found_pi0 =
	(* Get the model *)
	let model = Input.get_model() in
	(* Retrieve the input options *)
	let options = Input.get_options () in

	(* Check that the current pi0 does not belong to any constraint *)
	if dynArray_exists (pi0_in_returned_constraint current_pi0) !computed_constraints then (
		(* Update the number of unsuccessful points *)
		nb_useless_points := !nb_useless_points + 1;
		if verbose_mode_greater Verbose_medium then (
			print_message Verbose_medium "[Cartography.test_pi0_uncovered] The following pi0 is already included in a constraint.";
			print_message Verbose_medium (ModelPrinter.string_of_pi0 model current_pi0);
		);
		(*** TODO: could be optimized by finding the nearest multiple of tile next to the border, and directly switching to that one ***)
		
	(* Check that it satisfies the initial constraint *)
	) else if not (LinearConstraint.is_pi0_compatible current_pi0#get_value !init_constraint) then (
		(* Update the number of unsuccessful points *)
		nb_useless_points := !nb_useless_points + 1;
		if verbose_mode_greater Verbose_medium then (
			print_message Verbose_medium "[Cartography.test_pi0_uncovered] The following pi0 does not satisfy the initial constraint of the model.";
			print_message Verbose_medium (ModelPrinter.string_of_pi0 model current_pi0);
		);
	(* If both checks passed, then pi0 found *)
	)else(
		found_pi0 := true;
	);
	
	(*** BADPROG: this check has nothing to do in this function! put back to the BC algorithm ***)
	(* If pi0 still not found, check time limit *)
	if not !found_pi0 then(
		(* Stop if the time limit has been reached *)
		match options#time_limit with
		| None -> ()
		| Some limit ->
			if (get_time()) > (float_of_int limit)
				then time_limit_reached := true;
	);
	()



(*------------------------------------------------------------*)
(* Generate one random pi0 in a given interval for each parameter *)
(*------------------------------------------------------------*)
let one_random_pi0 () =
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
	let random_pi0 = new PVal.pval (*Array.make model.nb_parameters NumConst.zero*) in
	(* Fill it *)
	for i = 0 to model.nb_parameters - 1 do
		let min = v0#get_min i in
		let max = v0#get_max i in
		(* Generate a random value in the interval *)
		let random_value = NumConst.random_integer min max in
		
		(* Print some information *)
 		print_message Verbose_medium ("Generating randomly value '" ^ (NumConst.string_of_numconst random_value) ^ "' for parameter '" ^ (model.variable_names i) ^ "'.");
 		
		(* Add to the array *)
		random_pi0#set_value i random_value;
	done;
	(* Return the result *)
	random_pi0


(*------------------------------------------------------------*)
(** Try to generate an uncovered random pi0, and gives up after n tries *)
(*------------------------------------------------------------*)
let random_pi0 max_tries =
	counter_next_point#start;

	(* Print some information *)
	print_message Verbose_medium ("Trying to randomly find a fresh pi0 with " ^ (string_of_int max_tries) ^ " tries.");

	(* Flags *)
	let continue = ref true in
	let found_pi0 = ref false in
	(* Counter *)
	let nb_tries = ref 1 in
	
	(* Loop until impossible *)
	while !continue do
		(* Generate a random pi0 *)
		let pi0 = one_random_pi0 () in
		(* Try to see if valid (and updates found_pi0) *)
		test_pi0_uncovered pi0 found_pi0;
		(* If yes: stop *)
		if !found_pi0 then(
			(* Print some information *)
			print_message Verbose_medium ("Try " ^ (string_of_int !nb_tries) ^ " successful!");

			current_pi0 := Some pi0;
			
			continue := false;
		(* Otherwise: go further *)
		)else(
			(* Print some information *)
			print_message Verbose_medium ("Try " ^ (string_of_int !nb_tries) ^ " unsuccessful.");
			(* Increment counter *)
			nb_tries := !nb_tries + 1;
			(* Check whether limit reached *)
			if !nb_tries > max_tries then
				continue := false;
				(* Print some information *)
				print_message Verbose_medium ("Could not find a pi0 within " ^ (string_of_int max_tries) ^ " tries.");
		);
	done;
	
	counter_next_point#stop;

	(* Return whether a pi0 has been found *)
	!found_pi0



(*------------------------------------------------------------*)
(* Compute the next pi0 and directly modify the variable 'current_pi0' (standard BC) *)
(*------------------------------------------------------------*)

(* Auxiliary function: compute the immediately next pi0 in a sequential manner; returns true if a pi0 has been found, false if all V0 is covered *)
(*** BAD PROG: flag problem ***)
let compute_next_sequential_pi0 () =
	(* Retrieve the input options *)
	let options = Input.get_options () in

	(* Retrieve the current pi0 (that must have been initialized before) *)
	let current_pi0 = get_current_pi0_option () in

	(* Start with the first dimension *)
	let current_dimension = ref 0 in (** WARNING: should be sure that 0 is the first parameter dimension *)
	(* The current dimension is not yet the maximum *)
	let not_is_max = ref true in
	
	let more_pi0 = ref true in
	
	while !not_is_max do
		(* Try to increment the local dimension *)
		let current_dimension_incremented = NumConst.add (current_pi0#get_value !current_dimension) options#step in
		if current_dimension_incremented <= !max_bounds.(!current_dimension) then (
			(* Increment this dimension *)
			current_pi0#set_value (!current_dimension) current_dimension_incremented;
			(* Reset the smaller dimensions to the low bound *)
			for i = 0 to !current_dimension - 1 do
				current_pi0#set_value i !min_bounds.(i);
			done;
			(* Stop the loop *)
			not_is_max := false;
		)
		(* Else: try the next dimension *)
		else ( 
			current_dimension := !current_dimension + 1;
			(* If last dimension: the end! *)
			if !current_dimension >= !nb_dimensions then(
				more_pi0 := false;
				not_is_max := false;
			)
		);
	done; (* while not is max *)
	(* Return the flag *)
	!more_pi0



(** Compute the next pi0 by sequentially trying all points until a point not covered is found; and then directly modify the internal variable 'current_pi0' (standard BC)
 * Return (found_pi0 : bool, nb_useless_points : int)
 *)
let find_next_pi0_cover () =
	(* Get the model *)
(* 	let model = Input.get_model() in *)

	(* Retrieve the current pi0 (that must have been initialized before) *)
	let current_pi0 = get_current_pi0_option () in
	
(*		(* Counts the points actually member of an existing constraint (hence useless) for information purpose *)
	let nb_useless_points = ref 0 in*)
	
	(* Are there still possible points *)
	let more_pi0 = ref true in
	(* Did we find a suitable pi0 *)
	let found_pi0 = ref false in
(*(*		(* Did we reach the time limit *)
	let time_limit_reached = ref false in*)*)

	while !more_pi0 && not !time_limit_reached && not !found_pi0 do
		
		(* 1) Compute the next pi0 (if any left) in a sequential manner; the function returns false if all has been covered *)
		more_pi0 := compute_next_sequential_pi0 ();

		(* 2) Check that this pi0 is new *)
		if !more_pi0 then(
			print_message Verbose_high ("[Cartography.find_next_pi0_cover] check whether pi0 is covered");
			(* Generic function possibly updating found_pi0 *)
			test_pi0_uncovered current_pi0 found_pi0;
		); (*if more pi0 *)
		
	done; (* while more pi0 and so on *)
	
	(* Return info (note that current_pi0 has ALREADY been updated if a suitable pi0 was found !) *)
	!found_pi0 (*, !nb_useless_points*)
      


(*------------------------------------------------------------*)
(** Compute the next pi0 and directly modify the variable 'current_pi0' (border BC) *)
(*------------------------------------------------------------*)
let find_next_pi0_border latest_nature =
	find_next_pi0_cover () (*
	(* Print some information *)
	print_message Verbose_standard "Entering function 'find_next_pi0_border'.";
	
	(* Retrieve the input options *)
	let options = Input.get_options () in
	
	let step = options#step in

	(* Counts the points actually member of an existing constraint (hence useless) for information purpose *)
	let nb_useless_points = ref 0 in
	
	(* Are there still possible points *)
	let more_pi0 = ref true in
	(* Did we find a suitable pi0 *)
	let found_pi0 = ref false in
	(* Did we reach the time limit *)
	let time_limit_reached = ref false in
	
	
	(** !!!!! BOUCLE WHILE PROBABLEMENT A SUPPRIMER *)
	
(* 	while !more_pi0 && not !time_limit_reached && not !found_pi0 do *)
			
		(* 1) Compute the next pi0 (if any left) *)
		
		(* Start with the first dimension *)
		let current_dimension = ref 0 in (** WARNING: should be sure that 0 is the first parameter dimension *)
		(* The current dimension is not yet the maximum *)
		let not_is_max = ref true in
		
		print_message Verbose_standard ("Starting from dimension " ^ (string_of_int !current_dimension) ^ ".");
		
		begin
		try
		while !not_is_max do

			(* 1) Dichotomy: find a point between a good and a bad zone *)
			try
			(* While current_max - current_min > step, there are some points in between *)
			while true do
				(* Find a multiple of step from min_bound in [current_min, current_max] *)
				let current_min = current_intervals_min.(!current_dimension) in
				let current_max = current_intervals_max.(!current_dimension) in

				print_message Verbose_standard ("Looking for a multiple of step " ^ (NumConst.string_of_numconst step) ^ " from " ^ (NumConst.string_of_numconst min_bounds.(!current_dimension)) ^ " in [" ^ (NumConst.string_of_numconst current_min) ^ ", " ^ (NumConst.string_of_numconst current_max) ^ "] in dimension " ^ (string_of_int !current_dimension) ^ ".");
		
				(* Can raise Not_found, in which case we exit the loop *)
				let middle_point = find_multiple_in_between_and_from min_bounds.(!current_dimension) current_min current_max step in
				
				(* Print some information *)
				print_message Verbose_standard ("Found " ^ (NumConst.string_of_numconst middle_point) ^ ".");
		
				(* Update our current and tentative pi0 *)
				current_pi0.(!current_dimension) <- middle_point;

				(* Convert the current pi0 to functional representation *)
				let pi0 = fun parameter -> current_pi0.(parameter) in
				
				(* Print some information *)
				print_message Verbose_standard ("Constructing a tentative point: " ^ (ModelPrinter.string_of_pi0 model pi0) ^ ".");
		
				print_message Verbose_standard ("Checking whether this point belongs to a tile.");

				(* Check that the current pi0 does not belong to any constraint *)
				if dynArray_exists (fun returned_constraint ->
					(* If the point belongs to a tile *)
					if pi0_in_returned_constraint pi0 returned_constraint then (
						(* Print some information *)
						print_message Verbose_standard ("  Pi0 belongs to this tile.");

						(* Get the tile nature *)
						let tile_nature = tile_nature_of_returned_constraint returned_constraint in
						
						(* Print some information *)
						print_message Verbose_standard ("    This tile is " ^ (string_of_tile_nature tile_nature) ^ ".");
						
						begin
						match tile_nature with
						(* If this tile is good, reduce the interval from below: update to [middle_point + step ; current_max] *)
						(** TODO: could be optimized by finding the nearest multiple of tile next to the border *)
						| Good ->
							current_intervals_min.(!current_dimension) <- NumConst.add middle_point step;
							
						(* If this tile is bad, reduce the interval from above: update to [current_min ; middle_point - step ] *)
						(** TODO: could be optimized by finding the nearest multiple of tile next to the border *)
						| Bad ->
							current_intervals_max.(!current_dimension) <- NumConst.sub middle_point step;
						
						| _ -> raise (InternalError ("Tile nature should be good or bad only at this point "));
						end;
						
						(* Print some information *)
						print_message Verbose_standard ("    Reducing the interval to [" ^ (NumConst.string_of_numconst current_intervals_min.(!current_dimension)) ^ ", " ^ (NumConst.string_of_numconst current_intervals_max.(!current_dimension)) ^ "] in dimension " ^ (string_of_int !current_dimension) ^ ".");
						
						(* Return true because we found a tile *)
						true
					)else(
						(* This tile has not yet been found *)
						print_message Verbose_standard ("  Pi0 does not belong to this tile.");
						false
					)
				) computed_constraints then (
					(* Update the number of unsuccessful points *)
					nb_useless_points := !nb_useless_points + 1;
					if verbose_mode_greater Verbose_medium then (
						print_message Verbose_medium "The following pi0 is already included in a constraint.";
						print_message Verbose_medium (ModelPrinter.string_of_pi0 model pi0);
					);
					
				) else (
					(* Found a point not covered by any constraint! *)
					(** WARNING: should test compatibility with initial constraint (and might be quite tricky, now) *)
					raise Found
				);

			done; (* end while *)
			with Not_found -> (); (* at this point, there is no multiple of step between min and max for this dimension *)
			(* Print some information *)
			print_message Verbose_standard ("No multiple found in the interval.");
			
			(* Switch to the next *)
			current_dimension := !current_dimension + 1;

			(* Print some information *)
			print_message Verbose_standard ("Now increasing to dimension " ^ (string_of_int !current_dimension) ^ ".");
		
			(* If last dimension: the end! *)
			if !current_dimension >= nb_dimensions then(
	(* 			more_pi0 := false; *)
				not_is_max := false;
				
				(* Print some information *)
				print_message Verbose_standard ("Maximum dimension " ^ (string_of_int nb_dimensions) ^ " has been reached.");
		
			)else(
				(* Reset the intervals of the smaller dimensions to the initial min / max bounds *)
				for i = 0 to !current_dimension - 1 do
					(* On ne touche pas pi0 *)
					(*current_pi0.(i) <- min_bounds.(i);*)
					
					(* Mais on peut conserver le max *)
					current_intervals_min.(i) <- min_bounds.(i);
					
					(* Print some information *)
					print_message Verbose_standard ("  New interval for dimension " ^ (string_of_int i) ^ ": [" ^ (NumConst.string_of_numconst current_intervals_min.(i)) ^ ", " ^ (NumConst.string_of_numconst current_intervals_max.(i)) ^ "].");
		
				done;
			
			
			); (* end if last dimension *)
				
			(** !!!!! AUTRE TEST A FAIRE *)
			(*if current_dimension_incremented <= max_bounds.(!current_dimension) then (
				(* Increment this dimension *)
				(** !!!!! OU DECREMENTE **)
				current_pi0.(!current_dimension) <- current_dimension_incremented;
				(* Reset the smaller dimensions to the low bound *)
				for i = 0 to !current_dimension - 1 do
					(** !!!!! REINITIALISER A CHAQUE FOIS LEUR INTERVALLE ? (attention, la borne sup minimal est connue, c'est la meme qu'au coup precedent) **)
					(** !!!!! PAR CONTRE, NE PAS TOUCHER PI0 ? **)
					current_pi0.(i) <- min_bounds.(i);
				done;
				(* Stop the loop *)
				not_is_max := false;
			)
			(* Else: try the next dimension *)
			else ( 
				current_dimension := !current_dimension + 1;
				(* If last dimension: the end! *)
				if !current_dimension >= nb_dimensions then(
					more_pi0 := false;
					not_is_max := false;
				)
			);*)
		done; (* while not is max *)

		
(* 	done; (* while more pi0 and so on *) *)

	(*!found_pi0 *)false, (*!time_limit_reached*) false , !nb_useless_points

	with Found -> (*!found_pi0 *)true, (*!time_limit_reached*) false , !nb_useless_points
	end

	*)


(* Generic function to find the next pi0 *)
(*** WARNING: duplicate code ***)
let find_next_pi0 tile_nature_option =
	counter_next_point#start;
	(* Retrieve the input options *)
	let options = Input.get_options () in

	let found_pi0 (*, new_nb_useless_points*) =
	(* Branching *)
	match options#imitator_mode with
	| Cover_cartography ->
		find_next_pi0_cover ()

	| Border_cartography ->
		begin
		match tile_nature_option with
			| Some tile_nature -> find_next_pi0_border tile_nature
			| None -> raise (InternalError("The case of a border cartography with no tile nature in the most recent constraint is not handled yet."))
		end

	| _ -> raise (InternalError("In function 'find_next_pi0', the mode should be a cover / border cartography only."))
	in
	(* Update the number of useless points *)
(* 	nb_useless_points := !nb_useless_points + new_nb_useless_points; *)
	
	counter_next_point#stop;

	(* Return *)
	found_pi0 , !time_limit_reached


(* Generic function to find the next pi0 (in shuffle mode) *)
(*** WARNING: duplicate code ***)
let find_next_pi0_shuffle tile_nature_option =
	counter_next_point#start;
	(* Retrieve the input options *)
	let options = Input.get_options () in

	let found_pi0 , new_nb_useless_points =
	(* Branching *)
	match options#imitator_mode with
	| Cover_cartography ->
		(* Are there still possible points *)
		let more_pi0 = ref true in
		(* Did we find a suitable pi0 *)
		let found_pi0 = ref false in

		let all_pi0_array = get_all_pi0_array_option () in

		while !more_pi0 && not !time_limit_reached && not !found_pi0 do
			
			(* 1) Compute the next pi0 (if any left) using the shuffle mode; the function returns false if all has been covered *)
			if !all_pi0_array_current < Array.length all_pi0_array - 1 then(
				(* Go to next pi0 *)
				all_pi0_array_current := !all_pi0_array_current + 1;
				(* Update *)
				current_pi0 := Some all_pi0_array.(!all_pi0_array_current);
			)else(
				(* Finished *)
				more_pi0 := false;
			);

			(* 2) Check that this pi0 is new *)
			if !more_pi0 then(
				(* Generic function possibly updating found_pi0 *)
				test_pi0_uncovered (get_current_pi0_option ()) found_pi0;
			); (*if more pi0 *)
			
		done; (* while more pi0 and so on *)
		
		(* Return info (note that current_pi0 has ALREADY been updated if a suitable pi0 was found !) *)
		!found_pi0 , !nb_useless_points
	| _ -> raise (InternalError("In function 'find_next_pi0_shuffle', the mode should be a cover cartography only."))
	in
	(* Update the number of useless points *)
	nb_useless_points := !nb_useless_points + new_nb_useless_points;
	
	counter_next_point#stop;

	(* Return *)
	found_pi0 , !time_limit_reached



(************************************************************)
(* BEHAVIORAL CARTOGRAPHY ALGORITHM functions *)
(************************************************************)
	
	
(*** TODO: merge both algorithms into cover_behavioral_cartography !!! ***)


(*------------------------------------------------------------*)
(** Auxiliary function: initialize the behavioral cartography *)
(*------------------------------------------------------------*)
(*** TODO: merge with bc_initialize_subpart; technically, split between the actual initialization, and the initialization that depends on V0 ***)
let bc_initialize () =
	(* Get the model *)
	let model = Input.get_model() in
	(* Get the v0 *)
	let v0 = Input.get_v0() in
	(* Retrieve the input options *)
	let options = Input.get_options () in

	(* Print some information *)
	print_message Verbose_medium ("Starting preprocessing for the behavioral cartography");

	(* Time counter for recording the globl time spent on BC *)
	time_spent_on_IM := 0.;
	(* Record start time to compute the time spent only on calling IM *)
	start_time := Unix.gettimeofday();

	(* Number of dimensions in the system *)
	nb_dimensions := model.nb_parameters;
	
	(* Print some information *)
	print_message Verbose_medium ("Number of dimensions: " ^ (string_of_int !nb_dimensions));

	(* Check that the cartography is not applied to 0 dimension! *)
	if !nb_dimensions = 0 then(
		print_error "The cartography has 0 dimension in V0, and cannot be applied.";
		abort_program();
	);
	
	
	(* Min & max bounds for the parameters *)
	
(*	min_bounds := Array.map (fun (low, high) -> low) v0;
	max_bounds := Array.map (fun (low, high) -> high) v0;*)
	(*** BADPROG (to improve ***)
	(* Initialize *)
	min_bounds := Array.make !nb_dimensions NumConst.zero;
	max_bounds := Array.make !nb_dimensions NumConst.zero;
	(* Fill *)
	for parameter_index = 0 to !nb_dimensions - 1 do
		!min_bounds.(parameter_index) <- v0#get_min parameter_index;
		!max_bounds.(parameter_index) <- v0#get_max parameter_index;
	done;
	
	
	(* Compute the (actually slightly approximate) number of points in V0 (for information purpose) *)
(*	nb_points := Array.fold_left (fun current_number (low, high) ->
		(* Multiply current number of points by the interval + 1, itself divided by the step *)
		NumConst.mul
			current_number
			(NumConst.div
				(NumConst.add
					(NumConst.sub high low)
					NumConst.one
				)
				options#step
			)
	) NumConst.one v0;*)
	nb_points := NumConst.one;
	for parameter_index = 0 to !nb_dimensions - 1 do
		nb_points :=
		let low = v0#get_min parameter_index in
		let high = v0#get_max parameter_index in
		(* Multiply current number of points by the interval + 1, itself divided by the step *)
		NumConst.mul
			!nb_points
			(NumConst.div
				(NumConst.add
					(NumConst.sub high low)
					NumConst.one
				)
				options#step
			)
		;
	done;
	
	(*** TODO: check that it is not empty (or is it done elsewhere?) ***)
	
	(* Counts the points actually member of an existing constraint (hence useless) for information purpose *)
	nb_useless_points := 0;
	
	(* Compute the initial state *)
	init_state := get_initial_state_or_abort model;

	(* Initial constraint of the model *)
	let _, init_px_constraint = !init_state in
	(* Hide non parameters *)
	init_constraint := LinearConstraint.px_hide_nonparameters_and_collapse init_px_constraint;

(*	(* (Dynamic) Array for the pi0 (USELESS SO FAR) *)
	let pi0_computed = DynArray.create() in*)
	(* (Dynamic) Array for the results *)
	computed_constraints := DynArray.create();
	
	(* Compute some variables for the border cartography only *)
	(* Current_intervals_min and Current_intervals_max represent, for each dimension, the interval (multiple of step) in which the points have not been classified as good or bad *)
	let the_current_intervals_min, the_current_intervals_max =
	match options#imitator_mode with
		| Border_cartography -> 
			Array.copy !min_bounds, Array.copy !max_bounds
(*			let first, others =
			begin match model.parameters with 
				| first :: others -> first , others 
				| _ -> raise (InternalError("There should be at least one parameter (but this may not have been checked somewhere else, right?)."))
			end
			in
			(* Current dimension we consider *)
			ref first,
			(* The other dimensions *)
			ref others,
			(* The initial interval: min and max bound for the current dimension *)
			ref min_bounds.(first), ref max_bounds.(first)*)
	
		(* Otherwise, does not matter *)
		| _ -> (*ref 0, ref [], ref NumConst.zero, ref NumConst.zero*) Array.make 0 NumConst.zero, Array.make 0 NumConst.zero
	in
	current_intervals_min := the_current_intervals_min;
	current_intervals_max := the_current_intervals_max;
	

	(* Current iteration (for information purpose) *)
	current_iteration := 1;
	(* Sum of number of states (for information purpose) *)
	nb_states := 0;
	(* Sum of number of transitions (for information purpose) *)
	nb_transitions := 0;

	(* Verbose mode *)
	global_verbose_mode := get_verbose_mode();
	
	(*** TODO : check that initial pi0 is suitable!! (could be incompatible with initial constraint) ***)
	
	(* Print *)
	print_message Verbose_standard ("\n**************************************************");
	print_message Verbose_standard (" START THE BEHAVIORAL CARTOGRAPHY ALGORITHM");
	print_message Verbose_standard ("**************************************************");
	print_message Verbose_standard (" Parametric rectangle V0: ");
	print_message Verbose_standard (ModelPrinter.string_of_v0 model v0);
	print_message Verbose_standard (" Number of points inside V0: " ^ (NumConst.string_of_numconst !nb_points));
	()

(*Hoang Gia modified bc_initialize *)
(*** WARNING!!!! why ?? What is the difference with bc_initialize ?? ***)
(*** TODO: merge with bc_initialize ***)
let bc_initialize_subpart () =
	(* Get the model *)
	let model = Input.get_model() in
	(* Get the v0 *)
	let v0 = Input.get_v0() in
	(* Retrieve the input options *)
	let options = Input.get_options () in

	(* Print some information *)
	print_message Verbose_medium ("Starting preprocessing for the behavioral cartography");

	(* Time counter for recording the globl time spent on BC *)
	time_spent_on_IM := 0.;
	(* Record start time to compute the time spent only on calling IM *)
	start_time := Unix.gettimeofday();

	(* Number of dimensions in the system *)
	nb_dimensions := model.nb_parameters;
	
	(* Print some information *)
	print_message Verbose_medium ("Number of dimensions: " ^ (string_of_int !nb_dimensions));

	(* Check that the cartography is not applied to 0 dimension! *)
	if !nb_dimensions = 0 then(
		print_error "The cartography has 0 dimension in V0, and cannot be applied.";
		abort_program();
	);
	
	
	(* Min & max bounds for the parameters *)
	
(*	min_bounds := Array.map (fun (low, high) -> low) v0;
	max_bounds := Array.map (fun (low, high) -> high) v0;*)
	(*** BADPROG (to improve ***)
	(* Initialize *)
	min_bounds := Array.make !nb_dimensions NumConst.zero;
	max_bounds := Array.make !nb_dimensions NumConst.zero;
	(* Fill *)
	for parameter_index = 0 to !nb_dimensions - 1 do
		!min_bounds.(parameter_index) <- v0#get_min parameter_index;
		!max_bounds.(parameter_index) <- v0#get_max parameter_index;
	done;
	
	(*** TODO: check that it is not empty (or is it done elsewhere?) ***)
	
	(* Counts the points actually member of an existing constraint (hence useless) for information purpose *)
	nb_useless_points := 0;
	
	(* Compute the initial state *)
	init_state := get_initial_state_or_abort model;
	
	
	(* Compute some variables for the border cartography only *)
	(* Current_intervals_min and Current_intervals_max represent, for each dimension, the interval (multiple of step) in which the points have not been classified as good or bad *)
	let the_current_intervals_min, the_current_intervals_max =
	match options#imitator_mode with
		| Border_cartography -> 
			Array.copy !min_bounds, Array.copy !max_bounds
	
		(* Otherwise, does not matter *)
		| _ -> (*ref 0, ref [], ref NumConst.zero, ref NumConst.zero*) Array.make 0 NumConst.zero, Array.make 0 NumConst.zero
	in
	current_intervals_min := the_current_intervals_min;
	current_intervals_max := the_current_intervals_max;
	
	(*** NOTE: what is missing from bc_initialize() is the initialization to 0 of current_iteration, nb_states, nb_transitions, as well as computed_constraints ***)


	(* Verbose mode *)
	global_verbose_mode := get_verbose_mode();
	
	(*** TODO : check that initial pi0 is suitable!! (could be incompatible with initial constraint) ***)
	()




(*------------------------------------------------------------*)
(** Auxiliary function: process the result of IM; return true if the constraint is indeed added, false otherwise *)
(*------------------------------------------------------------*)
let bc_process_im_result im_result =
	(* Get the model *)
	let model = Input.get_model() in
	(* Retrieve the input options *)
	let options = Input.get_options () in

	(* Update the time spent on IM *)
	time_spent_on_IM := !time_spent_on_IM +. im_result.total_time;
	
	(* Update the counters *)
	nb_states := !nb_states + im_result.nb_states;
	nb_transitions := !nb_transitions + im_result.nb_transitions;
	
	(* Check validity of returned constraint *)
	let valid_result = ref true in
	if im_result.premature_stop then(
		print_warning "This execution of IM has stopped prematurely.";
		
		(* Should the result be taken into account? *)
		(* Yes only for EFIM and a bad tile *)
		if options#efim && im_result.tile_nature = Bad then(
			valid_result := true; (* useless assignment since valid_result was already initialized to true *)
		)else(
			valid_result := false;
		);
	);
	
	(* Print message *)
	print_message Verbose_standard (
		"\nK" ^ (string_of_int (!current_iteration)) ^ " computed by IM after "
		^ (string_of_int im_result.nb_iterations) ^ " iteration" ^ (s_of_int im_result.nb_iterations) ^ ""
		^ " in " ^ (string_of_seconds im_result.total_time) ^ ": "
		^ (string_of_int im_result.nb_states) ^ " state" ^ (s_of_int im_result.nb_states)
		^ " with "
		^ (string_of_int im_result.nb_transitions) ^ " transition" ^ (s_of_int im_result.nb_transitions) ^ " explored.");
		
	(* Iterate *)
	current_iteration := !current_iteration + 1;
		
	(* VALID RESULT *)
	if !valid_result then(
		if im_result.premature_stop then(
			print_message Verbose_standard "This constraint is valid despite premature termination.";
		);
	
		(* Print the constraint *)
		
		(*** NOTE: it may actually be more clever to check the tile nature from the graph, especially if we go for more complicated properties!! ***)
		
		
	(* 			let bad_string = if StateSpace.is_bad model graph then "BAD." else "GOOD." in *)
		print_message Verbose_low ("Constraint K0 computed:");
		print_message Verbose_standard (ModelPrinter.string_of_returned_constraint model.variable_names im_result.result);
		if model.correctness_condition <> None then(
			print_message Verbose_medium ("This tile is " ^ (StateSpace.string_of_tile_nature im_result.tile_nature) ^ ".");
		);

		(* Process the constraint(s) in some cases *)
		begin
		(* Branching *)
		match options#imitator_mode with
		| Cover_cartography ->
			(* Just return the constraint *)
			()

		| Border_cartography ->
			(* The function depends whether the zone is good or bad *)
			let nb_enlargements = ref 0 in
			let enlarge =
				match im_result.tile_nature with
				(* If good: take all points from zero *)
				| Good -> LinearConstraint.grow_to_zero_assign
				(* If bad: take all points above *)
				| Bad -> LinearConstraint.grow_to_infinite_assign
				| _ -> raise (InternalError ("Tile nature should be good or bad only, so far "))
			in
			(* Apply this to the constraint(s) *)
			begin match im_result.result with
				| Convex_constraint (k, _) ->
					(*** NOTE: Quite costly test, but interesting for statistics and readability ***)
					let old_k = LinearConstraint.p_copy k in
					enlarge model.parameters model.clocks_and_discrete k;
					if not (LinearConstraint.p_is_equal k old_k) then nb_enlargements := !nb_enlargements + 1;
				| Union_of_constraints (k_list, _) ->
					List.iter (fun k ->
						(*** NOTE: Quite costly test, but interesting for statistics and readability ***)
						let old_k = LinearConstraint.p_copy k in
						enlarge model.parameters model.clocks_and_discrete k;
						if not (LinearConstraint.p_is_equal k old_k) then nb_enlargements := !nb_enlargements + 1;
					) k_list
				| NNCConstraint _ -> raise (InternalError ("NNCCs are not available everywhere yet."))
			end;
			
			if !nb_enlargements > 0 then(
				print_message Verbose_standard ("Constraint after enlarging:" ^ (if !nb_enlargements > 1 then " ("  ^ (string_of_int !nb_enlargements) ^ " enlargements)" else ""));
				print_message Verbose_standard (ModelPrinter.string_of_returned_constraint model.variable_names im_result.result);
			);

		| _ -> raise (InternalError("In function 'cover_behavioral_cartography', the mode should be a cover / border cartography only."))
		end; (* end process constraint *)
		
		
		(* Add the pi0 and the computed constraint *)
		(* USELESS SO FAR 
		DynArray.add pi0_computed pi0; *)
		
		(*------------------------------------------------------------*)
		(* At least checks whether the new constraint is INCLUDED (or equal) into a former one *)
		(*** TODO: check reverse inclusion / merge constraints together / etc. ***)
		let found = ref false in
		let array_index = ref 0 in
		while not !found && !array_index < (DynArray.length !computed_constraints) do
			if verbose_mode_greater Verbose_high then
				print_message Verbose_high ("Comparing new constraint with " ^ (string_of_int (!array_index+1)) ^ "th old constraint.");
			(* Retrieve the i-th constraint *)
			let ith_constraint = DynArray.get !computed_constraints !array_index in
			(* Compare *)
			(*** TODO: perform the inclusion check "new \in old" only in PaTATOR mode! because the situation new \in old will never happen in the mononode, sequential cartography ***)
			if leq_returned_constraint im_result.result ith_constraint then(
				(* Stop *)
				found := true;
				(* Print some information *)
				print_message Verbose_standard "Constraint included in another one previously computed: dropped.";
			)else(
				(* Compare the other way round (if included, just replace) *)
				if leq_returned_constraint ith_constraint im_result.result then(
					(* Replace *)
					DynArray.set !computed_constraints !array_index im_result.result;
					(* Stop *)
					found := true;
					(* Print some information *)
					print_message Verbose_standard "Constraint larger than another one previously computed: replace.";
				); (* if larger *)
			);
			
			
			
			(* Increment *)
			array_index := !array_index + 1;
		done;
		
		(* Only add if not found *)
		if not !found then(
			print_message Verbose_medium "Constraint not found earlier: add.";
			(*** TODO: add to file if options#output_result is enabled ***)
			DynArray.add !computed_constraints im_result.result;
		)else(
			(*** TODO: add a counter or something, for information purpose ***)
			()
		);
		(*------------------------------------------------------------*)
		
		(* Return true only if really added *)
		not (!found)
	
	(* INVALID RESULT *)
	)else(
		print_message Verbose_standard "This constraint is discarded due to premature termination.";
		
		(*** TODO: add 1 to the number of invalid points *)
	
		(* Print the constraint only in verbose mode *)
		if verbose_mode_greater Verbose_low then(
			print_message Verbose_low ("The constraint computed was:");
			print_message Verbose_low (ModelPrinter.string_of_returned_constraint model.variable_names im_result.result);
			if model.correctness_condition <> None then(
				print_message Verbose_medium ("This tile would have been " ^ (StateSpace.string_of_tile_nature im_result.tile_nature) ^ ".");
			);
		);
		(* Return true only if really added *)
		false
	
	)


(*------------------------------------------------------------*)
(** Auxiliary function: finalize the behavioral cartography *)
(*** TODO: split or redefine this function (that both print statistics and export to file BUT does not produce graphics!! ***)
(*------------------------------------------------------------*)
let bc_finalize () =
	(* Retrieve the input options *)
	let options = Input.get_options () in

	let nb_tiles = DynArray.length !computed_constraints in
	(*** TODO: round !!! ***)
	let nb_states = int_of_float ((float_of_int (!nb_states)) /. (float_of_int nb_tiles)) in
	(*** TODO: round !!! ***)
	let nb_transitions = int_of_float ((float_of_int (!nb_transitions)) /. (float_of_int nb_tiles)) in
	
	(*** TODO: round !!! ***)
	let global_time = time_from !start_time in
	(*** TODO: round !!! ***)
(* 	let time_spent_on_BC = global_time -. (!time_spent_on_IM) in *)
	
	(*** TODO: move graphics generation to here ***)
	
	(* Write to file *)
	if options#output_result then(
		write_result_to_file global_time;
	);
	
	(* Print the result *)
	print_message Verbose_standard ("\n**************************************************");
	print_message Verbose_standard (" END OF THE BEHAVIORAL CARTOGRAPHY ALGORITHM");
	print_message Verbose_standard ("Size of V0: " ^ (NumConst.string_of_numconst !nb_points) ^ "");
	print_message Verbose_standard ("Unsuccessful points: " ^ (string_of_int !nb_useless_points) ^ "");
	print_message Verbose_standard ("" ^ (string_of_int nb_tiles) ^ " different constraints were computed.");
	print_message Verbose_standard ("Average number of states        : " ^ (string_of_int nb_states) ^ "");
	print_message Verbose_standard ("Average number of transitions   : " ^ (string_of_int nb_transitions) ^ "");
	print_message Verbose_standard ("Global time spent               : " ^ (string_of_float global_time) ^ " s");
	print_message Verbose_standard ("Time spent on IM                : " ^ (string_of_float (!time_spent_on_IM)) ^ " s");
(* 	print_message Verbose_standard ("Time spent on BC only: " ^ (string_of_float (time_spent_on_BC)) ^ " s"); *)
	print_message Verbose_standard ("Time spent to compute next point: " ^ (string_of_float (counter_next_point#value)) ^ " s");
	print_message Verbose_standard ("**************************************************");
	
	if options#statistics then (
		(* PPL *)
		print_message Verbose_standard "--------------------";
		print_message Verbose_standard "Statistics on PPL";
		print_message Verbose_standard ("--------------------" ^ (LinearConstraint.get_statistics global_time));
	);
	()
		




(*
(* Get the current pi0 in the form of a list (for PaTATOR) *)
let get_current_pi0 () =
(* 	print_message Verbose_high ("Entering get_current_pi0() ..."); *)

	(* Get the model *)
	let model = Input.get_model() in

	(* Retrieve the current pi0 (that must have been initialized before) *)
	let current_pi0 = get_current_pi0_option () in

(* 		print_message Verbose_high ("About to convert..."); *)
(* 		let result =  *)
		List.map (fun parameter_index ->
(* 				print_message Verbose_high ("Convert"); *)
			parameter_index , current_pi0.(parameter_index)) model.parameters
(* 		in *)
(* 		print_message Verbose_high ("Computed result in get_current_pi0() "); *)
(* 		result *)
(* 	end *)*)


(* Get the current pi0 (for PaTATOR) *)
(*** TODO: remove this function ***)
let get_current_pi0 () =
	(* Retrieve the current pi0 (that must have been initialized before) *)
	let current_pi0 = get_current_pi0_option () in
	current_pi0


(* Get the number of unsuccessful points (for PaTATOR) *)
let get_nb_unsuccessful_points () =
	!nb_useless_points



(** Compute an array made of *all* points in V0 (for PaTATOR) *)
let compute_all_pi0 () =
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
	if NumConst.equal !nb_points NumConst.zero then(
		raise (InternalError("The number of points in V0 has not been computed (but it should)."));
	);
	
	(* First check that the number of points can be represented as an int *)
	if not (NumConst.is_int !nb_points) then(
		raise (InternalError("The number of points in V0 is too big to be represented as an int."));
	);
	
	(* Convert to int *)
	let int_nb_points = NumConst.to_int !nb_points in
	
	print_message Verbose_medium ("[Cartography.compute_all_pi0] Computing the initial pi0");
	(* Set the first point *)
	compute_initial_pi0 ();
	print_message Verbose_medium ("[Cartography.compute_all_pi0] Done computing the initial pi0");

	(* Create a array for all the pi0, initially containing a useless object everywhere *)
	let useless_pi0 = new PVal.pval in
	print_message Verbose_medium ("[Cartography.compute_all_pi0] Creating an array of " ^ (string_of_int int_nb_points) ^ " points");
	let all_points = Array.make int_nb_points useless_pi0 in
	
	print_message Verbose_medium ("[Cartography.compute_all_pi0] Retrieving the initial pi0");
	(* Retrieve the initial pi0 (that must have been initialized before) *)
	let initial_pi0 = get_current_pi0_option () in
	print_message Verbose_medium ("[Cartography.compute_all_pi0] Copying pi0 just in case");
	let initial_pi0_copy = initial_pi0#copy() in
	print_message Verbose_medium ("[Cartography.compute_all_pi0] Setting pi0 to the first point");
	(* Fill the first point with a COPY of the initial pi0 *)
	all_points.(0) <- initial_pi0_copy;
	
	print_message Verbose_medium ("[Cartography.compute_all_pi0] Computing the other points");
	print_message Verbose_medium ("[Cartography.compute_all_pi0] Computing the other points");
	(* Fill it for the other points *)
	for pi0_index = 1 to int_nb_points - 1 do
		(* Compute the next pi0 *)
		let more_pi0 = compute_next_sequential_pi0 () in
		(* If no more pi0: problem! *)
		if not more_pi0 then(
			raise (InternalError("No more pi0 before completing the fill the static array of all pi0."));
		);
		(* Get the current pi0 and COPY it! Very important *)
		let current_pi0_copy = (get_current_pi0_option ())#copy() in
		(* Fill the array *)
		all_points.(pi0_index) <- current_pi0_copy;
		
	done;
	
	print_message Verbose_medium ("[Cartography.compute_all_pi0] Done computing the other points");
	
	(* Set the global variable *)
	all_pi0_array := Some all_points;

	(* Set the first pi0 *)
	current_pi0 := Some (all_points.(0));
	all_pi0_array_current := 0;
	
	
(*	(*** BEGIN DEBUG ***)
	(* Print all pi0 *)
	let model = Input.get_model() in
	for pi0_index = 0 to int_nb_points - 1 do
		let pi0_fun = pi0_fun_of_pi0 all_points.(pi0_index) in
			print_message Verbose_standard ((string_of_int pi0_index) ^ ":");
			print_message Verbose_standard (ModelPrinter.string_of_pi0 model pi0_fun);
	done;
(*	raise (InternalError ("bye bye"));*)
	(*** END DEBUG ***)*)
	
	(* Return *)
	()


(** Shuffle the array made of *all* points in V0 (for PaTATOR) *)
let shuffle_all_pi0 () =
	let all_points = get_all_pi0_array_option () in
	(*** NOTE: applied two times, because once is quite deterministic (see warning in the array_shuffle code) ***)
	array_shuffle all_points;
	array_shuffle all_points;
	
(*
	(*** BEGIN DEBUG ***)
	(* Print all pi0 *)
	let model = Input.get_model() in
	for pi0_index = 0 to Array.length all_points - 1 do
		let pi0_fun = pi0_fun_of_pi0 all_points.(pi0_index) in
			print_message Verbose_standard ((string_of_int pi0_index) ^ ":");
			print_message Verbose_standard (ModelPrinter.string_of_pi0 model pi0_fun);
	done;
(* 	raise (InternalError ("bye bye")); *)
	(*** END DEBUG ***)
	*)
	
	(* Return *)
	()


(*
 * Move to the next uncovered pi0 and do not move if the current pi0 is still not covered; update global variable current_pi0 (if necessary); return true if indeed moved
 *)
let move_to_next_uncovered_pi0 () =
  match !current_pi0 with
    | None -> raise (InternalError("Current_pi0 is not defined, " ^
				     "altough it should have at this point."))
    | Some current_pi0 ->
	let found_pi0 = ref false in
	    print_message Verbose_high " [Cartography.move_to_next_uncovered_pi0] check coverage of pi0";
	  test_pi0_uncovered current_pi0 found_pi0;
	  (* If !found_pi0 set to true: means it is NOT covered *)
	  if !found_pi0
	  then (
	    print_message Verbose_medium" [Cartography.move_to_next_uncovered_pi0] pi0 was not covered: do not move";
	    (* Not covered means no move *)
	    false
	  )
	  else(
	    let found = find_next_pi0_cover ()
	    in found
	    )


(*------------------------------------------------------------*)
(** Auxiliary function: return the result of the behavioral cartography *)
(*------------------------------------------------------------*)
let bc_result () =
	(* Return a list of the generated zones *)
	let zones = DynArray.to_list !computed_constraints in
	zones


(*------------------------------------------------------------*)
(** Generate the graphical cartography, and possibly personnalize the file suffix *)
(*------------------------------------------------------------*)
let output_graphical_cartography suffix_option =
	(* Get the model *)
(* 	let model = Input.get_model() in *)
	(* Retrieve the input options *)
	let options = Input.get_options () in
	
	(* Retrieve the results *)
	let zones = bc_result () in
	
	let suffix =
	match suffix_option with
	| Some str -> str
	| None -> "_cart_bc"
	in
	
	(* Render zones in a graphical form *)
	if options#cart then (
		Graphics.cartography zones (options#files_prefix ^ suffix)
	) else (
			print_message Verbose_high "Graphical cartography not asked: graph not generated.";
	)


(*------------------------------------------------------------*)
(** Behavioral cartography algorithm with full coverage of V0 *)
(*------------------------------------------------------------*)
let cover_behavioral_cartography model =
	(* Retrieve the input options *)
	let options = Input.get_options () in

	(* Perform initialization *)
	bc_initialize ();

	(* Compute the first point pi0 *)
	compute_initial_pi0 ();
	
	let more_pi0 = ref true in
	time_limit_reached := false;
	
	(* Iterate on all the possible pi0 *)
	while !more_pi0 && not !time_limit_reached do

(* 		let pi0_fun = pi0_fun_of_current_pi0 () in *)
		let pi0 = get_current_pi0_option() in

		(* Print some messages *)
		(*** HACK: only print if non-distributed ***)
		if options#distribution_mode = Non_distributed then(
			print_message Verbose_standard ("\n**************************************************");
			print_message Verbose_standard ("BEHAVIORAL CARTOGRAPHY ALGORITHM: " ^ (string_of_int !current_iteration) ^ "");
			print_message Verbose_standard ("Considering the following pi" ^ (string_of_int !current_iteration));
			print_message Verbose_standard (ModelPrinter.string_of_pi0 model pi0);
		);
		
		(* Prevent the verbose messages (except in verbose medium, high or total) *)
		if not (verbose_mode_greater Verbose_medium) then
			set_verbose_mode Verbose_mute;
		
		(* Set the new pi0 *)
		Input.set_pi0 (pi0);
		
		(* Call the inverse method *)
		let im_result, reachability_graph = Reachability.inverse_method_gen model !init_state in
		
		(* Get the verbose mode back *)
		set_verbose_mode !global_verbose_mode;
		
		(* Process the result by IM *)
		let _ = bc_process_im_result im_result in ();
		
		(* Generate the dot graph (will not be performed if options are not suitable) *)
		(*** TODO: move inside inverse_method_gen ***)
		(*** HACK: have to decrease current_iteration because it was increased in bc_process_im_result ... ***)
		let radical = options#files_prefix ^ "_" ^ (string_of_int (!current_iteration-1)) in
			Graphics.generate_graph reachability_graph radical;

		(* Compute the next pi0 (note that current_pi0 is directly modified by the function!) and return flags for more pi0 and co *)
		let found_pi0 , _ = find_next_pi0 (Some im_result.tile_nature) in
		
		(* Update the found pi0 flag *)
		more_pi0 := found_pi0;

	done; (* while more pi0 *)

	(* Print info if premature termination *)
	if !time_limit_reached && !more_pi0 then (
		(*** WARNING : what about other limits?! (iterations, etc.?) ***)
		match options#time_limit with
			| None -> ()
			| Some limit -> if (get_time()) > (float_of_int limit) then print_warning (
				"The time limit (" ^ (string_of_int limit) ^ " second" ^ (s_of_int limit) ^ ") has been reached. The behavioral cartography algorithm now stops, although the cartography has not been covered yet."
			);
	);
	
	(* Process the finalization *)
	bc_finalize ();
	
	(* Generate graphical cartography *)
	output_graphical_cartography None;
	
	(* The end *)
	()



(*------------------------------------------------------------*)
(** Behavioral cartography algorithm with random selection of a pi0 *)
(*** WARNING: not tested for a LONG time ***)
(*** TODO: merge with cover_behavioral_cartography ***)
(*------------------------------------------------------------*)
let random_behavioral_cartography model nb =



	print_warning("The random behavioral cartography has not been used (nor tested) for a while, and may contain bugs, or lead to inconsistent results.");

	
	
	(* Retrieve the input options *)
	let options = Input.get_options () in

	(* Array for the pi0 *)
	(*** NOTE/TO OPTIMIZE: a bit stupid to compute a *random* pi0; one could just compute an all-0 one ***)
	let random_pi0 = one_random_pi0 () in
	(*** TO OPTIMIZE: why create such a big array?! ***)
	let pi0_computed = Array.make nb random_pi0 in

	(* Array for the results *)
	(*** TO OPTIMIZE: why create such a big array?! ***)
	let results = Array.make nb (Convex_constraint (LinearConstraint.p_false_constraint (), Unknown)) in
	
	(* Index of the iterations where we really found different constraints *)
	let interesting_interations = ref [] in
	
	(* Verbose mode *)
	let global_verbose_mode = get_verbose_mode() in
	
	(* Prevent the printing of messages in algorithm Inverse Method *)
	let cut_messages = not (verbose_mode_greater Verbose_low) in

	(* Compute the initial state *)
	let init_state = get_initial_state_or_abort model in

	(* Initial constraint of the model *)
	let _, init_constraint = init_state in
	(* Hide non parameters *)
	let init_constraint = LinearConstraint.px_hide_nonparameters_and_collapse init_constraint in

	(* Current iteration *)
	let i = ref 1 in
	time_limit_reached := false;
	
	while !i <= nb && not !time_limit_reached do
		let pi0 = one_random_pi0 () in

		(* Print messages *)
		print_message Verbose_standard ("\n**************************************************");
		print_message Verbose_standard ("RANDOM BEHAVIORAL CARTOGRAPHY ALGORITHM: " ^ (string_of_int !i) ^ " / " ^ (string_of_int nb) ^ "");
		
		(* First check that it was not computed before *)
		let already_computed, index = 
			try true, index_of pi0 pi0_computed
			with Not_found -> false, 0
		in

		(* If already computed: message *)
		if already_computed then (
			print_message Verbose_standard ("This pi" ^ (string_of_int !i) ^ " is equal to pi" ^ (string_of_int (index + 1)) ^ ".");
		(* Only consider new pi0 *)
		) else (
			(* Convert the pi0 to a functional representation *)
(* 			let pi0_functional = fun parameter -> pi0.(parameter) in *)

			(* Check that it does not belong to any constraint *)
			if array_exists (pi0_in_returned_constraint pi0) results then (
				print_message Verbose_standard ("This pi" ^ (string_of_int !i) ^ " is already included in a constraint.");
				print_message Verbose_standard (ModelPrinter.string_of_pi0 model pi0);
				
			(* Check that it satisfies the initial constraint *)
			) else if not (LinearConstraint.is_pi0_compatible pi0#get_value init_constraint) then (
				print_message Verbose_standard ("This pi" ^ (string_of_int !i) ^ " does not satisfy the initial constraint of the model.");
				print_message Verbose_standard (ModelPrinter.string_of_pi0 model pi0);
				
			) else (
				(* Consider from here a brand new and correct pi0 *)
				print_message Verbose_standard ("Considering pi" ^ (string_of_int !i) ^ " :=");
				print_message Verbose_standard (ModelPrinter.string_of_pi0 model pi0);

				(* Prevent the messages if needed *)
				if cut_messages then (
					set_verbose_mode Verbose_mute;
				);
				
				(* Set the new pi0 *)
				Input.set_pi0 pi0;
			
				(* Call the inverse method *)
				let (*returned_constraint, graph, (*tile_nature*)_, (*deterministic*)_, nb_iterations, total_time*) im_result, reachability_graph = Reachability.inverse_method_gen model init_state in
				(* Get the verbose mode back *)
				set_verbose_mode global_verbose_mode;

				(* Retrieve some info *)
				let current_nb_states = StateSpace.nb_states reachability_graph in
				let current_nb_transitions = StateSpace.nb_transitions reachability_graph in
				
				print_message Verbose_standard (
					"\nK" ^ (string_of_int !i) ^ " computed using algorithm InverseMethod after "
					^ (string_of_int im_result.nb_iterations) ^ " iteration" ^ (s_of_int im_result.nb_iterations) ^ ""
					^ " in " ^ (string_of_seconds im_result.total_time) ^ ": "
					^ (string_of_int current_nb_states) ^ " reachable state" ^ (s_of_int current_nb_states)
					^ " with "
					^ (string_of_int current_nb_transitions) ^ " transition" ^ (s_of_int current_nb_transitions) ^ ".");

				(* Add the pi0 *)
				pi0_computed.(!i - 1) <- pi0;

				(* Generate the dot graph *)
				(*** TODO: move to inverse_method_gen ***)
				let radical = options#files_prefix ^ "_" ^ (string_of_int !i) in
				Graphics.generate_graph reachability_graph radical;
				(* Add the index to the interesting list *)
				interesting_interations := !i :: !interesting_interations;

				(* compute k0 *)
(*				let k0 =  if options#dynamic || options#union then ( returned_constraint )
				else  match returned_constraint with 
					| Convex_constraint _ -> Convex_constraint (StateSpace.compute_k0_destructive model graph)
					| _ -> print_error ("Internal error when getting the result of post_star in cover: 'options#dynamic' is activated but the constraint returned is not convex (type 'Convex_constraint')."); abort_program (); exit(0)

				in*)
				let k0 = im_result.result in
												
				(* Print the constraint *)
				print_message Verbose_low ("Constraint K0 computed:");
				print_message Verbose_standard (ModelPrinter.string_of_returned_constraint model.variable_names k0);

				(* Add the result *)
				results.(!i - 1) <- k0;
			);
		);
		(* Stop if the time limit has been reached *)
		let _ =
		match options#time_limit with
			| None -> ()
			| Some limit -> if (get_time()) > (float_of_int limit) then time_limit_reached := true;
		in

		(* Increment the iteration *)
		i := !i + 1;
	done;

	if !time_limit_reached && !i <= nb then (
		match options#time_limit with
			| None -> ()
			| Some limit -> if (get_time()) > (float_of_int limit) then print_warning (
				"The time limit (" ^ (string_of_int limit) ^ " second" ^ (s_of_int limit) ^ ") has been reached. The behavioral cartography algorithm now stops, although the cartography has not been covered yet."
			);
	);
	
	(* Print the result *)
	print_message Verbose_standard ("\n**************************************************");
	print_message Verbose_standard ("" ^ (string_of_int (List.length !interesting_interations)) ^ " different constraint" ^ (s_of_int (List.length !interesting_interations)) ^ " were computed at the following iteration" ^ (s_of_int (List.length !interesting_interations)) ^ " :");
	print_message Verbose_standard (string_of_list_of_string_with_sep ", " (List.map string_of_int (List.rev !interesting_interations)));
	print_message Verbose_standard ("**************************************************");

	(* Return a list of generated zones *)
	let zones = List.map (fun index -> results.(index)) !interesting_interations in
	
	(* Render zones in a graphical form *)
	(*** WARNING: duplicate code (cannot use output_graphical_cartography due to the different representation of zones here... ***)
	if options#cart then (
		Graphics.cartography zones (options#files_prefix ^ "_cart_bc_random")
	) else (
			print_message Verbose_high "Graphical cartography not asked: graph not generated.";
	)





(*
 *  functions used by the coordinator in the distributed-unsupervised
 *  cartography (the coordinator maintaints a list of points instead of
 *  a single one
 *)
let next_unproc = ref []
;;

let next_unproc_len = ref 0
;;

let next_unproc_max_size = ref 0
;;

let more_pi0 = ref false
;;

let pr = print_message Verbose_standard
;;

(* return point next to pi0.  returns (Some point) is pi0 is not the
   last point, or None if pi0 if it is *)
   (*** WARNING!!! (added by EA, 2014/10/01); this code seems to be duplicated !!! ***)
let next_pi0 (pi0 : PVal.pval) =
	let options = Input.get_options () in
	let res = pi0#copy() in
	let found = ref true in
	let current_dimension = ref 0 in
	let continue = ref true in
	while !continue do
		let current_dimension_incremented =
			NumConst.add (res#get_value !current_dimension) options#step
		in
		if current_dimension_incremented <= !max_bounds.(!current_dimension)
		then (
			res#set_value (!current_dimension) current_dimension_incremented; (*(res.(!current_dimension) <- current_dimension_incremented;*)
			for i = 0 to !current_dimension - 1 do
				res#set_value  i !min_bounds.(i);
			done;
			continue := false
		) else (
			current_dimension := !current_dimension + 1;
			if !current_dimension >= !nb_dimensions
			then (
				found := false;
				continue := false
			)
		)
	done;
	if !found then Some res else None
;;

let constraint_list_fill last =
  let rec loop = function
      (0, _) -> ()
    | (n, last) ->
	match next_pi0 last
	with None    -> more_pi0 := false
	  |  Some pt -> 
	       let uncovered = ref false in
		 test_pi0_uncovered pt uncovered;
		 if not (!uncovered)
		 then loop (n, pt)
		 else (next_unproc := pt :: (!next_unproc);
		       next_unproc_len := !next_unproc_len + 1;
		       loop (n - 1, pt))
  in
    if (not (!more_pi0)) || (!next_unproc_len) >= (!next_unproc_max_size)
    then ()
    else loop ((!next_unproc_max_size) - (!next_unproc_len), last)
;;

let constraint_list_init size =
  next_unproc := [];
  more_pi0 := true;
  next_unproc_max_size := size;
  compute_initial_pi0 ();
  let first_pi0 = (get_current_pi0_option())#copy() in 
  (*match !current_pi0 with
      None -> raise (InternalError
		       "init_unprocessed_pi0_list: no first point found")
    | Some first_pi0 -> Array.copy (first_pi0)
  in*)
    if size <= 0
    then raise (InternalError
		  "init_unprocessed_pi0_list: called with negative size")
    else (next_unproc_len := 1;
	  next_unproc := [ first_pi0 ];
	  constraint_list_fill first_pi0)
;;

let constraint_list_update im_res =
  match !next_unproc with
    | [] -> ()
    | last :: _ ->    
	let check_point (pt:PVal.pval) =
	  not (pi0_in_returned_constraint pt(*(Array.get pt)*) im_res.result)
	in
	  next_unproc := List.filter check_point (!next_unproc);
	  next_unproc_len := List.length (!next_unproc);
	  constraint_list_fill last
;;

let constraint_list_random () =
  if !next_unproc_len = 0
  then None
  else
(*       let model = Input.get_model () in *)
      let n = Random.int (!next_unproc_len) in
      let a = List.nth (!next_unproc) n in
	Some a(*(List.map (fun idx -> idx, a.(idx)) model.parameters)*)
;;

let constraint_list_empty () = !next_unproc_len = 0
;;
