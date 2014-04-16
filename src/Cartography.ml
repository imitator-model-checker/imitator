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
 * Last modified: 2014/04/16
 *
 ****************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)

open Global
open AbstractModel
open StateSpace
open Reachability






(************************************************************)
(* Types *)
(************************************************************)

(*** BADPROG ***)
(* type current_pi0 = NumConst.t array *)


(************************************************************)
(* Global variable used for BC *)
(************************************************************)

(*** NOTE: all this could be turned in a structure, or a .mli file ***)


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(* Constants *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(* Debug mode *)
let global_debug_mode = ref (Debug_standard)

(* Record start time to compute the time spent only on calling IM *)
let start_time = ref (Unix.gettimeofday())

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

(* Time counter for recording the globl time spent on BC *)
let time_spent_on_IM = ref 0.

(* Counts the points actually member of an existing constraint (hence useless) for information purpose *)
let nb_useless_points = ref 0

(* Compute the initial state (TOTALLY RANDOM VALUE) *)
let init_state = ref (Automaton.make_location [] [], LinearConstraint.px_true_constraint())

(* Initial constraint of the model *)
let init_constraint = ref (LinearConstraint.p_true_constraint())

(* (Dynamic) Array for the results *)
let computed_constraints = ref (DynArray.create())

(* Compute some variables for the border cartography only *)
(* Current_intervals_min and Current_intervals_max represent, for each dimension, the interval (multiple of step) in which the points have not been classified as good or bad *)
let current_intervals_min = ref (Array.make 0 NumConst.zero)
let current_intervals_max = ref (Array.make 0 NumConst.zero)

let current_pi0 = ref None


(************************************************************)
(* Functions on tile nature *)
(************************************************************)

(* let string_of_array_pi0 pi0_array = 
	(* Retrieve the program *)
	let program = Input.get_program () in
	(* Convert to functional *)
	let pi0_functional = fun parameter -> pi0_array.(parameter) in
	(* Convert to string *)
	ModelPrinter.string_of_pi0 program pi0_functional
	*)

(*** TODO: move this translation somewhere else ***)
(*** WARNING: code duplicated ***)
let string_of_tile_nature = function
	| Good -> "good"
	| Bad -> "bad"
	| _ -> raise (InternalError ("Tile nature should be good or bad only, so far "))


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
(* 	print_message Debug_standard ("(" ^ (NumConst.string_of_numconst number) ^ " - " ^ (NumConst.string_of_numconst base_number) ^ ") / " ^ (NumConst.string_of_numconst step) ^ " = " ^ (NumConst.string_of_numconst number_minus_base_divided) ^ ""); *)
	
	(* Print some information again *)
(* 	print_message Debug_standard ("  Is it positive? " ^ (string_of_bool (NumConst.ge number_minus_base_divided NumConst.zero))); *)
(* 	print_message Debug_standard ("  Is it an integer? " ^ (string_of_bool (NumConst.is_integer number_minus_base_divided))); *)
	
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
(* 				print_message Debug_standard ((NumConst.string_of_numconst average) ^ " is multiple of " ^ (NumConst.string_of_numconst step) ^ "."); *)
		average
		
		(* Otherwise try below *)
		) else(
(* 				print_message Debug_standard ((NumConst.string_of_numconst average) ^ " is NOT multiple of " ^ (NumConst.string_of_numconst step) ^ ""); *)
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
	| Convex_constraint (k,_) -> LinearConstraint.is_pi0_compatible pi0 k
	(** Disjunction of constraints *)
	| Union_of_constraints (k_list , _) -> List.exists (LinearConstraint.is_pi0_compatible pi0) k_list
	| NNCConstraint _ -> raise (InternalError ("NNCCs are not available everywhere yet."))




(************************************************************)
(* Pi0 function (to move to "next pi0 functions" section) *)
(************************************************************)
(*------------------------------------------------------------*)
(* Generate a random pi0 in a given interval for each parameter (array view!) *)
(*------------------------------------------------------------*)
let random_pi0 model pi0 =
	raise (InternalError "not implemented")
(*	(* Create the pi0 *)
	let random_pi0 = Array.make model.nb_parameters NumConst.zero in
	(* Fill it *)
	for i = 0 to model.nb_parameters - 1 do
		let a, b = pi0.(i) in
		(* Generate a random value in the interval *)
		Random.self_init();
		let random_value = Random.int (b - a + 1) + a in
		(* Debug *)
(* 		print_message Debug_medium ("Generating randomly value '" ^ (string_of_int random_value) ^ "' for parameter '" ^ (model.variable_names i) ^ "'."); *)
		(* Convert to a num *)
		random_pi0.(i) <- NumConst.numconst_of_int random_value;
	done;
	(* Return the result as an array *)
	random_pi0*)


(************************************************************)
(* Initial pi0 functions *)
(************************************************************)
(*------------------------------------------------------------*)
(* First point pi0 *)
(*** WARNING / TODO: technically, we should check that pi0 models the initial constraint ***)
(*------------------------------------------------------------*)
let compute_initial_pi0 () =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	
	(*** WARNING: should be sure that 0 is the first parameter dimension!! ***)
	let first_dimension = 0 in
	
	let step = options#step in
	
	(* Case by case *)
	match options#imitator_mode with
	
	(* Instantiate with the lower bounds *)
	| Cover_cartography ->
		current_pi0 := Some (Array.copy !min_bounds)

		(* Instantiate with the point in the middle of V0 *)
	| Border_cartography ->
		(* Start with the min bounds everywhere *)
		let initial_pi0 = (*Array.create (Array.length min_bounds) NumConst.zero*) Array.copy !min_bounds in
(* 		for i = 0 to (Array.length initial_pi0) - 1 do *)
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
		
	| _ -> raise (InternalError("In function 'initial_pi0', the mode should be a cover / border cartography only."))


(************************************************************)
(* Next pi0 functions *)
(************************************************************)

(*------------------------------------------------------------*)
(** Compute the next pi0 and directly modify the variable 'current_pi0' (standard BC) *)
(*------------------------------------------------------------*)
let find_next_pi0_cover () =
	(* Get the model *)
	let model = Input.get_model() in
	(* Retrieve the input options *)
	let options = Input.get_options () in

	match !current_pi0 with
	(* First initialization *)
	| None -> 
		raise (InternalError("Current_pi0 is not defined, altough it should have at this point."))
(*		current_pi0 := Some (compute_initial_pi0 !min_bounds !max_bounds 0) (*** WARNING: should be sure that 0 is the first parameter dimension ***)
		;
		(* Return *)
		true, false, 0*)
	
	(* Next point *)
	| Some current_pi0 -> (
		
		(* Counts the points actually member of an existing constraint (hence useless) for information purpose *)
		let nb_useless_points = ref 0 in
		
		(* Are there still possible points *)
		let more_pi0 = ref true in
		(* Did we find a suitable pi0 *)
		let found_pi0 = ref false in
		(* Did we reach the time limit *)
		let time_limit_reached = ref false in

		while !more_pi0 && not !time_limit_reached && not !found_pi0 do
			
			(* 1) Compute the next pi0 (if any left) *)
			
			(* Start with the first dimension *)
			let current_dimension = ref 0 in (** WARNING: should be sure that 0 is the first parameter dimension *)
			(* The current dimension is not yet the maximum *)
			let not_is_max = ref true in
			
			while !not_is_max do
				(* Try to increment the local dimension *)
				let current_dimension_incremented = NumConst.add current_pi0.(!current_dimension) options#step in
				if current_dimension_incremented <= !max_bounds.(!current_dimension) then (
					(* Increment this dimension *)
					current_pi0.(!current_dimension) <- current_dimension_incremented;
					(* Reset the smaller dimensions to the low bound *)
					for i = 0 to !current_dimension - 1 do
						current_pi0.(i) <- !min_bounds.(i);
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
		
			(* 2) Check that this pi0 is new *)
			
			if !more_pi0 then(

				(* Convert the current pi0 to functional representation *)
				let pi0 = fun parameter -> current_pi0.(parameter) in
				
				(* Check that the current pi0 does not belong to any constraint *)
				if dynArray_exists (pi0_in_returned_constraint pi0) !computed_constraints then (
					(* Update the number of unsuccessful points *)
					nb_useless_points := !nb_useless_points + 1;
					if debug_mode_greater Debug_medium then (
						print_message Debug_medium "The following pi0 is already included in a constraint.";
						print_message Debug_medium (ModelPrinter.string_of_pi0 model pi0);
					);
					(** TODO: could be optimized by finding the nearest multiple of tile next to the border, and directly switching to that one *)
					
				(* Check that it satisfies the initial constraint *)
				) else if not (LinearConstraint.is_pi0_compatible pi0 !init_constraint) then (
					(* Update the number of unsuccessful points *)
					nb_useless_points := !nb_useless_points + 1;
					if debug_mode_greater Debug_medium then (
						print_message Debug_medium "The following pi0 does not satisfy the initial constraint of the model.";
						print_message Debug_medium (ModelPrinter.string_of_pi0 model pi0);
					);
				(* If both checks passed, then pi0 found *)
				)else(
					found_pi0 := true;
				);
				
				(* If pi0 still not found, check time limit *)
				if not !found_pi0 then(
					(* Stop if the time limit has been reached *)
					match options#time_limit with
						| None -> ()
						| Some limit -> if (get_time()) > (float_of_int limit) then time_limit_reached := true;
				);
			); (*if more pi0 *)
		done; (* while more pi0 and so on *)
		
		(* Return info (note that current_pi0 has ALREADY been updated if a suitable pi0 was found !) *)
		!found_pi0 , !time_limit_reached , !nb_useless_points
	)


(*------------------------------------------------------------*)
(** Compute the next pi0 and directly modify the variable 'current_pi0' (border BC) *)
(*------------------------------------------------------------*)
let find_next_pi0_border latest_nature =
	find_next_pi0_cover () (*
	(* Print some information *)
	print_message Debug_standard "Entering function 'find_next_pi0_border'.";
	
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
		
		print_message Debug_standard ("Starting from dimension " ^ (string_of_int !current_dimension) ^ ".");
		
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

				print_message Debug_standard ("Looking for a multiple of step " ^ (NumConst.string_of_numconst step) ^ " from " ^ (NumConst.string_of_numconst min_bounds.(!current_dimension)) ^ " in [" ^ (NumConst.string_of_numconst current_min) ^ ", " ^ (NumConst.string_of_numconst current_max) ^ "] in dimension " ^ (string_of_int !current_dimension) ^ ".");
		
				(* Can raise Not_found, in which case we exit the loop *)
				let middle_point = find_multiple_in_between_and_from min_bounds.(!current_dimension) current_min current_max step in
				
				(* Print some information *)
				print_message Debug_standard ("Found " ^ (NumConst.string_of_numconst middle_point) ^ ".");
		
				(* Update our current and tentative pi0 *)
				current_pi0.(!current_dimension) <- middle_point;

				(* Convert the current pi0 to functional representation *)
				let pi0 = fun parameter -> current_pi0.(parameter) in
				
				(* Print some information *)
				print_message Debug_standard ("Constructing a tentative point: " ^ (ModelPrinter.string_of_pi0 model pi0) ^ ".");
		
				print_message Debug_standard ("Checking whether this point belongs to a tile.");

				(* Check that the current pi0 does not belong to any constraint *)
				if dynArray_exists (fun returned_constraint ->
					(* If the point belongs to a tile *)
					if pi0_in_returned_constraint pi0 returned_constraint then (
						(* Print some information *)
						print_message Debug_standard ("  Pi0 belongs to this tile.");

						(* Get the tile nature *)
						let tile_nature = tile_nature_of_returned_constraint returned_constraint in
						
						(* Print some information *)
						print_message Debug_standard ("    This tile is " ^ (string_of_tile_nature tile_nature) ^ ".");
						
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
						print_message Debug_standard ("    Reducing the interval to [" ^ (NumConst.string_of_numconst current_intervals_min.(!current_dimension)) ^ ", " ^ (NumConst.string_of_numconst current_intervals_max.(!current_dimension)) ^ "] in dimension " ^ (string_of_int !current_dimension) ^ ".");
						
						(* Return true because we found a tile *)
						true
					)else(
						(* This tile has not yet been found *)
						print_message Debug_standard ("  Pi0 does not belong to this tile.");
						false
					)
				) computed_constraints then (
					(* Update the number of unsuccessful points *)
					nb_useless_points := !nb_useless_points + 1;
					if debug_mode_greater Debug_medium then (
						print_message Debug_medium "The following pi0 is already included in a constraint.";
						print_message Debug_medium (ModelPrinter.string_of_pi0 model pi0);
					);
					
				) else (
					(* Found a point not covered by any constraint! *)
					(** WARNING: should test compatibility with initial constraint (and might be quite tricky, now) *)
					raise Found
				);

			done; (* end while *)
			with Not_found -> (); (* at this point, there is no multiple of step between min and max for this dimension *)
			(* Print some information *)
			print_message Debug_standard ("No multiple found in the interval.");
			
			(* Switch to the next *)
			current_dimension := !current_dimension + 1;

			(* Print some information *)
			print_message Debug_standard ("Now increasing to dimension " ^ (string_of_int !current_dimension) ^ ".");
		
			(* If last dimension: the end! *)
			if !current_dimension >= nb_dimensions then(
	(* 			more_pi0 := false; *)
				not_is_max := false;
				
				(* Print some information *)
				print_message Debug_standard ("Maximum dimension " ^ (string_of_int nb_dimensions) ^ " has been reached.");
		
			)else(
				(* Reset the intervals of the smaller dimensions to the initial min / max bounds *)
				for i = 0 to !current_dimension - 1 do
					(* On ne touche pas pi0 *)
					(*current_pi0.(i) <- min_bounds.(i);*)
					
					(* Mais on peut conserver le max *)
					current_intervals_min.(i) <- min_bounds.(i);
					
					(* Print some information *)
					print_message Debug_standard ("  New interval for dimension " ^ (string_of_int i) ^ ": [" ^ (NumConst.string_of_numconst current_intervals_min.(i)) ^ ", " ^ (NumConst.string_of_numconst current_intervals_max.(i)) ^ "].");
		
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
let find_next_pi0 tile_nature_option =
	(* Retrieve the input options *)
	let options = Input.get_options () in

	let found_pi0 , time_limit_reached , new_nb_useless_points =
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

	| _ -> raise (InternalError("In function 'cover_behavioral_cartography', the mode should be a cover / border cartography only."))
	in
	(* Update the number of useless points *)
	nb_useless_points := !nb_useless_points + new_nb_useless_points;
	
	(* Return *)
	found_pi0 , time_limit_reached




(************************************************************)
(* BEHAVIORAL CARTOGRAPHY ALGORITHM functions *)
(************************************************************)
	
	
(*** TODO: merge both algorithms into cover_behavioral_cartography !!! ***)


(*------------------------------------------------------------*)
(** Auxiliary function: initialize the behavioral cartography *)
(*------------------------------------------------------------*)
let bc_initialize () =
	(* Get the model *)
	let model = Input.get_model() in
	(* Get the v0 *)
	let v0 = Input.get_v0() in
	(* Retrieve the input options *)
	let options = Input.get_options () in

	(* Print some information *)
	print_message Debug_medium ("Starting preprocessing for the behavioral cartography");

	(* Time counter for recording the globl time spent on BC *)
	time_spent_on_IM := 0.;
	(* Record start time to compute the time spent only on calling IM *)
	start_time := Unix.gettimeofday();

	(* Number of dimensions in the system *)
	nb_dimensions := Array.length v0;
	
	(* Print some information *)
	print_message Debug_medium ("Number of dimensions: " ^ (string_of_int !nb_dimensions));

	(* Check that the cartography is not applied to 0 dimension! *)
	if !nb_dimensions = 0 then(
		print_error "The cartography has 0 dimension in V0, and cannot be applied.";
		abort_program();
	);
	
	(* Min & max bounds for the parameters *)
	min_bounds := Array.map (fun (low, high) -> low) v0;
	max_bounds := Array.map (fun (low, high) -> high) v0;
	
	(* Compute the (actually slightly approximate) number of points in V0 (for information purpose) *)
	nb_points := Array.fold_left (fun current_number (low, high) ->
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
	) NumConst.one v0;
	
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
	current_iteration := 0;
	(* Sum of number of states (for information purpose) *)
	nb_states := 0;
	(* Sum of number of transitions (for information purpose) *)
	nb_transitions := 0;

	(* Debug mode *)
	global_debug_mode := get_debug_mode();
	
	(*** TODO : check that initial pi0 is suitable!! (could be incompatible with initial constraint) ***)
	
	(* Print *)
	print_message Debug_standard ("\n**************************************************");
	print_message Debug_standard (" START THE BEHAVIORAL CARTOGRAPHY ALGORITHM");
	print_message Debug_standard ("**************************************************");
	print_message Debug_standard (" Parametric rectangle V0: ");
	print_message Debug_standard (ModelPrinter.string_of_v0 model v0);
	print_message Debug_standard (" Number of points inside V0: " ^ (NumConst.string_of_numconst !nb_points));
	()






(*------------------------------------------------------------*)
(** Auxiliary function: process the result of IM *)
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
	
	(* Print message *)
	print_message Debug_standard (
		"\nK" ^ (string_of_int (!current_iteration)) ^ " computed using algorithm InverseMethod after "
		^ (string_of_int im_result.nb_iterations) ^ " iteration" ^ (s_of_int im_result.nb_iterations) ^ ""
		^ " in " ^ (string_of_seconds im_result.total_time) ^ ": "
		^ (string_of_int im_result.nb_states) ^ " state" ^ (s_of_int im_result.nb_states)
		^ " with "
		^ (string_of_int im_result.nb_transitions) ^ " transition" ^ (s_of_int im_result.nb_transitions) ^ " explored.");
	
	(* Print the constraint *)
	
	(*** NOTE: it may actually be more clever to check the tile nature from the graph, especially if we go for more complicated properties!! ***)
	
	
(* 			let bad_string = if StateSpace.is_bad model graph then "BAD." else "GOOD." in *)
	print_message Debug_low ("Constraint K0 computed:");
	print_message Debug_standard (ModelPrinter.string_of_returned_constraint model.variable_names im_result.result);
	if model.correctness_condition <> None then(
		print_message Debug_standard ("This tile is " ^ (string_of_tile_nature im_result.tile_nature) ^ ".");
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
			print_message Debug_standard ("Constraint after enlarging:" ^ (if !nb_enlargements > 1 then " ("  ^ (string_of_int !nb_enlargements) ^ " enlargements)" else ""));
			print_message Debug_standard (ModelPrinter.string_of_returned_constraint model.variable_names im_result.result);
		);

	| _ -> raise (InternalError("In function 'cover_behavioral_cartography', the mode should be a cover / border cartography only."))
	end; (* end process constraint *)
	
	
	(* Add the pi0 and the computed constraint *)
	(* USELESS SO FAR 
	DynArray.add pi0_computed pi0; *)
	
	(*** WARNING: so stupid here!! Better flatten the structure! ***)
	DynArray.add !computed_constraints im_result.result;
	
	()


(*------------------------------------------------------------*)
(** Auxiliary function: finalize the behavioral cartography *)
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
	let time_spent_on_BC = global_time -. (!time_spent_on_IM) in
	
	(* Print the result *)
	print_message Debug_standard ("\n**************************************************");
	print_message Debug_standard (" END OF THE BEHAVIORAL CARTOGRAPHY ALGORITHM");
	print_message Debug_standard ("Size of V0: " ^ (NumConst.string_of_numconst !nb_points) ^ "");
	print_message Debug_standard ("Unsuccessful points: " ^ (string_of_int !nb_useless_points) ^ "");
	print_message Debug_standard ("" ^ (string_of_int nb_tiles) ^ " different constraints were computed.");
	print_message Debug_standard ("Average number of states     : " ^ (string_of_int nb_states) ^ "");
	print_message Debug_standard ("Average number of transitions: " ^ (string_of_int nb_transitions) ^ "");
	print_message Debug_standard ("Global time spent    : " ^ (string_of_float global_time) ^ " s");
	print_message Debug_standard ("Time spent on IM     : " ^ (string_of_float (!time_spent_on_IM)) ^ " s");
	print_message Debug_standard ("Time spent on BC only: " ^ (string_of_float (time_spent_on_BC)) ^ " s");
	print_message Debug_standard ("**************************************************");
	
	if options#statistics then (
		(* PPL *)
		print_message Debug_standard "--------------------";
		print_message Debug_standard "Statistics on PPL";
		print_message Debug_standard ("--------------------" ^ (LinearConstraint.get_statistics ()));
	);
	()
		

(*------------------------------------------------------------*)
(** Auxiliary function: return the result of the behavioral cartography *)
(*------------------------------------------------------------*)
let bc_result () =
	(* Return a list of the generated zones *)
	let zones = DynArray.to_list !computed_constraints in
	zones



(* Convert the array into a functional representation *)
let create_pi0_fun () =
	match !current_pi0 with
	| None -> raise (InternalError("Functional pi0 called before its initialization."))
	| Some current_pi0 ->
		fun parameter_index -> current_pi0.(parameter_index)
	

(* Get the current pi0 in the form of a list (for PaTATOR) *)
let get_current_pi0 () =
	(* Get the model *)
	let model = Input.get_model() in
	match !current_pi0 with
	| None -> raise (InternalError("Current pi0 called before its initialization."))
	| Some current_pi0 ->
		List.map (fun parameter_index -> parameter_index , current_pi0.(parameter_index)) model.parameters



(*------------------------------------------------------------*)
(** Behavioral cartography algorithm with full coverage of V0 *)
(*------------------------------------------------------------*)
let cover_behavioral_cartography model v0 =
	(* Retrieve the input options *)
	let options = Input.get_options () in

	(* Perform initialization *)
	bc_initialize ();

	(* Compute the first point pi0 *)
	compute_initial_pi0 ();
	
	(* Iterate on all the possible pi0 *)
	let more_pi0 = ref true in
	let limit_reached = ref false in
	
	while !more_pi0 && not !limit_reached do

		(* Iterate *)
		current_iteration := !current_iteration + 1;
		
		(*** WARNING : duplicate operation (quite cheap anyway) ***)
		let pi0_fun = create_pi0_fun () in

		(* Debug messages *)
		print_message Debug_standard ("\n**************************************************");
		print_message Debug_standard ("BEHAVIORAL CARTOGRAPHY ALGORITHM: " ^ (string_of_int !current_iteration) ^ "");
		print_message Debug_standard ("Considering the following pi" ^ (string_of_int !current_iteration));
		print_message Debug_standard (ModelPrinter.string_of_pi0 model pi0_fun);
		
		(* Prevent the debug messages (except in debug medium, high or total) *)
		if not (debug_mode_greater Debug_medium) then
			set_debug_mode Debug_nodebug;
		
		(* Set the new pi0 *)
		Input.set_pi0 (pi0_fun);
		
		(* Call the inverse method *)
		let im_result, reachability_graph = Reachability.inverse_method_gen model !init_state in
		
		(* Get the debug mode back *)
		set_debug_mode !global_debug_mode;
		
		(* Process the result by IM *)
		bc_process_im_result im_result;
		
		(* Generate the dot graph (will not be performed if options are not suitable) *)
		(*** TODO: move inside inverse_method_gen ***)
		let radical = options#files_prefix ^ "_" ^ (string_of_int !current_iteration) in
			Graphics.generate_graph model reachability_graph radical;

		(* Compute the next pi0 (note that current_pi0 is directly modified by the function!) and return flags for more pi0 and co *)
		let found_pi0 , time_limit_reached = find_next_pi0 (Some im_result.tile_nature) in
		
		(* Update the time limit *)
		limit_reached := time_limit_reached;
		(* Update the found pi0 flag *)
		more_pi0 := found_pi0;

	done; (* while more pi0 *)

	(* Print info if premature termination *)
	if !limit_reached && !more_pi0 then (
		(*** WARNING : what about other limits?! (iterations, etc.?) ***)
		match options#time_limit with
			| None -> ()
			| Some limit -> if (get_time()) > (float_of_int limit) then print_warning (
				"The time limit (" ^ (string_of_int limit) ^ " second" ^ (s_of_int limit) ^ ") has been reached. The behavioral cartography algorithm now stops, although the cartography has not been covered yet."
			);
	);
	
	(* Process the finalization *)
	bc_finalize ();

	(* Process the result and return *)
	bc_result ()



(*------------------------------------------------------------*)
(** Behavioral cartography algorithm with random selection of a pi0 *)
(*** TODO: merge with the other !!!! ***)
(*** WARNING: not tested for a LONG time ***)
(*------------------------------------------------------------*)
let random_behavioral_cartography model v0 nb =
	(* Retrieve the input options *)
	let options = Input.get_options () in

	(* Array for the pi0 *)
	(***** TO OPTIMIZE: why create such a big array?! *****)
	let pi0_computed = Array.make nb (random_pi0 model v0) in
	(* Array for the results *)
	(***** TO OPTIMIZE: why create such a big array?! *****)
	let results = Array.make nb (Convex_constraint (LinearConstraint.p_false_constraint (), Unknown)) in
	(* Index of the iterations where we really found different constraints *)
	let interesting_interations = ref [] in
	(* Debug mode *)
	let global_debug_mode = get_debug_mode() in
	(* Prevent the printing of messages in algorithm Inverse Method *)
	let cut_messages = not (debug_mode_greater Debug_low) in

	(* Compute the initial state *)
	let init_state = get_initial_state_or_abort model in

	(* Initial constraint of the model *)
	let _, init_constraint = init_state in
	(* Hide non parameters *)
	let init_constraint = LinearConstraint.px_hide_nonparameters_and_collapse init_constraint in

	(* Current iteration *)
	let i = ref 1 in
	let limit_reached = ref false in
	while !i <= nb && not !limit_reached do
		let pi0 = random_pi0 model v0 in

		(* Print messages *)
		print_message Debug_standard ("\n**************************************************");
		print_message Debug_standard ("RANDOM BEHAVIORAL CARTOGRAPHY ALGORITHM: " ^ (string_of_int !i) ^ " / " ^ (string_of_int nb) ^ "");
		
		(* First check that it was not computed before *)
		let already_computed, index = 
			try true, index_of pi0 pi0_computed
			with Not_found -> false, 0
		in

		(* If already computed: message *)
		if already_computed then (
			print_message Debug_standard ("This pi" ^ (string_of_int !i) ^ " is equal to pi" ^ (string_of_int (index + 1)) ^ ".");
		(* Only consider new pi0 *)
		) else (
			(* Convert the pi0 to a functional representation *)
			let pi0_functional = fun parameter -> pi0.(parameter) in

			(* Check that it does not belong to any constraint *)
			if array_exists (pi0_in_returned_constraint pi0_functional) results then (
				print_message Debug_standard ("This pi" ^ (string_of_int !i) ^ " is already included in a constraint.");
				print_message Debug_standard (ModelPrinter.string_of_pi0 model pi0_functional);
				
			(* Check that it satisfies the initial constraint *)
			) else if not (LinearConstraint.is_pi0_compatible pi0_functional init_constraint) then (
				print_message Debug_standard ("This pi" ^ (string_of_int !i) ^ " does not satisfy the initial constraint of the model.");
				print_message Debug_standard (ModelPrinter.string_of_pi0 model pi0_functional);
				
			) else (
				(* Consider from here a brand new and correct pi0 *)
				print_message Debug_standard ("Considering pi" ^ (string_of_int !i) ^ " :=");
				print_message Debug_standard (ModelPrinter.string_of_pi0 model pi0_functional);

				(* Prevent the messages if needed *)
				if cut_messages then (
					set_debug_mode Debug_nodebug;
				);
				
				(* Set the new pi0 *)
				Input.set_pi0 pi0_functional;
			
				(* Call the inverse method *)
				let (*returned_constraint, graph, (*tile_nature*)_, (*deterministic*)_, nb_iterations, total_time*) im_result, reachability_graph = Reachability.inverse_method_gen model init_state in
				(* Get the debug mode back *)
				set_debug_mode global_debug_mode;

				(* Retrieve some info *)
				let current_nb_states = StateSpace.nb_states reachability_graph in
				let current_nb_transitions = StateSpace.nb_transitions reachability_graph in
				
				print_message Debug_standard (
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
				Graphics.generate_graph model reachability_graph radical;
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
				print_message Debug_low ("Constraint K0 computed:");
				print_message Debug_standard (ModelPrinter.string_of_returned_constraint model.variable_names k0);

				(* Add the result *)
				results.(!i - 1) <- k0;
			);
		);
		(* Stop if the time limit has been reached *)
		let _ =
		match options#time_limit with
			| None -> ()
			| Some limit -> if (get_time()) > (float_of_int limit) then limit_reached := true;
		in

		(* Increment the iteration *)
		i := !i + 1;
	done;

	if !limit_reached && !i <= nb then (
		match options#time_limit with
			| None -> ()
			| Some limit -> if (get_time()) > (float_of_int limit) then print_warning (
				"The time limit (" ^ (string_of_int limit) ^ " second" ^ (s_of_int limit) ^ ") has been reached. The behavioral cartography algorithm now stops, although the cartography has not been covered yet."
			);
	);
	
	(* Print the result *)
	print_message Debug_standard ("\n**************************************************");
	print_message Debug_standard ("" ^ (string_of_int (List.length !interesting_interations)) ^ " different constraint" ^ (s_of_int (List.length !interesting_interations)) ^ " were computed at the following iteration" ^ (s_of_int (List.length !interesting_interations)) ^ " :");
	print_message Debug_standard (string_of_list_of_string_with_sep ", " (List.map string_of_int (List.rev !interesting_interations)));
	print_message Debug_standard ("**************************************************");

	(* Return a list of generated zones *)
	let zones = List.map (fun index -> results.(index)) !interesting_interations in
	zones
