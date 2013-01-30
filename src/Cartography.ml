(*****************************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2012/06/18
 * Last modified: 2013/01/30
 *
 ****************************************************************)

 open Global
 open AbstractModel
 open Graph
 open Reachability
 
(**************************************************)
(* Global variable (for testing purpose) *)
(**************************************************)
let giuseppe_idea = false


(**************************************************)
(* Functions on NumConst (that may not need to be defined here) *)
(**************************************************)
(* Check if number is a multiple of step since base_number *)
(* That is: does there exist an integer k such that number = base_number + k * step ? *)
let is_multiple_with_step base_number step number =
	(* Substract base_number *)
	let number_minus_base = NumConst.sub number base_number in
	(* Divide by step *)
	let number_minus_base_divided = NumConst.div number_minus_base step in
	(* Check if positive integer *)
	(NumConst.ge number_minus_base_divided NumConst.zero)
	&&
	(NumConst.is_integer number_minus_base_divided)


(* Find the closest multiple of step from base_number below (or equal to) number *)
(* That is: find the largest n s.t. n = k * step + base_number, with k integer, and n <= number *)
let find_multiple_below base_number step number =
	raise (InternalError("Not implemented!"))

(* Find the closest multiple of step from base_number above (or equal to) number *)
(* That is: find the smallest n s.t. n = k * step + base_number, with k integer, and n >= number *)
let find_multiple_above base_number step number =
	raise (InternalError("Not implemented!"))



(**************************************************)
(* General functions *)
(**************************************************)
(** Check if a pi_0 belongs to a 'returned_constraint'*)
let pi0_in_returned_constraint pi0 = function
	| Convex_constraint k -> LinearConstraint.is_pi0_compatible pi0 k
	(** Disjunction of constraints *)
	| Union_of_constraints k_list -> List.exists (LinearConstraint.is_pi0_compatible pi0) k_list



(**************************************************)
(* Pi0 function (to move to "next pi0 functions" section) *)
(**************************************************)
(* Generate a random pi0 in a given interval for each parameter (array view!) *)
let random_pi0 program pi0 =
	(* Create the pi0 *)
	let random_pi0 = Array.make program.nb_parameters NumConst.zero in
	(* Fill it *)
	for i = 0 to program.nb_parameters - 1 do
		let a, b = pi0.(i) in
		(* Generate a random value in the interval *)
		Random.self_init();
		let random_value = Random.int (b - a + 1) + a in
		(* Debug *)
		print_message Debug_medium ("Generating randomly value '" ^ (string_of_int random_value) ^ "' for parameter '" ^ (program.variable_names i) ^ "'.");
		(* Convert to a num *)
		random_pi0.(i) <- NumConst.numconst_of_int random_value;
	done;
	(* Return the result as an array *)
	random_pi0



(**************************************************)
(* Initial pi0 functions *)
(**************************************************)
(* First point pi0 *)
let initial_pi0 min_bounds max_bounds =
	(* Retrieve the options *)
	let options = Input.get_options () in
	
	let step = options#step in
	
	(* Case by case *)
	match options#imitator_mode with
	
	(* Instantiate with the lower bounds *)
	| Cover_cartography ->
		Array.copy min_bounds

		(* Instantiate with the point in the middle of V0 *)
	| Border_cartography ->
		let initial_pi0 = Array.create (Array.length min_bounds) NumConst.zero in
		for i = 0 to (Array.length initial_pi0) - 1 do
			(* Get some variables *)
			let min_bound = min_bounds.(i) in
			let max_bound = max_bounds.(i) in
			(* Compute the average *)
			let average = NumConst.div
				(NumConst.add min_bound max_bound)
				(NumConst.numconst_of_int 2)
			in
			let local_point =
			(* Check if the average is a valid point *)
			if is_multiple_with_step min_bound step average then average else(
				(* Otherwise try below *)
				let below = find_multiple_below min_bound step average in
				if NumConst.ge below min_bound then below else(
					(* Otherwise try above *)
					let above = find_multiple_above min_bound step average in
					if NumConst.le above max_bound then above else(
						(* Otherwise cannot start (but this should not happen if max_bound >= min_bound, for whatever step) *)
						raise (Failure("V0 does not contain any point multiple of step '" ^ (NumConst.string_of_numconst step) ^"' in some direction."))
					) (* end if above valid *)
				) (* end if below valid *)
			) (* end if average valid *)
			in
			(* Assign it to the array *)
			initial_pi0.(i) <- local_point;
		done;
		(* Return the initial_pi0 *)
		initial_pi0
		
	| _ -> raise (InternalError("In function initial_pi0, the mode should be a cover / border cartography only."))


(**************************************************)
(* Next pi0 functions *)
(**************************************************)

(** Compute the next pi0 and directly modify the variable 'current_pi0' (standard BC) *)
let find_next_pi0 program init_constraint min_bounds max_bounds dimension computed_constraints current_pi0 =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	
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
		
		let not_is_max = ref true in
		let local_index = ref 0 in
		
		while !not_is_max do
			(* Try to increment the local index *)
			let local_index_incremented = NumConst.add current_pi0.(!local_index) options#step in
			if local_index_incremented <= max_bounds.(!local_index) then (
				(* Increment this index *)
				current_pi0.(!local_index) <- local_index_incremented;
				(* Reset the smaller indexes to the low bound *)
				for i = 0 to !local_index - 1 do
					current_pi0.(i) <- min_bounds.(i);
				done;
				(* Stop the loop *)
				not_is_max := false;
			)
			(* Else: try the next index *)
			else ( 
				local_index := !local_index + 1;
				(* If last index: the end! *)
				if !local_index >= dimension then(
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
			if dynArray_exists (pi0_in_returned_constraint pi0) computed_constraints then (
				(* Update the number of unsuccessful points *)
				nb_useless_points := !nb_useless_points + 1;
				if debug_mode_greater Debug_medium then (
					print_message Debug_medium "The following pi0 is already included in a constraint.";
					print_message Debug_medium (ModelPrinter.string_of_pi0 program pi0);
				);
				
			(* Check that it satisfies the initial constraint *)
			) else if not (LinearConstraint.is_pi0_compatible pi0 init_constraint) then (
				(* Update the number of unsuccessful points *)
				nb_useless_points := !nb_useless_points + 1;
				if debug_mode_greater Debug_medium then (
					print_message Debug_medium "The following pi0 does not satisfy the initial constraint of the program.";
					print_message Debug_medium (ModelPrinter.string_of_pi0 program pi0);
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
	done;
	
	(* Return info (note that current_pi0 has ALREADY been updated if a suitable pi0 was found !) *)
	!found_pi0 , !time_limit_reached , !nb_useless_points





(**************************************************)
(* BEHAVIORAL CARTOGRAPHY ALGORITHM functions *)
(**************************************************)
	
	
	
	
(* TODO: merge both algorithms into cover_behavioral_cartography !!! *)




(** Behavioral cartography algorithm with full coverage of V0 *)
let cover_behavioral_cartography program v0 init_state =
	(* Retrieve the input options *)
	let options = Input.get_options () in

	(* Time counter for recording the globl time spent on BC *)
	let time_spent_on_IM = ref 0. in
	(* Record start time to compute the time spent only on calling IM *)
	let start_time = Unix.gettimeofday() in

	(* Dimension of the system *)
	let dimension = Array.length v0 in
	(* Min & max bounds for the parameters *)
	let min_bounds = Array.map (fun (low, high) -> NumConst.numconst_of_int low) v0 in
	let max_bounds = Array.map (fun (low, high) -> NumConst.numconst_of_int high) v0 in
	
	(* Compute the (actually slightly approximate) number of points in V0 (for information purpose) *)
	let nb_points = Array.fold_left (fun current_number (low, high) ->
		(* Multiply current number of points by the interval + 1, itself divided by the step *)
		NumConst.mul
			current_number
			(NumConst.div
				(NumConst.add
					(NumConst.sub 
						(NumConst.numconst_of_int high)
						(NumConst.numconst_of_int low)
					)
					NumConst.one
				)
				options#step
			)
	) NumConst.one v0 in
	
	(* Counts the points actually member of an existing constraint (hence useless) for information purpose *)
	let nb_useless_points = ref 0 in
	
	(* Initial constraint of the program *)
	let _, init_constraint = init_state in
	(* Hide non parameters *)
	let init_constraint = LinearConstraint.hide program.clocks_and_discrete init_constraint in

(*	(* (Dynamic) Array for the pi0 (USELESS SO FAR) *)
	let pi0_computed = DynArray.create() in*)
	(* (Dynamic) Array for the results *)
	let computed_constraints = DynArray.create() in

	(* Compute the first point pi0 *)
	let current_pi0 = initial_pi0 min_bounds max_bounds in
	
	(* Current iteration (for information purpose) *)
	let current_iteration = ref 0 in
	(* Sum of number of states (for information purpose) *)
	let nb_states = ref 0 in
	(* Sum of number of transitions (for information purpose) *)
	let nb_transitions = ref 0 in

	(* Debug mode *)
	let global_debug_mode = get_debug_mode() in
	
	(** TODO : check that initial pi0 is suitable!! (could be incompatible with initial constraint) *)
	
	(* Print *)
	print_message Debug_standard ("\n**************************************************");
	print_message Debug_standard (" START THE BEHAVIORAL CARTOGRAPHY ALGORITHM");
	print_message Debug_standard ("**************************************************");
	print_message Debug_standard (" Parametric rectangle V0: ");
	print_message Debug_standard (ModelPrinter.string_of_v0 program v0);
	print_message Debug_standard (" Number of points inside V0: " ^ (NumConst.string_of_numconst nb_points));

	(* Iterate on all the possible pi0 *)
	let more_pi0 = ref true in
	let limit_reached = ref false in
	while !more_pi0 && not !limit_reached do
		(* Copy the array current_pi0 *)
		(*(** NOTE: this copy looks completely useless *)
		let pi0_array = Array.copy current_pi0 in*)
		
		(** WARNING : duplicate operation (quite cheap anyway) *)
		(* Convert to functional representation *)
		let pi0 = fun parameter -> current_pi0.(parameter) in
		
		(* Iterate *)
		current_iteration := !current_iteration + 1;

		(* Debug messages *)
		print_message Debug_standard ("\n**************************************************");
		print_message Debug_standard ("BEHAVIORAL CARTOGRAPHY ALGORITHM: " ^ (string_of_int !current_iteration) ^ "");
		print_message Debug_standard ("Considering the following pi" ^ (string_of_int !current_iteration));
		print_message Debug_standard (ModelPrinter.string_of_pi0 program pi0);
		
		(* Prevent the debug messages (except in debug high or total) *)
		if not (debug_mode_greater Debug_medium) then
			set_debug_mode Debug_nodebug;
		
		(* Set the new pi0 *)
		Input.set_pi0 pi0;
		
		(* Call the inverse method *)
		let returned_constraint, graph, tile_nature, (*deterministic*)_, nb_iterations, total_time = Reachability.inverse_method_gen program init_state in
		
		(* Update the time spent on IM *)
		time_spent_on_IM := !time_spent_on_IM +. total_time;
		
		(* Get the debug mode back *)
		set_debug_mode global_debug_mode;
		
		(* Retrieve some info *)
		let current_nb_states = Graph.nb_states graph in
		let current_nb_transitions = Graph.nb_transitions graph in
		
		(* Update the counters *)
		nb_states := !nb_states + current_nb_states;
		nb_transitions := !nb_transitions + current_nb_transitions;
		
		(* Print message *)
		print_message Debug_standard (
			"\nK" ^ (string_of_int (!current_iteration)) ^ " computed using algorithm InverseMethod after "
			^ (string_of_int nb_iterations) ^ " iteration" ^ (s_of_int nb_iterations) ^ ""
			^ " in " ^ (string_of_seconds total_time) ^ ": "
			^ (string_of_int current_nb_states) ^ " reachable state" ^ (s_of_int current_nb_states)
			^ " with "
			^ (string_of_int current_nb_transitions) ^ " transition" ^ (s_of_int current_nb_transitions) ^ ".");
		
		(* Generate the dot graph (will not be performed if options are not suitable) *)
		let radical = options#program_prefix ^ "_" ^ (string_of_int !current_iteration) in
			Graphics.generate_graph program graph radical;
		
		let k0 = returned_constraint in
		
		(* Add the pi0 and the computed constraint *)
		(*USELESS SO FAR 
		DynArray.add pi0_computed pi0;*)
		DynArray.add computed_constraints k0;
		
		(* Print the constraint *)
		
		
		(** NOTE: it may actually be more clever to check the tile nature from the graph, especially if we go for more complicated properties!! *)
		
		
(* 			let bad_string = if Graph.is_bad program graph then "BAD." else "GOOD." in *)
		print_message Debug_low ("Constraint K0 computed:");
		print_message Debug_standard (ModelPrinter.string_of_returned_constraint program.variable_names k0);
		if program.bad <> Nobad then(
			(* TODO: move this translation somewhere else *)
			let bad_string = match tile_nature with
				| Good -> "good"
				| Bad -> "bad"
				| _ -> raise (InternalError ("Tile nature should be good or bad only, so far "))
			in
			print_message Debug_standard ("This zone is " ^ bad_string ^ ".");
		);


		(* Compute the next pi0 (note that current_pi0 is directly modified by the function!) and return flags for more pi0 and co *)
		let found_pi0 , time_limit_reached , new_nb_useless_points =
			find_next_pi0 program init_constraint min_bounds max_bounds dimension computed_constraints current_pi0 in
			
		(* Update the number of useless points *)
		nb_useless_points := !nb_useless_points + new_nb_useless_points;
		(* Update the time limit *)
		limit_reached := time_limit_reached;
		(* Update the found pi0 flag *)
		more_pi0 := found_pi0;

	done; (* while more pi0 *)

	if !limit_reached && !more_pi0 then (
		match options#time_limit with
			| None -> ()
			| Some limit -> if (get_time()) > (float_of_int limit) then print_warning (
				"The time limit (" ^ (string_of_int limit) ^ " second" ^ (s_of_int limit) ^ ") has been reached. The behavioral cartography algorithm now stops, although the cartography has not been covered yet."
			);
	);
	
	let nb_tiles = DynArray.length computed_constraints in
	let nb_states = (float_of_int (!nb_states)) /. (float_of_int nb_tiles) in
	let nb_transitions = (float_of_int (!nb_transitions)) /. (float_of_int nb_tiles) in
	
	let global_time = time_from start_time in
	let time_spent_on_BC = global_time -. (!time_spent_on_IM) in
	
	(* Print the result *)
	print_message Debug_standard ("\n**************************************************");
	print_message Debug_standard (" END OF THE BEHAVIORAL CARTOGRAPHY ALGORITHM");
	print_message Debug_standard ("Size of V0: " ^ (NumConst.string_of_numconst nb_points) ^ "");
	print_message Debug_standard ("Unsuccessful points: " ^ (string_of_int !nb_useless_points) ^ "");
	print_message Debug_standard ("" ^ (string_of_int nb_tiles) ^ " different constraints were computed.");
	print_message Debug_standard ("Average number of states     : " ^ (string_of_float nb_states) ^ "");
	print_message Debug_standard ("Average number of transitions: " ^ (string_of_float nb_transitions) ^ "");
	print_message Debug_standard ("Global time spent    : " ^ (string_of_float global_time) ^ "");
	print_message Debug_standard ("Time spent on IM     : " ^ (string_of_float (!time_spent_on_IM)) ^ "");
	print_message Debug_standard ("Time spent on BC only: " ^ (string_of_float (time_spent_on_BC)) ^ "");
	print_message Debug_standard ("**************************************************");

	(* Return a list of the generated zones *)
	let zones = DynArray.to_list computed_constraints in
	zones




(** Behavioral cartography algorithm with random selection of a pi0 *)
let random_behavioral_cartography program v0 init_state nb =
	(* Retrieve the input options *)
	let options = Input.get_options () in

	(* Array for the pi0 *)
	(***** TO OPTIMIZE: why create such a big array?! *****)
	let pi0_computed = Array.make nb (random_pi0 program v0) in
	(* Array for the results *)
	(***** TO OPTIMIZE: why create such a big array?! *****)
	let results = Array.make nb (Convex_constraint (LinearConstraint.false_constraint ())) in
	(* Index of the iterations where we really found different constraints *)
	let interesting_interations = ref [] in
	(* Debug mode *)
	let global_debug_mode = get_debug_mode() in
	(* Prevent the printing of messages in algorithm Inverse Method *)
	let cut_messages = not (debug_mode_greater Debug_low) in
	(* Initial constraint of the program *)
	let _, init_constraint = init_state in
	(* Hide non parameters *)
	let init_constraint = LinearConstraint.hide (List.rev_append program.discrete program.clocks) init_constraint in

	(* Current iteration *)
	let i = ref 1 in
	let limit_reached = ref false in
	while !i <= nb && not !limit_reached do
		let pi0 = random_pi0 program v0 in

		(* Print messages *)
		print_message Debug_standard ("\n**************************************************");
		print_message Debug_standard ("BEHAVIORAL CARTOGRAPHY ALGORITHM: " ^ (string_of_int !i) ^ " / " ^ (string_of_int nb) ^ "");
		
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
				print_message Debug_standard (ModelPrinter.string_of_pi0 program pi0_functional);
				
			(* Check that it satisfies the initial constraint *)
			) else if not (LinearConstraint.is_pi0_compatible pi0_functional init_constraint) then (
				print_message Debug_standard ("This pi" ^ (string_of_int !i) ^ " does not satisfy the initial constraint of the program.");
				print_message Debug_standard (ModelPrinter.string_of_pi0 program pi0_functional);
				
			) else (
				(* Consider from here a brand new and correct pi0 *)
				print_message Debug_standard ("Considering pi" ^ (string_of_int !i) ^ " :=");
				print_message Debug_standard (ModelPrinter.string_of_pi0 program pi0_functional);

				(* Prevent the messages if needed *)
				if cut_messages then (
					set_debug_mode Debug_nodebug;
				);
				
				(* Set the new pi0 *)
				Input.set_pi0 pi0_functional;
			
				(* Call the inverse method *)
				let returned_constraint, graph, (*tile_nature*)_, (*deterministic*)_, nb_iterations, total_time = Reachability.inverse_method_gen program init_state in
				(* Get the debug mode back *)
				set_debug_mode global_debug_mode;

				(* Retrieve some info *)
				let current_nb_states = Graph.nb_states graph in
				let current_nb_transitions = Graph.nb_transitions graph in
				
				print_message Debug_standard (
					"\nK" ^ (string_of_int !i) ^ " computed using algorithm InverseMethod after "
					^ (string_of_int nb_iterations) ^ " iteration" ^ (s_of_int nb_iterations) ^ ""
					^ " in " ^ (string_of_seconds total_time) ^ ": "
					^ (string_of_int current_nb_states) ^ " reachable state" ^ (s_of_int current_nb_states)
					^ " with "
					^ (string_of_int current_nb_transitions) ^ " transition" ^ (s_of_int current_nb_transitions) ^ ".");

				(* Add the pi0 *)
				pi0_computed.(!i - 1) <- pi0;

				(* Generate the dot graph *)
				let radical = options#program_prefix ^ "_" ^ (string_of_int !i) in
				Graphics.generate_graph program graph radical;
				(* Add the index to the interesting list *)
				interesting_interations := !i :: !interesting_interations;

				(* compute k0 *)
(*				let k0 =  if options#dynamic || options#union then ( returned_constraint )
				else  match returned_constraint with 
					| Convex_constraint _ -> Convex_constraint (Graph.compute_k0_destructive program graph)
					| _ -> print_error ("Internal error when getting the result of post_star in cover: 'options#dynamic' is activated but the constraint returned is not convex (type 'Convex_constraint')."); abort_program (); exit(0)

				in*)
				let k0 = returned_constraint in
												
				(* Print the constraint *)
				print_message Debug_low ("Constraint K0 computed:");
				print_message Debug_standard (ModelPrinter.string_of_returned_constraint program.variable_names k0);

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
