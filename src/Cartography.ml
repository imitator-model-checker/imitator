(*****************************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2012/06/18
 * Last modified: 2012/06/18
 *
 ****************************************************************)

 open Global
 open AbstractModel
 open Graph
 open Reachability
 

(**************************************************)
(* Pi0 function *)
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
(* BEHAVIORAL CARTOGRAPHY ALGORITHM functions *)
(**************************************************)
(** Check if a pi_0 belongs to a 'returned_constraint'*)
let pi0_in_returned_constraint pi0 = function
	| Convex_constraint k -> LinearConstraint.is_pi0_compatible pi0 k
	(** Disjunction of constraints *)
	| Union_of_constraints k_list -> List.exists (LinearConstraint.is_pi0_compatible pi0) k_list


(** Behavioral cartography algorithm with full coverage of V0 *)
let cover_behavioral_cartography program v0 init_state =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	(* Dimension of the system *)
	let dimension = Array.length v0 in
	(* Min & max bounds for the parameters *)
	let min_bounds = Array.map (fun (low, high) -> NumConst.numconst_of_int low) v0 in
	let max_bounds = Array.map (fun (low, high) -> NumConst.numconst_of_int high) v0 in
	
	(* Initial constraint of the program *)
	let _, init_constraint = init_state in
	(* Hide non parameters *)
	let init_constraint = LinearConstraint.hide (List.rev_append program.discrete program.clocks) init_constraint in

	(* Current pi0, instantiated with the lower bounds *)
	let current_pi0 = Array.copy min_bounds in
	(* (Dynamic) Array for the pi0 *)
	let pi0_computed = DynArray.create() in
	(* (Dynamic) Array for the results *)
	let results = DynArray.create() in
	(* Current iteration *)
	let current_iteration = ref 0 in
	(* Sum of number of states *)
	let nb_states = ref 0 in
	(* Sum of number of transitions *)
	let nb_transitions = ref 0 in

	(* Debug mode *)
	let global_debug_mode = get_debug_mode() in
	
	(* Iterate on all the possible pi0 *)
	let more_pi0 = ref true in
	let limit_reached = ref false in
	while !more_pi0 && not !limit_reached do
		(* Copy the array current_pi0*)
		let pi0_array = Array.copy current_pi0 in
		let pi0 = fun parameter -> pi0_array.(parameter) in
		
		(* Check that it does not belong to any constraint *)
		if dynArray_exists (pi0_in_returned_constraint pi0) results then (
			if debug_mode_greater Debug_medium then (
				print_message Debug_medium "The following pi0 is already included in a constraint.";
				print_message Debug_medium (ModelPrinter.string_of_pi0 program pi0);
			);
		(* Check that it satisfies the initial constraint *)
		) else if not (LinearConstraint.is_pi0_compatible pi0 init_constraint) then (
			if debug_mode_greater Debug_medium then (
				print_message Debug_medium "The following pi0 does not satisfy the initial constraint of the program.";
				print_message Debug_medium (ModelPrinter.string_of_pi0 program pi0);
			);
		) else (
			(* Iterate *)
			current_iteration := !current_iteration + 1;

			(* Debug messages *)
			print_message Debug_standard ("\n**************************************************");
			print_message Debug_standard ("BEHAVIORAL CARTOGRAPHY ALGORITHM: " ^ (string_of_int !current_iteration) ^ "");
			print_message Debug_standard ("Considering the following pi" ^ (string_of_int !current_iteration));
			print_message Debug_standard (ModelPrinter.string_of_pi0 program pi0);
			
			(* Prevent the debug messages *)
			if not (debug_mode_greater Debug_medium) then
				set_debug_mode Debug_nodebug;
			(* Compute the post and the constraint *)
			let returned_constraint, graph, nb_iterations, counter = Reachability.post_star program pi0 init_state in
			(* Get the debug mode back *)
			set_debug_mode global_debug_mode;
			(* Update the counters *)
			nb_states := !nb_states + (Graph.nb_states graph);
			nb_transitions := !nb_transitions + (Hashtbl.length (graph.transitions_table));
			(* Print message *)
			print_message Debug_standard (
				"\nK" ^ (string_of_int (!current_iteration)) ^ " computed using algorithm InverseMethod after "
				^ (string_of_int nb_iterations) ^ " iteration" ^ (s_of_int nb_iterations) ^ ""
				^ " in " ^ (string_of_seconds (time_from counter)) ^ ": "
				^ (string_of_int (Graph.nb_states graph)) ^ " reachable state" ^ (s_of_int (Graph.nb_states graph))
				^ " with "
				^ (string_of_int (Hashtbl.length (graph.transitions_table))) ^ " transition" ^ (s_of_int (Hashtbl.length (graph.transitions_table))) ^ ".");
			
			(* Generate the dot graph *)			
			let radical = options#program_prefix ^ "_" ^ (string_of_int !current_iteration) in
			Graphics.generate_graph program pi0 graph radical;
			
			let k0 = returned_constraint in
			
			(* Add the pi0 and the computed constraint *)
			DynArray.add pi0_computed pi0;
			DynArray.add results k0;
			
			(* Print the constraint *)
(* 			let bad_string = if Graph.is_bad program graph then "BAD." else "GOOD." in			 *)
			print_message Debug_low ("Constraint K0 computed:");
			print_message Debug_standard (string_of_returned_constraint program.variable_names k0);
(* 			print_message Debug_standard ("This zone is " ^ bad_string); *)


		); (* else if new pi0 *)

		(* Find the next pi0 *)
		let not_is_max = ref true in
		let local_index = ref 0 in
		while !not_is_max do
			(* Try to increment the local index *)
			if current_pi0.(!local_index) < max_bounds.(!local_index) then(
				(* Increment this index *)
				current_pi0.(!local_index) <- NumConst.add current_pi0.(!local_index) options#step;
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
		
		(* Stop if the time limit has been reached *)
		match options#time_limit with
			| None -> ()
			| Some limit -> if (get_time()) > (float_of_int limit) then limit_reached := true;

	done; (* while more pi0 *)

	if !limit_reached && !more_pi0 then (
		match options#time_limit with
			| None -> ()
			| Some limit -> if (get_time()) > (float_of_int limit) then print_warning (
				"The time limit (" ^ (string_of_int limit) ^ " second" ^ (s_of_int limit) ^ ") has been reached. The behavioral cartography algorithm now stops, although the cartography has not been covered yet."
			);
	);
	
	let nb_tiles = DynArray.length results in
	let nb_states = (float_of_int (!nb_states)) /. (float_of_int nb_tiles) in
	let nb_transitions = (float_of_int (!nb_transitions)) /. (float_of_int nb_tiles) in
	
	(* Print the result *)
	print_message Debug_standard ("\n**************************************************");
	print_message Debug_standard ("" ^ (string_of_int nb_tiles) ^ " different constraints were computed.");
	print_message Debug_standard ("Average number of states     : " ^ (string_of_float nb_states) ^ "");
	print_message Debug_standard ("Average number of transitions: " ^ (string_of_float nb_transitions) ^ "");
	print_message Debug_standard ("**************************************************");

	(* Return a list of the generated zones *)
	let zones = DynArray.to_list results in
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
				(* Compute the post *)
				let returned_constraint, graph, nb_iterations, counter = Reachability.post_star program pi0_functional init_state in
				(* Get the debug mode back *)
				set_debug_mode global_debug_mode;
				print_message Debug_standard (
					"\nK" ^ (string_of_int !i) ^ " computed using algorithm InverseMethod after "
					^ (string_of_int nb_iterations) ^ " iteration" ^ (s_of_int nb_iterations) ^ ""
					^ " in " ^ (string_of_seconds (time_from counter)) ^ ": "
					^ (string_of_int (Graph.nb_states graph)) ^ " reachable state" ^ (s_of_int (Graph.nb_states graph))
					^ " with "
					^ (string_of_int (Hashtbl.length (graph.transitions_table))) ^ " transition" ^ (s_of_int (Hashtbl.length (graph.transitions_table))) ^ ".");

				(* Add the pi0 *)
				pi0_computed.(!i - 1) <- pi0;

				(* Generate the dot graph *)
				let radical = options#program_prefix ^ "_" ^ (string_of_int !i) in
				Graphics.generate_graph program pi0_functional graph radical;
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
				print_message Debug_standard (string_of_returned_constraint program.variable_names k0);

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
