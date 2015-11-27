(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: main class to explore the state space in breadth-first search manner
 * 
 * File contributors : Étienne André
 * Created           : 2015/11/23
 * Last modified     : 2015/11/27
 *
 ************************************************************)


(**************************************************************)
(* Modules *)
(**************************************************************)
open OCamlUtilities
open ImitatorUtilities
open AbstractModel


(**************************************************************)
(* Object-independent functions *)
(**************************************************************)

(*------------------------------------------------------------*)
(* Print warning(s) if the limit of an exploration has been reached, according to the analysis options *)
(*------------------------------------------------------------*)
let print_warnings_limit depth nb_states time nb_states_to_visit =
(*** TODO: rewrite by passing in addition an argument of type 'Reachability.limit_reached' ***)
	(* Retrieve the input options *)
	let options = Input.get_options () in
	begin
	match options#depth_limit with
		| None -> ()
		| Some limit -> if depth > limit then print_warning (
			"The limit depth (" ^ (string_of_int limit) ^ ") has been reached. The exploration now stops, although there were still " ^ (string_of_int nb_states_to_visit) ^ " state" ^ (s_of_int nb_states_to_visit) ^ " to explore."
		);
	end;
	begin
	match options#states_limit with
		| None -> ()
		| Some limit -> if nb_states > limit then print_warning (
			"The limit number of states (" ^ (string_of_int limit) ^ ") has been reached. The exploration now stops, although there were still " ^ (string_of_int nb_states_to_visit) ^ " state" ^ (s_of_int nb_states_to_visit) ^ " to explore."
		);
	end;
	begin
	match options#time_limit with
		| None -> ()
		| Some limit -> if time > (float_of_int limit) then print_warning (
			"The time limit (" ^ (string_of_int limit) ^ " second" ^ (s_of_int limit) ^ ") has been reached. The exploration now stops, although there were still " ^ (string_of_int nb_states_to_visit) ^ " state" ^ (s_of_int nb_states_to_visit) ^ " to explore at this iteration."
		);
	end

	
	
(**************************************************************)
(* Class definition *)
(**************************************************************)
class virtual algoBFS =
	object (self)
	
	(* Start time for the algorithm *)
	val mutable start_time = 0.
	
	(* Depth in the explored state space *)
	val mutable current_depth = 0
	
	(*** TODO: better have some option, or better initialize it to the good value from now on ***)
	val mutable state_space = StateSpace.make 0
	
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Virtual method: the algorithm name is not defined for BFS as it is not supposed to be called *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual algorithm_name : string
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual initialize_variables : unit

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Main method running the algorithm: implements here a BFS search, and call other functions that may be modified in subclasses *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method run (init_state : StateSpace.state) =
		(* Retrieve the model *)
		let model = Input.get_model () in
		
		(* Retrieve the input options *)
		let options = Input.get_options () in
		
		(* Get some variables *)
		let nb_actions = model.nb_actions in
		let nb_variables = model.nb_variables in
		let nb_automata = model.nb_automata in

		(* Time counter *)
		start_time <- Unix.gettimeofday();

		(* copy init state, as it might be destroyed later *)
		(*** NOTE: this operation appears to be here totally useless ***)
		let init_loc, init_constr = init_state in
		let init_state = (init_loc, LinearConstraint.px_copy init_constr) in

(*		(*Initialization of slast : used in union mode only*)
		slast := [];*)
		
	(*		(* Set the counter of selections to 0 *)
		nb_random_selections := 0;*)

		(* Variable initialization *)
		print_message Verbose_low ("Initializing algorithm variables...");
		self#initialize_variables;

		(* Debut prints *)
		print_message Verbose_low ("Starting exploring the parametric zone graph from the following initial state:");
		print_message Verbose_low (ModelPrinter.string_of_state model init_state);
		(* Guess the number of reachable states *)
		let guessed_nb_states = 10 * (nb_actions + nb_automata + nb_variables) in 
		let guessed_nb_transitions = guessed_nb_states * nb_actions in 
		print_message Verbose_high ("I guess I will reach about " ^ (string_of_int guessed_nb_states) ^ " states with " ^ (string_of_int guessed_nb_transitions) ^ " transitions.");
		
		(* Create the reachability graph *)
		state_space <- StateSpace.make guessed_nb_transitions;
		
		(* Add the initial state to the reachable states *)
		let init_state_index, _ = StateSpace.add_state state_space init_state in
		
		(* Increment the number of computed states *)
		StateSpace.increment_nb_gen_states state_space;
		
		(* Set the depth to 1 *)
		current_depth <- 1;
		
		
		(*------------------------------------------------------------*)
		(* Perform the post^* *)
		(*------------------------------------------------------------*)
		(* Set of states computed at the previous depth *)
		let newly_found_new_states = ref [init_state_index] in
		
		(* Boolean to check whether the time limit / state limit is reached *)
		let limit_reached = ref Reachability.Keep_going in

		(* Explore further until the limit is reached or the list of lastly computed states is empty *)
		while !limit_reached = Reachability.Keep_going && !newly_found_new_states <> [] do
			(* Print some information *)
			if verbose_mode_greater Verbose_standard then (
				print_message Verbose_low ("\n");
				print_message Verbose_standard ("Computing post^" ^ (string_of_int current_depth) ^ " from "  ^ (string_of_int (List.length !newly_found_new_states)) ^ " state" ^ (s_of_int (List.length !newly_found_new_states)) ^ ".");
			);
			
			(* Count the states for debug purpose: *)
			let num_state = ref 0 in
			(* Length of 'newly_found_new_states' for debug purpose *)
			let nb_newly_found_states = List.length !newly_found_new_states in

			let new_newly_found_new_states =
			(* For each newly found state: *)
			List.fold_left (fun new_newly_found_new_states orig_state_index ->
				(* Count the states for debug purpose: *)
				num_state := !num_state + 1;
				(* Perform the post *)
				let new_states = Reachability.post_from_one_state model state_space orig_state_index in
				(* Print some information *)
				if verbose_mode_greater Verbose_medium then (
					let beginning_message = if new_states = [] then "Found no new state" else ("Found " ^ (string_of_int (List.length new_states)) ^ " new state" ^ (s_of_int (List.length new_states)) ^ "") in
					print_message Verbose_medium (beginning_message ^ " for the post of state " ^ (string_of_int !num_state) ^ " / " ^ (string_of_int nb_newly_found_states) ^ " in post^" ^ (string_of_int current_depth) ^ ".\n");
				);
				
				(* Return the concatenation of the new states *)
				(**** OPTIMIZED: do not care about order (else shoud consider 'list_append new_newly_found_new_states (List.rev new_states)') *)
				List.rev_append new_newly_found_new_states new_states
			) [] !newly_found_new_states in

			
			(* Merge states! *)
			let new_states_after_merging = ref new_newly_found_new_states in
			(*** HACK here! For #merge_before, we should ONLY merge here; but, in order not to change the full structure of the post computation, we first merge locally before the pi0-compatibility test, then again here *)
			if options#merge || options#merge_before then (
	(* 			new_states_after_merging := try_to_merge_states state_space !new_states_after_merging; *)
				(* New version *)
				let eaten_states = StateSpace.merge state_space !new_states_after_merging in
				new_states_after_merging := list_diff !new_states_after_merging eaten_states;
			);


			(* Update the newly_found_new_states *)
			newly_found_new_states := !new_states_after_merging;
			(* Print some information *)
			if verbose_mode_greater Verbose_medium then (
				let beginning_message = if !newly_found_new_states = [] then "\nFound no new state" else ("\nFound " ^ (string_of_int (List.length !newly_found_new_states)) ^ " new state" ^ (s_of_int (List.length !newly_found_new_states)) ^ "") in
				print_message Verbose_medium (beginning_message ^ " for post^" ^ (string_of_int current_depth) ^ ".\n");
			);
			
			(* If acyclic option: empty the list of already reached states for comparison with former states *)
			if options#acyclic then(
				print_message Verbose_low ("\nMode acyclic: empty the list of states to be compared.");
				StateSpace.empty_states_for_comparison state_space;
			);
			
	(************************************************************************************************************************)
	(************************************************************************************************************************)
	(************************************************************************************************************************)
	(************************************************************************************************************************)
	(***                                        BEGIN ALGORITHM-SPECIFIC CODE                                             ***)
	(************************************************************************************************************************)
	(************************************************************************************************************************)
	(************************************************************************************************************************)
	(************************************************************************************************************************)
	(************************************************************************************************************************)
(*			(* If check-point option: check if the constraint is equal to pi0 *)
			(*** TO OPTIMIZE !!! (at least compute pi0_constraint once for all) ***)
			(*** WARNING!! ONLY works for the classical inverse method (not for variants) ***)
			(*** TODO: also allow for BC ***)
			if options#imitator_mode = Inverse_method  && options#check_point then(
				print_message Verbose_low ("\nMode check-point: checking whether the resulting constraint is restricted to pi0...");
				(* Get all constraints *)
				let all_p_constraints = StateSpace.all_p_constraints state_space in
				(* Computing the constraint intersection *)
				let current_intersection = LinearConstraint.p_intersection all_p_constraints in
				(* Get pi0 *)
				let pi0 = Input.get_pi0() in
				(* Converting pi0 to a list *)
				let pi0_list = List.map (fun p -> (p, pi0#get_value p)) model.parameters in
				(* Converting pi0 to a constraint *)
				let pi0_constraint = LinearConstraint.p_constraint_of_point pi0_list in
				(* Print *)
				if verbose_mode_greater Verbose_medium then(
					print_message Verbose_medium ("\nPi0: " ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names pi0_constraint));
				);
				(* Checking whether the constraint is *included* within pi0 *)
				if LinearConstraint.p_is_leq current_intersection pi0_constraint then(
					(* Print message *)
					print_message Verbose_standard ("\nCurrent accumulated constraint is now restricted to pi0. Analysis can safely terminate.");
					(* Stop *)
					limit_reached := true;
				);
			);*)
	(************************************************************************************************************************)
	(************************************************************************************************************************)
	(************************************************************************************************************************)
	(************************************************************************************************************************)
	(***                                          END ALGORITHM-SPECIFIC CODE                                             ***)
	(************************************************************************************************************************)
	(************************************************************************************************************************)
	(************************************************************************************************************************)
	(************************************************************************************************************************)
	(************************************************************************************************************************)
			
			(* Print some memory information *)
			if options#statistics then(
				
			);
			
			(* Clean up a little *)
			(*** NOTE: LOOKS LIKE COMPLETELY USELESS !!! it even increases memory x-( ***)
			Gc.major ();
			
			(* Iterate *)
			current_depth <- current_depth + 1;
			
			(* Check if the limit has been reached *)
			limit_reached := Reachability.check_bfs_limit current_depth (StateSpace.nb_states state_space) (time_from start_time);
		done;
		
		(* Flag to detect premature stop in case of limit reached *)
		(*** NOTE/TODO: this variable is now useless ***)
		let premature_stop = ref false in
		
		(* There were still states to explore *)
(* 		if !limit_reached && !newly_found_new_states <> [] then( *)
		if !limit_reached <> Reachability.Keep_going && !newly_found_new_states <> [] then(
			(* Update flag *)
			premature_stop := true;
			
			(* Print some information *)
			print_warnings_limit current_depth (StateSpace.nb_states state_space) (time_from start_time) (List.length !newly_found_new_states);
			
		);

		print_message Verbose_standard (
			let nb_states = StateSpace.nb_states state_space in
			let nb_transitions = StateSpace.nb_transitions state_space in
			"\nFixpoint reached at a depth of "
			^ (string_of_int current_depth) ^ ""
			^ ": "
			^ (string_of_int nb_states) ^ " state" ^ (s_of_int nb_states)
			^ " with "
			^ (string_of_int nb_transitions) ^ " transition" ^ (s_of_int nb_transitions) ^ " explored.");
			(*** NOTE: in fact, more states and transitions may have been explored (and deleted); here, these figures are the number of states in the state space. ***)

		(*------------------------------------------------------------*)
		(* Return result *)
		(*------------------------------------------------------------*)
(*		{
			(* State space *)
			reachability_graph	= state_space;
			(* State space depth *)
			depth				= current_depth;
			(* Computation time *)
			total_time			= (time_from start_time);
			(* Premature stop? (i.e., states / depth / time limit reached) *)
			premature_stop		= !premature_stop;
			(* Number of random selections *)
			(*** TODO: remove that, as it is specific to IM... ***)
			nb_random_selections = !nb_random_selections;
			(* Unexplored states indices in case of early termination *)
			unexplored_states	= !newly_found_new_states;
		}*)

		self#compute_result

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual compute_result : Result.imitator_result

end;;