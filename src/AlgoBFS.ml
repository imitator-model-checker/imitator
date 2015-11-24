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
 * Last modified     : 2015/11/24
 *
 ************************************************************)


(**************************************************************)
(* Modules *)
(**************************************************************)
open CamlUtilities
open ImitatorUtilities
open AbstractModel


(**************************************************************)
(* Class definition *)
(**************************************************************)
class algoBFS =
	object (self)
	
	(* Start time for the algorithm *)
	val mutable start_time = 0.
	
	(* Depth in the explored state space *)
	val mutable current_depth = 0
	
	(*** TODO: better have some option, or better initialize it to the good value from now on ***)
	val mutable state_space = StateSpace.make 0
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Main method running the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method run (init_state:StateSpace.state) =
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
		let init_loc, init_constr = init_state in
		let init_state = (init_loc, LinearConstraint.px_copy init_constr) in

(*		(* Initialization of global variables *)
		print_message Verbose_low ("Initializing algorithm variables...");
		k_result := LinearConstraint.px_hide_nonparameters_and_collapse init_constr;
		p_constraints := [];

		(* Print some information *)
		if verbose_mode_greater Verbose_low then(
			print_message Verbose_low ("Initialized k_result to ");
			print_message Verbose_low (LinearConstraint.string_of_p_linear_constraint model.variable_names !k_result);
			print_message Verbose_low ("");
		);

		(*Initialization of slast : used in union mode only*)
		slast := [];*)
		
	(*		(* Set the counter of selections to 0 *)
		nb_random_selections := 0;*)

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
		let limit_reached = ref false in

		(* Check if the list of new states is empty *)
		while not (!limit_reached || !newly_found_new_states = []) do
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

			
			()

(*			JE SUIS LA
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
				empty_states_for_comparison state_space;
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
			(* If check-point option: check if the constraint is equal to pi0 *)
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
			);
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
			nb_iterations := current_depth + 1;
			
			(* Check if the limit has been reached *)
			limit_reached := !limit_reached || (check_limit current_depth (StateSpace.nb_states state_space) (time_from start_time));*)
		done;
		
		(* Flag to detect premature stop in case of limit reached *)
		let premature_stop = ref false in
		
		(* There were still states to explore *)
		if !limit_reached && !newly_found_new_states != [] then(
			(* Update flag *)
			premature_stop := true;
			
(*			(* Print some information *)
			print_warnings_limit current_depth (StateSpace.nb_states state_space) (time_from start_time) (List.length !newly_found_new_states);*)
			
		);

		print_message Verbose_standard (
			let nb_states = StateSpace.nb_states state_space in
			let nb_transitions = StateSpace.nb_transitions state_space in
			"\nFixpoint reached after "
			^ (string_of_int current_depth) ^ " iteration" ^ (s_of_int current_depth) ^ ""
			^ ": "
			^ (string_of_int nb_states) ^ " state" ^ (s_of_int nb_states)
			^ " with "
			^ (string_of_int nb_transitions) ^ " transition" ^ (s_of_int nb_transitions) ^ " explored.");

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

		(*** HACK: just a test ***)
		Result.Noresultbecausethatsatest
end;;