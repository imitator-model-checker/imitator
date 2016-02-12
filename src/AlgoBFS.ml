(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: main class to explore the state space in breadth-first search manner
 * 
 * File contributors : Étienne André
 * Created           : 2015/11/23
 * Last modified     : 2016/02/12
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
open AlgoStateBased
open Result


(************************************************************)
(************************************************************)
(* Class-independent functions *)
(************************************************************)
(************************************************************)

type limit_reached =
	(* No limit *)
	| Keep_going

	(* Termination due to time limit reached *)
	| Time_limit_reached
	
	(* Termination due to state space depth limit reached *)
	| Depth_limit_reached
	
	(* Termination due to a number of explored states reached *)
	| States_limit_reached

exception Limit_detected of limit_reached




(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class virtual algoBFS =
	object (self) inherit algoStateBased as super

	(************************************************************)
	(* Class variables *)
	(************************************************************)

	(* Depth in the explored state space *)
	val mutable current_depth = 0
	
	(* Function to be called from the distributed IMITATOR *)
	val mutable patator_termination_function = None
	
	(*** TODO: better have some option, or better initialize it to the good value from now on ***)
	val mutable state_space = StateSpace.make 0
	
	(* Status of the analysis *)
	val mutable termination_status = None
	
	(* Constraint of the initial state (used by some algorithms to initialize their variables) *)
	val mutable initial_constraint : LinearConstraint.px_linear_constraint option = None
	
	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Set the PaTATOR termination function *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method set_patator_termination_function (f : unit -> unit) =
		patator_termination_function <- Some f


	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check whether the limit of an BFS exploration has been reached, according to the analysis options *)
	(*** NOTE: May raise an exception when used in PaTATOR mode (the exception will be caught by PaTATOR) ***)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private check_bfs_limit =
		(* Retrieve the input options *)
		let options = Input.get_options () in
		
		(* Check all limits *)
		
		(* Depth limit *)
		try(
		begin
		match options#depth_limit with
			| None -> ()
			| Some limit -> if current_depth >= limit then(
(* 				termination_status <- Depth_limit; *)
				raise (Limit_detected Depth_limit_reached)
			)
		end
		;
		(* States limit *)
		begin
		match options#states_limit with
			| None -> ()
			| Some limit -> if StateSpace.nb_states state_space > limit then(
(* 				termination_status <- States_limit; *)
				raise (Limit_detected States_limit_reached)
			)
		end
		;
		(* Time limit *)
		begin
		match options#time_limit with
			| None -> ()
			| Some limit -> if time_from start_time > (float_of_int limit) then(
(* 				termination_status <- Time_limit; *)
				raise (Limit_detected Time_limit_reached)
			)
		end
		;
		(* External function for PaTATOR (would raise an exception in case of stop needed) *)
		begin
		match patator_termination_function with
			| None -> ()
			| Some f -> f (); () (*** NOTE/BADPROG: Does nothing but in fact will directly raise an exception in case of required termination, caught at a higher level (PaTATOR) ***)
		end
		;
		(* If reached here, then everything is fine: keep going *)
		Keep_going
		)
		(* If exception caught, then update termination status, and return the reason *)
		with Limit_detected reason -> reason

		

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Print warning(s) if the limit of an exploration has been reached, according to the analysis options *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private print_warnings_limit () =
		match termination_status with
			| Some Regular_termination -> ()

			| Some (Depth_limit nb_unexplored_successors) -> print_warning (
				"The limit depth has been reached. The exploration now stops, although there " ^ (waswere_of_int nb_unexplored_successors) ^ " still " ^ (string_of_int nb_unexplored_successors) ^ " state" ^ (s_of_int nb_unexplored_successors) ^ " to explore."
			)

			| Some (States_limit nb_unexplored_successors) -> print_warning (
				"The limit number of states has been reached. The exploration now stops, although there " ^ (waswere_of_int nb_unexplored_successors) ^ " still " ^ (string_of_int nb_unexplored_successors) ^ " state" ^ (s_of_int nb_unexplored_successors) ^ " to explore."
			)
 
			| Some (Time_limit nb_unexplored_successors) -> print_warning (
				"The time limit has been reached. The exploration now stops, although there " ^ (waswere_of_int nb_unexplored_successors) ^ " still " ^ (string_of_int nb_unexplored_successors) ^ " state" ^ (s_of_int nb_unexplored_successors) ^ " to explore at this iteration."
					(* (" ^ (string_of_int limit) ^ " second" ^ (s_of_int limit) ^ ")*)
			)
			
			| None -> raise (InternalError "The termination status should be set when displaying warnings concerning early termination.")


	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
		super#initialize_variables;
		(* The end *)
		()

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Actions to perform at the end of the computation of the *successors* of post^n (i.e., when this method is called, the successors were just computed) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual process_post_n : StateSpace.state_index list -> unit
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Check whether the algorithm should terminate at the end of some post, independently of the number of states to be processed (e.g., if the constraint is already true or false) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual check_termination_at_post_n : bool

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Main method running the algorithm: implements here a BFS search, and calls other functions that may be modified in subclasses *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method run () =
		(* Retrieve the model *)
		let model = Input.get_model () in
		
		(* Retrieve the input options *)
		let options = Input.get_options () in
		
		(* Get some variables *)
		let nb_actions = model.nb_actions in
		let nb_variables = model.nb_variables in
		let nb_automata = model.nb_automata in

		(* Time counter for the algorithm *)
		start_time <- Unix.gettimeofday();

		(* Compute initial state *)
		let init_state = AlgoStateBased.compute_initial_state_or_abort() in
		
		(* copy init state, as it might be destroyed later *)
		(*** NOTE: this operation appears to be here totally useless ***)
		let init_loc, init_constr = init_state in
		let init_state = (init_loc, LinearConstraint.px_copy init_constr) in

		(* Set up the initial state constraint *)
		initial_constraint <- Some init_constr;

(*		(*Initialization of slast : used in union mode only*)
		slast := [];*)
		
		(* Print some information *)
		print_message Verbose_standard ("Starting running algorithm " ^ self#algorithm_name ^ "...\n");
		
		(* Variable initialization *)
		print_message Verbose_low ("Initializing the algorithm local variables...");
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
		let post_n = ref [init_state_index] in
		
		(* Boolean to check whether the time limit / state limit is reached *)
		let limit_reached = ref Keep_going in
		
		(* Flag modified by the algorithm to perhaps terminate earlier *)
		let algorithm_keep_going = ref true in

		(* Explore further until the limit is reached or the list of lastly computed states is empty *)
		while !limit_reached = Keep_going && !post_n <> [] && !algorithm_keep_going do
			(* Print some information *)
			if verbose_mode_greater Verbose_standard then (
				print_message Verbose_low ("\n");
				print_message Verbose_standard ("Computing post^" ^ (string_of_int current_depth) ^ " from "  ^ (string_of_int (List.length !post_n)) ^ " state" ^ (s_of_int (List.length !post_n)) ^ ".");
			);
			
			(* Count the states for debug purpose: *)
			let num_state = ref 0 in

			let post_n_plus_1 =
			(* For each newly found state: *)
			List.fold_left (fun current_post_n_plus_1 orig_state_index ->
				(* Count the states for debug purpose: *)
				num_state := !num_state + 1;
				(* Perform the post *)
				let new_states = self#post_from_one_state state_space orig_state_index in
				(* Print some information *)
				if verbose_mode_greater Verbose_medium then (
					let beginning_message = if new_states = [] then "Found no new state" else ("Found " ^ (string_of_int (List.length new_states)) ^ " new state" ^ (s_of_int (List.length new_states)) ^ "") in
					print_message Verbose_medium (beginning_message ^ " for the post of state " ^ (string_of_int !num_state) ^ " / " ^ (string_of_int (List.length !post_n)) ^ " in post^" ^ (string_of_int current_depth) ^ ".\n");
				);
				
				(* Return the concatenation of the new states *)
				(**** OPTIMIZED: do not care about order (else shoud consider 'list_append current_post_n_plus_1 (List.rev new_states)') *)
				List.rev_append current_post_n_plus_1 new_states
			) [] !post_n in
			
			self#process_post_n !post_n;
			
			(* Merge states! *)
			let new_states_after_merging = ref post_n_plus_1 in
			(*** HACK here! For #merge_before, we should ONLY merge here; but, in order not to change the full structure of the post computation, we first merge locally before the pi0-compatibility test, then again here ***)
			if options#merge || options#merge_before then (
	(* 			new_states_after_merging := try_to_merge_states state_space !new_states_after_merging; *)
				(* New version *)
				let eaten_states = StateSpace.merge state_space !new_states_after_merging in
				new_states_after_merging := list_diff !new_states_after_merging eaten_states;
			);


			(* Update the post_n, i.e., at that point we replace the post^n by post^n+1 in our BFS algorithm, and go one step deeper in the state space *)
			post_n := !new_states_after_merging;
			(* Print some information *)
			if verbose_mode_greater Verbose_medium then (
				let beginning_message = if !post_n = [] then "\nFound no new state" else ("\nFound " ^ (string_of_int (List.length !post_n)) ^ " new state" ^ (s_of_int (List.length !post_n)) ^ "") in
				print_message Verbose_medium (beginning_message ^ " for post^" ^ (string_of_int current_depth) ^ ".\n");
			);
			
			(* If acyclic option: empty the list of already reached states for comparison with former states *)
			if options#acyclic then(
				print_message Verbose_low ("\nMode acyclic: empty the list of states to be compared.");
				StateSpace.empty_states_for_comparison state_space;
			);
			
	(**********************************************************************************************************************)
	(**********************************************************************************************************************)
	(**********************************************************************************************************************)
	(**********************************************************************************************************************)
	(***                                        BEGIN ALGORITHM-SPECIFIC CODE                                             ***)
	(**********************************************************************************************************************)
	(**********************************************************************************************************************)
	(**********************************************************************************************************************)
	(**********************************************************************************************************************)
	(**********************************************************************************************************************)
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
	(**********************************************************************************************************************)
	(**********************************************************************************************************************)
	(**********************************************************************************************************************)
	(**********************************************************************************************************************)
	(***                                          END ALGORITHM-SPECIFIC CODE                                             ***)
	(**********************************************************************************************************************)
	(**********************************************************************************************************************)
	(**********************************************************************************************************************)
	(**********************************************************************************************************************)
	(**********************************************************************************************************************)
			
			(* Print some memory information *)
			if options#statistics then(
				(*** TODO ***)
			);
			
			(* Clean up a little *)
			(*** NOTE: LOOKS LIKE COMPLETELY USELESS !!! it even increases memory x-( ***)
			Gc.major ();
			
			(* Go one step deeper *)
			current_depth <- current_depth + 1;
			
			(* Check if the limit has been reached *)
			limit_reached := self#check_bfs_limit;
			
			(* If still going, ask the concrete algorithm whether it wants to terminate for other reasons *)
			if !limit_reached = Keep_going then(
				(* Print some information *)
				(*** HACK: 'current_depth - 1' because current_depth was just incremented... ***)
				self#print_algo_message Verbose_low("Checking termination at post^" ^ (string_of_int (current_depth - 1)) ^ "...");

				if self#check_termination_at_post_n then(
					algorithm_keep_going := false;
				);
			);
			
		done;
		
		(* Were they any more states to explore? *)
		let nb_unexplored_successors = List.length !post_n in
		
		(* Update termination condition *)
		begin
		match !limit_reached with
			(* No limit: regular termination *)
			| Keep_going -> termination_status <- Some (Regular_termination)
			(* Termination due to time limit reached *)
			| Time_limit_reached -> termination_status <- Some (Time_limit nb_unexplored_successors)
			
			(* Termination due to state space depth limit reached *)
			| Depth_limit_reached -> termination_status <- Some (Depth_limit nb_unexplored_successors)
			
			(* Termination due to a number of explored states reached *)
			| States_limit_reached -> termination_status <- Some (States_limit nb_unexplored_successors)
		end
		;
	
		(* Print some information *)
		(*** NOTE: must be done after setting the limit (above) ***)
		self#print_warnings_limit ();
		
		if not !algorithm_keep_going && nb_unexplored_successors > 0 then(
			self#print_algo_message Verbose_standard ("A sufficient condition to ensure termination was met although there were still " ^ (string_of_int nb_unexplored_successors) ^ " state" ^ (s_of_int nb_unexplored_successors) ^ " to explore");
		);


		print_message Verbose_standard (
			let nb_states = StateSpace.nb_states state_space in
			let nb_transitions = StateSpace.nb_transitions state_space in
			let fixpoint_str = if nb_unexplored_successors > 0 then "State space exploration stopped" else "Fixpoint reached" in
			"\n" ^ fixpoint_str ^ " at a depth of "
			^ (string_of_int current_depth) ^ ""
			^ ": "
			^ (string_of_int nb_states) ^ " state" ^ (s_of_int nb_states)
			^ " with "
			^ (string_of_int nb_transitions) ^ " transition" ^ (s_of_int nb_transitions) ^ " explored.");
			(*** NOTE: in fact, more states and transitions may have been explored (and deleted); here, these figures are the number of states in the state space. ***)

		(* Return the algorithm-dependent result *)
		self#compute_result
		
		(*** TODO: split between process result and return result; in between, add some info (algo_name finished after....., etc.) ***)

	
(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
