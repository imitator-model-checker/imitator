(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * 
 * Module description: Parametric deadlock-freeness
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * File contributors : Étienne André
 * Created           : 2016/02/08
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
open AlgoStateBased (* for type UnexSucc_some *)
open AlgoPostStar
open State


(************************************************************)
(************************************************************)
(* Class-independent functions *)
(************************************************************)
(************************************************************)

(* Convert a state_index for pretty-printing purpose *)
let debug_string_of_state state_index =
	"s_" ^ (string_of_int state_index)


(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoDeadlockFree =
	object (self) inherit algoPostStar as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	(* Non-necessarily convex parameter constraint for which deadlocks may arise *)
	val mutable bad_constraint : LinearConstraint.p_nnconvex_constraint = LinearConstraint.false_p_nnconvex_constraint ()

	(* Convex parameter constraint ensuring all clocks and parameters are non-negative (constant object used as a shortcut, as it is often used in the algorithm) *)
	
	(*** NOTE/TODO: technically, clocks should be non-negative, but parameters should just be conform to the initial p_constraint ***)
	
	val all_clocks_and_parameters_nonnegative : LinearConstraint.px_linear_constraint =
		(* Retrieve the model *)
		let model = Input.get_model() in
		(* Find clocks and parameters *)
		let clocks_and_parameters = list_union model.clocks model.parameters in
		(* Constrain non-negative *)
		LinearConstraint.px_constraint_of_nonnegative_variables clocks_and_parameters

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "Parametric deadlock-freeness checking"
	
	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
		super#initialize_variables;
		
		self#print_algo_message Verbose_low "Initializing variables…";
		
		bad_constraint <- LinearConstraint.false_p_nnconvex_constraint ();

		(* Print some information *)
		if verbose_mode_greater Verbose_low then(
			self#print_algo_message Verbose_low ("The global bad constraint is now:\n" ^ (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names bad_constraint));
		);
		
		(* The end *)
		()
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Computing the p_nnconvex_constraint for which there may exist a deadlock from a given state; the second argument is the list of successors (in case we may want to consider not all successors, typically in backward exploration) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private compute_deadlock_p_constraint state_index (successors : (StateSpace.combined_transition * State.state_index) list) : LinearConstraint.p_nnconvex_constraint =
	
		(* Define a local constraint storing the union of PX-constraints allowing to leave s *)
		let good_constraint_s = LinearConstraint.false_px_nnconvex_constraint () in
		
		(* Get the location and the constraint of s *)
		let state : state = StateSpace.get_state state_space state_index in
		let s_location, s_constraint = state.global_location, state.px_constraint in
		
		(* For all state s' in the successors of s *)
		List.iter (fun (combined_transition, state_index') ->
		
			(* Print some information *)
			if verbose_mode_greater Verbose_medium then(
				self#print_algo_message Verbose_medium ("Considering transition from state " ^ (string_of_int state_index) ^ " via action '" ^ (model.action_names (StateSpace.get_action_from_combined_transition combined_transition)) ^ "' to state " ^ (string_of_int state_index') ^ "…");
			);
			
			let precondition = DeadlockExtra.dl_weakest_precondition state_space state_index combined_transition state_index' in
			self#print_algo_message Verbose_medium ("Direct Precondition:\n" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names precondition));
			DeadlockExtra.dl_inverse_time state_space state_index precondition;
			self#print_algo_message Verbose_medium ("Timed  Precondition:\n" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names precondition));
			let precondition_px = DeadlockExtra.dl_instantiate_discrete state_space state_index precondition in
			self#print_algo_message Verbose_medium ("Hidden Precondition:\n" ^ (LinearConstraint.string_of_px_linear_constraint model.variable_names precondition_px));

			(* Update the local constraint by adding the new constraint as a union *)
			(*** WARNING: ugly (and expensive) to convert from pxd to px ***)
			(*** NOTE: still safe since discrete values are all instantiated ***)
			LinearConstraint.px_nnconvex_px_union_assign good_constraint_s precondition_px;
			
			(* Print some information *)
			if verbose_mode_greater Verbose_medium then(
				self#print_algo_message Verbose_medium ("The local good constraint (allowing exit) is:\n" ^ (LinearConstraint.string_of_px_nnconvex_constraint model.variable_names good_constraint_s));
			);
		) successors;
			
(*			(* Compute the difference True^+ \ good_constraint_s *)
		(*** TODO: add a field clocks_and_parameters to abstract_model ***)
		let trueplus = LinearConstraint.px_constraint_of_nonnegative_variables (list_union model.clocks model.parameters) in
		let px_bad_constraint_s = LinearConstraint.px_nnconvex_constraint_of_px_linear_constraint trueplus in
		
		(* Print some information *)
		if verbose_mode_greater Verbose_medium then(
			self#print_algo_message Verbose_medium ("px_bad_constraint_s ('trueplus') is now: " ^ (LinearConstraint.string_of_px_nnconvex_constraint model.variable_names px_bad_constraint_s));
		);

		LinearConstraint.px_nnconvex_difference px_bad_constraint_s good_constraint_s;

		(* Print some information *)
		if verbose_mode_greater Verbose_low then(
			self#print_algo_message Verbose_low ("px_bad_constraint_s (True \ good, not allowing exit) is now: " ^ (LinearConstraint.string_of_px_nnconvex_constraint model.variable_names px_bad_constraint_s));
		);
		
		*)

		(* Compute the difference s \ good_constraint_s *)
		let nnconvex_s = LinearConstraint.px_nnconvex_constraint_of_px_linear_constraint s_constraint in
		
		(* Print some information *)
		if verbose_mode_greater Verbose_high then(
			self#print_algo_message Verbose_high ("nnconvex_s (= s) is:\n" ^ (LinearConstraint.string_of_px_nnconvex_constraint model.variable_names nnconvex_s));
		);

		LinearConstraint.px_nnconvex_difference_assign nnconvex_s good_constraint_s;
		
		(* Print some information *)
		if verbose_mode_greater Verbose_high then(
			self#print_algo_message Verbose_high ("nnconvex_s (s \ good, not allowing exit) is:\n" ^ (LinearConstraint.string_of_px_nnconvex_constraint model.variable_names nnconvex_s));
		);
		
		(* Ensure clocks and parameters are not negative *)
		LinearConstraint.px_nnconvex_intersection_assign nnconvex_s all_clocks_and_parameters_nonnegative;
		
		
		(* Print some information *)
		if verbose_mode_greater Verbose_high then(
			self#print_algo_message Verbose_high ("After constraining clocks and parameters to be positive, nnconvex_s (s \ good, not allowing exit) is now:\n" ^ (LinearConstraint.string_of_px_nnconvex_constraint model.variable_names nnconvex_s));
		);
		
		(* Project onto the parameters *)
		let p_bad_constraint_s = LinearConstraint.px_nnconvex_hide_nonparameters_and_collapse nnconvex_s in
			
		(* Print some information *)
		if verbose_mode_greater Verbose_medium then(
			self#print_algo_message Verbose_medium ("p_bad_constraint_s (s \ good, not allowing exit, projected onto P) is:\n" ^ (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names p_bad_constraint_s));
		);
		
		(* Return the p_constraint *)
		p_bad_constraint_s

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Actions to perform at the end of the computation of the *successors* of post^n (i.e., when this method is called, the successors were just computed). Nothing to do for this algorithm. *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_post_n (post_n : State.state_index list) =
		
		self#print_algo_message Verbose_medium "Entering process_post_n";
		
		(* For all state s in post^n *)
		List.iter (fun state_index ->
			(* Retrieve all successors of this state with their action *)
			let succs_of_s = StateSpace.get_successors_with_combined_transitions state_space state_index in
			
			let p_bad_constraint_s = self#compute_deadlock_p_constraint state_index succs_of_s in
			
			(* Update the bad constraint using the local constraint *)
			LinearConstraint.p_nnconvex_union_assign bad_constraint p_bad_constraint_s;
		
			(* Print some information *)
			if verbose_mode_greater Verbose_medium then(
				self#print_algo_message Verbose_medium ("The global bad constraint is now:\n" ^ (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names bad_constraint));
			);
		) post_n;

		(* Print some information *)
		if verbose_mode_greater Verbose_low then(
			self#print_algo_message Verbose_low ("After processing post^n, the global bad constraint is now: " ^ (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names bad_constraint));
		);
		
		print_message Verbose_low ("");

		(* The end *)
		()


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Check whether the algorithm should terminate at the end of some post, independently of the number of states to be processed (e.g., if the constraint is already true or false) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method check_termination_at_post_n =
		(* Print some information *)
		self#print_algo_message Verbose_high ("Entering check_termination_at_post_n…");

		(* Retrieve the initial parameter constraint *)
		let initial_p_nnconvex_constraint : LinearConstraint.p_nnconvex_constraint = self#get_initial_p_nnconvex_constraint_or_die in
		
		(* True if the computed bad constraint is exactly equal to (or larger than) the initial parametric constraint *)
		let stop = LinearConstraint.p_nnconvex_constraint_is_leq initial_p_nnconvex_constraint bad_constraint in
		
		(* Print some information *)
		if (stop && verbose_mode_greater Verbose_medium) || verbose_mode_greater Verbose_medium then(
			self#print_algo_message Verbose_medium ("Initial constraint:\n" ^ (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names initial_p_nnconvex_constraint));
			self#print_algo_message Verbose_medium ("Current bad constraint:\n" ^ (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names bad_constraint));
		);
		
		(* Print some information *)
		if stop then(
			self#print_algo_message Verbose_standard ("None of the parameter valuations compatible with the initial state is deadlock-free: terminate.");
		)else(
			self#print_algo_message Verbose_medium ("The bad constraint is not greater than or equal to the initial state: no termination required for now.");
		);
		
		(* Return result *)
		stop

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method to compute an under-approximation in a backward manner, when the analysis stopped prematurely *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private backward_underapproximation =
		self#print_algo_message_newline Verbose_low "Retrieving successors…";

		(* Retrieve predecessors *)
		let predecessors_table = StateSpace.compute_predecessors_with_combined_transitions state_space in
		
		(* Retrieve all state indexes *)
		let all_state_indexes = StateSpace.all_state_indexes state_space in
		
		(*------------------------------------------------------------*)
		(* Print successors and predecessors *)
		if verbose_mode_greater Verbose_high then(
			(* Successors *)
			self#print_algo_message_newline Verbose_high ("SUCCESSORS");
			(* Iterate on all states in the state space *)
			List.iter (fun state_index ->
				self#print_algo_message_newline Verbose_high ("State " ^ (string_of_int state_index) ^ ":");
				(* Retrieve successors *)
				let successors = StateSpace.get_successors_with_combined_transitions state_space state_index in
				(* Print each of them *)
				List.iter (fun (combined_transition, state_index') -> 
					self#print_algo_message Verbose_high ("- " ^ (string_of_int state_index') ^ " (via action " ^ (model.action_names (StateSpace.get_action_from_combined_transition combined_transition)) ^ ")");
				) successors;
			) all_state_indexes;

			(* Predecessors *)
			self#print_algo_message_newline Verbose_high ("PREDECESSORS");
			(* Iterate on all states in the state space *)
			List.iter (fun state_index ->
				self#print_algo_message_newline Verbose_high ("State " ^ (string_of_int state_index) ^ ":");
				(* Retrieve predecessors *)
				let predecessors = Array.get predecessors_table state_index in
				(* Print each of them *)
				List.iter (fun (combined_transition, state_index') -> 
					self#print_algo_message Verbose_high ("- " ^ (string_of_int state_index') ^ " (via action " ^ (model.action_names (StateSpace.get_action_from_combined_transition combined_transition)) ^ ")");
				) predecessors;
			) all_state_indexes;
		); (* end if verbose_mode >= high *)
		(*------------------------------------------------------------*)
		
		(* Retrieve states with unexplored successors *)
		let unexplored_successors =
		match unexplored_successors with
			| UnexSucc_undef -> raise (InternalError "Impossible situation: unexplored_successors should not be undefined at that point (method backward_underapproximation)")
			| UnexSucc_some l -> l
		in

		(* Define a set of (currently) marked state_index *)
		let marked = new State.stateIndexSet in

		(* Define a set of state_index, that were marked once, and hence are disabled forever *)
		let disabled = new State.stateIndexSet in

		(* Mark all states with potential successors *)
		List.iter marked#add unexplored_successors;
		
		(* Initialize variables before backward exploration *)
		let current_marked_states = ref (marked#all_elements) in
		
		(* Only for debug and pretty-printing purpose *)
		let iteration = ref 1 in
		
		(* While there are some marked states *)
		while List.length !current_marked_states > 0 do
		
			(* Print some information *)
			if verbose_mode_greater Verbose_low then(
				print_message Verbose_medium ("\n------------------------------------------------------------");
				self#print_algo_message Verbose_low ("Backward underapproximation: iteration " ^ (string_of_int !iteration) ^ " (" ^ (string_of_int (List.length !current_marked_states)) ^ " marked state" ^ (s_of_int (List.length !current_marked_states)) ^ ")");
				
				self#print_algo_message_newline Verbose_medium ("List of marked states: ");
				self#print_algo_message Verbose_medium (string_of_list_of_string_with_sep " - " (List.map debug_string_of_state !current_marked_states));
			);

			(*------------------------------------------------------------*)
			(* Step 1: Negate the constraint associated with marked states *)

			(* Print some information *)
			self#print_algo_message_newline Verbose_low ("Negating constraints assocated with marked states…");

			List.iter (fun state_index -> 
				(* Only consider this state if it is not already disabled *)
				if not (disabled#mem state_index) then(
					
					(* Retrieve the state constraint *)
					let s_constraint = (StateSpace.get_state state_space state_index).px_constraint in
					
					(* Project onto the parameters *)
					let p_constraint = LinearConstraint.px_hide_nonparameters_and_collapse s_constraint in
					
					(* Remove its constraint, i.e., add not C to bad *)
					LinearConstraint.p_nnconvex_p_union_assign bad_constraint p_constraint;
					
					(* Print some information *)
					if verbose_mode_greater Verbose_medium then(
						self#print_algo_message Verbose_medium ("Negating the constraint associated with state " ^ (debug_string_of_state state_index) ^ ":");
						
						self#print_algo_message Verbose_medium (LinearConstraint.string_of_p_linear_constraint model.variable_names p_constraint);
						
						self#print_algo_message Verbose_medium ("The global bad constraint is now:\n" ^ (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names bad_constraint));
					);

				) (* end if not a member of marked *)
				else(
					(* Print some information *)
					self#print_algo_message Verbose_medium ("State " ^ (debug_string_of_state state_index) ^ " already disabled: skip");
				);
			) !current_marked_states;
			
			
			(*------------------------------------------------------------*)
			(* Step 2: Compute predecessors of marked states *)

			(* Print some information *)
			self#print_algo_message_newline Verbose_low ("Computing predecessors of marked states…");

			let predecessors_of_marked = new State.stateIndexSet in
			List.iter (fun state_index -> 
				(* Only consider this state if it is not already disabled *)
				if not (disabled#mem state_index) then(
					(* Find its predecessors *)
					let local_predecessors = Array.get predecessors_table state_index in
					(* Add them *)
					List.iter (fun (_ , state_index ) -> predecessors_of_marked#add state_index) local_predecessors;
				) (* end if not a member of marked *)
			) !current_marked_states;
			
			(* Get disabled in the form of a list (useful in the loop below) *)
			let disabled_list = disabled#all_elements in
			
			(* Create the set of newly marked states (for the next iteration) *)
			let newly_marked = new State.stateIndexSet in
			

			(*------------------------------------------------------------*)
			(* Step 3: Update the deadlock-freeness constraint for all predecessors of marked states *)
			
			(* Print some information *)
			self#print_algo_message_newline Verbose_low ("Updating the deadlock-freeness constraint for all predecessors of marked states…");

			List.iter (fun state_index ->
				(* Find its successors *)
				let successors = StateSpace.get_successors_with_combined_transitions state_space state_index in
				(* Remove the disabled successors *)
				let not_disabled_successors = List.filter (fun (_, state_index) -> not (List.mem state_index disabled_list)) successors in
				
				(* Compute the deadlock constraint *)
				let p_bad_constraint_s = self#compute_deadlock_p_constraint state_index not_disabled_successors in
			
				(* Update the bad constraint using the local constraint *)
				LinearConstraint.p_nnconvex_union_assign bad_constraint p_bad_constraint_s;
			
				(* Print some information *)
				if verbose_mode_greater Verbose_medium then(
					self#print_algo_message Verbose_medium ("The global bad constraint is now:\n" ^ (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names bad_constraint));
				);
				
				
				(* If the state has now an empty constraint (and is not yet marked), then mark it for the next iteration *)
				if not (newly_marked#mem state_index) then(
					
					(* Retrieve the state constraint *)
					let s_constraint = (StateSpace.get_state state_space state_index).px_constraint in
					(* Project onto the parameters *)
					(*** TO OPTIMIZE: this projection was (maybe) already computed earlier; use a cache??? ***)
					let p_constraint = LinearConstraint.px_hide_nonparameters_and_collapse s_constraint in
					
					(*** NOTE: p_constraint \leq bad <=> p_constraint \ bad = false ***)
					(*** WARNING: certainly very expensive conversion and test ***)
					(*** WARNING: is this test enough? can't the state be "false" while not being included into bad_constraint? (e.g., if the initial p_constraint is not true) ***)
					if LinearConstraint.p_nnconvex_constraint_is_leq (LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint p_constraint) bad_constraint then(
						(* Mark it *)
						newly_marked#add state_index
					);
				);
				
			) predecessors_of_marked#all_elements;
			

			(*------------------------------------------------------------*)
			(* Step 4: Disable marked states *)

			(* Print some information *)
			self#print_algo_message_newline Verbose_low ("Disabling marked states…");

			List.iter disabled#add !current_marked_states;
			
			
			(*------------------------------------------------------------*)
			(* Step 5: Update marked states: newly marked = {predecessors of marked} \ {marked} *)
			(*** NOTE: no need to mark states already marked, as they have been processed (and disabled) ***)
			marked#empty;
			List.iter marked#add newly_marked#all_elements;
			List.iter marked#remove_or_do_nothing !current_marked_states;
			
			(* Update variable *)
			current_marked_states := marked#all_elements;
			
			(* Update iteration *)
			iteration := !iteration + 1;
			
		done; (* end while there are marked states *)
		
		(* Print some information *)
		if verbose_mode_greater Verbose_low then(
			self#print_algo_message_newline Verbose_low ("No marked state anymore. The end.");
			let nb_disabled = disabled#cardinal in
			self#print_algo_message Verbose_low ((string_of_int nb_disabled) ^ " state" ^ (s_of_int nb_disabled) ^ " disabled.");
		);
		
		(* The end *)
		()
		
			

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_result =
		self#print_algo_message_newline Verbose_standard (
			"Algorithm completed " ^ (after_seconds ()) ^ "."
		);
		
		self#print_algo_message_newline Verbose_low (
			"Performing negation of final constraint…"
		);
		
		(* Retrieve the initial parameter constraint *)
		let initial_p_nnconvex_constraint : LinearConstraint.p_nnconvex_constraint = self#get_initial_p_nnconvex_constraint_or_die in

		(* Perform result = initial_state|P \ bad_constraint *)
		let result = LinearConstraint.p_nnconvex_copy initial_p_nnconvex_constraint in

		self#print_algo_message_newline Verbose_high(
			"Initial parameter constraint:\n" ^
			(LinearConstraint.string_of_p_nnconvex_constraint model.variable_names result)
		);

		LinearConstraint.p_nnconvex_difference_assign result bad_constraint;
		
		self#print_algo_message_newline Verbose_high(
			"After negation:\n" ^
			(LinearConstraint.string_of_p_nnconvex_constraint model.variable_names result)
		);



		self#print_algo_message_newline Verbose_medium (
			"Negation of final constraint completed."
		);
		
		(* Get the termination status *)
		 let termination_status = match termination_status with
			| None -> raise (InternalError "Termination status not set in AlgoDeadlockFree.compute_result")
			| Some status -> status
		in
		
		(* Constraint is exact if termination is normal, possibly over-approximated otherwise (since we compute the negation) *)
		let soundness = if termination_status = Regular_termination then Constraint_exact else Constraint_maybe_over in

		let result =
		(* If exact: everything is fine *)
		if termination_status = Regular_termination then(
			Good_constraint (result, soundness)
		)
		(* Else: compute backward under-approximation *)
		else(
			(* Print some information *)
			if verbose_mode_greater Verbose_low then(
				self#print_algo_message_newline Verbose_low "Over-approximated constraint:";
				self#print_algo_message Verbose_low (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names result);
			);
			
			self#print_algo_message_newline Verbose_standard "Starting backward under-approximation…";
			
			(* Update the constraint so as to obtain an under-approximation in addition to the over-approximation *)
			self#backward_underapproximation;
			
			self#print_algo_message Verbose_standard (
				"Backward under-approximation completed " ^ (after_seconds ()) ^ "."
			);
			
			self#print_algo_message_newline Verbose_low (
				"Performing negation of final under-approximated constraint…"
			);
			
			(* Perform result = initial_state|P \ bad_constraint *)
			let good_under_result = LinearConstraint.p_nnconvex_copy initial_p_nnconvex_constraint in
			LinearConstraint.p_nnconvex_difference_assign good_under_result bad_constraint;
			
			self#print_algo_message_newline Verbose_medium (
				"Negation of final under-approximated constraint completed."
			);
			
			(* Test if under=over (in which case the result is exact *)
			if LinearConstraint.p_nnconvex_constraint_is_equal good_under_result result then(
				(* Print some information *)
				self#print_algo_message_newline Verbose_standard (
					"Under-approximation is equal to over-approximation: result is exact."
				);
			
				(* Then the constraint is exact *)
				Good_constraint (result, Constraint_exact)
			
			)else(
				(* A possibly under-approximated good constraint, and a possibly over-approximated bad constraint *)
				Good_bad_constraint {
					good	= (good_under_result, Constraint_maybe_under);
					bad		= (bad_constraint, Constraint_maybe_over);
				}
			)
			
		) (* end if not regular termination *)
		in

		(* Return the result *)
		Single_synthesis_result
		{
			(* Non-necessarily convex constraint guaranteeing the non-reachability of the bad location *)
			result				= result;
			
			(* English description of the constraint *)
			constraint_description = "constraint guaranteeing deadlock-freeness";
	
			(* Explored state space *)
			state_space			= state_space;
			
			(* Total computation time of the algorithm *)
			computation_time	= time_from start_time;
			
			(* Termination *)
			termination			= termination_status;
		}


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
