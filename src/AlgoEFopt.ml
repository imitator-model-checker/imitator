(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: "EF optimized" algorithm: minimization or minimization of a parameter valuation for which there exists a run leading to some states [ABPP19]
 * 
 * File contributors : Étienne André
 * Created           : 2017/05/02
 * Last modified     : 2020/09/14
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
open AbstractProperty
open Result
open AlgoStateBased
open Statistics
open State


(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class virtual algoEFopt (state_predicate : AbstractProperty.state_predicate) (parameter_index : Automaton.parameter_index) =
	object (self) inherit algoStateBased as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	(*------------------------------------------------------------*)
	(* Class "parameters" to be initialized *)
	(*------------------------------------------------------------*)
	val mutable synthesize_valuations : bool option = None

	(*------------------------------------------------------------*)
	(* Variables *)
	(*------------------------------------------------------------*)
	
	val mutable current_optimum : LinearConstraint.p_linear_constraint option = None
	
	val mutable negated_optimum  : LinearConstraint.p_linear_constraint option = None
	
	(* Parameter valuations in all |P| dimensions for which the optimum is reached *)
	val mutable current_optimum_valuations : LinearConstraint.p_nnconvex_constraint option = None
	
	(*------------------------------------------------------------*)
	(* Timing info *)
	(*------------------------------------------------------------*)
	
    val mutable t_start = ref max_float; (* Start time for t_found and t_done *)
    val mutable t_found = ref max_float; (* Time to the first time that the target location is reached *)
    val mutable t_done = ref max_float; (* Time to the end of the algorithm *)
	
	(*------------------------------------------------------------*)
	(* Shortcuts *)
	(*------------------------------------------------------------*)
	
	val parameters_to_hide =
			OCamlUtilities.list_remove_first_occurence parameter_index (Input.get_model ()).parameters
	

	(*------------------------------------------------------------*)
	(* Counters *)
	(*------------------------------------------------------------*)
	
	(* State discarded because of a not interesting parameter constraint *)
	val counter_discarded_state = create_discrete_counter_and_register "EFopt:state discarded" PPL_counter Verbose_low

	
	(************************************************************)
	(* Class methods *)
	(************************************************************)
	(*------------------------------------------------------------*)
	(* Instantiating min/max *)
	(*------------------------------------------------------------*)
	(* Function to remove upper bounds (if minimum) or lower bounds (if maximum) *)
	method virtual remove_bounds : Automaton.parameter_index list -> Automaton.parameter_index list -> LinearConstraint.p_linear_constraint -> unit
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Function to negate an inequality (to be defined in subclasses) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual negate_inequality : LinearConstraint.p_linear_constraint -> LinearConstraint.p_linear_constraint

	(* The closed operator (>= for minimization, and <= for maximization) *)
	method virtual closed_op : LinearConstraint.op

	(* Various strings *)
	method virtual str_optimum : string
	method virtual str_upper_lower : string

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Set the 'synthesize_valuations' flag (must be done right after creating the algorithm object!) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method set_synthesize_valuations flag =
		synthesize_valuations <- Some flag

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Shortcuts methods *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private get_synthesize_valuations =
		match synthesize_valuations with
		| Some flag -> flag
		| None -> raise (InternalError "Variable 'synthesize_valuations' not initialized in AlgoEFopt although it should have been at this point")
		
	method private get_current_optimum =
		match current_optimum with
		| Some optimum -> optimum
		| None -> raise (InternalError "Variable 'current_optimum' not initialized in AlgoEFopt although it should have been at this point")
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Compute the p-constraint of a state, projected onto the parameter to be optimized *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private project_constraint px_constraint =
		(* Project the constraint onto that parameter *)
		let projected_constraint = LinearConstraint.px_hide_allclocks_and_someparameters_and_collapse parameters_to_hide px_constraint in
		
		(* Return result *)
		projected_constraint


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Check if goal state *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private is_goal_state state =
		
		(* Print some information *)
		self#print_algo_message Verbose_total "Entering AlgoEFopt:is_goal_state…";
		
		(* Check the state_predicate *)

		State.match_state_predicate model.is_accepting state_predicate state


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Update the current optimum *)
	(*** WARNING: side effect on projected_constraint ***)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private update_optimum projected_constraint =
		(* Print some information *)
		if verbose_mode_greater Verbose_low then(
			self#print_algo_message Verbose_medium "Associated constraint:";
			self#print_algo_message Verbose_medium (LinearConstraint.string_of_p_linear_constraint model.variable_names projected_constraint);
			self#print_algo_message Verbose_medium ("Removing " ^ self#str_upper_lower ^ " bound…");
		);

		(* Relax the constraint, i.e., grow to infinity (for minimization) or to zero (for maximization) *)
		self#remove_bounds [parameter_index] parameters_to_hide projected_constraint;
			
		(* Print some information *)
		if verbose_mode_greater Verbose_standard then(
			self#print_algo_message Verbose_low ("Updating the " ^ self#str_optimum ^ ":");
			self#print_algo_message Verbose_standard (LinearConstraint.string_of_p_linear_constraint model.variable_names projected_constraint);
		);
		
		(* Update the min *)
		current_optimum <- Some projected_constraint;
		
		let new_negated_optimum = self#negate_inequality projected_constraint in
		
		(* Print some information *)
		if verbose_mode_greater Verbose_low then(
			self#print_algo_message_newline Verbose_low ("New negated optimum: " ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names new_negated_optimum));
		);
		
		(* Update the negated optimum too *)
		negated_optimum <- Some new_negated_optimum
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Update the current optimum by updating it by union *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private update_optimum_valuations px_constraint =
		(* Get the updated optimum constraint *)
		let current_optimum_constraint = self#get_current_optimum in
		
		(* Compute the projection onto all parameters *)
		let projected_constraint_onto_P = LinearConstraint.px_hide_nonparameters_and_collapse px_constraint in

		(* Print some information *)
		if verbose_mode_greater Verbose_high then(
			self#print_algo_message_newline Verbose_high ("Considering the following constraint: " ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names projected_constraint_onto_P));
		);
		
		(* Intersect with the optimum *)
		LinearConstraint.p_intersection_assign projected_constraint_onto_P [current_optimum_constraint];
		
		(* Print some information *)
		if verbose_mode_greater Verbose_high then(
			self#print_algo_message_newline Verbose_high ("After intersection with the optimum, about to add to the optimum valuations: " ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names projected_constraint_onto_P));
		);
		
		(* Add to the collected current_optimum_constraint *)
		match current_optimum_valuations with
		| Some current_optimum_valuations ->
			LinearConstraint.p_nnconvex_p_union_assign current_optimum_valuations projected_constraint_onto_P;
			(* Print some information *)
			if verbose_mode_greater Verbose_low then(
				self#print_algo_message_newline Verbose_low ("New " ^ self#str_optimum ^ " constraint after addition: " ^ (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names current_optimum_valuations));
			);
		| None -> raise (InternalError "Variable 'current_optimum_valuations' not initialized in AlgoEFopt although it should have been at this point")

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Update the current optimum by replacing it *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private replace_optimum_valuations px_constraint =
		(* Replace the current synthesized valuations with the new ones *)
						
		(* Get the updated optimum constraint *)
		let current_optimum_constraint = self#get_current_optimum in

		(* Compute the projection onto all parameters *)
		let projected_constraint_onto_P = LinearConstraint.px_hide_nonparameters_and_collapse px_constraint in
		(* Intersect with the optimum *)
		LinearConstraint.p_intersection_assign projected_constraint_onto_P [current_optimum_constraint];
		
		(* Replace *)
		current_optimum_valuations <- Some (LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint projected_constraint_onto_P);

		(* Print some information *)
		if verbose_mode_greater Verbose_low then(
			self#print_algo_message_newline Verbose_low ("New " ^ self#str_optimum ^ " constraint after replacement: " ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names projected_constraint_onto_P));
		);
		
		(* The end *)
		()
		
		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Actions to perform when trying to minimize/maximize a parameter. Returns true if the same should be kept, false if discarded. *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private process_state (state : state) = 
		(* Print some information *)
		if verbose_mode_greater Verbose_high then(
			self#print_algo_message Verbose_high "Entering AlgoEFopt:process_state…";
		);

		(* Retrieve the constraint *)
		let state_location, px_constraint = state.global_location, state.px_constraint in
		
		(* Check if an optimum constraint was defined *)
		match current_optimum with
		| None ->
			(* Print some information *)
			if verbose_mode_greater Verbose_high then(
				self#print_algo_message Verbose_high "No optimum known for now";
			);
			
			(* If goal state, update the constraint *)
			let is_goal_state = self#is_goal_state state in
			if is_goal_state then(
				(* Compute the projection *)
				let projected_constraint = self#project_constraint px_constraint in
				
				self#print_algo_message Verbose_standard ("Found a first " ^ self#str_optimum);
				
				self#update_optimum projected_constraint;
				
				(* Case synthesis *)
				if self#get_synthesize_valuations then(
					self#replace_optimum_valuations px_constraint;
				);
				
				(* Timing info *)
				if !t_found = max_float then (
					t_found := time_from !t_start;
					print_message Verbose_standard ("t_found: " ^ (string_of_seconds !t_found));
				);
				
			)else(
				(* Print some information *)
				self#print_algo_message Verbose_medium ("Not yet a goal state");
			
			);

			(* Keep the state only if not a goal state *)
			(*** NOTE: here, we cannot use the optimum to update the state ***)
			not is_goal_state

		| Some current_optimum_constraint ->
			(* Print some information *)
			if verbose_mode_greater Verbose_high then(
				self#print_algo_message Verbose_high "An optimum already exists";
			);

			(*** NOTE: this is an expensive test, as ALL states will be projected to the goal parameters and compared to the current optimum ***)
			(*** TODO: try with emptiness of intersection? ***)
			let projected_constraint = self#project_constraint px_constraint in

			(* Test if the current optimum is already larger *)
			if LinearConstraint.p_is_leq projected_constraint current_optimum_constraint then(
				(* Print some information *)
				if verbose_mode_greater Verbose_high then(
					self#print_algo_message Verbose_high "The known optimum is already better than the new state: discard";
				);

				(* Statistics *)
				counter_discarded_state#increment;
				
				(* Flag that might be set to false in the following if condition *)
				let discard = ref true in
				
				(* Case synthesis AND goal location *)
				if self#get_synthesize_valuations then(
					(* Print some information *)
					if verbose_mode_greater Verbose_high then(
						self#print_algo_message Verbose_high "…but since we want synthesis, still checks whether the optimum is *equal*";
	
						self#print_algo_message Verbose_total ("About to compare:\n" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names projected_constraint) ^ "\n=?=\n" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names current_optimum_constraint) ^ "…");
					);
					
					(* If optimum is equal: still add the p-constraint *)
					
					(*** NOTE: this part is a bit technical: the current_optimum_constraint is necessarily of the form p >= n or p > n (for EFmin), while the projected_constraint might be of the form p = i, or i <= p <= i'; if i=n then and large inequalities are used, then the projected_constraint is still as good as the current_optimum_constraint. We therefore use the remove_bounds function for projected_constraint. ***)
					
					(* Apply extrapolation *)
					(*** WARNING: do not copy only because this object is not used anymore afterwards ***)
					let projected_constraint_extrapolated = (*LinearConstraint.p_copy*) projected_constraint in
					self#remove_bounds [parameter_index] [] projected_constraint_extrapolated;
					
					(* Print some information *)
					if verbose_mode_greater Verbose_high then(
						self#print_algo_message Verbose_high ("Extrapolation of the new state optimum:\n" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names projected_constraint_extrapolated) ^ "");
					);
					
					if LinearConstraint.p_is_equal projected_constraint_extrapolated current_optimum_constraint then(
						(* Print some information *)
						if verbose_mode_greater Verbose_high then(
							self#print_algo_message Verbose_high "Known optimum equal to that of the new state";
						);
						
						(* Don't discard because the optimum is exactly equivalent to the known optimum, so there may be interesting successors (recall that the state is not necessarily a target state!) *)
						discard := false;
						
						(* If goal location: update optimum! *)
						if self#is_goal_state state then(
							(* Print some information *)
							self#print_algo_message Verbose_medium ("This is a goal state: Update the optimum valuations");
							
							self#update_optimum_valuations px_constraint;
							
							(* Discard as nothing more interesting can be found that way because the state is already a target state *)
							discard := true;
						);
					)else(
						(* Print some information *)
						if verbose_mode_greater Verbose_high then(
							self#print_algo_message Verbose_high "Known optimum strictly better than that of the new state: really discard";
						);
						(* Redundant assignment (safety) *)
						discard := true;
					);
				);
				
				(* Print some information *)
				if verbose_mode_greater Verbose_total then(
					self#print_algo_message Verbose_total ("Discard? " ^ (string_of_bool !discard));
				);
				
				(* Discard state, i.e., do not keep it; EXCEPT if synthesis AND equivalent optimum, because we can find more constraints in that direction! *)
				not !discard
			(* Otherwise: keep the state *)
			)else(
				(* If goal state, update the constraint *)
				if self#is_goal_state state then(
				
					(* Print some information *)
					if verbose_mode_greater Verbose_medium then(
						self#print_algo_message Verbose_medium ("Goal state found!");
					
						self#print_algo_message_newline Verbose_medium ("Current " ^ self#str_optimum ^ ": " ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names current_optimum_constraint));
						self#print_algo_message_newline Verbose_medium ("New state projected constraint: " ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names projected_constraint));
					);
				
					self#print_algo_message Verbose_standard ("Found a better " ^ self#str_optimum);
				
					self#update_optimum projected_constraint;

					(* Case synthesis *)
					if self#get_synthesize_valuations then(
						self#replace_optimum_valuations px_constraint;
					);
					
					(* Hack: discard the state! Since no better successor can be found *)
					false
				)else(
				
					(* Print some information *)
					self#print_algo_message Verbose_medium ("Not a goal state");

					(* Keep the state, but add the negation of the optimum to squeeze the state space! (no need to explore the part with parameters smaller/larger than the optimum) *)
					(*** NOTE: not in synthesis mode ***)
					if not self#get_synthesize_valuations then(
						let negated_optimum = match negated_optimum with
							| Some negated_optimum -> negated_optimum
							| None -> raise (InternalError("A negated optimum should be defined at that point"))
						in
						
						(* Print some information *)
						if verbose_mode_greater Verbose_high then(
							self#print_algo_message_newline Verbose_high ("Intersecting state with: " ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names negated_optimum));
						);
						
						(* Intersect with side-effects *)
						LinearConstraint.px_intersection_assign_p px_constraint [negated_optimum];
					);
					
					(* Keep the state *)
					(*** NOTE: what if it becomes unsatisfiable? ***)
					true
				)
			)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Add a new state to the reachability_graph (if indeed needed) *)
	(* Return true if the state is not discarded by the algorithm, i.e., if it is either added OR was already present before *)
	(* Can raise an exception TerminateAnalysis to lead to an immediate termination *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** TODO: return the list of actually added states ***)
	method add_a_new_state source_state_index combined_transition new_state =
		(* Print some information *)
		if verbose_mode_greater Verbose_medium then(
			self#print_algo_message Verbose_medium "Entering AlgoEFopt:add_a_new_state…";
		);
		
		(* If we have to optimize a parameter, do that now *)
		let keep_processing = self#process_state new_state in

		(* Print some information *)
		if verbose_mode_greater Verbose_high then(
			self#print_algo_message Verbose_high ("New state to be kept? " ^ (string_of_bool keep_processing) ^ "");
		);
		
		(* Only process if we have to *)
		if keep_processing then(
			(* Try to add the new state to the state space *)
			let addition_result = StateSpace.add_state state_space (self#state_comparison_operator_of_options) new_state in
			
			begin
			match addition_result with
			(* If the state was present: do nothing *)
			| StateSpace.State_already_present _ -> ()
			(* If this is really a new state, or a state larger than a former state *)
			| StateSpace.New_state new_state_index | StateSpace.State_replacing new_state_index ->

				(* First check whether this is a bad tile according to the property and the nature of the state *)
				self#update_statespace_nature new_state;
				
				(* Will the state be added to the list of new states (the successors of which will be computed)? *)

				(* Add the state_index to the list of new states (used to compute their successors at the next iteration) *)
				if true then
					new_states_indexes <- new_state_index :: new_states_indexes;
				
			end (* end if new state *)
			;
			
			(*** TODO: move the rest to a higher level function? (post_from_one_state?) ***)
			
			(* Add the transition to the state space *)
			self#add_transition_to_state_space (source_state_index, combined_transition, (*** HACK ***) match addition_result with | StateSpace.State_already_present new_state_index | StateSpace.New_state new_state_index | StateSpace.State_replacing new_state_index -> new_state_index) addition_result;
		
			(* The state is kept in any case *)
			true
		)else(
			(* If state discarded after minimization: do not keep it *)
			false
		)
	

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Actions to perform with the initial state; returns true unless the initial state cannot be kept (in which case the algorithm will stop immediately) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_initial_state initial_state = (
        (* Timing info *)
        t_start := Unix.gettimeofday();
		self#process_state initial_state
	)

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Actions to perform when meeting a state with no successors: nothing to do for this algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_deadlock_state state_index = ()
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Actions to perform at the end of the computation of the *successors* of post^n (i.e., when this method is called, the successors were just computed). Nothing to do for this algorithm. *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_post_n (post_n : State.state_index list) = ()

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Check whether the algorithm should terminate at the end of some post, independently of the number of states to be processed (e.g., if the constraint is already true or false) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** TODO: could be stopped when the bad constraints are equal to the initial p-constraint ***)
	method check_termination_at_post_n = false

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_result =
		(* Print some information *)
		self#print_algo_message_newline Verbose_standard (
			"Algorithm completed " ^ (after_seconds ()) ^ "."
		);
	
        (* Timing info *)
        t_done := time_from !t_start;
        print_message Verbose_standard ("t_done:  " ^ (string_of_seconds !t_done));
		
		let result =
		(* Case synthesis: get the synthesized multidimensional constraint *)
		if self#get_synthesize_valuations then (
			
			(* Get the constraint *)
			match current_optimum_valuations with
				| None -> LinearConstraint.false_p_nnconvex_constraint()
				| Some current_optimum_valuations ->
					(*** NOTE: Here, if the optimum is of the form p >= c, we need to impose p = c, as the minimization is requested ***)
					
					(* First get the optimum (necessarily defined) *)
					let current_optimum = self#get_current_optimum in
					
					(* Get its operator and coefficient *)
					let (_, op, coefficient) = try(
						LinearConstraint.parameter_constraint_of_p_linear_constraint parameter_index current_optimum
					) with
						LinearConstraint.Not_a_1d_parameter_constraint -> raise (InternalError ("Problem when looking for a strict or non-strict optimum in AlgoEFopt:compute_result: the constraint " ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names current_optimum) ^  " is not of the expected form."))
					in
					
					(* Print some information *)
					if verbose_mode_greater Verbose_low then(
						self#print_algo_message Verbose_low ("The almost final optimum is: " ^ (model.variable_names parameter_index) ^ " " ^ (LinearConstraint.string_of_op op) ^ " " ^ (NumConst.string_of_numconst coefficient) ^ "");
					);
					
					(* If the optimum is a >=, then convert to equality *)
					if op = self#closed_op then(
						(* If closed op, we need to force equality *)
						
						(* Print some information *)
						if verbose_mode_greater Verbose_low then(
							self#print_algo_message Verbose_low ("Non-necessarily punctual optimum detected: restrains to equality");
						);
						
						(* Reconstruct a linear constraint param = coefficient *)
						let equality_constraint = LinearConstraint.p_constraint_of_point [(parameter_index, coefficient)] in
						
						(* Intersect with the optimum valuations *)
						LinearConstraint.p_nnconvex_intersection_assign current_optimum_valuations equality_constraint;
					);
					
					(* Return the constraint *)
					current_optimum_valuations
		
		)else(
		(* Otherwise get the optimum *)
		
			(* Get the constraint *)
			match current_optimum with
				| None -> LinearConstraint.false_p_nnconvex_constraint()
				| Some current_optimum -> LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint current_optimum
		)
		in
		
		(* Get the termination status *)
		 let termination_status = match termination_status with
			| None -> raise (InternalError "Termination status not set in EFopt.compute_result")
			| Some status -> status
		in

		(* Constraint is exact if termination is normal, possibly under-approximated otherwise *)
		let soundness = if termination_status = Regular_termination then Constraint_exact else Constraint_maybe_under in

		(* Return the result *)
		Single_synthesis_result
		{
			(* Non-necessarily convex constraint guaranteeing the non-reachability of the bad location *)
			result				= Good_constraint (result, soundness);
			
			(* English description of the constraint *)
			constraint_description = "constraint guaranteeing " ^ self#str_optimum ^ "-time reachability";
	
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
