(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Parametric deadlock-freeness
 * 
 * File contributors : Étienne André
 * Created           : 2016/02/08
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
open Result
open AlgoPostStar


(************************************************************)
(************************************************************)
(* Class-independent functions *)
(************************************************************)
(************************************************************)

(*** WARNING! big hack: due to the fact that StateSpace only maintains the action, then we have to hope that the PTA is deterministic to retrieve the edge, and hence the guard ***)
let get_guard state_space state_index action_index state_index' =
	(* Retrieve the model *)
	let model = Input.get_model () in
	
	(* Retrieve source and destination locations *)
	let (location : Location.global_location), _ = StateSpace.get_state state_space state_index in
	let (location' : Location.global_location), _ = StateSpace.get_state state_space state_index' in
	
	(* Create the list of local guards *)
	let local_guards = ref [] in
	
	(* For all PTA *)
	List.iter (fun automaton_index ->
		(* Retrieve source and destination location indexes *)
		let l : Automaton.location_index = Location.get_location location automaton_index in
		let l' : Automaton.location_index = Location.get_location location' automaton_index in
		
		(* Now, compute the local guard, i.e., the guard in the current PTA *)
		let local_guard =
		(* If source and destination are equal: either a self-loop (if there exists a self-loop with this action), or the current PTA is not concerned by the transition *)
		if l = l' then (
			(* Find the transitions l -> action_index -> l' *)
			(*** NOTE: type transition = guard * clock_updates * discrete_update list * location_index ***)
			let transitions = List.filter (fun (_,_,_, destination) -> destination = l') (model.transitions automaton_index l action_index) in
			
			(* If none: then not concerned -> true gard *)
			if List.length transitions = 0 then LinearConstraint.pxd_true_constraint()
			
			(* If exactly one: good situation: return the guard *)
			else if List.length transitions = 1 then let g,_,_,_ = List.nth transitions 0 in g
			(* If more than one: take the first one (*** HACK ***) and warn *)
			else(
				(* Warning *)
				print_warning ("Non-deterministic PTA! Selecting a guard arbitrarily among the " ^ (string_of_int (List.length transitions)) ^ " transitions from '" ^ (model.location_names automaton_index l) ^ "' via action '" ^ (model.action_names action_index) ^ "' to '" ^ (model.location_names automaton_index l') ^ "' in automaton '" ^ (model.automata_names automaton_index) ^ "'.");
				
				(* Take arbitrarily the first element *)
				let g,_,_,_ = List.nth transitions 0 in g
			
			)

		(* Otherwise, if the source and destination locations differ: necessarily a transition with this action *)
		) else (
			(* Find the transitions l -> action_index -> l' *)
			let transitions = List.filter (fun (_,_,_, destination) -> destination = l') (model.transitions automaton_index l action_index) in
			
			(* There cannot be none *)
			if List.length transitions = 0 then raise (raise (InternalError("There cannot be no transition from '" ^ (model.location_names automaton_index l) ^ "' to '" ^ (model.location_names automaton_index l') ^ "' with action to '" ^ (model.action_names action_index) ^ "' in automaton '" ^ (model.automata_names automaton_index) ^ ".")))
			
			(* If exactly one: good situation: return the guard *)
			else if List.length transitions = 1 then let g,_,_,_ = List.nth transitions 0 in g
			(* If more than one: take the first one (*** HACK ***) and warn *)
			else(
				(* Warning *)
				print_warning ("Non-deterministic PTA! Selecting a guard arbitrarily among the " ^ (string_of_int (List.length transitions)) ^ " transitions from '" ^ (model.location_names automaton_index l) ^ "' via action '" ^ (model.action_names action_index) ^ "' to '" ^ (model.location_names automaton_index l') ^ "' in automaton '" ^ (model.automata_names automaton_index) ^ "'.");
				
				(* Take arbitrarily the first element *)
				let g,_,_,_ = List.nth transitions 0 in g
			)
			
		) in
		
		(* Add the guard *)
		local_guards := local_guard :: !local_guards;
	
	) model.automata;
	
	(* Compute constraint for assigning a (constant) value to discrete variables *)
	print_message Verbose_high ("Computing constraint for discrete variables");
	let discrete_values = List.map (fun discrete_index -> discrete_index, (Location.get_discrete_value location discrete_index)) model.discrete in
	(* Constraint of the form D_i = d_i *)
	let discrete_constraint = LinearConstraint.pxd_constraint_of_point discrete_values in

	(* Create the constraint guard ^ D_i = d_i *)
	let guard = LinearConstraint.pxd_intersection (discrete_constraint :: !local_guards) in
	
	(* Finally! Return the guard *)
	guard


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
		let model = Input.get_model () in
		(* Find clocks and parameters *)
		let clocks_and_parameters = list_union model.clocks model.parameters in
		(* Constrain non-negative *)
		(*let px_linear_constraint = *)LinearConstraint.px_constraint_of_nonnegative_variables clocks_and_parameters (*in
		(* Convert to px_nnconvex_constraint *)
		LinearConstraint.px_nnconvex_constraint_of_px_linear_constraint px_linear_constraint*)

	(* Non-necessarily convex parameter constraint of the initial state (constant object used as a shortcut, as it is often used in the algorithm) *)
	val init_p_nnconvex_constraint : LinearConstraint.p_nnconvex_constraint =
		(* Retrieve the model *)
		let model = Input.get_model () in
		LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint model.initial_p_constraint
	
	
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
		
		self#print_algo_message Verbose_low "Initializing variables...";
		
		bad_constraint <- LinearConstraint.false_p_nnconvex_constraint ();

		(* Print some information *)
		if verbose_mode_greater Verbose_low then(
			(* Retrieve the model *)
			let model = Input.get_model () in
			self#print_algo_message Verbose_low ("The global bad constraint is now:\n" ^ (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names bad_constraint));
		);
		
		(* The end *)
		()
	


(*	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Actions to perform when meeting a state with no successors: nothing to do for this algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_deadlock_state state_index = ()*)
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Actions to perform at the end of the computation of the *successors* of post^n (i.e., when this method is called, the successors were just computed). Nothing to do for this algorithm. *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_post_n (post_n : StateSpace.state_index list) =
		(* Retrieve the model *)
		let model = Input.get_model () in
		
		self#print_algo_message Verbose_medium "Entering process_post_n";
		
		(* For all state s in post^n *)
		List.iter (fun state_index ->
			(* Get the constraint of s *)
			let s_location, s_constraint = StateSpace.get_state state_space state_index in
		
			(* Define a local constraint storing the union of PX-constraints allowing to leave s *)
			let good_constraint_s = LinearConstraint.false_px_nnconvex_constraint () in
			
			(* Retrieve all successors of this state with their action *)
			let succs_of_s = StateSpace.get_successors_with_actions state_space state_index in
			
			(* For all state s' in the successors of s *)
			List.iter (fun (state_index', action_index) ->
			
				(* Print some information *)
				if verbose_mode_greater Verbose_medium then(
					self#print_algo_message Verbose_medium ("Considering transition from state " ^ (string_of_int state_index) ^ " via action '" ^ (model.action_names action_index) ^ "' to state " ^ (string_of_int state_index') ^ "...");
				);
				
				(* retrieve the guard *)
				(*** WARNING! big hack: due to the fact that StateSpace only maintains the action, then we have to hope that the PTA is deterministic to retrieve the edge, and hence the guard ***)
				let guard = get_guard state_space state_index action_index state_index' in
				
				(* Print some information *)
				if verbose_mode_greater Verbose_high then(
					self#print_algo_message Verbose_high ("Guard computed via action '" ^ (model.action_names action_index) ^ "':\n" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names guard));
				);
				
				(* Retrieving the constraint s'|P *)
				let _, px_destination = StateSpace.get_state state_space state_index' in
				let p_destination = LinearConstraint.px_hide_nonparameters_and_collapse px_destination in
	
				(* Intersect with the guard with s *)
				(*** UGLY: conversion of dimensions..... ***)
				LinearConstraint.pxd_intersection_assign guard [LinearConstraint.pxd_of_px_constraint s_constraint ; LinearConstraint.pxd_of_p_constraint p_destination];
				
				(* Print some information *)
				if verbose_mode_greater Verbose_high then(
					self#print_algo_message Verbose_high ("Intersection of guards:\n" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names guard));
				);
	
				(* Process past *)
				AlgoStateBased.apply_time_past s_location guard;
				
				(* Print some information *)
				if verbose_mode_greater Verbose_high then(
					self#print_algo_message Verbose_high ("After time past:\n" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names guard));
				);
	
				(* Update the local constraint by adding the new constraint as a union *)
				(*** WARNING: ugly (and expensive) to convert from pxd to px ***)
				(*** NOTE: still safe since discrete values are all instantiated ***)
				LinearConstraint.px_nnconvex_px_union good_constraint_s (LinearConstraint.pxd_hide_discrete_and_collapse guard);
				
				(* Print some information *)
				if verbose_mode_greater Verbose_medium then(
					self#print_algo_message Verbose_medium ("The local good constraint (allowing exit) is:\n" ^ (LinearConstraint.string_of_px_nnconvex_constraint model.variable_names good_constraint_s));
				);
			) succs_of_s;
				
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

			LinearConstraint.px_nnconvex_difference nnconvex_s good_constraint_s;
			
			(* Print some information *)
			if verbose_mode_greater Verbose_high then(
				self#print_algo_message Verbose_high ("nnconvex_s (s - good, not allowing exit) is:\n" ^ (LinearConstraint.string_of_px_nnconvex_constraint model.variable_names nnconvex_s));
			);
			
			(* Ensure clocks and parameters are not negative *)
			LinearConstraint.px_nnconvex_intersection nnconvex_s all_clocks_and_parameters_nonnegative;
			
			
			(* Print some information *)
			if verbose_mode_greater Verbose_high then(
				self#print_algo_message Verbose_high ("After constraining clocks and parameters to be positive, nnconvex_s (s - good, not allowing exit) is now:\n" ^ (LinearConstraint.string_of_px_nnconvex_constraint model.variable_names nnconvex_s));
			);
			
			(* Project onto the parameters *)
			let p_bad_constraint_s = LinearConstraint.px_nnconvex_hide_nonparameters_and_collapse nnconvex_s(*px_bad_constraint_s*) in
				
			(* Print some information *)
			if verbose_mode_greater Verbose_medium then(
				self#print_algo_message Verbose_medium ("p_bad_constraint_s (True - good, not allowing exit, projected onto P) is:\n" ^ (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names p_bad_constraint_s));
			);

			(* Update the bad constraint using the local constraint *)
			LinearConstraint.p_nnconvex_union bad_constraint p_bad_constraint_s;
		
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

	
(*	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Actions to perform when meeting a state with no successors: in that case add the entire state to the bad constraint *)
	(*** NOTE: this step is in fact necessary only for the deadlocked states at the deepest position in the state space; for other deadlock states, they are already taken into account by process_post_n when computing n+1 ***)
	(*** TO OPTIMIZE ***)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_deadlock_state state_index =
		
		self#print_algo_message Verbose_standard ("Entering process_deadlock_state " ^ (string_of_int state_index) ^ "...");
		
		(* Get the constraint of the state *)
		let _, s_constraint = StateSpace.get_state state_space state_index in
		
		(* Project onto P *)
		let p_constraint = LinearConstraint.px_hide_nonparameters_and_collapse s_constraint in
		
		(* Print some information *)
		if verbose_mode_greater Verbose_low then(
			(* Retrieve the model *)
			let model = Input.get_model () in
			self#print_algo_message Verbose_low ("Found a deadlock state! Adding " ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names p_constraint) ^ ".");
		);
		
		(* Add union to bad_constraint *)
		LinearConstraint.p_nnconvex_p_union bad_constraint p_constraint;
		
		(* Print some information *)
		if verbose_mode_greater Verbose_low then(
			(* Retrieve the model *)
			let model = Input.get_model () in
			self#print_algo_message Verbose_low ("The global bad constraint is now: " ^ (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names bad_constraint));
		);

		print_message Verbose_low ("");

		(* The end *)
		()
		*)

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Check whether the algorithm should terminate at the end of some post, independently of the number of states to be processed (e.g., if the constraint is already true or false) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method check_termination_at_post_n =
		(* Print some information *)
		self#print_algo_message Verbose_high ("Entering check_termination_at_post_n...");
		
		(* Retrieve the model *)
		let model = Input.get_model () in

		(* True if the computed bad constraint is exactly equal to (or larger than) the initial parametric constraint *)
		let stop = LinearConstraint.p_nnconvex_constraint_is_leq init_p_nnconvex_constraint bad_constraint in
		
		(* Print some information *)
		if (stop && verbose_mode_greater Verbose_medium) || verbose_mode_greater Verbose_medium then(
			self#print_algo_message Verbose_medium ("Initial constraint:\n" ^ (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names init_p_nnconvex_constraint));
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
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_result =
		(* Retrieve the model *)
(* 		let model = Input.get_model () in *)
		
		self#print_algo_message_newline Verbose_standard (
			"Algorithm completed " ^ (after_seconds ()) ^ "."
		);
		
		(* Perform result = initial_state|P \ bad_constraint *)
		let result = init_p_nnconvex_constraint in
		(*** WARNING: will actually MODIFY init_p_nnconvex_constraint; but not a problem since about to exit the algorithm, and variable not used anymore afterwards ***)
		LinearConstraint.p_nnconvex_difference result bad_constraint;
		
		(* Get the termination status *)
		 let termination_status = match termination_status with
			| None -> raise (InternalError "Termination status not set in EFsynth.compute_result")
			| Some status -> status
		in

		(* The tile nature is good if 1) it is not bad, and 2) the analysis terminated normally *)
		let statespace_nature =
			if statespace_nature = StateSpace.Unknown && termination_status = Regular_termination then StateSpace.Good
			(* Otherwise: unchanged *)
			else statespace_nature
		in
		
		(* Constraint is exact if termination is normal, possibly under-approximated otherwise *)
		let soundness = if termination_status = Regular_termination then Constraint_exact else Constraint_maybe_under in

		(* Return the result *)
		PDFC_result
		{
			(* List of constraints ensuring potential deadlocks *)
			result				= result;
			
			(* Explored state space *)
			state_space			= state_space;
			
			(* Nature of the state space *)
			statespace_nature	= statespace_nature;
			
			(* Total computation time of the algorithm *)
			computation_time	= time_from start_time;
			
			(* Soudndness of the result *)
			soundness			= soundness;
	
			(* Termination *)
			termination			= termination_status;
		}
	
(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
