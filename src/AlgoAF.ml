(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13 (France)
 * 
 * Module description: AF synthesis [JLR15]
 * 
 * File contributors : Étienne André
 * Created           : 2018/03/15
 * Last modified     : 2018/03/15
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
class algoAF =
	object (self) inherit algoPostStar as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	(* Non-necessarily convex parameter constraint for which AF holds *)
	val mutable af_constraint : LinearConstraint.p_nnconvex_constraint = LinearConstraint.false_p_nnconvex_constraint ()

	(*** NOTE/TODO: technically, clocks should be non-negative, but parameters should just be conform to the initial p_constraint ***)
	
	val all_clocks_and_parameters_nonnegative : LinearConstraint.px_linear_constraint =
		(* Retrieve the model *)
		let model = Input.get_model() in
		(* Find clocks and parameters *)
		let clocks_and_parameters = list_union model.clocks model.parameters in
		(* Constrain non-negative *)
		LinearConstraint.px_constraint_of_nonnegative_variables clocks_and_parameters

	(* Non-necessarily convex parameter constraint of the initial state (constant object used as a shortcut, as it is often used in the algorithm) *)
	val init_p_nnconvex_constraint : LinearConstraint.p_nnconvex_constraint =
		(* Retrieve the model *)
		let model = Input.get_model() in
		LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint model.initial_p_constraint
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "AFsynth"
	
	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
		super#initialize_variables;
		
		self#print_algo_message Verbose_low "Initializing variables…";
		
		af_constraint <- LinearConstraint.false_p_nnconvex_constraint ();

		(* Print some information *)
		if verbose_mode_greater Verbose_low then(
			self#print_algo_message Verbose_low ("The global bad constraint is now:\n" ^ (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names af_constraint));
		);
		
		(* The end *)
		()
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Computing the p_nnconvex_constraint for which there may exist a deadlock from a given state; the second argument is the list of successors (in case we may want to consider not all successors, typically in backward exploration) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private compute_deadlock_p_constraint state_index (successors : (State.state_index * Automaton.action_index) list) : LinearConstraint.p_nnconvex_constraint =
	
		(* Define a local constraint storing the union of PX-constraints allowing to leave s *)
		let good_constraint_s = LinearConstraint.false_px_nnconvex_constraint () in
		
		(* Get the location and the constraint of s *)
		let s_location, s_constraint = StateSpace.get_state state_space state_index in
		
		(* For all state s' in the successors of s *)
		List.iter (fun (state_index', action_index) ->
		
			(* Print some information *)
			if verbose_mode_greater Verbose_medium then(
				self#print_algo_message Verbose_medium ("Considering transition from state " ^ (string_of_int state_index) ^ " via action '" ^ (model.action_names action_index) ^ "' to state " ^ (string_of_int state_index') ^ "…");
			);
			
			(* retrieve the guard *)
			(*** WARNING! big hack: due to the fact that StateSpace only maintains the action, then we have to hope that the PTA is deterministic to retrieve the edge, and hence the guard ***)
			(*** WARNING: very expensive function (for now) ***)
			let guard = StateSpace.get_guard state_space state_index action_index state_index' in
			
			(* Print some information *)
			if verbose_mode_greater Verbose_high then(
				self#print_algo_message Verbose_high ("Guard computed via action '" ^ (model.action_names action_index) ^ "':\n" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names guard));
			);
			
			(* Retrieving the constraint s'|P *)
			let _, px_destination = StateSpace.get_state state_space state_index' in
			let p_destination = LinearConstraint.px_hide_nonparameters_and_collapse px_destination in

			(* Intersect with the guard with s *)
			(*** UGLY: conversion of dimensions….. ***)
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
		) successors;
			
(*			(* Compute the difference True^+ \ good_constraint_s *)
		(*** TODO: add a field clocks_and_parameters to abstract_model ***)
		let trueplus = LinearConstraint.px_constraint_of_nonnegative_variables (list_union model.clocks model.parameters) in
		let px_af_constraint_s = LinearConstraint.px_nnconvex_constraint_of_px_linear_constraint trueplus in
		
		(* Print some information *)
		if verbose_mode_greater Verbose_medium then(
			self#print_algo_message Verbose_medium ("px_af_constraint_s ('trueplus') is now: " ^ (LinearConstraint.string_of_px_nnconvex_constraint model.variable_names px_af_constraint_s));
		);

		LinearConstraint.px_nnconvex_difference px_af_constraint_s good_constraint_s;

		(* Print some information *)
		if verbose_mode_greater Verbose_low then(
			self#print_algo_message Verbose_low ("px_af_constraint_s (True \ good, not allowing exit) is now: " ^ (LinearConstraint.string_of_px_nnconvex_constraint model.variable_names px_af_constraint_s));
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
		let p_af_constraint_s = LinearConstraint.px_nnconvex_hide_nonparameters_and_collapse nnconvex_s in
			
		(* Print some information *)
		if verbose_mode_greater Verbose_medium then(
			self#print_algo_message Verbose_medium ("p_af_constraint_s (True - good, not allowing exit, projected onto P) is:\n" ^ (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names p_af_constraint_s));
		);
		
		(* Return the p_constraint *)
		p_af_constraint_s


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Actions to perform with the initial state; returns true unless the initial state cannot be kept (in which case the algorithm will stop immediately) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_initial_state _ =
		(* Always keep the initial state *)
		true

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Actions to perform at the end of the computation of the *successors* of post^n (i.e., when this method is called, the successors were just computed). Nothing to do for this algorithm. *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_post_n (post_n : State.state_index list) =
		
		self#print_algo_message Verbose_medium "Entering process_post_n";
		
		(* For all state s in post^n *)
		List.iter (fun state_index ->
			(* Retrieve all successors of this state with their action *)
			let succs_of_s = StateSpace.get_successors_with_actions state_space state_index in
			
			let p_af_constraint_s = self#compute_deadlock_p_constraint state_index succs_of_s in
			
			(* Update the bad constraint using the local constraint *)
			LinearConstraint.p_nnconvex_union af_constraint p_af_constraint_s;
		
			(* Print some information *)
			if verbose_mode_greater Verbose_medium then(
				self#print_algo_message Verbose_medium ("The global bad constraint is now:\n" ^ (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names af_constraint));
			);
		) post_n;

		(* Print some information *)
		if verbose_mode_greater Verbose_low then(
			self#print_algo_message Verbose_low ("After processing post^n, the global bad constraint is now: " ^ (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names af_constraint));
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
		
		(* True if the computed bad constraint is exactly equal to (or larger than) the initial parametric constraint *)
		let stop = LinearConstraint.p_nnconvex_constraint_is_leq init_p_nnconvex_constraint af_constraint in
		
		(* Print some information *)
		if (stop && verbose_mode_greater Verbose_medium) || verbose_mode_greater Verbose_medium then(
			self#print_algo_message Verbose_medium ("Initial constraint:\n" ^ (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names init_p_nnconvex_constraint));
			self#print_algo_message Verbose_medium ("Current bad constraint:\n" ^ (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names af_constraint));
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
		self#print_algo_message_newline Verbose_standard (
			"Algorithm completed " ^ (after_seconds ()) ^ "."
		);
		
		self#print_algo_message_newline Verbose_low (
			"Performing negation of final constraint…"
		);
		
		(* Perform result = initial_state|P \ af_constraint *)
		let result = LinearConstraint.p_nnconvex_copy init_p_nnconvex_constraint in
		LinearConstraint.p_nnconvex_difference result af_constraint;
		
		self#print_algo_message_newline Verbose_medium (
			"Negation of final constraint completed."
		);
		
		(* Get the termination status *)
		 let termination_status = match termination_status with
			| None -> raise (InternalError "Termination status not set in EFsynth.compute_result")
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
			raise Not_implemented
			
		) (* end if not regular termination *)
		in

		(* Return the result *)
		Single_synthesis_result
		{
			(* Non-necessarily convex constraint guaranteeing the non-reachability of the bad location *)
			result				= result;
			
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
