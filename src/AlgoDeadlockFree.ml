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
 * Last modified     : 2016/02/10
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
	
	
	(*** WORK IN PROGRESS ***)
	
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
				print_warning ("Non-deterministic PTA! Selecting a guard arbitrarily among the " ^ (string_of_int (List.length transitions)) ^ " transitions from '" ^ (model.location_names automaton_index l) ^ "' to '" ^ (model.location_names automaton_index l') ^ "' with action to '" ^ (model.action_names action_index) ^ "' in automaton '" ^ (model.automata_names automaton_index) ^ "'.");
				
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
				print_warning ("Non-deterministic PTA! Selecting a guard arbitrarily among the " ^ (string_of_int (List.length transitions)) ^ " transitions from '" ^ (model.location_names automaton_index l) ^ "' to '" ^ (model.location_names automaton_index l') ^ "' with action to '" ^ (model.action_names action_index) ^ "' in automaton '" ^ (model.automata_names automaton_index) ^ "'.");
				
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

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "Parametric deadlock-checking"
	
	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
		super#initialize_variables;
		
		bad_constraint <- LinearConstraint.false_p_nnconvex_constraint ();

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
		
		self#print_algo_message Verbose_standard "Entering process_post_n";
		
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
			
				(* retrieve the guard *)
				(*** WARNING! big hack: due to the fact that StateSpace only maintains the action, then we have to hope that the PTA is deterministic to retrieve the edge, and hence the guard ***)
				let guard = get_guard state_space state_index action_index state_index' in
				
				(* Print some information *)
				if verbose_mode_greater Verbose_standard then(
					(* Retrieve the model *)
					let model = Input.get_model () in
					print_message Verbose_standard ("Guard computed:" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names guard));
				);
	
				(* Intersect with the guard with s *)
				(*** UGLY: conversion of dimensions..... ***)
				LinearConstraint.pxd_intersection_assign guard [LinearConstraint.pxd_of_px_constraint s_constraint];
				
				(* Process past *)
				AlgoStateBased.apply_time_past s_location guard;
				
				(* Update the local constraint by adding the new constraint as a union *)
				(*** WARNING: ugly (and expensive) to convert from pxd to px ***)
				(*** NOTE: still safe since discrete values are all instantiated ***)
				LinearConstraint.px_nnconvex_px_union good_constraint_s (LinearConstraint.pxd_hide_discrete_and_collapse guard);
				
			) succs_of_s;
				
			(* Compute the difference True \ good_constraint_s *)
			let px_bad_constraint_s = LinearConstraint.true_px_nnconvex_constraint() in
			LinearConstraint.px_nnconvex_difference px_bad_constraint_s good_constraint_s;
			
			(* Project onto the parameters *)
			let p_bad_constraint_s = LinearConstraint.px_nnconvex_hide_nonparameters_and_collapse px_bad_constraint_s in
				
			(* Update the bad constraint using the local constraint *)
			LinearConstraint.p_nnconvex_union bad_constraint p_bad_constraint_s;
		
		) post_n;

		(* The end *)
		()

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_result =
		self#print_algo_message_newline Verbose_standard (
			"Algorithm completed " ^ (after_seconds ()) ^ "."
		);
		
		(*** TODO: compute as well *good* zones, depending whether the analysis was exact, or early termination occurred ***)
		
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
			result				= bad_constraint;
			
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
