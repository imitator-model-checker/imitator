(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 *
 * Module description: Description of the symbolic states and of the state space
 *
 * File contributors : Ulrich Kühne, Étienne André
 * Created           : 2009/12/08
 * Last modified     : 2018/05/30
 *
 ************************************************************)



(************************************************************)
(* Modules *)
(************************************************************)
(*module Ppl = Ppl_ocaml
open Ppl*)

open Exceptions
open OCamlUtilities
open ImitatorUtilities
open Statistics
open Automaton
open AbstractModel
open State



(************************************************************)
(** Nature of a state space according to some property *)
(************************************************************)
type statespace_nature =
	| Good
	| Bad
	| Unknown


(************************************************************)
(** Check when adding a new state *)
(************************************************************)
type state_comparison =
	(* Does not check whether the state is present, add directly *)
	| No_check
	(* Does not add the new state if another state is exactly equal to it *)
	| Equality_check
	(* Does not add the new state if it is included in another state *)
	| Inclusion_check
	(* Does not add the new state if it is included in another state, or if another state is included into the current state (in which case the new state replaces the old one in the state space) *)
	| Double_inclusion_check


(************************************************************)
(** Result of the function adding a new state *)
(************************************************************)
type addition_result =
	(* Completely new state *)
	| New_state of state_index
	(* State already present (possibly included depending on options), returns the old state index *)
	| State_already_present of state_index
	(* The new state replaced a former state (because the newer is larger), returns the old state index *)
	| State_replacing of state_index



(************************************************************)
(** Set of state index *)
(************************************************************)

(* state struct for constructing set type *)
module State = struct
	type t = state_index
	let compare = compare
end

(* set of states for efficient lookup *)
module StateIndexSet = Set.Make(State)



(************************************************************)
(** State space structure *)
(************************************************************)
type state_space = {
	(** The number of generated states (even not added to the state space) *)
	nb_generated_states : int ref;

	(** An Array 'state_index' -> 'State.abstract_state'; contains ALL states *)
	all_states : (state_index, abstract_state) Hashtbl.t;

	(** The id of the initial state *)
	(*** NOTE: mutable due to the fact that the initial state can be merged with another state *)
	mutable initial : state_index option;

	(** A hashtable location -> location_index *)
	index_of_locations : (Location.global_location, location_index) Hashtbl.t;

	(** A DynArray location_index -> location *)
	locations : Location.global_location DynArray.t;

	(** A hashtable to quickly find states with identical locations (? ; made by Ulrich); only for states to be compared *)
	states_for_comparison : (int, state_index) Hashtbl.t;

	(** A hashtable '(state_index, action_index)' -> 'target_state_index' *)
	transitions_table : ((state_index * action_index), state_index) Hashtbl.t;

	(** An integer that remembers the next index of state_index (may not be equal to the number of states, if states are removed *)
	next_state_index : state_index ref;
}


(** An SCC is just a list of states *)
type scc = state_index list


(************************************************************)
(** Statistics *)
(************************************************************)
(*let nb_state_comparisons = ref 0
let nb_constraint_comparisons = ref 0*)
let statespace_dcounter_nb_state_comparisons = create_discrete_counter_and_register "number of state comparisons" States_counter Verbose_standard
let statespace_dcounter_nb_constraint_comparisons = create_discrete_counter_and_register "number of constraints comparisons" States_counter Verbose_standard

(* Numbers of new states that were in fact included into an old state *)
let statespace_dcounter_nb_states_included = create_discrete_counter_and_register "number of new states <= old" States_counter Verbose_standard
(* Numbers of new states that were in fact larger than an old state *)
let statespace_dcounter_nb_states_including = create_discrete_counter_and_register "number of new states >= old" States_counter Verbose_standard

(* Numbers of merging attempts (for states that have the same discrete location) *)
let nb_merging_attempts = create_discrete_counter_and_register "StateSpace.merging attempts" States_counter Verbose_standard
(* Numbers of actual merges *)
let nb_merged = create_discrete_counter_and_register "StateSpace.merges" States_counter Verbose_standard

(* Functions *)
let counter_add_state = create_hybrid_counter_and_register "StateSpace.add_state" States_counter Verbose_experiments
let counter_compute_predecessors_with_actions = create_hybrid_counter_and_register "StateSpace.compute_predecessors" States_counter Verbose_experiments
let counter_get_location = create_hybrid_counter_and_register "StateSpace.counter_get_location" States_counter Verbose_experiments
let counter_get_state = create_hybrid_counter_and_register "StateSpace.get_state" States_counter Verbose_experiments

let counter_add_transition = create_hybrid_counter_and_register "StateSpace.add_transition" States_counter Verbose_experiments
let counter_get_successors = create_hybrid_counter_and_register "StateSpace.get_successors" States_counter Verbose_experiments
let counter_get_transitions = create_hybrid_counter_and_register "StateSpace.get_transitions" States_counter Verbose_experiments
let counter_nb_states = create_hybrid_counter_and_register "StateSpace.nb_states" States_counter Verbose_experiments
let counter_nb_transitions = create_hybrid_counter_and_register "StateSpace.nb_transitions" States_counter Verbose_experiments
let counter_empty_states_for_comparison = create_hybrid_counter_and_register "StateSpace.empty_states_for_comparison" States_counter Verbose_experiments

(*
	(* Statistics *)
	counter_xx#increment;
	counter_xx#start;
	(* Statistics *)
	counter_xx#stop;
*)


(************************************************************)
(** Local exception *)
(************************************************************)
exception Found_cycle


(************************************************************)
(** Debug string function *)
(************************************************************)
let string_of_state_index state_index = "s_" ^ (string_of_int state_index)


(************************************************************)
(** State space creation *)
(************************************************************)

(** Create a fresh state space *)
let make guessed_nb_transitions =
	(* Create a Hashtbl : state_index -> (location_index, linear_constraint) for the reachable states *)
	let states = Hashtbl.create Constants.guessed_nb_states_for_hashtable in
	(* Create a hashtable : location -> location_index for the locations *)
	let index_of_locations = Hashtbl.create Constants.guessed_nb_states_for_hashtable in
	(* Create a DynArray : location_index -> location for the locations *)
	let locations = DynArray.make Constants.guessed_nb_states_for_hashtable in
	(* Create an empty lookup table : hash -> state_index *)
	let states_for_comparison = Hashtbl.create Constants.guessed_nb_states_for_hashtable in
	(* Create a hashtable for the state space *)
	let transitions_table = Hashtbl.create guessed_nb_transitions in

	(* Create the state space *)
	{
		nb_generated_states = ref 0;
		all_states = states;
		initial = None;
		index_of_locations = index_of_locations;
		locations = locations;
		states_for_comparison = states_for_comparison;
		transitions_table = transitions_table;
		next_state_index = ref 0;
	}


(************************************************************)
(** Interrogation on a state space *)
(************************************************************)

(** Return the number of generated states (not necessarily present in the state space) *)
let get_nb_gen_states state_space =
	!(state_space.nb_generated_states)

(** Return the number of states in a state space *)
let nb_states state_space =

	(* Statistics *)
	counter_nb_states#increment;
	counter_nb_states#start;

	let result =
		Hashtbl.length state_space.all_states
	in

	(* Statistics *)
	counter_nb_states#stop;

	result



(** Return the number of transitions in a state space *)
let nb_transitions state_space =
	(* Statistics *)
	counter_nb_transitions#increment;
	counter_nb_transitions#start;

	let result =
		Hashtbl.length state_space.transitions_table
	in

	(* Statistics *)
	counter_nb_transitions#stop;

	result


(* Return the global_location corresponding to a location_index *)
let get_location state_space location_index =
	(* Statistics *)
	counter_get_location#increment;
	counter_get_location#start;

	let result =
		DynArray.get state_space.locations location_index
	in

	(* Statistics *)
	counter_get_location#stop;

	result


(** Return the state of a state_index *)
let get_state state_space state_index =
	(* Statistics *)
	counter_get_state#increment;
	counter_get_state#start;

	(* Find the couple (location_index, constraint) *)
	let location_index, linear_constraint =
		(* Exception just in case *)
		try (
			Hashtbl.find state_space.all_states state_index
		) with Not_found -> raise (InternalError ("State of index '" ^ (string_of_int state_index) ^ "' was not found in state_space (in function: StateSpace.get_state)."))
	in
	(* Find the location *)
	let global_location = get_location state_space location_index in

	(* Statistics *)
	counter_get_state#stop;

	(* Return the state *)
	(global_location, linear_constraint)


(** Return the index of the initial state, or raise Not_found if not defined *)
let get_initial_state_index state_space =
	match state_space.initial with
	| Some state_index -> state_index
	| None -> raise Not_found


(*(** Return the initial state, or raise Not_found if not defined *)
let get_initial_state state_space =
	get_state state_space (get_initial_state_index state_space)*)


(** Return the table of transitions *)
let get_transitions state_space =

	(* Statistics *)
	counter_get_transitions#increment;
	counter_get_transitions#start;

	let result =
		state_space.transitions_table
	in

	(* Statistics *)
	counter_get_transitions#stop;

	result


(** Compte and return the list of index successors of a state *)
let get_successors state_space state_index =
	(* Statistics *)
	counter_get_successors#increment;
	counter_get_successors#start;

	(* Retrieve the model *)
	let model = Input.get_model() in

	let result =
	List.fold_left (fun succs action_index ->
		try (
			let succ = Hashtbl.find_all state_space.transitions_table (state_index, action_index) in
			List.rev_append succ succs
		) with Not_found -> succs
	) [] model.actions
	in

	(* Statistics *)
	counter_get_successors#stop;

	result


(** Compte and return the list of pairs (index successor of a state, corresponding action) *)
let get_successors_with_actions state_space state_index =
	(* Retrieve the model *)
	let model = Input.get_model() in

	List.fold_left (fun succs action_index ->
		try (
			let succ = Hashtbl.find_all state_space.transitions_table (state_index, action_index) in
			let succ_with_action = List.map (fun state_index -> state_index , action_index) succ in
			List.rev_append succ_with_action succs
		) with Not_found -> succs
	) [] model.actions


(*------------------------------------------------------------*)
(** Compute and return a predecessor table state_index -> (state_index, action_index) list *)
(*------------------------------------------------------------*)
let compute_predecessors_with_actions state_space =

	(* Statistics *)
	counter_compute_predecessors_with_actions#increment;
	counter_compute_predecessors_with_actions#start;

	(* Retrieve the model *)
	let model = Input.get_model() in

	(* Create a hash table for predecessors: state_index -> (state_index, action_index) list *)
	let predecessors = Hashtbl.create (Hashtbl.length state_space.all_states) in

	(* Iterate on all states in the state space *)
	Hashtbl.iter(fun source_state_index _ ->
		(* Iterate on actions, and fill *)
		List.iter (fun action_index ->
			(* Test if the origin state has some transitions with this action *)
			if Hashtbl.mem state_space.transitions_table (source_state_index, action_index) then(
				(* Find successors for this action and transition *)
				let successors = Hashtbl.find_all state_space.transitions_table (source_state_index, action_index) in

				(* Update the predecessors hash table for each successor *)
				List.iter (fun target_state_index ->
					(* Retrieve the predecessors of target_state_index, if any *)
					let current_predecessors = if Hashtbl.mem predecessors target_state_index then Hashtbl.find predecessors target_state_index else [] in

					(*** NOTE: state_index may be added several times (is that a problem…?) ***)
					let updated_predecessors = (source_state_index, action_index) :: current_predecessors in

					(* Update predecessors *)
					Hashtbl.replace predecessors target_state_index updated_predecessors;
				) successors;
			);
		) model.actions;
	) state_space.all_states;

	(* Statistics *)
	counter_compute_predecessors_with_actions#stop;

	(* Return structure *)
	predecessors


(** Return the list of all state indexes *)
let all_state_indexes state_space = hashtbl_get_all_keys state_space.all_states
(*let all_state_indexes state_space =
	Hashtbl.fold
		(fun state_index _ current_list ->
			state_index :: current_list)
		state_space.all_states []*)



(*** WARNING: big memory, here! Why not perform intersection on the fly? *)

(** Return the list of all constraints on the parameters associated to the states of a state space *)
let all_p_constraints state_space =
	Hashtbl.fold
		(fun _ (_, linear_constraint) current_list ->
			let p_constraint = LinearConstraint.px_hide_nonparameters_and_collapse linear_constraint in
			p_constraint :: current_list)
		state_space.all_states []



(** Iterate over the reahable states *)
let iterate_on_states f state_space =
	Hashtbl.iter f state_space.all_states


(*(** Compute the intersection of all parameter constraints, DESTRUCTIVE!!! *)
(** HERE PROBLEM IF ONE WANTS TO COMPUTE THE states FILE AFTER **)
let compute_k0_destructive program state_space =
	let k0 = LinearConstraint.true_constraint () in
	iterate_on_states (fun _ (_, constr) ->
		LinearConstraint.hide_assign program.clocks_and_discrete constr;
		LinearConstraint.px_intersection_assign k0 [constr];

	) state_space;
	k0*)

(** find all "last" states on finite or infinite runs *)
(* Uses a depth first search on the reachability state_space. The *)
(* prefix of the current DFS path is kept during the search *)
(* in order to detect cycles. *)
let last_states model state_space =
	(* list to keep the resulting last states *)
	let last_states = ref [] in
	(* Table to keep all states already visited during DFS *)
	let dfs_table = ref StateIndexSet.empty in

	(* functional version for lookup *)
	let already_seen node = StateIndexSet.mem node !dfs_table in

	(* function to find all last states *)
	let rec cycle_detect node prefix =
		(* get all successors of current node *)
		let succs = get_successors state_space node in
		if succs = [] then
			(* no successors -> last node on finite path *)
			last_states := node :: !last_states
		else (
			(* insert node in DFS table *)
			dfs_table := StateIndexSet.add node !dfs_table;
			(* go on with successors *)
			List.iter (fun succ ->
				(* successor in current path prefix (or self-cycle)? *)
				if succ = node || StateIndexSet.mem succ prefix then
					(* found cycle *)
					last_states := succ :: !last_states
				else if not (already_seen succ) then
					(* go on recursively on newly found node *)
					cycle_detect succ (StateIndexSet.add node prefix)
			) succs;
		) in

	(* start cycle detection with initial state *)
	cycle_detect 0 StateIndexSet.empty;

	(* return collected last states *)
	!last_states


(*exception Satisfied

(** Checks if a state exists that satisfies a given predicate *)
let exists_state p state_space =
		try (
			iter (fun s ->
				if p s then raise Satisfied
			) state_space;
			false
		) with Satisfied -> true

(** Checks if all states satisfy a given predicate *)
let forall_state p state_space =
		try (
			iter (fun s ->
				if not (p s) then raise Satisfied
			) state_space;
			true
		) with Satisfied -> false	*)



(*(** Check if bad states are reachable *)
let is_bad program state_space =
	(* get bad state pairs from program *)
	let bad_states = program.bad in
	(* if no bad state specified, then must be good *)
	if bad_states = [] then false else (
		let is_bad_state = fun (location, _) ->
			List.for_all (fun (aut_index, loc_index) ->
				loc_index = Location.get_location location aut_index
			) bad_states in
		exists_state is_bad_state state_space
	) *)



(*------------------------------------------------------------*)
(** Big hack: guard and resets reconstruction *)
(*------------------------------------------------------------*)

(*** NOTE: this function is defined here just because it was necessary here first; perhaps a better module would be more logical… ***)
(*** NOTE/HACK: duplicate function in ModelConverter ***)
let continuous_part_of_guard (*: LinearConstraint.pxd_linear_constraint*) = function
	| True_guard -> LinearConstraint.pxd_true_constraint()
	| False_guard -> LinearConstraint.pxd_false_constraint()
	| Discrete_guard discrete_guard -> LinearConstraint.pxd_true_constraint()
	| Continuous_guard continuous_guard -> continuous_guard
	| Discrete_continuous_guard discrete_continuous_guard -> discrete_continuous_guard.continuous_guard


(*** WARNING! big hack: due to the fact that StateSpace only maintains the action, then we have to hope that the PTA is deterministic to retrieve the edge, and hence the guard ***)
let get_guard state_space state_index action_index state_index' =
	(* Retrieve the model *)
	let model = Input.get_model () in

	(* Retrieve source and target locations *)
	let (location : Location.global_location), _ = get_state state_space state_index in
	let (location' : Location.global_location), _ = get_state state_space state_index' in

	(* Create the list of local guards *)
	let local_guards = ref [] in

	(* For all PTA *)
	List.iter (fun automaton_index ->
		(* Retrieve source and target location indexes *)
		let l : Automaton.location_index = Location.get_location location automaton_index in
		let l' : Automaton.location_index = Location.get_location location' automaton_index in

		(* Now, compute the local guard, i.e., the guard in the current PTA *)
		let local_guard =
		(* If source and target are equal: either a self-loop (if there exists a self-loop with this action), or the current PTA is not concerned by the transition *)
		if l = l' then (
			(* Find the transitions l -> action_index -> l' *)
			(*** NOTE: type transition = guard * clock_updates * discrete_update list * location_index ***)
			let transitions = List.filter (fun (_,_, target) -> target = l') (model.transitions automaton_index l action_index) in

			(* If none: then not concerned -> true gard *)
			if List.length transitions = 0 then LinearConstraint.pxd_true_constraint()

			(* If exactly one: good situation: return the guard *)
			else if List.length transitions = 1 then let g,_,_ = List.nth transitions 0 in continuous_part_of_guard g
			(* If more than one: take the first one (*** HACK ***) and warn *)
			else(
				(* Warning *)
				print_warning ("Non-deterministic PTA! Selecting a guard arbitrarily among the " ^ (string_of_int (List.length transitions)) ^ " transitions from '" ^ (model.location_names automaton_index l) ^ "' via action '" ^ (model.action_names action_index) ^ "' to '" ^ (model.location_names automaton_index l') ^ "' in automaton '" ^ (model.automata_names automaton_index) ^ "'.");

				(* Take arbitrarily the first element *)
				let g,_,_ = List.nth transitions 0 in continuous_part_of_guard g

			)

		(* Otherwise, if the source and target locations differ: necessarily a transition with this action *)
		) else (
			(* Find the transitions l -> action_index -> l' *)
			let transitions = List.filter (fun (_,_, target) -> target = l') (model.transitions automaton_index l action_index) in

			(* There cannot be none *)
			if List.length transitions = 0 then raise (raise (InternalError("There cannot be no transition from '" ^ (model.location_names automaton_index l) ^ "' to '" ^ (model.location_names automaton_index l') ^ "' with action to '" ^ (model.action_names action_index) ^ "' in automaton '" ^ (model.automata_names automaton_index) ^ ".")))

			(* If exactly one: good situation: return the guard *)
			else if List.length transitions = 1 then let g,_,_ = List.nth transitions 0 in continuous_part_of_guard g
			(* If more than one: take the first one (*** HACK ***) and warn *)
			else(
				(* Warning *)
				print_warning ("Non-deterministic PTA! Selecting a guard arbitrarily among the " ^ (string_of_int (List.length transitions)) ^ " transitions from '" ^ (model.location_names automaton_index l) ^ "' via action '" ^ (model.action_names action_index) ^ "' to '" ^ (model.location_names automaton_index l') ^ "' in automaton '" ^ (model.automata_names automaton_index) ^ "'.");

				(* Take arbitrarily the first element *)
				let g,_,_ = List.nth transitions 0 in continuous_part_of_guard g
			)

		) in

		(* Add the guard *)
		(*** NOTE: VERY inefficient as we create a lot of pxd_true_constraint() (when the guards are true) although it would be better to just NOT add them to the list… ***)
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


(*** WARNING! big hack: due to the fact that StateSpace only maintains the action, then we have to hope that the PTA is deterministic to retrieve the edge, and hence the set of clocks to be reset along a transition ***)
(*** NOTE: the function only works for regular resets (it raises an NotImplemented for other updates) ***)
let get_resets state_space state_index action_index state_index' =
	(* Retrieve the model *)
	let model = Input.get_model () in

	(* Retrieve source and target locations *)
	let (location : Location.global_location), _ = get_state state_space state_index in
	let (location' : Location.global_location), _ = get_state state_space state_index' in

	(* Create the list of clocks to be reset *)
	let clock_resets = ref [] in

	(* For all PTA *)
	List.iter (fun automaton_index ->
		(* Retrieve source and target location indexes *)
		let l : Automaton.location_index = Location.get_location location automaton_index in
		let l' : Automaton.location_index = Location.get_location location' automaton_index in

		(* Now, compute the clock_updates in the current PTA *)
		let clock_updates =
		(* If source and target are equal: either a self-loop (if there exists a self-loop with this action), or the current PTA is not concerned by the transition *)
		if l = l' then (
			(* Find the transitions l -> action_index -> l' *)
			(*** NOTE: type transition = guard * clock_updates * discrete_update list * location_index ***)
			let transitions = List.filter (fun (_,_, target) -> target = l') (model.transitions automaton_index l action_index) in

			(* If none: then not concerned -> no reset nor update *)
			if List.length transitions = 0 then No_update

			(** TODO: Here we need to evaluate the conditions  *)
			(* If exactly one: good situation: find the clock_updates *)
			else if List.length transitions = 1
				then let _,updates,_ = List.nth transitions 0 in updates.clock (*HERE*)
				(* If more than one: take the first one (*** HACK ***) and warn *)
				else (
					(* Warning *)
					print_warning ("Non-deterministic PTA! Selecting an edge arbitrarily among the " ^ (string_of_int (List.length transitions)) ^ " transitions from '" ^ (model.location_names automaton_index l) ^ "' via action '" ^ (model.action_names action_index) ^ "' to '" ^ (model.location_names automaton_index l') ^ "' in automaton '" ^ (model.automata_names automaton_index) ^ "'.");

					(* Take arbitrarily the first element *)
					let _,updates,_ = List.nth transitions 0 in updates.clock (*HERE*)
				)

		(* Otherwise, if the source and target locations differ: necessarily a transition with this action *)
		) else (
			(* Find the transitions l -> action_index -> l' *)
			let transitions = List.filter (fun (_,_, target) -> target = l') (model.transitions automaton_index l action_index) in

			(* There cannot be none *)
			if List.length transitions = 0 then raise (raise (InternalError("There cannot be no transition from '" ^ (model.location_names automaton_index l) ^ "' to '" ^ (model.location_names automaton_index l') ^ "' with action to '" ^ (model.action_names action_index) ^ "' in automaton '" ^ (model.automata_names automaton_index) ^ ".")))

			(* If exactly one: good situation: return the clock_updates *)
			else if List.length transitions = 1
			then let _,updates,_ = List.nth transitions 0 in updates.clock (*HERE*)
			(* If more than one: take the first one (*** HACK ***) and warn *)
			else (
				(* Warning *)
				print_warning ("Non-deterministic PTA! Selecting an edge arbitrarily among the " ^ (string_of_int (List.length transitions)) ^ " transitions from '" ^ (model.location_names automaton_index l) ^ "' via action '" ^ (model.action_names action_index) ^ "' to '" ^ (model.location_names automaton_index l') ^ "' in automaton '" ^ (model.automata_names automaton_index) ^ "'.");

				(* Take arbitrarily the first element *)
				let _,updates,_ = List.nth transitions 0 in updates.clock (*HERE*)
			)
		) in

		let local_clock_resets =
		(*** WARNING: we only accept clock resets (no arbitrary updates) ***)
		match clock_updates with
			(* No update at all *)
			| No_update -> []
			(* Reset to 0 only *)
			| Resets clock_resets -> clock_resets
			(* Reset to arbitrary value (including discrete, parameters and clocks) *)
			| Updates _ -> raise (NotImplemented "Only clock resets are allowed for now in StateSpace.get_resets")
		in

		(* Add the guard *)
		clock_resets := List.rev_append !clock_resets local_clock_resets;

	) model.automata;

	(* Keep each clock once *)
	let unique_clock_resets = list_only_once !clock_resets in

	(* Finally! Return the list of clocks to reset *)
	unique_clock_resets



(************************************************************)
(** SCC detection *)
(************************************************************)

(*** NOTE: this part is heavily based on the Tarjan's strongly connected components algorithm, as presented on Wikipedia, with some mild modification:
	- the top level algorithm is run once only (as we start from a state which we know belongs to the desired SCC)
	- the other SCCs are discarded
***)

(* Data structure: node with index / lowlink / onStack *)
type tarjan_node = {
	(* Additional field used to record the real state_index in the state space *)
	state_index	: state_index;
	mutable index		: state_index option;
	mutable lowlink		: state_index option;
	mutable onStack		: bool;
}

(* Exception raised when the SCC is found *)
exception Found_scc of scc


(* When a state is encountered for a second time, then a loop exists (or more generally an SCC): 'reconstruct_scc state_space source_state_index' reconstructs the SCC from source_state_index to source_state_index (using the actions) using a variant of Tarjan's strongly connected components algorithm; returns None if no SCC found *)
(*** NOTE: added the requirement that a single node is not an SCC in our setting ***)
let reconstruct_scc state_space source_state_index : scc option =
	(* Print some information *)
	print_message Verbose_high ("Entering reconstruct_scc " ^ (string_of_state_index source_state_index) ^ "…");

	(* Variables used for Tarjan *)
	let current_index = ref 0 in

	(* Hashtable state_index -> tarjan_node *)
	let tarjan_nodes = Hashtbl.create Constants.guessed_nb_states_for_hashtable in

	(* tarjan_node stack *)
	let stack = Stack.create () in

	(* Get the tarjan_node associated with a state_index; create it if not found in the Hashtable *)
	let tarjan_node_of_state_index state_index =
		(* If present in the hashtable already *)
		if Hashtbl.mem tarjan_nodes state_index then(
			Hashtbl.find tarjan_nodes state_index
		)else(
			(* Create an empty node *)
			let tarjan_node = {
				state_index = state_index;
				index		= None;
				lowlink		= None;
				onStack		= false;
			} in
			(* Add it *)
			Hashtbl.add tarjan_nodes state_index tarjan_node;
			(* Return it *)
			tarjan_node
		)
	in

	(* Get the index field from a tarjan_node; raise InternalError if index = None *)
	let get_index tarjan_node =
		match tarjan_node.index with
			| Some ll -> ll
			| _ -> raise (InternalError "v.index should not be undefined at that point")
	in

	(* Get the lowlink field from a tarjan_node; raise InternalError if lowlink = None *)
	let get_lowlink tarjan_node =
		match tarjan_node.lowlink with
			| Some ll -> ll
			| _ -> raise (InternalError "v.lowlink should not be undefined at that point")
	in


	(* Define a main local, recursive function *)
	let rec strongconnect state_index =
		(* Print some information *)
		print_message Verbose_high ("  Entering strongconnect " ^ (string_of_state_index state_index) ^ "…");

		(* Get the associated tarjan node *)
		let v = tarjan_node_of_state_index state_index in
		(* Set the depth index for v to the smallest unused index *)
		(*v.index := index*)
		v.index <- Some !current_index;

		(*v.lowlink := index*)
		v.lowlink <- Some !current_index;

		(*index := index + 1*)
		incr current_index;

		(* S.push(v) *)
		Stack.push v stack;

		(* Print some information *)
		print_message Verbose_high ("  Push " ^ (string_of_state_index state_index) ^ "");

		(* v.onStack := true *)
		v.onStack <- true;

		(* Print some information *)
		print_message Verbose_high ("  Considering successors of " ^ (string_of_state_index state_index) ^ "…");

		(* Consider successors of v *)
		(* for each (v, w) in E do *)
		let successors = get_successors state_space state_index in

		(* Print some information *)
		if verbose_mode_greater Verbose_high then(
			print_message Verbose_high (string_of_list_of_string_with_sep ", " (List.map string_of_state_index successors));
		);

		List.iter (fun successor_index ->
			(* Print some information *)
			print_message Verbose_high ("    Considering successor " ^ (string_of_state_index successor_index) ^ "");

			let w = tarjan_node_of_state_index successor_index in
			(* if (w.index is undefined) then *)
			if w.index = None then (
			(* // Successor w has not yet been visited; recurse on it
				strongconnect(w) *)
				strongconnect successor_index;

				(* v.lowlink  := min(v.lowlink, w.lowlink) *)

				let vlowlink = get_lowlink v in
				let wlowlink = get_lowlink w in

				(* Print some information *)
				print_message Verbose_high ("    " ^ (string_of_state_index v.state_index) ^ ".lowlink  := min(" ^ (string_of_state_index v.state_index) ^ ".lowlink, " ^ (string_of_state_index w.state_index) ^ ".lowlink) = min(" ^ (string_of_int vlowlink) ^ ", " ^ (string_of_int wlowlink) ^ ")");

				if wlowlink < vlowlink then(
					v.lowlink <- Some wlowlink;
				);
			(* else if (w.onStack) then *)
			) else if w.onStack then (
				(* // Successor w is in stack S and hence in the current SCC
				v.lowlink  := min(v.lowlink, w.index) *)
				let vlowlink = get_lowlink v in
				let windex = get_index w in
				if windex < vlowlink then v.lowlink <- Some windex;
			(* end if *)
			);
		(* end for *)
		) successors;

		(* // If v is a root node, pop the stack and generate an SCC *)
		(* if (v.lowlink = v.index) then *)
		let vlowlink = get_lowlink v in
		let vindex = get_index v in

		if vlowlink = vindex then (
			(* Print some information *)
			print_message Verbose_high ("  Root node! Start SCC");

			(* start a new strongly connected component *)
			let scc : scc ref = ref [] in

			(* repeat *)
			let found_v = ref false in
			while not !found_v do
				(* w := S.pop() *)
				let w = Stack.pop stack in

				(* Print some information *)
				print_message Verbose_high ("  Pop " ^ (string_of_state_index w.state_index) ^ "");

				(* w.onStack := false *)
				w.onStack <- false;

				(* add w to current strongly connected component *)
				scc := w.state_index :: !scc;

				if w = v then found_v := true
			(* while (w != v) *)
			done;

			(* output the current strongly connected component *)
			(* In fact, we differ from the traditional Tarjan here: if the original source_state_index belongs to the scc, we are done; otherwise we keep working and this scc is discarded *)
			if List.mem source_state_index !scc then(

				(* In addition, the SCC must not be reduced to a singleton without a loop *)
				if List.length !scc > 1 then(
					print_message Verbose_high ("  Found SCC of size > 1 containing " ^ (string_of_state_index source_state_index) ^ "!");

					raise (Found_scc !scc)
				)else(
					(* If reduced to a state: must contain a self-loop, i.e., source_state_index must belong to its successors *)
					let successors = get_successors state_space source_state_index in

					(* Print some information *)
					if verbose_mode_greater Verbose_high then(
						print_message Verbose_high ("  Successors of " ^ (string_of_state_index source_state_index) ^ ": " ^ (string_of_list_of_string_with_sep ", " (List.map string_of_state_index successors)) );
					);

					let found_scc = List.mem source_state_index successors in

					if found_scc then(
						(* Print some information *)
						print_message Verbose_high ("  Found SCC containing exactly " ^ (string_of_state_index source_state_index) ^ "!");

						raise (Found_scc !scc)
					)else(
						(* Print some information *)
						print_message Verbose_high ("  Found SCC containing exactly " ^ (string_of_state_index source_state_index) ^ " but without a loop: discard.");
					);
				);
			)else(
				(* Print some information *)
				print_message Verbose_high ("  SCC NOT containing " ^ (string_of_state_index source_state_index) ^ ": discard.");
			);
		(* end if *)
		);

		(* Print some information *)
		print_message Verbose_high ("  Exiting strongconnect " ^ (string_of_state_index state_index) ^ ".");

		(* end local function strongconnect *)
		()


	in

	(* Call strongconnect from the desired starting node *)
	let scc =
	try
		strongconnect source_state_index;

		(* If an exception was not raised, we reach this point *)
		print_message Verbose_high ("SCC not found in strongconnect!");
		None
	with
		(* if an exception was raised while looking for the SCC: we found the SCC! *)
		Found_scc scc -> (
			(* Return the scc *)
			Some scc
		)
	in
	(* Return SCC *)
	scc


(*------------------------------------------------------------*)
(** From a set of states, return all transitions within this set of states, in the form of a triple (state_index, action_index, state_index) *)
(*------------------------------------------------------------*)
let find_transitions_in state_space (scc : scc) : (state_index * action_index * state_index) list =
	List.fold_left (fun current_list state_index ->
		(* Compute the successors of state_index *)
		let successors = get_successors_with_actions state_space state_index in

		(* Filter only those who belong to the scc *)
		let successors_in_scc = List.filter (fun (state_index, _) -> List.mem state_index scc) successors in

		(* Convert into triple (state_index, action_index, state_index) *)
		let triples = List.map (fun (target_state_index, action_index) -> (state_index, action_index, target_state_index) ) successors_in_scc in

		(* Add them to the current list *)
		List.rev_append triples current_list
	) [] scc



(************************************************************)
(** Actions on a state space *)
(************************************************************)
(* Old state found *)
exception Found_old of state_index
(* State found that is smaller that the new one (in which case the old state will be replaced with the new one) *)
exception Found_new of state_index

(** Increment the number of generated states (even though not member of the state space) *)
let increment_nb_gen_states state_space =
	state_space.nb_generated_states := !(state_space.nb_generated_states) + 1


(** compute a hash code for a state, depending only on the location *)
let hash_code (location, _) =
	Location.hash_code location


(** Check if two states are equal *)
let states_equal state1 state2 =
	let (loc1, constr1) = state1 in
	let (loc2, constr2) = state2 in
	if not (Location.location_equal loc1 loc2) then false else (
		(* Statistics *)
		print_message Verbose_high ("About to compare equality between two constraints.");

		(* Statistics *)
		statespace_dcounter_nb_constraint_comparisons#increment;

		if verbose_mode_greater Verbose_high then(
			let nb_comparisons = statespace_dcounter_nb_constraint_comparisons#discrete_value in
			print_message Verbose_high ("Already performed " ^ (string_of_int nb_comparisons) ^ " constraint comparison" ^ (s_of_int nb_comparisons) ^ ".");
		);

		LinearConstraint.px_is_equal constr1 constr2
	)

(* Check dynamically if two states are equal*)
let states_equal_dyn state1 state2 constr =
	let (loc1, constr1) = state1 in
	let (loc2, constr2) = state2 in
	if not (Location.location_equal loc1 loc2) then false else (
		(* Statistics *)
		print_message Verbose_high ("About to compare (dynamic) equality between two constraints.");

		(* Statistics *)
		statespace_dcounter_nb_constraint_comparisons#increment;

		if verbose_mode_greater Verbose_high then(
			let nb_comparisons = statespace_dcounter_nb_constraint_comparisons#discrete_value in
			print_message Verbose_high ("Already performed " ^ (string_of_int nb_comparisons) ^ " constraint comparison" ^ (s_of_int nb_comparisons) ^ ".");
		);

		(*** WARNING!!! Really sure that one wants do MODIFY the constraints here?!!! ***)
		LinearConstraint.px_intersection_assign constr1  [constr];
		LinearConstraint.px_intersection_assign constr2 [constr];
		LinearConstraint.px_is_equal constr1 constr2
	)


(** Check if a state is included in another one *)
(* (Despite the test based on the hash table, this is still necessary in case of hash collisions) *)
let state_included state1 state2 =
	let (loc1, constr1) = state1 in
	let (loc2, constr2) = state2 in
	if not (Location.location_equal loc1 loc2) then false else (

		(* Statistics *)
		statespace_dcounter_nb_constraint_comparisons#increment;

		if verbose_mode_greater Verbose_high then(
			let nb_comparisons = statespace_dcounter_nb_constraint_comparisons#discrete_value in
			print_message Verbose_high ("Already performed " ^ (string_of_int nb_comparisons) ^ " constraint comparison" ^ (s_of_int nb_comparisons) ^ ".");
		);

		LinearConstraint.px_is_leq constr1 constr2
	)


(** Perform the insertion of a new state in a state space *)
let insert_state state_space hash new_state =
	(* Compute the new state index *)
	let new_state_index = !(state_space.next_state_index) in
	(* Retrieve the location and the constraint *)
	let location, linear_constraint = new_state in
	(* Try to find the location index *)
	let location_index = try (
		Hashtbl.find state_space.index_of_locations location
	) with Not_found -> (
	(* If not found: add it *)
		(* Find new index *)
		let new_index = Hashtbl.length state_space.index_of_locations in
		(* Add to hash table *)
		Hashtbl.add state_space.index_of_locations location new_index;
		(* Add to Dyn Array *)
		DynArray.add state_space.locations location;
		(* Check length (COULD BE REMOVED) *)
		if DynArray.length state_space.locations != Hashtbl.length state_space.index_of_locations then(
			raise (InternalError "Locations and index_of_locations seem not to be consistent anymore.");
		);
		(* Return new index *)
		new_index;
	) in



(*	print_warning "warning: consistency check";
	(* Consistency check: the state should NOT be present (otherwise this is a duplicate) *)
	Hashtbl.iter (fun state_index _ ->
(* 		print_string "."; *)
		let state = get_state state_space state_index in
		if states_equal state new_state then(
			raise (InternalError "Trying to add a state that is present");
		);
	) state_space.all_states;*)

	(* Set the initial state if not yet set *)
	if state_space.initial = None then(
		print_message Verbose_low ("Initial state set in the reachability state_space.");
		state_space.initial <- Some new_state_index;
	);


	(* Add the state to the tables *)
	Hashtbl.add state_space.all_states new_state_index (location_index, linear_constraint);
	Hashtbl.add state_space.states_for_comparison hash new_state_index;
	(* Update next state index *)
	state_space.next_state_index := !(state_space.next_state_index) + 1;
	(* Return state_index *)
	new_state_index


(*(**** TO DO: merge with add_state !!! *)
(** Add a state to a state space, if it is not present yet with the on-the-fly intersection *)
let add_state_dyn program state_space new_state constr =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	(* compute hash value for the new state *)
	let hash = hash_code new_state in
	if verbose_mode_greater Verbose_total then (
		print_message Verbose_standard ("hash : " ^ (string_of_int hash));
	);
	(* In tree mode: does not test anything *)
	if options#tree then (
		(* Since the state does NOT belong to the state space: find the state index *)
		let new_state_index = insert_state state_space hash new_state in
		(* Return state_index, true *)
		new_state_index, true
	) else (
		(* The check used for equality *)
		let check_states = states_equal_dyn in
		try (
			(* use hash table to find states with same locations (modulo hash collisions) *)
			let old_states = Hashtbl.find_all state_space.states_for_comparison hash in
			if verbose_mode_greater Verbose_total then (
				let nb_old = List.length old_states in
				print_message Verbose_total ("hashed list of length " ^ (string_of_int nb_old));
			);

			(* Statistics *)
			print_message Verbose_medium ("About to compare new state with " ^ (string_of_int (List.length old_states)) ^ " state(s).");
			nb_state_comparisons := !nb_state_comparisons + (List.length old_states);
			print_message Verbose_medium ("Already performed " ^ (string_of_int (!nb_state_comparisons)) ^ " comparisons.");

			List.iter (fun index ->
				let state = get_state state_space index in
				if check_states new_state state constr then raise (Found index)
			) old_states;
			(* Not found -> insert state *)
			let new_state_index = insert_state state_space hash new_state in
			(* Return state_index, true *)
			new_state_index, true
		)	with Found state_index -> (
				state_index, false
		)
	)*)


(** Replace the constraint of a state in a state space by another one (the constraint is copied to avoid side-effects later) *)
let replace_constraint state_space state_index px_linear_constraint =
	(* Copy to avoid side-effects *)
	let linear_constraint_copy = LinearConstraint.px_copy px_linear_constraint in
	try (
		(* Get the location index *)
		let location_index, _ = Hashtbl.find state_space.all_states state_index in
		(* Replace with the new constraint *)
		Hashtbl.replace state_space.all_states state_index (location_index, linear_constraint_copy);
	) with Not_found -> raise (InternalError ("Error when replacing state '" ^ (string_of_int state_index) ^ "' in StateSpace.replace_constraint."))



(** Add a state to a state space: takes as input the state space, a comparison instruction, the state to add, and returns whether the state was indeed added or not *)
let add_state state_space state_comparison new_state =
	(* Statistics *)
	counter_add_state#increment;
	counter_add_state#start;

	(* Retrieve the input options *)
(* 	let options = Input.get_options () in *)

	let result =

	(* compute hash value for the new state *)
	let hash = hash_code new_state in
	if verbose_mode_greater Verbose_total then (
		print_message Verbose_total ("hash : " ^ (string_of_int hash));
	);
	(* If no check requested: does not test anything *)
	if state_comparison = No_check then (
		(* Since the state does NOT belong to the state space: insert directly and find the state index *)
		let new_state_index = insert_state state_space hash new_state in
		(* Return state_index  *)
		New_state new_state_index
	) else (
		try (
			(* use hash table to find all states with same locations (modulo hash collisions) *)
			let old_states = Hashtbl.find_all state_space.states_for_comparison hash in
			if verbose_mode_greater Verbose_total then (
				let nb_old = List.length old_states in
				print_message Verbose_total ("hashed list of length " ^ (string_of_int nb_old));
			);

			(* Statistics *)
			print_message Verbose_medium ("About to compare new state with " ^ (string_of_int (List.length old_states)) ^ " state" ^ (s_of_int (List.length old_states)) ^ ".");

(* 			statespace_dcounter_nb_state_comparisons#increment_by (List.length old_states); *)

(* 			nb_state_comparisons := !nb_state_comparisons + (List.length old_states); *)
			if verbose_mode_greater Verbose_medium then(
				let nb_comparisons = statespace_dcounter_nb_state_comparisons#discrete_value in
				print_message Verbose_medium ("Already performed " ^ (string_of_int nb_comparisons) ^ " state comparison" ^ (s_of_int nb_comparisons) ^ ".");
			);

			(* Iterate on each state *)
			List.iter (fun index ->
				let state = get_state state_space index in

				(* Branch depending on the check function used for state comparison *)
				match state_comparison with

					(* No_check: case considered above already *)
					| No_check -> raise (InternalError("Case 'No_check' should have been handled before, in function StateSpace.add_state"))

					(* Equality: check for equality *)
					| Equality_check ->
						statespace_dcounter_nb_state_comparisons#increment;
						if states_equal new_state state then raise (Found_old index)

					(* Inclusion: check for new <= old *)
					| Inclusion_check ->
						statespace_dcounter_nb_state_comparisons#increment;
						if state_included new_state state then(
							(* Statistics *)
							statespace_dcounter_nb_states_included#increment;
							raise (Found_old index)
						)

					(* Double inclusion: check for new <= old OR old <= new, in which case replace *)
					| Double_inclusion_check ->
						(* First check: new <= old *)
						statespace_dcounter_nb_state_comparisons#increment;
						if state_included new_state state then(
							(* Statistics *)
							statespace_dcounter_nb_states_included#increment;
							raise (Found_old index)
						)
						(* Second check: old <= new *)
						else(
						statespace_dcounter_nb_state_comparisons#increment;
						if state_included state new_state then (
							(* Print some information *)
							print_message Verbose_medium ("Found an old state <= the new state");

							(* Retrieve the constraint *)
							let _, new_constraint = new_state in

							(* Replace old with new *)
							replace_constraint state_space index new_constraint;

							(* Statistics *)
							statespace_dcounter_nb_states_including#increment;

							(* Stop looking for states *)
							raise (Found_new index)
						))

			) old_states;

			(* Not found -> insert state *)
			let new_state_index = insert_state state_space hash new_state in

			(* Return *)
			New_state new_state_index
		)	with
			| Found_old state_index -> State_already_present state_index
			| Found_new state_index -> State_replacing state_index
	)

	in
	(* Statistics *)
	counter_add_state#stop;
	result


let get_transitions_of_state state_space source_state_index action_index =
	try (
		Hashtbl.find_all state_space.transitions_table (source_state_index, action_index)
	)	with Not_found -> []


(** Add a transition to the state space. Transitions are unique in that this
    function will refuse to add two transitions with the same source and
		target index and the same label. *)
let add_transition state_space (source_state_index, action_index, target_state_index) =
	(* Statistics *)
	counter_add_transition#increment;
	counter_add_transition#start;

	(* check if it already exists *)
	let transitions = get_transitions_of_state state_space source_state_index action_index in
	if not (List.mem target_state_index transitions) then
		Hashtbl.add state_space.transitions_table (source_state_index, action_index) target_state_index
	;
	(* Statistics *)
	counter_add_transition#stop;
	()


(*
(** Add a transition to the state space *)
let add_transition state_space (source_state_index, action_index, target_state_index) =
	Hashtbl.add state_space.transitions_table (source_state_index, action_index) target_state_index*)


(** Add an inequality to all the states of the state space *)
(*** NOTE: it is assumed that the p_constraint does not render some states inconsistent! ***)
let add_p_constraint_to_states state_space p_constraint =
(* 	let constraint_to_add = LinearConstraint.make_p_constraint [inequality] in *)
	(* For all state: *)
	iterate_on_states (fun _ (_, constr) ->
		 LinearConstraint.px_intersection_assign_p constr [p_constraint]
	) state_space



(*
(** Merge two states by replacing the second one with the first one, in the whole state_space structure (lists of states, and transitions) *)
let merge_2_states state_space state_index1 state_index2 =
	(* Retrieve state2 (for hash later *)
	let location_index2, constr2 = Hashtbl.find state_space.all_states state_index2 in
	let state2 = get_location state_space location_index2, constr2 in

	(*-------------------------------------------------------------*)
	(* Replace s2 with s1 in transition table *)
	(*-------------------------------------------------------------*)
	(* First copy the table (MEMORY CONSUMING! but necessary in order to avoid unexpected behaviors) *)
	let transitions_copy = Hashtbl.copy state_space.transitions_table in
	Hashtbl.iter (fun (source_state_index, action_index) target_state_index ->
		(* Replace if source *)
		if source_state_index = state_index2 then (
			(* Replace target if needed *)
			let new_target_state_index = if target_state_index = state_index2 then state_index1 else target_state_index in
			Hashtbl.remove state_space.transitions_table (source_state_index, action_index);
			if Hashtbl.mem state_space.transitions_table (state_index1, action_index) && (Hashtbl.find state_space.transitions_table (state_index1, action_index)) = new_target_state_index then (

				(* TO RECHECK (exception disabled by Etienne, 16/03/2012 *)


				(* Unexpected case *)
(* 				raise (InternalError ("Error when merging states: a couple '(source_state_index, action_index)' is already bound in the transitions table.")); *)
			)else
				Hashtbl.add state_space.transitions_table (state_index1, action_index) new_target_state_index;
		)
		(* Replace if target *)
		else (if target_state_index = state_index2 then (
			Hashtbl.remove state_space.transitions_table (source_state_index, action_index);
			if Hashtbl.mem state_space.transitions_table (source_state_index, action_index)  && (Hashtbl.find state_space.transitions_table (source_state_index, action_index)) = state_index1 then (

				(* TO RECHECK (exception disabled by Etienne, 16/03/2012 *)

				(* Unexpected case *)
(* 				raise (InternalError ("Error when merging states: a couple '(source_state_index, action_index)' is already bound in the transitions table.")); *)
			)else
				Hashtbl.add state_space.transitions_table (source_state_index, action_index) state_index1;
		);
		(* Else do nothing *)
		)
	) transitions_copy;

	(*-------------------------------------------------------------*)
	(* Remove s2 from state hashtable *)
	(*-------------------------------------------------------------*)
	Hashtbl.remove state_space.all_states state_index2;

	(*-------------------------------------------------------------*)
	(* Replace s2 with s1 in states_for_comparison *)
	(*-------------------------------------------------------------*)
	(* Find the hash *)
	let hash2 = hash_code state2 in
	(* Get all states with that hash *)
	let all_states_with_hash2 = Hashtbl.find_all state_space.states_for_comparison hash2 in
	(* Remove them all *)
	while Hashtbl.mem state_space.states_for_comparison hash2 do
		Hashtbl.remove state_space.states_for_comparison hash2;
	done;
	(* Add them back *)
	List.iter (fun state_index ->
		(* Only add if not state2 *)
		if state_index != state_index2 then Hashtbl.add state_space.states_for_comparison hash2 state_index;
	) all_states_with_hash2;


	(* TO DO: remove merged state from states_for_comparison *)


(*	(* First copy the table (MEMORY CONSUMING! but necessary in order to avoid unexpected behaviors) *)
	let states_for_comparison_copy = Hashtbl.copy state_space.states_for_comparison in
	(* Empty the original hashtable *)
	Hashtbl.clear state_space.states_for_comparison;
	(* Fill it again *)
	Hashtbl.iter (fun hash state_index ->
		(* Replace the state if needed *)
		let new_state_index = if state_index = state_index2 then state_index1 else state_index in
		(* Add it *)
		Hashtbl.add hash new_state_index;
	) states_for_comparison_copy; *)
	()*)



(** Merge two states by replacing the second one by the first one, in the whole state_space structure (lists of states, and transitions) *)
let merge_states_ulrich state_space merger_state_index merged =
	(* NOTE: 'merged' is usually very small, e.g., 1 or 2, so no need to optimize functions using 'merged *)
	print_message Verbose_high ("Merging: update tables for state '" ^ (string_of_int merger_state_index) ^ "' with " ^ (string_of_int (List.length merged)) ^ " merged.");

	(* Rebuild transitions table *)
	print_message Verbose_high ("Merging: update transition table, containing " ^ (string_of_int (Hashtbl.length state_space.transitions_table)) ^ " elements");
	let t' = Hashtbl.copy state_space.transitions_table in
	Hashtbl.clear state_space.transitions_table;
	Hashtbl.iter (fun (src, a) trg ->
		let src' = if (List.mem src merged) then merger_state_index else src
		and trg' = if (List.mem trg merged) then merger_state_index else trg in
		(* Add if not *)
		add_transition state_space (src', a, trg')
	) t';

	(* Remove merged from hash table *)
	print_message Verbose_high "Merging: update hash table";
	let the_state = get_state state_space merger_state_index in
	let h = hash_code the_state in
	(* Get all states with that hash *)
	let bucket = Hashtbl.find_all state_space.states_for_comparison h in
	print_message Verbose_high ("Merging: got " ^ (string_of_int (List.length bucket)) ^ " states with hash " ^ (string_of_int h));
	(* Remove them all *)
	while Hashtbl.mem state_space.states_for_comparison h do
		Hashtbl.remove state_space.states_for_comparison h;
	done;
	(* Add them back *)
	List.iter (fun y ->
		(* Only add if not to be merged *)
		if not (List.mem y merged) then Hashtbl.add state_space.states_for_comparison h y;
	) bucket;

	(* Remove merged from state table *)
	print_message Verbose_high "Merging: update state table";
	List.iter (fun s ->
		print_message Verbose_high ("Merging: remove state " ^ (string_of_int s));
(*		while Hashtbl.mem state_space.states s do *)
			Hashtbl.remove state_space.all_states s;

			(* If the state was the initial state: replace with the merger state_index *)
			(*** WARNING: situation not much tested ***)
			if s = (get_initial_state_index state_space) then(
				print_message Verbose_low ("The initial state in the reachability state_space has been merged with another one.");
				state_space.initial <- Some merger_state_index;
			);
(*		done*)
	) merged



(* Get states sharing the same location and discrete values from hash_table, excluding s *)
let get_siblings state_space si =
	let s = get_state state_space si in
	let l, _ = s in
	let h = hash_code s in
	let sibs = Hashtbl.find_all state_space.states_for_comparison h in
	(* check for exact correspondence (=> hash collisions!), and exclude si *)
	List.fold_left (fun siblings sj ->
		if sj = si then siblings else begin
			let l', c' = get_state state_space sj in
			if (Location.location_equal l l') then
				(sj, (l',c')) :: siblings
			else
				siblings
		end
	) [] sibs


(*module IntSet = Set.Make(
	struct
		type t = state_index
		let compare a b = compare a b
	end
)*)


(* Check if two states can be merged *)
(*** NOTE: with side-effects! ***)
let are_mergeable s s' =
	(* Statistics *)
	nb_merging_attempts#increment;

	(* Call dedicated function *)
	LinearConstraint.px_hull_assign_if_exact s s'


(* Try to merge new states with existing ones. Returns list of merged states (ULRICH) *)
let merge state_space new_states =

	(* function for merging one state with its siblings *)
	let merge_state si =
		print_message Verbose_total ("[merging] Try to merge state " ^ (string_of_int si));
		let l, c = get_state state_space si in
		(* get merge candidates as pairs (index, state) *)
		let candidates = get_siblings state_space si in
		(* try to merge with siblings, restart if merge found, return eaten states *)
		let rec eat all_mc rest_mc = begin
			match rest_mc with
				| [] -> [] (* here, we are really done *)
				| m :: tail_mc -> begin
					let sj, (_, c') = m in
					if are_mergeable c c' then begin
						(* Statistics *)
						nb_merged#increment;

						(* Print some information *)
						print_message Verbose_high ("[merging] State " ^ (string_of_int si) ^ " merged with state " ^ (string_of_int sj));

						(* we ate sj, start over with new bigger state, removing sj *)
						let all_mc' = List.filter (fun (sk, _) -> sk <> sj) all_mc in
						sj :: eat all_mc' all_mc'
					end else begin
						(* try to eat the rest of them *)
						eat all_mc tail_mc
					end
				end
		end
		in
		eat candidates candidates
	in

	(* Iterate list of new states and try to merge them, return eaten states *)
	let rec main_merger states =
		match states with
			| [] -> []
			| s :: ss -> begin
					let eaten = merge_state s in
					if eaten = [] then
						(* nothing merged -> go on with the rest *)
						main_merger ss
					else begin
						(* update transitions and state table *)
						merge_states_ulrich state_space s eaten;
(*						List.iter (fun state_to_be_eaten ->
							merge_2_states state_space s state_to_be_eaten
						) eaten;*)

						(* go on, skipping eaten states from the rest of the list *)
						(* Optimization: does not care of the order (otherwise use list_append *)
						List.rev_append eaten (main_merger (list_diff ss eaten))
					end
				end in

	(* Do it! *)
	let eaten = main_merger new_states in
	let nb_eaten = List.length eaten in
	let nb_orig = List.length new_states in
	if nb_eaten > 0 then
		print_message Verbose_standard ("  " ^ (string_of_int nb_eaten) ^ " state" ^ (s_of_int nb_eaten) ^ " merged within " ^ (string_of_int nb_orig) ^ " state" ^ (s_of_int nb_orig) ^ ".");

	(* return eaten states *)
	(*list_diff new_states*) eaten



(** Empties the hash table giving the set of states for a given location; optimization for the jobshop example, where one is not interested in comparing  a state of iteration n with states of iterations < n *)
let empty_states_for_comparison state_space =
	(* Statistics *)
	counter_empty_states_for_comparison#increment;
	counter_empty_states_for_comparison#start;

	Hashtbl.clear state_space.states_for_comparison;

	(* Statistics *)
	counter_empty_states_for_comparison#stop;

	()




(************************************************************)
(** Misc: tile natures *)
(************************************************************)
(** Convert a statespace_nature into a string *)
let string_of_statespace_nature = function
	| Good -> "good"
	| Bad -> "bad"
	| Unknown -> "unknown"
(* 	| _ -> raise (InternalError ("Tile nature should be good or bad only, so far ")) *)



(************************************************************)
(** Statistics *)
(************************************************************)


(*(** Get statistics on the number of comparisons between states *)
let get_statistics () =
	(string_of_int !nb_state_comparisons) ^ " comparison" ^ (s_of_int !nb_state_comparisons) ^ " between states were performed."
	^ "\n" ^ (string_of_int !nb_constraint_comparisons) ^ " comparison" ^ (s_of_int !nb_constraint_comparisons) ^ " between constraints were performed."*)


(** Get statistics on the structure of the states: number of different locations, number of different constraints *)
let get_statistics_states state_space =
	let nb_states = nb_states state_space in
	(* Compute the number of constraints per location *)
	let nb_constraints_per_location_id = Hashtbl.create (DynArray.length state_space.locations) in
	(* Compute the number of constraints equal to each other (list of couples (constraint, nb) )*)
	let nb_per_constraint = DynArray.make 0 in
	(* Iterate on all states *)
	iterate_on_states (fun _ (location_index, the_constraint) ->
		(* Find former nb of constraints for this location *)
		let former_nb = try
			Hashtbl.find nb_constraints_per_location_id location_index
		with Not_found -> 0 in
		(* Add +1 *)
		Hashtbl.replace nb_constraints_per_location_id location_index (former_nb + 1);

		(*(* Find former nb of constraints *)
		let _ =
		try(
			(* Iterate on the array *)
			DynArray.iteri (fun array_index (current_constraint, current_nb) ->
				if LinearConstraint.is_equal the_constraint current_constraint then(
					DynArray.set nb_per_constraint array_index (the_constraint, current_nb + 1);
					(* Found index *)
					raise (Found 0 (*(don't care) *));
				);
			) nb_per_constraint;
			(* If here: not found, i.e., new index *)
			DynArray.add nb_per_constraint (the_constraint, 1);
		)
		with Found _ -> ();
		in ();*)
	) state_space;

	let nb_locations = Hashtbl.length nb_constraints_per_location_id in
	let nb_different_constraints = DynArray.length nb_per_constraint in
	let result_string = ref (
		(string_of_int nb_states) ^ " state" ^ (s_of_int nb_states) ^ ", " ^ (string_of_int nb_locations) ^ " location" ^ (s_of_int nb_locations) ^ ", " ^ (string_of_int nb_different_constraints) ^ " constraint" ^ (s_of_int nb_different_constraints) ^ ""
		^ "\nNumber of constraints per locations:"
	) in

	(* Add number of constraints per location *)
	Hashtbl.iter (fun location nb_constraints ->
		result_string := !result_string ^ " - " ^ (string_of_int nb_constraints);
	) nb_constraints_per_location_id;
	(* Add average *)
	result_string := !result_string ^ "\nAverage: " ^ (round3_float ((float_of_int nb_states) /. (float_of_int nb_locations)));
	(*
	(* Add number per constraint *)
	result_string := !result_string ^ "\nNumber of occurrence of constraints: ";
	DynArray.iter (fun (the_constraint , nb_constraints) ->
		result_string := !result_string ^ " - " ^ (string_of_int nb_constraints);
	) nb_per_constraint;
	(* Add average *)
	result_string := !result_string ^ "\nAverage: " ^ (string_of_float ((float_of_int nb_different_constraints) /. (float_of_int nb_states)));
	*)
	(* Return result *)
	!result_string



(*(** Get the number of comparisons between states (performance checking purpose) *)
let get_nb_state_comparisons () =
(*	print_message Verbose_standard ("About to return the number of comparisons (" ^ (string_of_int !nb_state_comparisons) ^ ").");*)
	!nb_state_comparisons

(** Get the number of comparisons between constraints (performance checking purpose) *)
let get_nb_constraint_comparisons () =
	!nb_constraint_comparisons*)
