(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Description of the symbolic states and of the state space
 *
 * File contributors : Étienne André, Jaime Arias, Ulrich Kühne, Dylan Marinho
 * Created           : 2009/12/08
 *
 ************************************************************)



(************************************************************)
(* Modules *)
(************************************************************)

open Exceptions
open OCamlUtilities
open ImitatorUtilities
open Statistics
open Automaton
open State
open AbstractModel
open AbstractAlgorithm


(************************************************************)
(************************************************************)
(** Types *)
(************************************************************)
(************************************************************)

(************************************************************)
(** Nature of a state space according to some property *)
(************************************************************)
type statespace_nature =
	| Good
	| Bad
	| Unknown



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
(** Transitions *)
(************************************************************)

(** A combined transition is a list of transitions (one for each automaton involved) *)
type combined_transition = AbstractModel.transition_index list


(************************************************************)
(** Predecessors table *)
(************************************************************)
type predecessors_table = ((combined_transition * state_index) list) array



(************************************************************)
(** Concrete run *)
(************************************************************)

type concrete_step = {
	(* First let time elapse *)
	time			: NumConst.t;
	(* Then take a discrete transition *)
	transition		: combined_transition;
	(* Then reach the target state *)
	target			: State.concrete_state;
}

(*** WARNING: the structure is here initial state followed by (transition, state) list, but in symbolic_run, it is (state, transition) followed by final state :( ***)

type concrete_run = {
	(* The parameter valuation for which this run exists *)
	p_valuation		: PVal.pval;
	(* The initial concrete state *)
	initial_state	: State.concrete_state;
	(* A possibly empty list of steps *)
	steps			: concrete_step list;
}


(************************************************************)
(** Impossible concrete run *)
(************************************************************)

(* An impossible concrete run is a run that starts with a concrete run prefix, and then follows by taking transitions NOT admissible in the state space. Transitions may be imaginary, but locations remain existing locations. *)

type impossible_concrete_step = {
	(* First let time elapse *)
	time			: NumConst.t;
	(* Then take a discrete transition *)
	action			: Automaton.action_index;
	(* Then reach the target state *)
	target			: State.concrete_state;
}

type impossible_concrete_run = {
	(* The parameter valuation for which this run exists *)
	p_valuation		: PVal.pval;
	(* The initial concrete state *)
	initial_state	: State.concrete_state;
	(* A possibly empty list of steps *)
	steps			: concrete_step list;
	(* A non-empty list of imaginary steps *)
	impossible_steps: impossible_concrete_step list;
}




(************************************************************)
(** Symbolic run in a state space *)
(************************************************************)

(*** WARNING: the structure is here (state, transition) followed by final state, but in concrete_run, it is initial state followed by (transition, state) list :( ***)

type symbolic_step = {
	source			: State.state_index;
	transition		: combined_transition;
}

type symbolic_run = {
	symbolic_steps	: symbolic_step list;
	final_state		: State.state_index;
}


(************************************************************)
(** Set of state index *)
(************************************************************)

(* state struct for constructing set type *)
module StateStruct = struct
	type t = State.state_index
	let compare = compare
end

(* set of states for efficient lookup *)
module StateIndexSet = Set.Make(StateStruct)




(************************************************************)
(** State space structure *)
(************************************************************)


type state_space = {
	(** The number of generated states (even not added to the state space) *)
	nb_generated_states : int ref;

	(** An Array 'state_index' -> 'State.abstract_state'; contains ALL states *)
	mutable all_states : (State.state_index, abstract_state) Hashtbl.t;

	(** The id of the initial state *)
	(*** NOTE: mutable due to the fact that the initial state can be merged with another state *)
	mutable initial : State.state_index option;

	(** A hashtable location -> location_index *)
	index_of_locations : (DiscreteState.global_location, location_index) Hashtbl.t;

	(** A DynArray location_index -> location *)
	locations : DiscreteState.global_location DynArray.t;

	(** A hashtable to quickly find states with identical locations (? ; made by Ulrich); only for states to be compared *)
        (* modified by Jaco van de Pol: use global_location_index as key, rather than hash code of global_location *)
	mutable states_for_comparison : (location_index, State.state_index) Hashtbl.t;
	(*2.12 diff here*)

	(** A HashTable state_index -> list of (combined_transition * 'target_state_index') *)
	mutable transitions_table : (State.state_index , (combined_transition * State.state_index) list) Hashtbl.t;

	(** An integer that remembers the next index of state_index (may not be equal to the number of states, if states are removed *)
	next_state_index : State.state_index ref;
}




(************************************************************)
(** SCC detection *)
(************************************************************)

(** An SCC is just a list of states *)
type scc = State.state_index list

(*** NOTE: this part is heavily based on the Tarjan's strongly connected components algorithm, as presented on Wikipedia, with some mild modification:
	- the top level algorithm is run once only (as we start from a state which we know belongs to the desired SCC)
	- the other SCCs are discarded
***)

(* Data structure: node with index / lowlink / onStack *)
type tarjan_node = {
	(* Additional field used to record the real state_index in the state space *)
	state_index     : State.state_index;
	mutable index   : State.state_index option;
	mutable lowlink : State.state_index option;
	mutable onStack : bool;
}



(************************************************************)
(************************************************************)
(** Local exceptions *)
(************************************************************)
(************************************************************)
exception Found_cycle

(** Get the combined_transition between a state_index and its successor. Raise Not_found if no such transition exists. If several combined transitions exist, only the first one is retrieved. *)
(* local exception *)
exception Found_transition of combined_transition

(* Exception raised when the SCC is found *)
exception Found_scc of scc

(* Old state found *)
exception Found_old of State.state_index
(* State found that is smaller that the new one (in which case the old state will be replaced with the new one) *)
exception Found_new of State.state_index



(************************************************************)
(************************************************************)
(** Statistics *)
(************************************************************)
(************************************************************)

let statespace_dcounter_nb_state_comparisons = create_discrete_counter_and_register "number of state comparisons" States_counter Verbose_standard

(* Numbers of new states that were in fact included into an old state *)
let statespace_dcounter_nb_states_included = create_discrete_counter_and_register "number of new states <= old" States_counter Verbose_standard
(* Numbers of new states that were in fact larger than an old state *)
let statespace_dcounter_nb_states_including = create_discrete_counter_and_register "number of new states >= old" States_counter Verbose_standard

(* Numbers of merging attempts (for states that have the same discrete location) *)
let nb_merging_attempts = create_discrete_counter_and_register "StateSpace.merging attempts" States_counter Verbose_standard
(* Numbers of actual merges *)
let nb_merged = create_discrete_counter_and_register "StateSpace.merges" States_counter Verbose_standard

(* Complete time for merging *)
let tcounter_merge = create_time_counter_and_register "StateSpace.Merge time" States_counter Verbose_standard
(* Complete time for state space reconstruction while merging *)
let tcounter_merge_statespace = create_time_counter_and_register "StateSpace.Merge (reconstruct state space)" States_counter Verbose_standard

(* Functions *)
let counter_add_state = create_hybrid_counter_and_register "StateSpace.add_state" States_counter Verbose_experiments
let counter_compute_predecessors_with_combined_transitions = create_hybrid_counter_and_register "StateSpace.compute_predecessors" States_counter Verbose_experiments
let counter_get_location = create_hybrid_counter_and_register "StateSpace.counter_get_location" States_counter Verbose_experiments
let counter_get_state = create_hybrid_counter_and_register "get_state" States_counter Verbose_experiments

let counter_add_transition = create_hybrid_counter_and_register "StateSpace.add_transition" States_counter Verbose_experiments
let counter_get_successors = create_hybrid_counter_and_register "StateSpace.get_successors" States_counter Verbose_experiments
(* let counter_get_successors_with_actions = create_hybrid_counter_and_register "StateSpace.get_successors_with_actions" States_counter Verbose_experiments *)
let counter_get_transitions = create_hybrid_counter_and_register "StateSpace.get_transitions" States_counter Verbose_experiments
let counter_nb_states = create_hybrid_counter_and_register "StateSpace.nb_states" States_counter Verbose_experiments
let counter_nb_transitions = create_hybrid_counter_and_register "StateSpace.nb_transitions" States_counter Verbose_experiments
let counter_empty_states_for_comparison = create_hybrid_counter_and_register "StateSpace.empty_states_for_comparison" States_counter Verbose_experiments

let data_recorder_merging = create_data_recorder_and_register "StateSpace.merging_sequences" Algorithm_counter Verbose_experiments

(* Counters for experiments of merging-in-pta *)
let tcounter_skip_test = create_discrete_counter_and_register "StateSpace.Skip tests" Algorithm_counter Verbose_experiments



(************************************************************)
(************************************************************)
(** Class-independent functions *)
(************************************************************)
(************************************************************)


(************************************************************)
(** Debug string function *)
(************************************************************)
let string_of_state_index (state_index : State.state_index) = "s_" ^ (string_of_int state_index)


(************************************************************)
(** Class-independent function: state space nature *)
(************************************************************)

(** Convert a statespace_nature into a string *)
let string_of_statespace_nature = function
	| Good -> "good"
	| Bad -> "bad"
	| Unknown -> "unknown"


(**************************************************************)
(* Class-independent functions on combined transitions *)
(**************************************************************)

(** Get the (unique) action associated with a combined_transition (if entry undefined in the hashtable, then []) *)
let get_action_from_combined_transition (model : AbstractModel.abstract_model) (combined_transition : combined_transition) =
	(* The action is the same for any transition of a combined_transition, therefore pick up the first one *)
	let action_index =
	try (
		(* Get the first transition *)
		let first_transition_index = List.nth combined_transition 0 in
		(* Get the actual transition *)
		let transition = model.transitions_description first_transition_index in
		(* Get the action index *)
		transition.action
	) with Failure msg -> raise (InternalError ("Empty list of transitions in a combined_transition found in get_action_from_combined_transition, although this should not have been the case: `" ^ msg ^ "`."))
	in
	action_index


(*** NOTE: the function only works for regular resets (it raises NotImplemented for other updates) ***)
(*** TODO: allow for all resets ***)
let get_resets (model : AbstractModel.abstract_model) (state_index : State.state_index) combined_transition (state_index' : State.state_index) =
	(* For all transitions involved in the combined transition *)
	let resets = List.fold_left (fun current_resets transition_index ->
		(* Get the actual transition *)
		let transition = model.transitions_description transition_index in
		(* TODO benjamin IMPORTANT get potential clock update here, do we want potential or effective ? *)
        let potential_clock_updates, _ = transition.updates in
		(*** WARNING: we only accept clock resets (no arbitrary updates) ***)
		match potential_clock_updates with
			(* No update at all *)
			| No_potential_update -> current_resets
			(* Reset to 0 only *)
			| Potential_resets clock_resets -> List.rev_append clock_resets current_resets
			(* Reset to arbitrary value (including discrete, parameters and clocks) *)
			| Potential_updates _ -> raise (NotImplemented "Only clock resets are allowed for now in StateSpace.get_resets")
	) [] combined_transition
	in

	(* Keep each clock once *)
	list_only_once resets


(**************************************************************)
(* Class-independent function on DiscreteState hash code *)
(**************************************************************)

(** compute a hash code for a state, depending only on the location *)
let location_hash_code (state : state) =
	DiscreteState.hash_code state.global_location


(**************************************************************)
(* Class-independent functions on linear constraints *)
(**************************************************************)

(* Check if two states can be merged *)
(*** NOTE: with side-effects! ***)
(*** NOTE/TODO (ÉA, 2022/10/19): is that different from the are_mergeable method in Dylan's code? If not: merge! ***)
let are_mergeable_212 s s' =
	(* Statistics *)
	nb_merging_attempts#increment;

	(* Call dedicated function *)
	LinearConstraint.px_hull_assign_if_exact s s'


(* Check if two states can be merged *)
(*** NOTE: with side-effects! ***)
let are_mergeable_32 (c : LinearConstraint.px_linear_constraint) (c' : LinearConstraint.px_linear_constraint) : bool =
	(* Statistics *)
	nb_merging_attempts#increment;
	(* Call dedicated function *)
	let merged = LinearConstraint.px_hull_assign_if_exact c c' in
	(* Return result *)
	merged


(**************************************************************)
(**************************************************************)
(* Class definition *)
(**************************************************************)
(**************************************************************)
class stateSpace (guessed_nb_transitions : int) =

	(* Create initial state space *)
	let initial_state_space =
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

	print_message Verbose_high ("Creating empty state space with an initial guessed number of " ^ (string_of_int Constants.guessed_nb_states_for_hashtable) ^ " state" ^ (s_of_int Constants.guessed_nb_states_for_hashtable) ^ " and " ^ (string_of_int guessed_nb_transitions) ^ " transition" ^ (s_of_int guessed_nb_transitions) ^ ".");

	(* Create the state space *)
	{
		nb_generated_states   = ref 0;
		all_states            = states;
		initial               = None;
		index_of_locations    = index_of_locations;
		locations             = locations;
		states_for_comparison = states_for_comparison;
		transitions_table     = transitions_table;
		next_state_index      = ref 0;
	}
	in
	(*------------------------------------------------------------*)
	object (self)


	(************************************************************)
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	(************************************************************)
	val mutable state_space = initial_state_space


	(************************************************************)
	(************************************************************)
	(* Simple get methods *)
	(************************************************************)
	(************************************************************)
	(** Return the number of generated states (not necessarily present in the state space) *)
	method get_nb_gen_states : int = !(state_space.nb_generated_states)

	(** Return the number of states in a state space *)
	method nb_states : int =

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
	method nb_transitions : int =
		(* Statistics *)
		counter_nb_transitions#increment;
		counter_nb_transitions#start;

		let result =
			Hashtbl.fold (fun _ current_transitions current_nb ->
				current_nb + (List.length current_transitions)
			) state_space.transitions_table 0
		in

		(* Statistics *)
		counter_nb_transitions#stop;

		result

	(* Return the global_location corresponding to a location_index *)
	method get_location location_index =
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
	method get_state state_index : State.state =
		(* Statistics *)
		counter_get_state#increment;
		counter_get_state#start;

		(* Find the state *)
		let state =
			(* Exception just in case *)
			try (
				Hashtbl.find state_space.all_states state_index
			) with Not_found -> raise (InternalError ("State of index `" ^ (string_of_int state_index) ^ "` was not found in state_space (in function: get_state)."))
		in

		(* Find the pair (location_index, constraint) *)
		let location_index, linear_constraint = state.global_location_index, state.px_constraint in

		(* Find the location *)
		let global_location = self#get_location location_index in

		(* Statistics *)
		counter_get_state#stop;

		(* Return the state *)
		{ global_location = global_location; px_constraint = linear_constraint; }


	(** Return the global_location_index of a state_index *)
	method get_global_location_index state_index =
		(* Statistics *)
		counter_get_state#increment;
		counter_get_state#start;

		(* Find the location_index *)
		let location_index =
			(* Exception just in case *)
			try (
				(Hashtbl.find state_space.all_states state_index).global_location_index
			) with Not_found -> raise (InternalError ("State of index `" ^ (string_of_int state_index) ^ "` was not found in state_space (in function: StateSpace.get_global_location_index)."))
		in

		(* Statistics *)
		counter_get_state#stop;

		(* Return the location_index *)
		location_index

	(** return the list of states with the same location *)
	method get_comparable_states state_index =
			let location_index = self#get_global_location_index state_index in
			Hashtbl.find_all state_space.states_for_comparison location_index


	(** Return the index of the initial state, or raise Not_found if not defined *)
	method get_initial_state_index =
		match state_space.initial with
		| Some state_index -> state_index
		| None -> raise Not_found



	(** Return the table of transitions *)
	method get_transitions_table =

		(* Statistics *)
		counter_get_transitions#increment;
		counter_get_transitions#start;

		let result =
			state_space.transitions_table
		in

		(* Statistics *)
		counter_get_transitions#stop;

		result


	(** Compte and return the list of pairs (combined transition, index successor of a state) (if entry undefined in the hashtable, then []) *)
	method get_successors_with_combined_transitions (state_index : state_index) : (combined_transition * state_index) list =
		(* Get all successors with their combined transition *)
		hashtbl_get_or_default state_space.transitions_table state_index []


	(** Compte and return the list of index successors of a state (if entry undefined in the hashtable, then []) *)
	method get_successors state_index =
		(* Statistics *)
		counter_get_successors#increment;
		counter_get_successors#start;

		(*** NOTE: we get all pairs "transition , target", then we keep the second elements, i.e., the target state indexes ***)
		let _ , target_state_indices = List.split (self#get_successors_with_combined_transitions state_index) in

		(* We eliminate duplicates *)
		let result = list_only_once target_state_indices in

		(* Statistics *)
		counter_get_successors#stop;

		result


	(** Get all combined transitions from a state_index *)
	method private get_transitions_of_state state_index =
		(* Print some information *)
		print_message Verbose_total ("Entering StateSpace.get_transitions_of_state");

		let transitions_and_targets = self#get_successors_with_combined_transitions state_index in

		let result , _ = List.split transitions_and_targets in

		(* The end *)
		result




	method private get_combined_transition_between_states (source_state_index : state_index) (target_state_index : state_index) : combined_transition =
		(* Get all successors of source_state_index *)
		let successors = self#get_successors_with_combined_transitions source_state_index in

		try(
			(* Iterate *)

			List.iter (fun (combined_transition, state_index) -> if state_index = target_state_index then raise (Found_transition combined_transition)) successors;

			(* If not found *)
			raise Not_found
		) with Found_transition combined_transition -> combined_transition



	(************************************************************)
	(************************************************************)
	(* Methods computing things from the state space without modifications *)
	(************************************************************)
	(************************************************************)

	(*------------------------------------------------------------*)
	(** Compute and return a predecessor array state_index -> (combined_transition , state_index) list *)
	(*------------------------------------------------------------*)
	method compute_predecessors_with_combined_transitions : predecessors_table =

		(* Statistics *)
		counter_compute_predecessors_with_combined_transitions#increment;
		counter_compute_predecessors_with_combined_transitions#start;

		(* Print some information *)
		print_message Verbose_total "Computing predecessors…";

		(* Get the highest id of the state space *)
		(*** NOTE: we get the highest id and not the length of the Hashtbl due to the fact that states may be merged/removed by bidirectional inclusion ***)
		let highest_id = !(state_space.next_state_index) - 1 in

		(* Print some information *)
		print_message Verbose_total ("Creating an array of length " ^ (string_of_int (highest_id + 1)) ^ "");

		(* Create an array for predecessors: state_index -> (state_index, action_index) list *)
		let predecessors = Array.make (highest_id + 1) [] in

		(* Iterate on all states in the state space *)
		Hashtbl.iter(fun source_state_index _ ->
			(* Print some information *)
			print_message Verbose_total ("Retrieving successors of state #" ^ (string_of_int source_state_index));

			(* Get all successors of this state *)
			let successors = hashtbl_get_or_default state_space.transitions_table source_state_index [] in

			(* Print some information *)
			if verbose_mode_greater Verbose_total then(
				print_message Verbose_total ("Successors of state #" ^ (string_of_int source_state_index) ^ ": " ^ (string_of_list_of_string_with_sep "," (List.map (fun (_, state_index) -> string_of_int state_index) successors)));
			);

			(* Iterate on pairs (combined_transition * 'target_state_index') *)
			List.iter (fun (combined_transition, target_state_index) ->

				(* Print some information *)
				if verbose_mode_greater Verbose_total then(
					print_message Verbose_total ("Adding #" ^ (string_of_int source_state_index) ^ " to the predecessors of #" ^ (string_of_int target_state_index) ^ "");
				);

				(* Add to the predecessor array *)
				Array.set predecessors target_state_index (
					(* Add the new element *)
					(combined_transition , source_state_index)
					(* to *)
					::
					(* the former list *)
					predecessors.(target_state_index)
					)
				;

				(* Print some information *)
				if verbose_mode_greater Verbose_total then(
					print_message Verbose_total ("Predecessors of #" ^ (string_of_int target_state_index) ^ " now: " ^ (string_of_list_of_string_with_sep "," (List.map (fun (_, state_index) -> string_of_int state_index) predecessors.(target_state_index))));
				);

			) successors;


		) state_space.all_states;

		(* Statistics *)
		counter_compute_predecessors_with_combined_transitions#stop;

		(* Return structure *)
		predecessors


	(** Return the list of all state indexes *)
	method all_state_indexes = hashtbl_get_all_keys state_space.all_states

	(** Test if state index is in the current statespace *)
	method test_state_index state_index = Hashtbl.mem state_space.all_states state_index


	(*** WARNING: big memory, here! Why not perform intersection on the fly? *)

	(** Return the list of all constraints on the parameters associated to the states of a state space *)
	method all_p_constraints =
		Hashtbl.fold
			(fun _ state current_list ->
				let p_constraint = LinearConstraint.px_hide_nonparameters_and_collapse state.px_constraint in
				p_constraint :: current_list)
			state_space.all_states []



	(** Iterate over the reahable states *)
	method iterate_on_states f =
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
	method last_states =
		(* list to keep the resulting last states *)
		let last_states = ref [] in
		(* Table to keep all states already visited during DFS *)
		let dfs_table = ref StateIndexSet.empty in

		(* functional version for lookup *)
		let already_seen node = StateIndexSet.mem node !dfs_table in

		(* function to find all last states *)
		let rec cycle_detect node prefix =
			(* get all successors of current node *)
			let succs = self#get_successors node in
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
					loc_index = DiscreteState.get_location location aut_index
				) bad_states in
			exists_state is_bad_state state_space
		) *)


	(*------------------------------------------------------------*)
	(** Access to guard *)
	(*------------------------------------------------------------*)

	method get_guard (model : AbstractModel.abstract_model) state_index combined_transition =
		(* Retrieve source and target locations *)
		let (location : DiscreteState.global_location) = (self#get_state state_index).global_location in

		(* For all transitions involved in the combined transition *)
		let continuous_guards : LinearConstraint.pxd_linear_constraint list = List.fold_left (fun current_list_of_guards transition_index ->
			(* Get the actual transition *)
			let transition : AbstractModel.transition = model.transitions_description transition_index in

			(* Add the actual guard to the list *)
			match transition.guard with
				| True_guard -> current_list_of_guards
				(*** NOTE: we could optimize the case of False (because no need to go further); BUT the number of transitions is usually small and, most importantly, it is unlikely a guard is false in the model ***)
				| False_guard -> (LinearConstraint.pxd_false_constraint()) :: current_list_of_guards
				| Discrete_guard discrete_guard -> current_list_of_guards
				| Continuous_guard continuous_guard -> continuous_guard :: current_list_of_guards
				| Discrete_continuous_guard discrete_continuous_guard -> discrete_continuous_guard.continuous_guard :: current_list_of_guards

		) [] combined_transition
		in

		(* Replace with true if empty list *)
		let continuous_guards = if continuous_guards = [] then [LinearConstraint.pxd_true_constraint()] else continuous_guards in

		(*** NOTE (ÉA, 2019/05/30): Not sure what I did there??? ***)
		(* Compute constraint for assigning a (constant) value to discrete variables *)
		print_message Verbose_high ("Computing constraint for discrete variables");
		(* Only use rational discrete values for preparing constraint, this behavior was checked with Etienne A. *)
		let only_discrete_rational_values =
            List.map (fun discrete_index ->
                (* Get variable name of by index *)
                let variable_name = model.variable_names discrete_index in
                (* Get value of GLOBAL variable variable_name *)
                let value = DiscreteState.get_discrete_value_by_name location variable_name in
                discrete_index, value
            ) model.discrete_rationals
        in
        (* Get numconst values *)
		let discrete_rational_numconst_values = List.map (fun (discrete_index, discrete_value) -> discrete_index, AbstractValue.numconst_value discrete_value) only_discrete_rational_values in

		(* Constraint of the form D_i = d_i *)
		let discrete_constraint = LinearConstraint.pxd_constraint_of_point discrete_rational_numconst_values in

		(* Create the constraint guard ^ D_i = d_i *)
		LinearConstraint.pxd_intersection (discrete_constraint :: continuous_guards)




	(*------------------------------------------------------------*)
	(** SCC detection *)
	(*------------------------------------------------------------*)

	(* When a state is encountered for a second time, then a loop exists (or more generally an SCC): `reconstruct_scc state_space source_state_index` reconstructs the SCC from source_state_index to source_state_index (using the actions) using a variant of Tarjan's strongly connected components algorithm; returns None if no SCC found *)
	(*** NOTE: added the requirement that a single node is not an SCC in our setting ***)
	method reconstruct_scc source_state_index : scc option =
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
			let successors = self#get_successors state_index in

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
				(* while (w <> v) *)
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
						let successors = self#get_successors source_state_index in

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
	(** From a set of states, return all transitions within this set of states, in the form of a triple (state_index, combined_transition, state_index) *)
	(*------------------------------------------------------------*)
	method find_transitions_in (scc : scc) : (state_index * combined_transition * state_index) list =
		List.fold_left (fun current_list state_index ->
			(* Compute the successors of state_index *)
			let successors = self#get_successors_with_combined_transitions state_index in

			(* Filter only those who belong to the scc *)
			let successors_in_scc = List.filter (fun (_, state_index) -> List.mem state_index scc) successors in

			(* Convert into triple (state_index, combined_transition, state_index) *)
			let triples = List.map (fun (combined_transition , target_state_index) -> (state_index, combined_transition, target_state_index) ) successors_in_scc in

			(* Add them to the current list *)
			List.rev_append triples current_list
		) [] scc


	(************************************************************)
	(** Backward run-computation *)
	(************************************************************)


	(*------------------------------------------------------------*)
	(** Returns the symbolic run (list of pairs (state, combined transition)) from the source_state_index to the target_state_index. Can take a predecessors_table as an option, otherwise recomputes it from the state space. The list of transitions is ordered from the initial state to the target state; optionally one can pass a list of states (a "lasso") for which we already know the succession of state indices. the final (target) state is not included. Raise Not_found if run not found. *)
	(*------------------------------------------------------------*)

	method backward_symbolic_run (target_state_index : state_index) (lasso : state_index list) (source_state_index : state_index) (predecessors_table_option : predecessors_table option) : symbolic_run =
		(* First manage the predecessors_table *)
		let predecessors_table = match predecessors_table_option with
			(* If given: keep it *)
			| Some predecessors_table -> predecessors_table
			(* Otherwise: recompute *)
			| None -> self#compute_predecessors_with_combined_transitions
		in

		(*------------------------------------------------------------*)
		(** Table to color/mark states *)
		(*------------------------------------------------------------*)
		let colortable_create init_nb : ('a, bool) Hashtbl.t = Hashtbl.create init_nb in

		let mark colortable element =
			Hashtbl.replace colortable element true
		in

		(*let unmark colortable element =
			Hashtbl.replace colortable element false
		in *)

		let is_marked colortable element : bool =
			(*** NOTE: split the test in case OCaml does not evaluate the way we think it does ***)
			if not (Hashtbl.mem colortable element) then false
			else Hashtbl.find colortable element
		in

		(*------------------------------------------------------------*)

		(* Create a table to remember whether a state is marked or not *)
		(*** NOTE: use the number of states in the state space as default init ***)
		let colortable = colortable_create (self#nb_states) in

		(* Function to sort the predecessors made of a pair of a combined_transition and a state_index *)
		let sort_predecessors = List.sort (fun (_, a) (_, b) -> compare a b) in

		(*------------------------------------------------------------*)
		(* Use a recursive procedure returning a (list of (state, combined transition))'option ; None denotes the current run is useless. The states are returned in reversed order. *)
		let rec backward_symbolic_run_rec (current_state_index : state_index) : (symbolic_step list) option =
			(* If target is reached: return *)
			if current_state_index = source_state_index then Some [] (*** NOTE: do not add index, it will be added during the recursion together with the transition ***)

			(* If the state is marked, give up *)
			else if is_marked colortable current_state_index then None

			(* Else process this state *)
			else(
				(* Mark it! *)
				mark colortable current_state_index;

				(* Get the predecessors *)
				let predecessors = predecessors_table.(current_state_index) in

				(* Heuristics: test the predecessors by increasing state_index (intuitively, a smaller state_index may be closer to the initial state, hence should be tried first) *)
				let sorted_predecessors = sort_predecessors predecessors in

				(* Iterate on the predecessors *)
				let symbolic_steps : (symbolic_step list) option = List.fold_left (fun current_steps (combined_transition, predecessor_index) ->
					(* If predecessor is marked: skip and go to next predecessor *)
					if is_marked colortable predecessor_index then current_steps

					(* If unmarked: call recursively *)
					else
					let recursive_result = backward_symbolic_run_rec predecessor_index in
					match recursive_result with
						(* If no result in this direction: skip and go to next predecessor *)
						| None -> current_steps
						(* Otherwise: return the result and add the symbolic_step *)
						| Some symbolic_steps -> Some ({source = predecessor_index ; transition = combined_transition } :: symbolic_steps)

				) None sorted_predecessors in

				match symbolic_steps with
					(* If no steps found after iterating, we are in a deadlock *)
					| None -> None
					(* Otherwise return (do NOT add current state, as it was added above) *)
					| Some symbolic_steps -> Some symbolic_steps
			)

		in
		(*------------------------------------------------------------*)

		(* If a lasso is provided, first reconstruct the final symbolic steps for the lasso *)
		let (final_symbolic_steps , target_state_index_for_backward_reconstruction) = match lasso with
			(* No lasso *)
			| [] ->
				[] , target_state_index

			(* A non-empty lasso *)
			| first_state_of_the_lasso :: rest_of_the_lasso ->

				(*** BADPROG: mix imperative and functional programming… ***)
				let current_state_index : state_index ref = ref first_state_of_the_lasso in

				(* Convert each state of the lasso (except the last one) into a pair (state, combined_transition) *)
				let final_symbolic_steps : symbolic_step list =
				(* Iterate on the successor of each state of the lasso except the first one *)
				List.map (fun (current_successor : state_index) : symbolic_step ->
					(* Retrieve the transition (state, successor) *)
					let combined_transition : combined_transition = self#get_combined_transition_between_states !current_state_index current_successor in

					(* Compute *)
					let symbolic_step : symbolic_step=
						{source = !current_state_index ; transition = combined_transition }
					in

					(* Update current index for next step *)
					current_state_index := current_successor;

					(* Replace with the previously computed symbolic_step *)
					symbolic_step
				) rest_of_the_lasso
				in

				(*** NOTE: the last state of the lasso is actually the current value of !current_state_index ***)
				final_symbolic_steps, !current_state_index
		in

		(* Call the recursive procedure and reverse the result *)
		match backward_symbolic_run_rec target_state_index_for_backward_reconstruction with

		(* Oops! *)
		(*** NOTE: what if the lasso is non-empty in this situation?! ***)
		| None -> raise Not_found

		| Some symbolic_steps ->
			(* Construct the structure symbolic_run *)
			{
				(* Reverse because states were added in reversed order *)
				symbolic_steps = OCamlUtilities.list_append ( List.rev symbolic_steps) final_symbolic_steps;
				(* Add final state *)
				final_state = target_state_index;
			}


	(************************************************************)
	(** Statistics *)
	(************************************************************)

	(** Get statistics on the structure of the states: number of different locations, number of different constraints *)
	method get_statistics_states : string =
		let nb_states = self#nb_states in
		(* Compute the number of constraints per location *)
		let nb_constraints_per_location_id = Hashtbl.create (DynArray.length state_space.locations) in
		(* Compute the number of constraints equal to each other (list of pairs (constraint, nb) )*)
		let nb_per_constraint = DynArray.make 0 in
		(* Iterate on all states *)
		self#iterate_on_states (fun _ state ->
			(* Find former nb of constraints for this location *)
			let former_nb = try
				Hashtbl.find nb_constraints_per_location_id state.global_location_index
			with Not_found -> 0 in
			(* Add +1 *)
			Hashtbl.replace nb_constraints_per_location_id state.global_location_index (former_nb + 1);

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
		);

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



	(************************************************************)
	(************************************************************)
	(** Methods modifying the state space *)
	(************************************************************)
	(************************************************************)
	(** Increment the number of generated states (even though not member of the state space) *)
	method increment_nb_gen_states =
		state_space.nb_generated_states := !(state_space.nb_generated_states) + 1


	method private new_location_index location =
		let new_index = try (
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
			(* if DynArray.length state_space.locations <> Hashtbl.length state_space.index_of_locations then(
				raise (InternalError "Locations and index_of_locations seem not to be consistent anymore."); *)
				new_index;
		) in
		(* Return new index *)
		new_index

	(** Perform the insertion of a new state in a state space *)
	method private insert_state location_index (new_state : state) =
		(* Compute the new state index *)
		let new_state_index = !(state_space.next_state_index) in
		(* Retrieve the location and the constraint *)
		let location, linear_constraint = new_state.global_location, new_state.px_constraint in
	(*	print_warning "warning: consistency check";
		(* Consistency check: the state should NOT be present (otherwise this is a duplicate) *)
		Hashtbl.iter (fun state_index _ ->
	(* 		print_string "."; *)
			let state = get_state state_space state_index in
			if State.state_equals state new_state then(
				raise (InternalError "Trying to add a state that is present");
			);
		) state_space.all_states;*)


		(* Set the initial state if not yet set *)
		if state_space.initial = None then(
			print_message Verbose_low ("Initial state set in the reachability state_space.");
			state_space.initial <- Some new_state_index;
		);


		(* Add the state to the tables *)
		Hashtbl.add state_space.all_states new_state_index {global_location_index = location_index; px_constraint = linear_constraint;};
		Hashtbl.add state_space.states_for_comparison location_index new_state_index;
		(* Update next state index *)
		state_space.next_state_index := !(state_space.next_state_index) + 1;
		(* Return state_index *)
		new_state_index


	(** Replace the constraint of a state in a state space by another one (the constraint is copied to avoid side-effects later) *)
	method private replace_constraint (state_index : State.state_index) px_linear_constraint =
		(* Copy to avoid side-effects *)
		let linear_constraint_copy = LinearConstraint.px_copy px_linear_constraint in
		try (
			(* Get the location index *)
			let location_index = (Hashtbl.find state_space.all_states state_index).global_location_index in
			(* Replace with the new constraint *)
			Hashtbl.replace state_space.all_states state_index {global_location_index = location_index; px_constraint = linear_constraint_copy};
		) with Not_found -> raise (InternalError ("Error when replacing state `" ^ (string_of_int state_index) ^ "` in StateSpace.replace_constraint."))



	(** Add a state to a state space: takes as input the state space, a comparison instruction, the state to add, and returns whether the state was indeed added or not *)
	(*** NOTE: side-effects possible! If the former state is SMALLER than the new state and the state_comparison is Including_check, then the constraint of this former state is updated to the newer one ***)
	method add_state state_comparison (new_state : state) : addition_result =
		(* Statistics *)
		counter_add_state#increment;
		counter_add_state#start;

		let result : addition_result =

			let location_index = self#new_location_index new_state.global_location in
		(* If no check requested: does not test anything *)
		if state_comparison = No_check then (
			(* Since the state does NOT belong to the state space: insert directly and find the state index *)
			let new_state_index = self#insert_state location_index new_state in
			(* Return state_index  *)
			New_state new_state_index
		) else (
			try (
				(* use hash table to find all states with same locations*)
				let old_states = Hashtbl.find_all state_space.states_for_comparison location_index in
				if verbose_mode_greater Verbose_total then (
					let nb_old = List.length old_states in
					print_message Verbose_total ("The list of states with the same location has length " ^ (string_of_int nb_old));
				);

				(* Statistics *)
				print_message Verbose_medium ("About to compare new state with " ^ (string_of_int (List.length old_states)) ^ " old state" ^ (s_of_int (List.length old_states)) ^ ".");

	(* 			statespace_dcounter_nb_state_comparisons#increment_by (List.length old_states); *)

	(* 			nb_state_comparisons := !nb_state_comparisons + (List.length old_states); *)
				if verbose_mode_greater Verbose_medium then(
					let nb_comparisons = statespace_dcounter_nb_state_comparisons#discrete_value in
					print_message Verbose_medium ("Already performed " ^ (string_of_int nb_comparisons) ^ " state comparison" ^ (s_of_int nb_comparisons) ^ ".");
				);

				(* Prepare the removal of `global_time_clock` in comparisons, if needed *)
				(* Retrieve the input options *)
				let options = Input.get_options () in
				let clocks_to_remove_in_comparisons = if options#no_global_time_in_comparison then(
					(* Retrieve the model *)
					let model = Input.get_model() in
					match model.global_time_clock with
					(* Nothing to do *)
					| None -> []
					(* Nothing to do *)
					| Some global_time_clock -> [global_time_clock]
				)else(
					(* Nothing to remove *)
					[]
				)
				in

				(* Iterate on each state *)
				List.iter (fun (state_index : State.state_index) ->
					let state = self#get_state state_index in

					print_message Verbose_total ("Retrieved state #" ^ (string_of_int state_index) ^ ".");


					(* Branch depending on the check function used for state comparison *)
					match state_comparison with

						(* No_check: case considered above already *)
						| No_check -> raise (InternalError("Case `No_check` should have been handled before, in function StateSpace.add_state"))

						(* Equality: check for equality *)
						| Equality_check ->
							statespace_dcounter_nb_state_comparisons#increment;
							if State.state_equals new_state state clocks_to_remove_in_comparisons then raise (Found_old state_index)

						(* Inclusion: check for new <= old *)
						| Inclusion_check ->
							statespace_dcounter_nb_state_comparisons#increment;
							if State.state_included_in new_state state clocks_to_remove_in_comparisons then(
								(* Statistics *)
								statespace_dcounter_nb_states_included#increment;
								raise (Found_old state_index)
							)

						(* Inclusion: check for new >= old *)
						| Including_check ->
							statespace_dcounter_nb_state_comparisons#increment;
							if State.state_included_in state new_state clocks_to_remove_in_comparisons then(
								(* Statistics *)
								statespace_dcounter_nb_states_including#increment;

								(* Check if equality, i.e., reverse direction *)
								if State.state_included_in new_state state clocks_to_remove_in_comparisons then(
									(* Print some information *)
									print_message Verbose_medium ("Found an old state = the new state");

									(* Statistics *)
									statespace_dcounter_nb_states_included#increment;

									raise (Found_old state_index)
								)else(
									(* Print some information *)
									print_message Verbose_medium ("Found an old state < the new state");

									(* Replace old with new *)
									self#replace_constraint state_index new_state.px_constraint;

									(* Stop looking for states *)
									raise (Found_new state_index)
								)
							)

						(* Double inclusion: check for new <= old OR old <= new, in which case replace *)
						| Double_inclusion_check ->
							(* First check: new <= old *)
							statespace_dcounter_nb_state_comparisons#increment;
							if State.state_included_in new_state state clocks_to_remove_in_comparisons then(
								(* Statistics *)
								statespace_dcounter_nb_states_included#increment;
								raise (Found_old state_index)
							)
							(* Second check: old <= new *)
							else(
							statespace_dcounter_nb_state_comparisons#increment;
							if State.state_included_in state new_state clocks_to_remove_in_comparisons then (
								(* Print some information *)
								print_message Verbose_medium ("Found an old state < the new state");

								(* Replace old with new *)
								self#replace_constraint state_index new_state.px_constraint;

								(* Statistics *)
								statespace_dcounter_nb_states_including#increment;

								(* Stop looking for states *)
								raise (Found_new state_index)
							))

				) old_states;

				(* Not found -> insert state *)
				let new_state_index = self#insert_state location_index new_state in

				(* Print some information *)
				print_message Verbose_total ("Inserted new state #" ^ (string_of_int new_state_index) ^ ".");

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


	(** Add a combined transition to the state space *)
	method add_transition (source_state_index, combined_transition, target_state_index) : unit =
		(* Statistics *)
		counter_add_transition#increment;
		counter_add_transition#start;

		(* Print some information *)
		print_message Verbose_total ("Entering StateSpace.add_transition");

		(* check if it already exists *)
		(* Get the list of (transition,state) successors of source_state_index *)
		let transitions_and_states = self#get_successors_with_combined_transitions source_state_index in

		if List.mem (combined_transition , target_state_index) transitions_and_states then(
			print_message Verbose_total ("Transition belong to the list already");
		)else(
			(** Add to the data structure *)
			Hashtbl.replace state_space.transitions_table source_state_index
				(List.rev ((combined_transition , target_state_index) :: transitions_and_states))
		);

		(* Print some information *)
		print_message Verbose_total ("Existence check done");



		(*	(* check if it already exists *)
		let transitions = get_transitions_of_state state_space source_state_index in

		(* Print some information *)
		print_message Verbose_total ("Existence check done");

		(*** TODO: it seems that getting the list is done twice here; optimization? ***)
		if not (List.mem combined_transition transitions) then(
			(** Add to the data structure *)
			Hashtbl.replace state_space.transitions_table source_state_index
				(List.rev ((combined_transition , target_state_index) :: (get_successors_with_combined_transitions state_space source_state_index)))
		)else(
			print_message Verbose_total ("Transition belong to the list already");
		);*)
		(* Statistics *)
		counter_add_transition#stop;

		(* Print some information *)
		print_message Verbose_total ("Exiting StateSpace.add_transition");

		()


	(** Add an inequality to all the states of the state space *)
	(*** NOTE: it is assumed that the p_constraint does not render some states inconsistent! ***)
	method add_p_constraint_to_states p_constraint : unit =
	(* 	let constraint_to_add = LinearConstraint.make_p_constraint [inequality] in *)
		(* For all state: *)
		self#iterate_on_states (fun _ state ->
			LinearConstraint.px_intersection_assign_p state.px_constraint [p_constraint]
		)


	(** Empties the hash table giving the set of states for a given location; optimization for the jobshop example, where one is not interested in comparing  a state of iteration n with states of iterations < n *)
	method empty_states_for_comparison : unit =
		(* Statistics *)
		counter_empty_states_for_comparison#increment;
		counter_empty_states_for_comparison#start;

		Hashtbl.clear state_space.states_for_comparison;

		(* Statistics *)
		counter_empty_states_for_comparison#stop;

		()


	(************************************************************)
	(************************************************************)
	(** Methods modifying the state space: merging *)
	(************************************************************)
	(************************************************************)

	(************************************************************)
	(** THE FOLLOWING WAS ADDED FROM V2.12 *)
	(************************************************************)

	(** Merge two states by replacing the second one by the first one, in the whole state_space structure (lists of states, and transitions) *)
	method private merge_states_ulrich merger_state_index merged : unit =
		(* NOTE: 'merged' is usually very small, e.g., 1 or 2, so no need to optimize functions using 'merged *)
		print_message Verbose_high ("Merging: update tables for state '" ^ (string_of_int merger_state_index) ^ "' with " ^ (string_of_int (List.length merged)) ^ " merged.");

		(* Rebuild transitions table *)
		if verbose_mode_greater Verbose_high then
			print_message Verbose_high ("Merging: update transition table, containing " ^ (string_of_int (self#nb_transitions)) ^ " transitions");
		let t' = Hashtbl.copy state_space.transitions_table in
		Hashtbl.clear state_space.transitions_table;
		Hashtbl.iter (fun src successors ->
			List.iter (fun (combined_transition, target_state_index) ->
				let src' = if (List.mem src merged) then merger_state_index else src
				and trg' = if (List.mem target_state_index merged) then merger_state_index else target_state_index in
				(* Add if not *)
				self#add_transition (src', combined_transition, trg')
			) successors;
		) t';

		(* Remove merged from hash table *)
		print_message Verbose_high "Merging: update hash table";
		let the_state = self#get_state merger_state_index in
		let l = the_state.global_location in
		let li = self#new_location_index l in
		(* Get all states with that hash *)

		let bucket = Hashtbl.find_all state_space.states_for_comparison li in
		print_message Verbose_high ("Merging: got " ^ (string_of_int (List.length bucket)) ^ " states with hash " ^ (string_of_int li));
		(* Remove them all *)
		while Hashtbl.mem state_space.states_for_comparison li do
			Hashtbl.remove state_space.states_for_comparison li;
		done;
		(* Add them back *)
		List.iter (fun y ->
			(* Only add if not to be merged *)
			if not (List.mem y merged) then Hashtbl.add state_space.states_for_comparison li y;
		) bucket;

		(* Remove merged from state table *)
		print_message Verbose_high "Merging: update state table";
		List.iter (fun s ->
			print_message Verbose_high ("Merging: remove state " ^ (string_of_int s));
	(*		while Hashtbl.mem state_space.states s do *)
				Hashtbl.remove state_space.all_states s;

				(* If the state was the initial state: replace with the merger state_index *)
				(*** WARNING: situation not much tested ***)
				if s = (self#get_initial_state_index) then(
					print_message Verbose_low ("The initial state in the state_space has been merged with another one.");
					state_space.initial <- Some merger_state_index;
				);
	(*		done*)
		) merged

	(* Get states sharing the same location and discrete values from hash_table, excluding s *)
	(*** NOTE/TODO (ÉA, 2022/10/19): is that significantly different from the get_siblings method in Dylan's code (3.2)? Shall we merge both? ***)
	method private get_siblings_212 si =

		let s = self#get_state si in
		let l = s.global_location in
		let location_index = self#new_location_index l in

		let sibs = Hashtbl.find_all state_space.states_for_comparison location_index in

		(* check for exact correspondence (=> hash collisions!), and exclude si *)
		List.fold_left (fun siblings sj ->
			if sj = si then siblings else begin
				let state = self#get_state sj in
				let c' = state.px_constraint in
				(sj, c') :: siblings
			end
		) [] sibs


	(* Get states sharing the same location and discrete values from hash_table, excluding s *)
	method private get_siblings_32 (si : state_index) queue (look_in_queue : bool) =
		print_message Verbose_medium("Get siblings of state " ^ string_of_int si);
		let s = self#get_state si in
		let location = s.global_location in
		let location_index = self#new_location_index location in

		let sibs = Hashtbl.find_all state_space.states_for_comparison location_index in

		(* lookup px_constraints and exclude si itself *)
		let result = (List.fold_left (fun siblings sj ->
			(* Remove sj if sj=si or if look  *)
			if sj = si || (look_in_queue && not(List.mem sj queue))
				then siblings
				else begin
					let state = self#get_state sj in
					let c' = state.px_constraint in
					(sj,c') :: siblings
				end
		) [] sibs) in

		print_message Verbose_high ("Siblings (" ^ string_of_int si ^ ") : " ^ string_of_list_of_int (List.map fst result));
		result


	(* Try to merge new states with existing ones. Returns list of merged states (ULRICH) *)
	method merge212 new_states : state_index list =

		(* function for merging one state with its siblings *)
		let merge_state si =
			print_message Verbose_total ("[merging] Try to merge state " ^ (string_of_int si));
			let state = self#get_state si in
			let l, c = state.global_location, state.px_constraint in
			(* get merge candidates as pairs (index, state) *)
			let candidates = self#get_siblings_212 si in
			(* try to merge with siblings, restart if merge found, return eaten states *)
			let rec eat all_mc rest_mc = begin
				match rest_mc with
					| [] -> [] (* here, we are really done *)
					| m :: tail_mc -> begin
						let sj, c' = m in
						if are_mergeable_212 c c' then begin
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
							self#merge_states_ulrich s eaten;
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
		(*list_diff new_states*)
		eaten


	(************************************************************)
	(** BEGIN MERGE 3.2 - DYLAN *)
	(************************************************************)

	method private update_statespace merger_index merged_index_list =
		tcounter_merge_statespace#start;

		let options = Input.get_options () in

		let rec merge_transitions merger_index merged_index_list =
			match merged_index_list with
				| [] -> ()
				| merged_index::q ->
				begin
					merge_transitions merger_index q;

					(* Remove merged_index from transitions, replaced with merger_index*)
					let rec update_target src successors merger_index merged_index =
						match successors with
						| []->[]
						| (combined_transition, target_index)::tail ->
							if target_index = merged_index
							then
								begin
									print_message Verbose_high ("Merge transitions: update target in transition " ^ (string_of_list_of_int combined_transition)
											^ " (previous: " ^ string_of_int target_index ^ ", new: " ^ string_of_int merger_index ^ ")");
									(combined_transition, merger_index) :: (update_target src tail merger_index merged_index)
								end
							else (combined_transition, target_index) :: (update_target src tail merger_index merged_index)
					in

					(** Transitions with merged as target **)
					Hashtbl.iter (fun src successors -> (Hashtbl.replace state_space.transitions_table src (update_target src successors merger_index merged_index))) state_space.transitions_table;

					let transitions_merged = self#get_successors_with_combined_transitions merged_index in
					(** Transitions with merged as source **)
					Hashtbl.remove state_space.transitions_table merged_index;
					List.iter (
						fun (combined_transition , target_state_index) ->
							print_message Verbose_high ("Merge transitions: update source in transition " ^ (string_of_list_of_int combined_transition)
														^ " (previous: " ^ string_of_int merged_index ^ ", new: " ^ string_of_int merger_index ^ ")");
							self#add_transition (merger_index, combined_transition, target_state_index)
					) transitions_merged;

					(* If the state was the initial state: replace with the merger state_index *)
					let init = self#get_initial_state_index in
					if merged_index = init then(
						print_message Verbose_low ("The initial state in the reachability state_space has been merged with another one.");
						state_space.initial <- Some merger_index;
					);

					print_message Verbose_high ("Merging: remove state " ^ (string_of_int merged_index));
					(*Remove state from all_states and states_for_comparison*)
					Hashtbl.remove state_space.all_states merged_index;
					Hashtbl.filter_map_inplace (
							fun location_index state_index -> if state_index = merged_index then None else Some state_index
							(*filter_map_inplace discard binding associated to None, update if Some*)
						) state_space.states_for_comparison;

					()
				end
		in

		(*** TODO (ÉA, 2022/10/19: make standalone method? ***)
		(** Merge refactor copy_and_reduce **)
		let copy_and_reduce merger_state eaten =
			(* make a copy of the reachable part of the state space with the eaten states replaced by the merger_state *)
			let new_states = Hashtbl.create 1024 in
			let new_trans = Hashtbl.create 1024 in
			let new_compare = Hashtbl.create 1024 in
			let add_state s = (* adds s to new_states *)
				let state = Hashtbl.find state_space.all_states s in
				let loc = state.global_location_index in
				Hashtbl.replace new_states s state;
				Hashtbl.add new_compare loc s in
			let add_trans s a t = (* adds s --a--> t to new_transitions *)
				let transitions = hashtbl_get_or_default new_trans s [] in
				if not (List.mem (a,t) transitions)
				then Hashtbl.replace new_trans s ((a,t)::transitions) in
			let rec crs s = (* depth-first copy of reachable part of the state space *)
				if not (Hashtbl.mem new_states s || List.mem s eaten) then begin
					(* s has not been visited and is not eaten, so continue with successors *)
					print_message Verbose_total ("[Merge] Visiting new state " ^ (string_of_int s));
					add_state s;
					List.iter (fun (trans,target) ->
						if (List.mem target eaten)
						then add_trans s trans merger_state
						else begin
							add_trans s trans target;
							crs target
						end
					) (self#get_successors_with_combined_transitions s)
				end
				else if (Hashtbl.mem new_states s) then
					print_message Verbose_total ("[Merge] State " ^ (string_of_int s) ^ " was already visited")
				else if (List.mem s eaten) then
					print_message Verbose_total ("[Merge] State " ^ (string_of_int s) ^ " has been merged")
				in
			let init = self#get_initial_state_index in
			print_message Verbose_medium ("[Merge] Merging " ^ (string_of_list_of_int eaten) ^ " into state " ^ (string_of_int merger_state));
			crs init;
			add_state merger_state;
			if List.mem init eaten then state_space.initial <- Some merger_state;
			state_space.all_states <- new_states;
			state_space.transitions_table <- new_trans;
			state_space.states_for_comparison <- new_compare
		in

		begin
		match options#merge_algorithm with
		| Merge_reconstruct -> copy_and_reduce merger_index merged_index_list
		| Merge_onthefly -> merge_transitions merger_index merged_index_list
		| Merge_none -> raise(InternalError("Impossible case: merge_algorithm cannot be `Merge_none`"))
		| Merge_212 -> raise(InternalError("Impossible case: merge_algorithm cannot be `Merge_212`"))
		end;

		tcounter_merge_statespace#stop;
		()

	method merge queue =

	(*
			(*TEMP: print transitions*)
				let transitions = state_space.transitions_table in
				let print_transition_table transitions =
					let print_successors successors =
						string_of_list_of_string (
							List.map (
								fun (combined_transition, target) -> string_of_int target ^ " (" ^ (string_of_list_of_int combined_transition) ^ "), "
							)
							successors
						)
					in
						print_message Verbose_standard "Transition table:";
						Hashtbl.iter (
							fun src successors -> (print_message Verbose_standard ("\t [src: " ^ (string_of_int src) ^ " | successors: " ^ (print_successors successors) ^ "]"))
						)
						transitions
				in
				let print_all_states state_space =
					print_message Verbose_standard "States:";
					Hashtbl.iter (
						fun index _ -> (print_message Verbose_standard ("\t - " ^ (string_of_int index) ^ ""))
					)
					state_space.all_states
				in
				let print_states_for_comparison state_space =
					print_message Verbose_standard "States for comparison:";
					Hashtbl.iter (
						fun index location -> (print_message Verbose_standard ("\t - " ^ (string_of_int index) ^ " ("^ (string_of_int location) ^")"))
					)
					state_space.states_for_comparison
				in
				(print_all_states state_space);
				(print_states_for_comparison state_space);
				(print_transition_table transitions);
				(* END TEMP print transitions*)
	*)

		(* Statistics *)
		tcounter_merge#start;

		let options = Input.get_options () in


		(* function for merging one state with its siblings *)
		(*** TODO (ÉA, 2022/10/19: make standalone method? ***)
		let merge_state (si : state_index) (look_in_queue : bool) =
			print_message Verbose_medium("[Merge] Try to merge state " ^ (string_of_int si));

			let merging_states (s_merger : state_index) (s_merged : state_index) =
			(* Merge si and sj. Note that C(si) = siUsj from the test *)
				self#update_statespace s_merger [s_merged];
			in

			let state = self#get_state si in
			let (c : LinearConstraint.px_linear_constraint) = state.px_constraint in

			let did_something = ref true in
			let main_merging si look_in_queue =
				did_something := false;
				(* get merge candidates as pairs (index, state) *)
				let candidates = self#get_siblings_32 si queue look_in_queue in

				(* try to merge with siblings, restart if merge found, return merged states *)
				let rec merging merged_states candidates = begin
					begin
					match candidates with
						| [] -> () (* here, we are really done *)
						| m :: tail -> begin
							let sj,c' = m in


					print_message Verbose_high ("[Merge] Check if states " ^ (string_of_int si) ^ " and " ^ (string_of_int sj) ^ "are mergeable");

							if are_mergeable_32 c c'
							then begin
								(*Statistics*)
								nb_merged#increment;
								did_something := true;

								(*Here, si = siUsj from the test / IRL c = cUc', transitions not performed etc.'*)
								print_message Verbose_experiments ("[Merge] State " ^ (string_of_int si) ^ " is mergeable with " ^ (string_of_int sj));

								begin
								match options#merge_update with
								| Merge_update_merge -> merging_states si sj; ();
								| Merge_update_candidates -> ();
								(*| Merge_update_level -> ();*)
								end;

								(* Print some information *)
								print_message Verbose_high ("[Merge] State " ^ (string_of_int si) ^ " merged with state " ^ (string_of_int sj));

								let merged' = sj :: merged_states in
								(merging merged' tail)
							end
							else
							begin
								(* try to eat the rest of them *)
								merging merged_states tail
							end
						end;
					end;
					begin
					match options#merge_update with
					| Merge_update_merge -> ();
					| Merge_update_candidates -> self#update_statespace si merged_states;
					(*| Merge_update_level -> ();*)
					end;
				end;
				(**)
				in
				merging [] candidates;
			in

			main_merging si look_in_queue;
			while options#merge_restart && !did_something do (** Restart only if option restart is set **)
				print_message Verbose_experiments ("Restart for state " ^ (string_of_int si));
				main_merging si look_in_queue
			done;
		in

		(* Iterate list of states and try to merge them in the state space *)
		let rec main_merger states =
			match states with
				| [] -> ()
				| s :: tail ->
					begin
						main_merger tail;
						if Hashtbl.mem state_space.all_states s then (* treat s only if it is still reachable *)

							match options#merge_candidates with
							| Merge_candidates_visited -> merge_state s false
							| Merge_candidates_queue -> merge_state s true
							| Merge_candidates_ordered -> begin (merge_state s true) ; (merge_state s false) end
					end
		in

		(*Main*)
		main_merger queue;

		let new_queue = List.filter (self#test_state_index) queue in

		(* Statistics *)
		tcounter_merge#stop;
		(*state_space.states_for_comparison <- Hashtbl.create 1024;*)

		print_message Verbose_high "\n---\n";
		(* return *)
		new_queue



(************************************************************)
(************************************************************)
end;; (* end class *)
(************************************************************)
(************************************************************)
