(*****************************************************************
 *
 *                     IMITATOR II
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2009/12/08
 * Last modified: 2012/06/18
 *
 ****************************************************************)


open AbstractModel


(****************************************************************)
(** Reachable states *)
(****************************************************************)
type state_index = int

(** Unique identifier for each different global location *)
type location_index = int

(** State: location and constraint *)
type state = Automaton.global_location * LinearConstraint.linear_constraint

type abstract_state = location_index * LinearConstraint.linear_constraint

(****************************************************************)
(** Graph structure *)
(****************************************************************)
type reachability_graph = {
	(** An Array 'state_index' -> 'abstract_state'; contains ALL states *)
	all_states : (state_index, abstract_state) Hashtbl.t;
	
	(** A hashtable location -> location_index *)
	index_of_locations : (Automaton.global_location, location_index) Hashtbl.t;

	(** A DynArray location_index -> location *)
	locations : Automaton.global_location DynArray.t;

	(** A hashtable to quickly find states with identical locations (? ; made by Ulrich); only for states to be compared *)
	states_for_comparison : (int, state_index) Hashtbl.t;

	(** A hashtable '(state_index, action_index)' -> 'dest_state_index' *)
	transitions_table : ((state_index * action_index), state_index) Hashtbl.t;

	(** An integer that remembers the next index of state_index (may not be equal to the number of states, if states are removed *)
	next_state_index : state_index ref;
}


(****************************************************************)
(** Graph creation *)
(****************************************************************)

(** Create a fresh graph *)
val make : int -> reachability_graph


(****************************************************************)
(** Interrogation on a graph *)
(****************************************************************)

(** Return the number of states in a graph *)
val nb_states : reachability_graph -> int

(** Return the global_location corresponding to a location_index *)
val get_location : reachability_graph -> location_index -> Automaton.global_location

(** Return the state of a state_index *)
val get_state : reachability_graph -> state_index -> state

(** Return the list of all state indexes *)
val all_state_indexes : abstract_program -> reachability_graph -> state_index list

(** Return the list of all constraints on the parameters associated to the states of a graph *)
val all_p_constraints : abstract_program -> reachability_graph -> LinearConstraint.linear_constraint list

(** Returns the intersection of all parameter constraints, thereby destroying all constraints *)
(* val compute_k0_destructive : abstract_program -> reachability_graph -> LinearConstraint.linear_constraint *)

(** Check if two states are equal *)
val states_equal: state -> state -> bool

(** Check dynamically if two states are equal, i.e., if the first one + constraint equals second one + constraint *)
val states_equal_dyn: state -> state -> LinearConstraint.linear_constraint -> bool

(*(** Test if a state exists satisfying predicate s *)
val exists_state: (state -> bool) -> reachability_graph -> bool

(** test if all states satisfy predicate s *)
val forall_state: (state -> bool) -> reachability_graph -> bool*)

(** Find all "last" states on finite or infinite runs *)
val last_states: abstract_program -> reachability_graph -> state_index list 

(** Check if bad states are reached *)
(* val is_bad: abstract_program -> reachability_graph -> bool *)


(****************************************************************)
(** Actions on a graph *)
(****************************************************************)

(** Add a state to a graph: return (state_index, added), where state_index is the index of the state, and 'added' is false if the state was already in the graph, true otherwise *)
val add_state : AbstractModel.abstract_program -> reachability_graph -> state -> (state_index * bool)

(**Add a state to a graph dynamically**)
val add_state_dyn : AbstractModel.abstract_program -> reachability_graph -> state -> LinearConstraint.linear_constraint -> (state_index * bool)

(** Add a transition to the graph *)
val add_transition : reachability_graph -> (state_index * action_index * state_index) -> unit

(** Add an inequality to all the states of the graph *)
val add_inequality_to_states : reachability_graph -> LinearConstraint.linear_inequality -> unit

(** Replace the constraint of a state in a graph by another one (the constraint is copied to avoid side-effects later) *)
(* val replace_constraint : reachability_graph -> LinearConstraint.linear_constraint -> state_index -> unit *)

(** Merge two states by replacing the second one by the first one, in the whole graph structure (lists of states, and transitions) *)
val merge_2_states : reachability_graph -> state_index -> state_index -> unit

(* Try to merge new states with existing ones. Returns updated list of new states (ULRICH) *)
val merge : reachability_graph -> state_index list -> state_index list

(** Empties the hash table giving the set of states for a given location; optimization for the jobshop example, where one is not interested in comparing  a state of iteration n with states of iterations < n *)
val empty_states_for_comparison : reachability_graph -> unit

(** Iterate over the reachable states (with possible side effects) *)
val iterate_on_states : (state_index -> abstract_state -> unit) -> reachability_graph -> unit



(****************************************************************)
(** Debug and performances *)
(****************************************************************)
(** Get statistics on number of comparisons *)
val get_statistics : unit -> string

(** Get statistics on states *)
val get_statistics_states : reachability_graph -> string
