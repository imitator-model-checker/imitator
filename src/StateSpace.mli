(*****************************************************************
 *
 *                     IMITATOR
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2009/12/08
 * Last modified: 2015/09/24
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
type state = Automaton.global_location * LinearConstraint.px_linear_constraint

type abstract_state = location_index * LinearConstraint.px_linear_constraint

(****************************************************************)
(** Graph structure *)
(****************************************************************)
type reachability_graph


(****************************************************************)
(** Graph creation *)
(****************************************************************)

(** Create a fresh graph *)
val make : int -> reachability_graph


(****************************************************************)
(** Interrogation on a graph *)
(****************************************************************)

(** Return the number of generated states (not necessarily present in the graph) *)
val get_nb_gen_states : reachability_graph -> int

(** Return the number of states in a graph *)
val nb_states : reachability_graph -> int

(** Return the number of transitions in a graph *)
val nb_transitions : reachability_graph -> int

(** Return the global_location corresponding to a location_index *)
val get_location : reachability_graph -> location_index -> Automaton.global_location

(** Return the state of a state_index *)
val get_state : reachability_graph -> state_index -> state

(** Return the table of transitions *)
val get_transitions : reachability_graph -> ((state_index * action_index), state_index) Hashtbl.t

(** Return the list of all state indexes *)
val all_state_indexes : reachability_graph -> state_index list


(*** WARNING: big memory, here! Why not perform intersection on the fly? *)

(** Return the list of all constraints on the parameters associated to the states of a graph *)
val all_p_constraints : reachability_graph -> LinearConstraint.p_linear_constraint list

(** Returns the intersection of all parameter constraints, thereby destroying all constraints *)
(* val compute_k0_destructive : abstract_model -> reachability_graph -> LinearConstraint.linear_constraint *)

(** Check if two states are equal *)
val states_equal: state -> state -> bool

(** Check dynamically if two states are equal, i.e., if the first one + constraint equals second one + constraint *)
val states_equal_dyn: state -> state -> LinearConstraint.px_linear_constraint -> bool

(*(** Test if a state exists satisfying predicate s *)
val exists_state: (state -> bool) -> reachability_graph -> bool

(** test if all states satisfy predicate s *)
val forall_state: (state -> bool) -> reachability_graph -> bool*)

(** Find all "last" states on finite or infinite runs *)
val last_states: abstract_model -> reachability_graph -> state_index list 

(** Check if bad states are reached *)
(* val is_bad: abstract_model -> reachability_graph -> bool *)


(****************************************************************)
(** Actions on a graph *)
(****************************************************************)

(** Increment the number of generated states (even though not member of the graph) *)
val increment_nb_gen_states : reachability_graph -> unit

(** Add a state to a graph: return (state_index, added), where state_index is the index of the state, and 'added' is false if the state was already in the graph, true otherwise *)
val add_state : reachability_graph -> state -> (state_index * bool)

(**Add a state to a graph dynamically**)
(* val add_state_dyn : AbstractModel.abstract_model -> reachability_graph -> state -> LinearConstraint.linear_constraint -> (state_index * bool) *)

(** Add a transition to the graph *)
val add_transition : reachability_graph -> (state_index * action_index * state_index) -> unit

(** Add a p_inequality to all the states of the graph *)
val add_p_constraint_to_states : reachability_graph -> LinearConstraint.p_linear_constraint -> unit


(** Replace the constraint of a state in a graph by another one (the constraint is copied to avoid side-effects later) *)
(* val replace_constraint : reachability_graph -> LinearConstraint.linear_constraint -> state_index -> unit *)

(** Merge two states by replacing the second one by the first one, in the whole graph structure (lists of states, and transitions) *)
(* val merge_2_states : reachability_graph -> state_index -> state_index -> unit *)

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
