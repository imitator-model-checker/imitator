open Global
open LinearConstraint
open Automaton
open AbstractImitatorFile

type state_index = int
type 's graph_state = Automaton.location * 's
type 'l graph_transition = state_index * 'l * state_index

(** The type of the reachability graph is parameterized over
		the types of the constraints and the transition labels *)
type ('s, 'l) t


(** Construct a new graph, given the estimated number of nodes
		and a predicate for testing if a constraint is included in 
		another one *)
val make: int -> ('s -> 's -> bool) -> ('s, 'l) t


(** Type alias for symbolic reachabiliy graph *)
type reachability_graph = (linear_constraint, action_index) t

(** Type alias for abstract reachability graph *)
type abstract_reachability_graph = (predicate list, abstract_label) t
	 

(****************************************************************)
(** Generic interface *)
(****************************************************************)

(** Return the number of states in a graph *)
val nb_states : ('s, 'l) t -> int

(** Return the number of transitions in a graph *)
val nb_transitions : ('s, 'l) t -> int

(** Return the state of a state_index *)
val get_state : ('s, 'l) t -> state_index -> 's graph_state

(** iterates over the reachable states of a graph *)
val iter: ('s graph_state -> unit) -> ('s, 'l) t -> unit

(** test if a state exists satisfying predicate s *)
val exists_state: ('s graph_state -> bool) -> ('s, 'l) t -> bool

(** test if all states satisfy predicate s *)
val forall_state: ('s graph_state -> bool) -> ('s, 'l) t -> bool

(** find all "last" states on finite or infinite runs *)
val last_states: ('s, 'l) t -> state_index list 

(** Add a state to a graph: return (state_index, added), where state_index
 is the index of the state, and 'added' is false if the state was already 
 in the graph, true otherwise *)
val add_state : ('s, 'l) t -> 's graph_state -> (state_index * bool)

(** Add a transition to the graph *)
val add_transition : ('s, 'l) t -> (state_index * 'l * state_index) -> unit

(** Convert a reachability graph to a dot file *)
(*val dot_of_graph : ('s, 'l) t -> (string * string)*)


(****************************************************************)
(** Specialized interface for reachability_graph *)
(****************************************************************)

(** Return the list of all constraints on the parameters associated to the states of a graph *)
val all_p_constraints : reachability_graph -> linear_constraint list

(** Returns the intersection of all parameter constraints, thereby destroying all constraints *)
val compute_k0_destructive : reachability_graph -> linear_constraint

(** check if bad states are reached *)
val is_bad: reachability_graph -> bool

(** Add an inequality to all the states of the graph *)
val add_inequality_to_states : reachability_graph -> linear_inequality -> unit

(** Plot reachable states projected to the given two variables *)
val plot_graph : variable -> variable -> reachability_graph -> string

(** Convert a reachability graph to a dot file *)
val dot_of_graph : reachability_graph -> (string * string)
 