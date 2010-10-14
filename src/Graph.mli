(*****************************************************************
 *
 *                     IMITATOR II
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2009/12/08
 * Last modified: 2010/03/25
 *
 ****************************************************************)


open AbstractImitatorFile
(* open DynArray *)

(****************************************************************)
(** Graph structure *)
(****************************************************************)
type state_index = int

type reachability_graph = {
	(** An Array 'state_index' -> 'state' *)
	states : AbstractImitatorFile.state DynArray.t;
	
	(** A hashtable to quickly find identical states *)
	hash_table : (int, state_index) Hashtbl.t;

	(** A hashtable '(state_index, action_index)' -> 'dest_state_index' *)
	transitions_table : ((state_index * AbstractImitatorFile.action_index), state_index) Hashtbl.t;
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

(** Return the state of a state_index *)
val get_state : reachability_graph -> int -> AbstractImitatorFile.state

(** Return the list of all constraints on the parameters associated to the states of a graph *)
val all_p_constraints : abstract_program -> reachability_graph -> LinearConstraint.linear_constraint list

(** Returns the intersection of all parameter constraints, thereby destroying all constraints *)
val compute_k0_destructive : abstract_program -> reachability_graph -> LinearConstraint.linear_constraint

(** Check if two states are equal *)
val states_equal: AbstractImitatorFile.state -> AbstractImitatorFile.state -> bool

(** find all "last" states on finite or infinite runs *)
val last_states: abstract_program -> reachability_graph -> int list 


(****************************************************************)
(** Actions on a graph *)
(****************************************************************)

(** Add a state to a graph: return (state_index, added), where state_index is the index of the state, and 'added' is false if the state was already in the graph, true otherwise *)
val add_state : AbstractImitatorFile.abstract_program -> reachability_graph -> state -> (state_index * bool)

(** Add a transition to the graph *)
val add_transition : reachability_graph -> (state_index * action_index * state_index) -> unit

(** Add an inequality to all the states of the graph *)
val add_inequality_to_states : reachability_graph -> LinearConstraint.linear_inequality -> unit


(****************************************************************)
(** Interaction with dot *)
(****************************************************************)

(* Convert a graph to a dot file *)
val dot_of_graph : AbstractImitatorFile.abstract_program -> AbstractImitatorFile.pi0 -> reachability_graph -> fancy:bool -> (string * string)

