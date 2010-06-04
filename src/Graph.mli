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
(*	(** A hashtable 'state' -> 'state_index' *)
	states : (AbstractImitatorFile.state, state_index) Hashtbl.t;
*)
	(** An Array 'state_index' -> 'state' *)
	states : AbstractImitatorFile.state DynArray.t;

	(** A hashtable '(state_index, action_index)' -> 'dest_state_index' *)
	transitions_table : ((state_index * AbstractImitatorFile.action_index), state_index) Hashtbl.t;
	
	(** A constraint shared by all states *)
(*	mutable shared_constraint : LinearConstraint.linear_constraint;*)
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

(** Returns the shared constraint for all states *)
(*val get_shared_constraint : reachability_graph -> LinearConstraint.linear_constraint*)

(** Return the list of all constraints on the parameters associated to the states of a graph *)
val all_p_constraints : abstract_program -> reachability_graph -> LinearConstraint.linear_constraint list

(** Check if a state belongs to the graph *)
(* val in_graph : reachability_graph -> AbstractImitatorFile.state -> bool *)

(** Return the state_index of a state; raise Not_found if not found *)
(* val find_state_index : reachability_graph -> AbstractImitatorFile.state -> state_index *)

(** Check if two states are equal *)
val states_equal: AbstractImitatorFile.state -> AbstractImitatorFile.state -> bool

(** iterates over the reachable states of a graph *)
val iter: (AbstractImitatorFile.state -> unit) -> reachability_graph -> unit


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
val dot_of_graph : AbstractImitatorFile.abstract_program -> AbstractImitatorFile.pi0 -> reachability_graph -> (string * string)

