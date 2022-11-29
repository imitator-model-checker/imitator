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
 * File contributors : Étienne André, Dylan Marinho
 * Created           : 2009/12/08
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open Automaton
open State
open AbstractAlgorithm


(************************************************************)
(************************************************************)
(** Type definitions *)
(************************************************************)
(************************************************************)

(************************************************************)
(** Combined transition *)
(************************************************************)

(** A combined transition is a list of transitions (one for each automaton involved) *)
type combined_transition = AbstractModel.transition_index list



(************************************************************)
(** Nature of a state space (according to some property) *)
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
(** Predecessors table *)
(************************************************************)
type predecessors_table = ((combined_transition * state_index) list) array


(************************************************************)
(** State space structure *)
(************************************************************)
type state_space

(** An SCC is just a list of states *)
type scc = state_index list



(************************************************************)
(************************************************************)
(** Class-independent functions *)
(************************************************************)
(************************************************************)

(**************************************************************)
(* Class-independent functions on combined transitions *)
(**************************************************************)

(** Get the (unique) action associated with a combined_transition *)
val get_action_from_combined_transition : AbstractModel.abstract_model -> combined_transition -> action_index

(*** NOTE: the function only works for regular resets (it raises NotImplemented for other updates) ***)
val get_resets : AbstractModel.abstract_model -> state_index -> combined_transition -> state_index -> Automaton.clock_index list


(************************************************************)
(** Misc: conversion to string *)
(************************************************************)
val string_of_state_index : state_index -> string

val string_of_statespace_nature : statespace_nature -> string



(**************************************************************)
(**************************************************************)
(* Class definition *)
(**************************************************************)
(**************************************************************)
class stateSpace : int ->
	object

		(************************************************************)
		(************************************************************)
		(* Class methods *)
		(************************************************************)
		(************************************************************)

		(************************************************************)
		(* Simple get methods *)
		(************************************************************)

		(*------------------------------------------------------------*)
		(** Return the number of generated states (not necessarily present in the state space) *)
		(*------------------------------------------------------------*)
		method get_nb_gen_states : int

		(*------------------------------------------------------------*)
		(** Return the number of states in a state space *)
		(*------------------------------------------------------------*)
		method nb_states : int

		(*------------------------------------------------------------*)
		(** Return the number of transitions in a state space *)
		(*------------------------------------------------------------*)
		method nb_transitions : int

		(*------------------------------------------------------------*)
		(** Return the global_location corresponding to a location_index *)
		(*------------------------------------------------------------*)
		method get_location : DiscreteState.global_location_index -> DiscreteState.global_location

		(*------------------------------------------------------------*)
		(** Return the state of a state_index *)
		(*------------------------------------------------------------*)
		method get_state : state_index -> State.state

		(*------------------------------------------------------------*)
		(** return the list of states with the same location (modulo hash collisions) *)
		(*------------------------------------------------------------*)
		method get_comparable_states : state_index -> state_index list

		(*------------------------------------------------------------*)
		(** Return the global_location_index of a state_index *)
		(*------------------------------------------------------------*)
		method get_global_location_index : state_index -> DiscreteState.global_location_index

		(*------------------------------------------------------------*)
		(** Return the index of the initial state, or raise Not_found if not defined *)
		(*------------------------------------------------------------*)
		method get_initial_state_index : state_index

		(*------------------------------------------------------------*)
		(** Compte and return the list of index successors of a state *)
		(*------------------------------------------------------------*)
		method get_successors : state_index -> state_index list

		(*------------------------------------------------------------*)
		(** Compte and return the list of pairs (index successor of a state, corresponding combined_transition) *)
		(*------------------------------------------------------------*)
		method get_successors_with_combined_transitions : state_index -> (combined_transition * state_index) list


		(************************************************************)
		(* Methods computing things from the state space without modifications *)
		(************************************************************)

		(*------------------------------------------------------------*)
		(** Compute and return a predecessor array state_index -> (combined_transition , state_index) list *)
		(*------------------------------------------------------------*)
		method compute_predecessors_with_combined_transitions : predecessors_table

		(*------------------------------------------------------------*)
		(** Return the table of transitions *)
		(*------------------------------------------------------------*)
		method get_transitions_table : (state_index , ((combined_transition * state_index) list)) Hashtbl.t

		(*------------------------------------------------------------*)
		(** Return the list of all state indexes *)
		(*------------------------------------------------------------*)
		method all_state_indexes : state_index list

		(*------------------------------------------------------------*)
		(** Test if state index is in the current statespace *)
		(*------------------------------------------------------------*)
		method test_state_index : state_index -> bool

		(*------------------------------------------------------------*)
		(*** WARNING: big memory, here! Why not perform intersection on the fly? *)
		(** Return the list of all constraints on the parameters associated to the states of a state space *)
		(*------------------------------------------------------------*)
		method all_p_constraints : LinearConstraint.p_linear_constraint list


		(*------------------------------------------------------------*)
		(** Find all "last" states on finite or infinite runs *)
		(*------------------------------------------------------------*)
		method last_states: state_index list


		(*------------------------------------------------------------*)
		(* Get the (full) guard associated with a transition *)
		(*------------------------------------------------------------*)
		method get_guard : AbstractModel.abstract_model -> state_index -> combined_transition -> LinearConstraint.pxd_linear_constraint


		(*------------------------------------------------------------*)
		(* When a state is encountered for a second time, then a loop exists (or more generally an SCC): 'reconstruct_scc state_space state_index' reconstructs the SCC from state_index to state_index (using the actions) using a variant of Tarjan's strongly connected components algorithm; returns None if no SCC found *)
		(*------------------------------------------------------------*)
		method reconstruct_scc : state_index -> scc option

		(*------------------------------------------------------------*)
		(** From a set of states, return all transitions within this set of states, in the form of a triple (state_index, combined_transition, state_index) *)
		(*------------------------------------------------------------*)
		method find_transitions_in : scc -> (state_index * combined_transition * state_index) list


		(*------------------------------------------------------------*)
		(** Returns the symbolic run (list of pairs (state, combined transition)) from the source_state_index to the target_state_index. Can take a predecessors_table as an option, otherwise recomputes it from the state space. The list of transitions is ordered from the initial state to the target state; optionally one can pass a list of states (a "lasso") for which we already know the succession of state indices. the final (target) state is not included. Raise Not_found if run not found. *)
		(*------------------------------------------------------------*)
		method backward_symbolic_run : state_index -> state_index list -> state_index -> predecessors_table option -> symbolic_run


		(*------------------------------------------------------------*)
		(** Get statistics on states *)
		(*------------------------------------------------------------*)
		method get_statistics_states : string


		(************************************************************)
		(** Methods modifying the state space *)
		(************************************************************)

		(** Increment the number of generated states (even though not member of the state space) *)
		method increment_nb_gen_states : unit

		(** Add a state to a state space: takes as input the state space, a comparison instruction, the state to add, and returns whether the state was indeed added or not *)
		(*** NOTE: side-effects possible! If the former state is SMALLER than the new state and the state_comparison is Including_check, then the constraint of this former state is updated to the newer one ***)
		method add_state : AbstractAlgorithm.state_comparison_operator -> state -> addition_result

		(** Add a transition to the state space *)
		method add_transition : (state_index * combined_transition * state_index) -> unit

		(** Add a p_inequality to all the states of the state space *)
		(*** NOTE: it is assumed that the p_constraint does not render some states inconsistent! ***)
		method add_p_constraint_to_states : LinearConstraint.p_linear_constraint -> unit

		(* Merge of v2.12 (ULRICH) *)
		method merge212 : state_index list -> state_index list

		(* Merge refactor 2022 - DYLAN *)
		method merge : state_index list -> state_index list

		(** Empties the hash table giving the set of states for a given location; optimization for the jobshop example, where one is not interested in comparing  a state of iteration n with states of iterations < n *)
		method empty_states_for_comparison : unit

		(** Iterate over the reachable states (with possible side effects) *)
		method iterate_on_states : (state_index -> abstract_state -> unit) -> unit


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)

