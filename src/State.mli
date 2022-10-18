(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: Description of a state_index, a symbolic state and sets of states indexes
 * 
 * File contributors : Étienne André, Laure Petrucci
 * Created           : 2016/05/04
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)


(************************************************************)
(** Reachable states *)
(************************************************************)
type state_index = int

(** Concrete state: location and px-valuation *)
type concrete_state = {
	global_location: DiscreteState.global_location;
	px_valuation   : (Automaton.variable_index -> NumConst.t);
}

(** State: location and constraint *)
type state = {
	global_location: DiscreteState.global_location;
	px_constraint  : LinearConstraint.px_linear_constraint;
}

(** Abstract state: abstract location (index) and concrete constraint *)
type abstract_state = {
	global_location_index: DiscreteState.global_location_index;
	px_constraint        : LinearConstraint.px_linear_constraint;
}



(************************************************************)
(** State comparison functions *)
(************************************************************)

(** Compare two states. Arguments:
  * state1
  * state2
  * clocks_to_remove: some variables to remove before performing the comparison between states (typically used to remove the `global_time_clock` before comparing two states)
  *)
val state_equals      : state -> state -> Automaton.clock_index list -> bool
val state_included_in : state -> state -> Automaton.clock_index list -> bool


(************************************************************)
(** Interrogation on one state *)
(************************************************************)

(* Shortcut, the function is actually called on global locations *)
(* val match_state_predicate : AbstractProperty.state_predicate -> state -> bool *)

(* Tests whether a state matches `state_predicate`; takes as argument the accepting condition of the model of the form `automaton_index -> location_index -> acceptance of location_index in automaton_index` *)
val match_state_predicate : (Automaton.automaton_index -> Automaton.location_index -> bool) -> AbstractProperty.state_predicate -> state -> bool


(************************************************************)
(************************************************************)
(** Structure to define sets of state_index *)
(************************************************************)
(************************************************************)

class stateIndexSet :
	object
		(************************************************************)
		(* Class variables *)
		(************************************************************)
		
		(************************************************************)
		(* Access methods *)
		(************************************************************)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Is the set empty? *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method is_empty : bool

			
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Does the set contain a given element? *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method mem : state_index -> bool

		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Retrieve the number of elements *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method cardinal : int
		

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Retrieve all elements in the form of a list *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method all_elements : state_index list
		
		
		
		(************************************************************)
		(* Modification methods *)
		(************************************************************)
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Empty the set *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method empty : unit
		
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Add an element to the set *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method add : state_index -> unit

			
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Remove an element; raises Not_found if the element was not in the set *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method remove : state_index -> unit

		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Remove an element or do nothing if the element was not in the set *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method remove_or_do_nothing : state_index -> unit
		

(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
