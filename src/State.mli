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
val match_state_predicate : AbstractModel.abstract_model -> (Automaton.automaton_index -> Automaton.location_index -> bool) -> AbstractProperty.state_predicate -> state -> bool


(************************************************************)
(** Constraints satisfaction *)
(*************************************************************)

(** Check whether a discrete non-linear constraint is satisfied by the discrete values in a location **)
val evaluate_d_nonlinear_constraint_in_location : AbstractModel.abstract_model -> DiscreteState.global_location -> AbstractModel.discrete_guard -> bool

(** Check whether the discrete part of a guard is satisfied by the discrete values in a location *)
val is_discrete_guard_satisfied : AbstractModel.abstract_model -> DiscreteState.global_location -> AbstractModel.guard -> bool

(** Check whether the discrete part of a guards are satisfied by the discrete values in a location *)
val is_discrete_guards_satisfied : AbstractModel.abstract_model -> DiscreteState.global_location -> AbstractModel.guard list -> bool

(** Check whether the intersection between a pxd_constraint with an AbstractModel.guard if satisfiable (both inputs remain unchanged) *)
val is_constraint_and_continuous_guard_satisfiable : LinearConstraint.pxd_linear_constraint -> AbstractModel.guard -> bool

(************************************************************)
(** Computation of invariants *)
(*************************************************************)

(** Create a PXD constraint of the form D_i = d_i for the discrete variables *)
val discrete_constraint_of_global_location : AbstractModel.abstract_model -> DiscreteState.global_location -> LinearConstraint.pxd_linear_constraint


(** Compute the invariant associated to a location   *)
val compute_plain_continuous_invariant : AbstractModel.abstract_model -> DiscreteState.global_location -> LinearConstraint.pxd_linear_constraint

(** Compute the invariant I_l associated to a location, including renaming and time elapse. Uses cache.  *)
val compute_invariant : AbstractModel.abstract_model -> DiscreteState.global_location -> LinearConstraint.pxd_linear_constraint

(** Compute the invariant associated to a location and valuate the value of the discrete variables   *)
val compute_valuated_invariant : AbstractModel.abstract_model -> DiscreteState.global_location -> LinearConstraint.px_linear_constraint



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
