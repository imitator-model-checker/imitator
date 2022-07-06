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

open Exceptions
open OCamlUtilities
open ImitatorUtilities
open Automaton
open AbstractModel


(************************************************************)
(** Reachable states *)
(************************************************************)
type state_index = int

(** Concrete state: location and px-valuation *)
type concrete_state = {
	global_location: Location.global_location;
	px_valuation   : (Automaton.variable_index -> NumConst.t);
}

(** State: location and constraint *)
type state = {
	global_location: Location.global_location;
	px_constraint  : LinearConstraint.px_linear_constraint;
}

(** Abstract state: abstract location (index) and concrete constraint *)
type abstract_state = {
	global_location_index: Location.global_location_index;
	px_constraint        : LinearConstraint.px_linear_constraint;
}


(************************************************************)
(** Matching state predicates with a global location *)
(************************************************************)

(*let is_one_location_accepting (state : state) =
	
	(* Retrieve the model *)
	let model = Input.get_model() in
	(* Retrieve the locations *)
	let locations = Location.get_locations state.global_location in
	let result = ref false in
	(* Check whether a local location is accepting *)
	
	(*** TODO: rewrite using Array.exists! ***)
	
	Array.iteri (fun automaton_index location_index ->
		result := !result || model.is_accepting automaton_index location_index) locations;
	
	(* Return result *)
	!result
	
let match_state_predicate state_predicate state =
	(* Call dedicated function *)
	Location.match_state_predicate state_predicate state.global_location*)


(* Tests whether a state matches `state_predicate`; takes as argument the accepting condition of the model of the form `automaton_index -> location_index -> acceptance of location_index in automaton_index` *)
let match_state_predicate (locations_acceptance_condition : Automaton.automaton_index -> Automaton.location_index -> bool) (state_predicate : AbstractProperty.state_predicate) (state : state) : bool =
    let discrete_access = Location.discrete_access_of_location state.global_location in
    let functions_table = (Input.get_model ()).functions_table in
	DiscreteExpressionEvaluator.match_state_predicate (Some functions_table) discrete_access locations_acceptance_condition state.global_location state_predicate



(************************************************************)
(************************************************************)
(** Structure to define sets of state_index *)
(************************************************************)
(************************************************************)

(* state struct for constructing set type *)
module State = struct
	type t = state_index
	let compare = compare
end

(* set of states for efficient lookup *)
module StateIndexSet = Set.Make(State)




(**************************************************************)
(* Encapsulation in a class *)
(*** NOTE: technically, we could better create a *generic* class and instantiate it with StateIndexSet when needed ***)
(**************************************************************)
class stateIndexSet =
	object (self)

	(************************************************************)
	(* Class variables *)
	(************************************************************)
	(* Initially, the set is empty *)
	val mutable the_set = StateIndexSet.empty
	

	
	(************************************************************)
	(* Access methods *)
	(************************************************************)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Is the set empty? *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method is_empty =
		StateIndexSet.is_empty the_set

		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Does the set contain a given element? *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method mem element =
		StateIndexSet.mem element the_set

		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Retrieve the number of elements *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method cardinal =
		StateIndexSet.cardinal the_set

		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Retrieve all elements in the form of a list *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method all_elements =
		StateIndexSet.elements the_set
	
	
	
	(************************************************************)
	(* Modification methods *)
	(************************************************************)
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Empty the set *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method empty =
		(* Directly replace the set with a new empty set *)
		the_set <- StateIndexSet.empty
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Add an element to the set *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method add element =
		the_set <- StateIndexSet.add element the_set

		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Remove an element; raises Not_found if the element was not in the set *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method remove element =
		(* First check *)
		if not (self#mem element) then raise Not_found;
		(* Then remove *)
		self#remove_or_do_nothing element

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Remove an element or do nothing if the element was not in the set *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method remove_or_do_nothing element =
		the_set <- StateIndexSet.remove element the_set
	

(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
