(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * 
 * Module description: Description of a state_index, a symbolic state and sets of states indexes
 * 
 * File contributors : Étienne André
 * Created           : 2016/05/04
 * Last modified     : 2019/06/11
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
(** Interrogation on one state *)
(************************************************************)

(*------------------------------------------------------------*)
(* Get the discrete index from a discrete_constraint *)
(*------------------------------------------------------------*)
let get_discrete_index_from_discrete_constraint = function
	| Discrete_l (discrete_index , _)
	| Discrete_leq (discrete_index , _)
	| Discrete_equal (discrete_index , _)
	| Discrete_neq (discrete_index , _)
	| Discrete_geq (discrete_index , _)
	| Discrete_g (discrete_index , _)
		-> discrete_index
	| Discrete_interval (discrete_index , _, _)
		-> discrete_index


(*------------------------------------------------------------*)
(* Check that a discrete variable matches a discrete_constraint *)
(*------------------------------------------------------------*)
let match_discrete_constraint current_value = function
	| Discrete_l (_, constrained_value )
		-> NumConst.l current_value constrained_value
	| Discrete_leq (_, constrained_value )
		-> NumConst.le current_value constrained_value
	| Discrete_equal (_, constrained_value )
		-> NumConst.equal current_value constrained_value
	| Discrete_neq (_, constrained_value )
		-> NumConst.neq current_value constrained_value
	| Discrete_geq (_, constrained_value )
		-> NumConst.ge current_value constrained_value
	| Discrete_g (_, constrained_value )
		-> NumConst.g current_value constrained_value
	| Discrete_interval (_, min_constrained_value, max_constrained_value )
		-> (NumConst.ge current_value min_constrained_value)
		&&
		(NumConst.le current_value max_constrained_value)


(*------------------------------------------------------------*)
(* Check whether the global location matches a list of "unreachable_global_location" *)
(*------------------------------------------------------------*)
let match_unreachable_global_locations unreachable_global_locations location =
	(* Retrieve the model *)
	let model = Input.get_model() in
	
	(* Iterate on all unreachable global locations *)
	List.exists (fun unreachable_global_location ->
		
		(* 1) Check whether all local unreachable locations defined in 'unreachable_global_location' are matched by the current location *)
		(
		List.for_all (fun (unreachable_automaton_index , unreachable_location_index) ->
			(* Retrieve current location of unreachable_automaton_index *)
			let current_location_index = Location.get_location location unreachable_automaton_index in
			if verbose_mode_greater Verbose_high then(
				print_message Verbose_high ("Checking whether loc[" ^ (model.automata_names unreachable_automaton_index) ^ "] = " ^ (model.location_names unreachable_automaton_index unreachable_location_index) ^ " is satisfied when loc[" ^ (model.automata_names unreachable_automaton_index) ^ "] = " ^ (model.location_names unreachable_automaton_index current_location_index) ^ " ");
			);
			(* Check equality *)
			current_location_index = unreachable_location_index
		) unreachable_global_location.unreachable_locations)
		
		&&
		
		(* 2) Check whether all discrete constraints defined in 'discrete_constraints' are matched by the current location *)
		(List.for_all (fun discrete_constraint ->
			(* Get the discrete index *)
			let discrete_index = get_discrete_index_from_discrete_constraint discrete_constraint in
			(* Retrieve current discrete value *)
			let current_discrete_value = Location.get_discrete_value location discrete_index in
			(* Check matching *)
			match_discrete_constraint current_discrete_value discrete_constraint
		) unreachable_global_location.discrete_constraints)
	) unreachable_global_locations




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
