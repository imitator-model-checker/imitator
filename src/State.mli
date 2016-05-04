(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Description of a state_index, a symbolic state and sets of states indexes
 * 
 * File contributors : Étienne André
 * Created           : 2016/05/04
 * Last modified     : 2016/05/04
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
(* open Automaton *)


(************************************************************)
(** Reachable states *)
(************************************************************)
type state_index = int

(** State: location and constraint *)
(*** TODO: hide this definition, and use (at least) structure or functions ***)
type state = Location.global_location * LinearConstraint.px_linear_constraint

type abstract_state = Location.global_location_index * LinearConstraint.px_linear_constraint



(************************************************************)
(** Interrogation on one state *)
(************************************************************)

(*** NOTE: should NOT be defined in this module! But rather in some (yet to be created...) State.ml ***)

val match_unreachable_global_locations : AbstractModel.unreachable_global_location list -> Location.global_location -> bool



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
