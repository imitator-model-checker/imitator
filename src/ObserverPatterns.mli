(*****************************************************************
 *
 *                       IMITATOR
 * 
 * File containing the operations linked to the observer patterns
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        Etienne Andre
 * 
 * Created:       2013/02/04
 * Last modified: 2013/03/06
 *
 ****************************************************************)



(****************************************************************)
(** Modules *)
(****************************************************************)
open AbstractModel
open Automaton


(****************************************************************)
(** Functions *)
(****************************************************************)
(* Create the new automaton and new clock necessary for the observer *)
val new_elements : ParsingStructure.property_definition ->  (string option * string option)

(* Get the actions for this observer *)
(* val get_actions : ParsingStructure.property_definition ->  action_index list *)

(* Get the locations for this observer *)
val get_locations : ParsingStructure.property_definition ->  (location_name array)

(* Create the observer;
	Takes as parameters the number of actions, the automaton_index, the nosync index for the observer
	Returns all information for building the automaton + reduces the user-defined property to a non-reachability property
*)
val get_automaton : int -> automaton_index -> action_index -> clock_index -> property -> 
	(* Actions per automaton *)
	  action_index list
	(* Actions per location *)
	* (action_index list) array
	(* Invariants *)
	* (AbstractModel.invariant) array
	(* Transitions *)
	* ((AbstractModel.transition list) array) array
	(* Initial inequalities (if any) *)
	* LinearConstraint.pxd_linear_constraint option
	(* The reduced reachability property *)
	* reachability_property
