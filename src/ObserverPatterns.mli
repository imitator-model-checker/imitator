(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: operations linked to the observer patterns
 *
 * File contributors : Étienne André
 * Created:       2013/02/04
 * Last modified: 2020/08/21
 *
 ************************************************************)

(************************************************************)
(** Modules *)
(************************************************************)
open AbstractModel
open Automaton


(************************************************************)
(** Functions *)
(************************************************************)

(** Returns true whether the observer requires one clock *)
val needs_clock : ParsingStructure.parsed_property -> bool

(** Returns the name of the new automaton and the new clock (if any) necessary for the observer *)
val new_elements : ParsingStructure.parsed_property-> (string option * string option)

(** Get the locations for this observer *)
val get_locations : ParsingStructure.parsed_property -> (location_name array)

(* Create the observer;
	Takes as parameters the number of actions, the automaton_index, the nosync index for the observer
	Returns all information for building the automaton + reduces the user-defined property to a safety property
*)
val get_observer_automaton : (action_name -> action_index) -> int -> automaton_index -> action_index -> clock_index -> ParsingStructure.parsed_property -> 
	(* Actions per automaton *)
	  action_index list
	(* Actions per location *)
	* (action_index list) array
	(* Actions per location *)
	* location_urgency array
	(* Invariants *)
	* (AbstractModel.invariant) array
	(* Transitions: structure location_index -> action_index -> list of (transition) *)
	* ((AbstractModel.transition list) array) array
	(* Initial inequalities (if any) *)
	* LinearConstraint.px_linear_constraint option
	(* The reduced reachability or safety property *)
	* AbstractProperty.property

