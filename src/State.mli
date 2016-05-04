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
 * Created           : 2016/05/03
 * Last modified     : 2016/05/03
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

