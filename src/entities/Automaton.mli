(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: defines indexes and names for variables,actions,etc. in PTA
 * 
 * File contributors : Étienne André
 * Created           : 2010/03/10
 *
 ************************************************************)

(** defines indexes and names for variables,actions,etc. in PTA. *)



(************************************************************)
(* Indexes *)
(************************************************************)

(** Index uniquely identifying an action. *)
type action_index		= int
(** Human-readable name of an action. *)
type action_name		= string

(** Index uniquely identifying an automaton (a PTA component of the model). *)
type automaton_index	= int
(** Name of an automaton. *)
type automaton_name		= string

(** Index of a location within an automaton. *)
type location_index		= int
(** Name of a location. *)
type location_name		= string

(** Index uniquely identifying a variable (clock, parameter or discrete). *)
type variable_index		= int
(** A {!variable_index} known to denote a clock. *)
type clock_index		= variable_index
(** A {!variable_index} known to denote a parameter. *)
type parameter_index	= variable_index
(** A {!variable_index} known to denote a discrete variable. *)
type discrete_index		= variable_index
(** Value of a discrete variable (exact rational). *)
type discrete_value		= NumConst.t
(** Value of a variable (exact rational). *)
type variable_value		= NumConst.t
(** Name of a variable. *)
type variable_name		= string
(** Numeric id distinguishing variables that share a name (e.g. across scopes). *)
type variable_id        = int
(** Reference to a variable: its name together with its {!variable_id}. *)
type variable_ref       = variable_name * variable_id
