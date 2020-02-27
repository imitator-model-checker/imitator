(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Convert an abstract model to the input syntax of IMITATOR
 *
 * File contributors : Étienne André, Jaime Arias
 * Created           : 2009/12/02
 * Last modified     : 2020/02/27
 *
 ************************************************************)


open AbstractModel
open Expressions

(****************************************************************)
(** Local printings *)
(****************************************************************)
(* Convert a var_type into a string *)
val string_of_var_type : var_type -> string


(************************************************************)
(** Arithmetic expression *)
(************************************************************)
(** Convert a discrete_arithmetic_expression into a string *)
(* val string_of_arithmetic_expression : (Automaton.variable_index -> Automaton.variable_name) -> RationalExpressions.rational_arithmetic_expression -> string *)


(************************************************************)
(** State *)
(************************************************************)
(*** TODO/BADPROG : Move elsewhere? ***)
(** Convert a symbolic state into a string *)
val string_of_state : AbstractModel.abstract_model -> State.state -> string

(** Convert a symbolic state into a string *)
val string_of_concrete_state : AbstractModel.abstract_model -> State.concrete_state -> string

(************************************************************)
(** Guard *)
(************************************************************)
(** Convert a guard into a string *)
val string_of_guard : (Automaton.variable_index -> Automaton.variable_name) -> AbstractModel.guard -> string


(************************************************************)
(** Transitions *)
(************************************************************)
(* Convert a transition into a string: compact version for debugging/pretty-printing *)
val debug_string_of_transition : AbstractModel.abstract_model -> Automaton.automaton_index -> AbstractModel.transition -> string


(************************************************************)
(** Debug-print for symbolic run *)
(************************************************************)

val debug_string_of_symbolic_run : AbstractModel.abstract_model -> StateSpace.state_space -> StateSpace.symbolic_run -> string
val debug_string_of_concrete_run : AbstractModel.abstract_model -> StateSpace.concrete_run -> string
val debug_string_of_impossible_concrete_run : AbstractModel.abstract_model -> StateSpace.impossible_concrete_run -> string


(************************************************************)
(** Updates *)
(************************************************************)


(** Returns when add comma separators between clock and discrete updates and
between discrete and conditional updates *)
val separator_comma : updates -> bool * bool

(** Convert the discrete updates into a string *)
val string_of_discrete_updates : ?sep:string -> AbstractModel.abstract_model -> discrete_update list -> string

(** Template to convert clock updates into a string *)
(* val string_of_clock_updates_template : AbstractModel.abstract_model -> clock_update list -> (clock_update -> string) -> (clock_update -> LinearConstraint.pxd_linear_term -> string) -> string -> string *)

(** Convert the clock updates into a string *)
val string_of_clock_updates :  AbstractModel.abstract_model -> clock_update list -> string

(** Convert a discrete_boolean_expression into a string *)
val string_of_discrete_boolean_expression : (Automaton.discrete_index -> Automaton.variable_name) -> discrete_boolean_expression -> string

(** Template to convert conditional updates into a string *)
(* val string_of_conditional_updates_template : AbstractModel.abstract_model -> conditional_update list -> (abstract_model -> clock_update list -> string) -> (abstract_model -> discrete_update list -> string) -> (boolean_expression -> string) -> string -> string -> string -> string *)

(** Convert conditional updates into a string *)
val string_of_conditional_updates : AbstractModel.abstract_model -> conditional_update list -> string


(************************************************************)
(** Points and hyperrectangles *)
(************************************************************)

(* Convert a parameter valuation (PVal.pval) into a string *)
val string_of_pval : AbstractModel.abstract_model -> PVal.pval -> string

(* Convert a px-valuation into a string *)
val string_of_px_valuation : AbstractModel.abstract_model -> LinearConstraint.px_valuation -> string

(* Convert a x-valuation into a string *)
val string_of_x_valuation : AbstractModel.abstract_model -> LinearConstraint.x_valuation -> string

(* Convert a v0 into a string *)
val string_of_v0 : AbstractModel.abstract_model -> v0 -> string


(************************************************************)
(** Model and property *)
(************************************************************)

(* Convert a model into a string *)
val string_of_model : AbstractModel.abstract_model -> string

(** Convert an string_of_abstract_property to a string, using the naming functions of an AbstractModel.abstract_model *)
val string_of_abstract_property : AbstractModel.abstract_model -> AbstractProperty.abstract_property -> string

