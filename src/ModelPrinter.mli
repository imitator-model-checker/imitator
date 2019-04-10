(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 *
 * Module description: Convert an abstract model to the input syntax of IMITATOR
 *
 * File contributors : Étienne André
 * Created           : 2009/12/02
 * Last modified     : 2017/06/25
 *
 ************************************************************)


open AbstractModel

(****************************************************************)
(** Local printings *)
(****************************************************************)
(* Convert a var_type into a string *)
val string_of_var_type : var_type -> string


(************************************************************)
(** Arithmetic expression *)
(************************************************************)
(** Convert a AbstractModel.discrete_arithmetic_expression into a string *)
val string_of_arithmetic_expression : (Automaton.variable_index -> Automaton.variable_name) -> discrete_arithmetic_expression -> string


(************************************************************)
(** State *)
(************************************************************)
(*** TODO/BADPROG : Move elsewhere? ***)
(** Convert a state into a string *)
val string_of_state : abstract_model -> (Location.global_location * LinearConstraint.px_linear_constraint) -> string


(************************************************************)
(** Guard *)
(************************************************************)
(** Convert a guard into a string *)
val string_of_guard : (Automaton.variable_index -> Automaton.variable_name) -> AbstractModel.guard -> string



(************************************************************)
(** model *)
(************************************************************)
(* Convert a pi0 into a string *)
val string_of_pi0 : abstract_model -> pi0 -> string

(* Convert a v0 into a string *)
val string_of_v0 : abstract_model -> v0 -> string

(* Convert a model into a string *)
val string_of_model : abstract_model -> string

(** Convert the correctness property to a string *)
val string_of_property : abstract_model -> property_definition -> string

(** Returns when add comma separators between clock and discrete updates and
between discrete and conditional updates *)
val separator_comma : updates -> bool * bool

(** Convert the discrete updates into a string *)
val string_of_discrete_updates : ?sep:string -> abstract_model -> discrete_update list -> string

(** Convert the clock updates into a string *)
val string_of_clock_updates :  abstract_model -> clock_updates -> string

val string_of_clock_updates_template : abstract_model -> clock_updates -> (clock_update -> string) -> (clock_update -> LinearConstraint.pxd_linear_term -> string) -> string -> string

val string_of_boolean_template :  (Automaton.variable_index -> Automaton.variable_name) -> boolean_expression -> (boolean_expression -> string) -> string

val string_of_boolean :  (Automaton.discrete_index -> Automaton.variable_name) -> boolean_expression -> string

val string_of_conditional_updates : abstract_model -> conditional_update list -> string

val string_of_conditional_updates_template : abstract_model -> conditional_update list -> (abstract_model -> clock_updates -> string) -> (abstract_model -> discrete_update list -> string) -> (boolean_expression -> string) -> string -> string -> string -> string
