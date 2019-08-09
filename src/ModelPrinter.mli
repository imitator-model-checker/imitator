(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 *
 * Module description: Convert an abstract model to the input syntax of IMITATOR
 *
 * File contributors : Étienne André, Jaime Arias
 * Created           : 2009/12/02
 * Last modified     : 2019/08/09
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
(** Convert a symbolic state into a string *)
val string_of_state : abstract_model -> State.state -> string

(** Convert a symbolic state into a string *)
val string_of_concrete_state : abstract_model -> State.concrete_state -> string

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


(************************************************************)
(** Updates *)
(************************************************************)


(** Returns when add comma separators between clock and discrete updates and
between discrete and conditional updates *)
val separator_comma : updates -> bool * bool

(** Convert the discrete updates into a string *)
val string_of_discrete_updates : ?sep:string -> abstract_model -> discrete_update list -> string

(** Template to convert clock updates into a string *)
val string_of_clock_updates_template : abstract_model -> clock_updates -> (clock_update -> string) -> (clock_update -> LinearConstraint.pxd_linear_term -> string) -> string -> string

(** Convert the clock updates into a string *)
val string_of_clock_updates :  abstract_model -> clock_updates -> string

(** Template to convert a boolean expresion into a string *)
val string_of_boolean_template : (Automaton.variable_index -> Automaton.variable_name) -> boolean_expression -> (boolean_expression -> string) -> string

(** Convert a boolean expression into a string *)
val string_of_boolean :  (Automaton.discrete_index -> Automaton.variable_name) -> boolean_expression -> string

(** Template to convert conditional updates into a string *)
val string_of_conditional_updates_template : abstract_model -> conditional_update list -> (abstract_model -> clock_updates -> string) -> (abstract_model -> discrete_update list -> string) -> (boolean_expression -> string) -> string -> string -> string -> string

(** Convert conditional updates into a string *)
val string_of_conditional_updates : abstract_model -> conditional_update list -> string


(************************************************************)
(** Points and hyperrectangles *)
(************************************************************)

(* Convert a pi0 into a string *)
val string_of_pi0 : abstract_model -> pi0 -> string

(* Convert a px-valuation into a string *)
val string_of_px_valuation : abstract_model -> (Automaton.variable_index -> NumConst.t) -> string

(* Convert a v0 into a string *)
val string_of_v0 : abstract_model -> v0 -> string


(************************************************************)
(** Model and property *)
(************************************************************)

(* Convert a model into a string *)
val string_of_model : abstract_model -> string

(** Convert the correctness property to a string *)
val string_of_property : abstract_model -> property_definition -> string

