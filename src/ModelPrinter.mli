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
 *
 ************************************************************)


open AbstractModel
open DiscreteExpressions

(****************************************************************)
(** Local printings *)
(****************************************************************)
(* Convert a var_type into a string *)
val string_of_var_type : DiscreteType.var_type -> string

(************************************************************)
(** Arithmetic expression *)
(************************************************************)
(** Convert a AbstractModel.global_expression into a string *)
val string_of_global_expression : (Automaton.variable_index -> Automaton.variable_name) -> DiscreteExpressions.global_expression -> string

(************************************************************)
(** Arithmetic expression *)
(************************************************************)
(** Convert a AbstractModel.discrete_arithmetic_expression into a string *)
val string_of_arithmetic_expression : (Automaton.variable_index -> Automaton.variable_name) -> DiscreteExpressions.discrete_arithmetic_expression -> string


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
val customized_string_of_guard : Constants.customized_string -> (Automaton.variable_index -> Automaton.variable_name) -> AbstractModel.guard -> string


(************************************************************)
(** Debug-print for symbolic run *)
(************************************************************)

val debug_string_of_symbolic_run            : AbstractModel.abstract_model -> StateSpace.stateSpace -> StateSpace.symbolic_run -> string
val debug_string_of_concrete_run            : AbstractModel.abstract_model -> StateSpace.concrete_run -> string
val json_of_concrete_run                    : AbstractModel.abstract_model -> StateSpace.concrete_run -> string
val debug_string_of_impossible_concrete_run : AbstractModel.abstract_model -> StateSpace.impossible_concrete_run -> string
val json_of_impossible_concrete_run         : AbstractModel.abstract_model -> StateSpace.impossible_concrete_run -> string


(************************************************************)
(** Updates *)
(************************************************************)

(* Convert the function definitions into a string *)
val string_of_fun_definitions : AbstractModel.abstract_model -> string

(*(** Template to convert a boolean expresion into a string *)
val string_of_boolean_template : (Automaton.variable_index -> Automaton.variable_name) -> boolean_expression -> (boolean_expression -> string) -> string*)

(** Convert a boolean expression into a string *)
val string_of_boolean_expression : (Automaton.discrete_index -> Automaton.variable_name) -> boolean_expression -> string

val customized_string_of_scalar_or_index_update_type : Constants.customized_string -> DiscreteExpressions.variable_name_table -> scalar_or_index_update_type -> string
val string_of_scalar_or_index_update_type : DiscreteExpressions.variable_name_table -> scalar_or_index_update_type -> string

val string_of_seq_code_bloc : abstract_model -> int -> ?sep:string -> seq_code_bloc_list -> string

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


(************************************************************)
(** Getting the flows of a location *)
(************************************************************)

(*** BADPROG: very, very bad programming: this function should be in AlgoStateBased BUT ModelPrinter doesn't have access to AlgoStateBased (but the other way is possible); and it is called from both modules, so defined here (ÉA, 2021/11/02) ***)

val compute_flows_list : DiscreteState.global_location -> ((Automaton.clock_index * NumConst.t) list)
val compute_flows_fun  : DiscreteState.global_location -> (Automaton.clock_index -> NumConst.t)
