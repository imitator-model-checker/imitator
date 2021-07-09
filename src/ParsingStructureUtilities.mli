(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: General fonctions for map, filter, traverse, evaluating, etc. parsing structure tree
 *
 * File contributors : Benjamin L.
 * Created           : 2021/03/05
 * Last modified     : 2021/06/25
 *
 ************************************************************)

open ParsingStructure

(* Leaf of parsing structure *)
type parsing_structure_leaf =
    | Leaf_variable of string
    | Leaf_constant of DiscreteValue.discrete_value

(* Leaf of linear expression *)
type linear_expression_leaf =
    | Leaf_linear_constant of NumConst.t
    | Leaf_linear_variable of NumConst.t * variable_name

(** Check if all leaf of a parsing structure satisfy the predicate **)

val for_all_in_parsed_global_expression : (parsing_structure_leaf -> bool) -> variable_infos -> global_expression -> bool
val for_all_in_parsed_boolean_expression : (parsing_structure_leaf -> bool) -> variable_infos -> parsed_boolean_expression -> bool
val for_all_in_parsed_discrete_boolean_expression : (parsing_structure_leaf -> bool) -> variable_infos -> parsed_discrete_boolean_expression -> bool
val for_all_in_parsed_discrete_arithmetic_expression : (parsing_structure_leaf -> bool) -> variable_infos -> parsed_discrete_arithmetic_expression -> bool
val for_all_in_parsed_discrete_term : (parsing_structure_leaf -> bool) -> variable_infos -> parsed_discrete_term -> bool
val for_all_in_parsed_discrete_factor : (parsing_structure_leaf -> bool) -> variable_infos -> parsed_discrete_factor -> bool

(** Check if any leaf of a parsing structure satisfy the predicate **)

val exists_in_parsed_global_expression : (parsing_structure_leaf -> bool) -> variable_infos -> global_expression -> bool
val exists_in_parsed_boolean_expression : (parsing_structure_leaf -> bool) -> variable_infos -> parsed_boolean_expression -> bool
val exists_in_parsed_discrete_boolean_expression : (parsing_structure_leaf -> bool) -> variable_infos -> parsed_discrete_boolean_expression -> bool
val exists_in_parsed_discrete_arithmetic_expression : (parsing_structure_leaf -> bool) -> variable_infos -> parsed_discrete_arithmetic_expression -> bool
val exists_in_parsed_discrete_term : (parsing_structure_leaf -> bool) -> variable_infos -> parsed_discrete_term -> bool
val exists_in_parsed_discrete_factor : (parsing_structure_leaf -> bool) -> variable_infos -> parsed_discrete_factor -> bool

(** Apply units over leaf of a parsing structure **)

val iterate_parsed_global_expression : (parsing_structure_leaf -> unit) -> variable_infos -> global_expression -> unit
val iterate_parsed_boolean_expression : (parsing_structure_leaf -> unit) -> variable_infos -> parsed_boolean_expression -> unit
val iterate_parsed_discrete_boolean_expression : (parsing_structure_leaf -> unit) -> variable_infos -> parsed_discrete_boolean_expression -> unit
val iterate_parsed_discrete_arithmetic_expression : (parsing_structure_leaf -> unit) -> variable_infos -> parsed_discrete_arithmetic_expression -> unit
val iterate_parsed_discrete_term : (parsing_structure_leaf -> unit) -> variable_infos -> parsed_discrete_term -> unit
val iterate_parsed_discrete_factor : (parsing_structure_leaf -> unit) -> variable_infos -> parsed_discrete_factor -> unit


(** Check if all leaf of a linear expression satisfy the predicate **)
val for_all_in_parsed_linear_expression : (linear_expression_leaf -> bool) -> variable_infos -> linear_expression -> bool
(** Check if all leaf of a linear term satisfy the predicate **)
val for_all_in_parsed_linear_term : (linear_expression_leaf -> bool) -> variable_infos -> linear_term -> bool
(** Check if any leaf of a linear expression the predicate **)
val exists_in_parsed_linear_expression : (linear_expression_leaf -> bool) -> variable_infos -> linear_expression -> bool
(** Check if any leaf of a linear term the predicate **)
val exists_in_parsed_linear_term : (linear_expression_leaf -> bool) -> variable_infos -> linear_term -> bool
(** Iterate over a linear expression applying a unit function **)
val iterate_parsed_linear_expression : (linear_expression_leaf -> unit) -> variable_infos -> linear_expression -> unit
(** Iterate over a linear term applying a unit function **)
val iterate_parsed_linear_term : (linear_expression_leaf -> unit) -> variable_infos -> linear_term -> unit


val string_of_parsed_factor_constructor : parsed_discrete_factor -> string

(* Parsed expression to string *)
val string_of_parsed_global_expression : variable_infos -> global_expression -> string
val string_of_parsed_boolean_expression : variable_infos -> parsed_boolean_expression -> string
val string_of_parsed_discrete_boolean_expression : variable_infos -> parsed_discrete_boolean_expression -> string
val string_of_parsed_arithmetic_expression : variable_infos -> parsed_discrete_arithmetic_expression -> string
val string_of_parsed_term : variable_infos -> parsed_discrete_term -> string
val string_of_parsed_factor : variable_infos -> parsed_discrete_factor -> string
val string_of_parsed_relop : parsed_relop -> string -> string -> string

(* Parsed linear constraint to string *)
val string_of_parsed_linear_constraint : variable_infos -> linear_constraint -> string
val string_of_linear_expression : variable_infos -> linear_expression -> string
val string_of_linear_term : variable_infos -> linear_term -> string
val string_of_parsed_init_state_predicate : variable_infos -> parsed_init_state_predicate -> string

val string_of_parsed_nonlinear_constraint : variable_infos -> nonlinear_constraint -> string

val try_reduce_parsed_global_expression : (variable_name, DiscreteValue.discrete_value) Hashtbl.t -> global_expression -> DiscreteValue.discrete_value
val try_reduce_parsed_arithmetic_expression : (variable_name, DiscreteValue.discrete_value) Hashtbl.t -> parsed_discrete_arithmetic_expression -> DiscreteValue.discrete_value

val try_reduce_parsed_term : (variable_name, DiscreteValue.discrete_value) Hashtbl.t -> parsed_discrete_term -> DiscreteValue.discrete_value
val try_reduce_parsed_factor : (variable_name, DiscreteValue.discrete_value) Hashtbl.t -> parsed_discrete_factor -> DiscreteValue.discrete_value

(** Utils **)
val is_parsed_global_expression_constant : variable_infos -> global_expression -> bool
val is_parsed_arithmetic_expression_constant : variable_infos -> parsed_discrete_arithmetic_expression -> bool
val all_variables_defined_in_parsed_global_expression : variable_infos -> global_expression -> bool
val all_variables_defined_in_parsed_boolean_expression : variable_infos -> parsed_boolean_expression -> bool
val only_discrete_in_nonlinear_term : variable_infos -> parsed_discrete_boolean_expression -> bool

val is_parsed_linear_expression_constant : variable_infos -> linear_expression -> bool

val variable_infos_of_parsed_model : useful_parsing_model_information -> variable_infos