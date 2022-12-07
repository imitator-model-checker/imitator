(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: This module aims to convert a parsed expression to a abstract typed expression
 *
 * File contributors : Benjamin L.
 * Created           : 2021/11/20
 *
 ************************************************************)

open ParsingStructure
open DiscreteType

open AbstractModel
open DiscreteExpressions
open LinearConstraint

(* COMMENT *)
val convert_discrete_init : variable_infos -> variable_name -> parsed_boolean_expression -> global_expression
(* COMMENT *)
val convert_discrete_constant : constants_table -> variable_name * parsed_boolean_expression * var_type -> global_expression
(* Convert a parsed guard (list of parsed discrete boolean expression) to guard for abstract model *)
val convert_guard : variable_infos -> ParsingStructure.guard -> AbstractModel.guard
(* Convert a parsed update to update for abstract model *)
val convert_update : variable_infos -> parsed_scalar_or_index_update_type -> parsed_boolean_expression -> scalar_or_index_update_type * global_expression
(* Convert a parsed continuous update to continuous update for abstract model *)
val convert_continuous_update : variable_infos -> parsed_scalar_or_index_update_type -> parsed_boolean_expression -> scalar_or_index_update_type * pxd_linear_term
(* Convert a parsed boolean expression to boolean expression for abstract model *)
val convert_conditional : variable_infos -> parsed_boolean_expression -> boolean_expression
(* Convert a parsed sequential code bloc to sequential code bloc for abstract model *)
val convert_seq_code_bloc : variable_infos -> parsed_functions_table -> parsed_seq_code_bloc_list -> clock_updates * seq_code_bloc_list
(* Convert a parsed function definition to function definition for abstract model *)
val convert_fun_definition : variable_infos -> parsed_fun_definition -> fun_definition