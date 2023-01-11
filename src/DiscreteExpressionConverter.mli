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

(* Convert a discrete init to abstract model *)
val convert_discrete_init : variable_infos -> variable_name -> parsed_boolean_expression -> global_expression
(* Convert discrete constants to abstract model *)
val convert_discrete_constant : constants_table -> variable_name * parsed_boolean_expression * var_type -> global_expression
(* Convert a parsed guard (list of parsed discrete boolean expression) to guard for abstract model *)
val convert_guard : variable_infos -> ParsingStructure.guard -> AbstractModel.guard
(* Convert a parsed sequential code bloc to sequential code bloc for abstract model *)
val convert_seq_code_bloc : variable_infos -> parsed_functions_table -> parsed_seq_code_bloc_list -> potential_clock_updates * seq_code_bloc_list
(* Convert a parsed function definition to function definition for abstract model *)
val convert_fun_definition : variable_infos -> parsed_fun_definition -> fun_definition