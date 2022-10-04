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
open DiscreteExpressions

val convert_discrete_init : variable_infos -> variable_name -> ParsingStructure.parsed_boolean_expression -> DiscreteExpressions.global_expression
val convert_discrete_constant : constants_table -> variable_name * ParsingStructure.parsed_boolean_expression * DiscreteType.var_type -> DiscreteExpressions.global_expression
val convert_guard : variable_infos -> ParsingStructure.guard -> AbstractModel.guard
val convert_update : variable_infos -> updates_type -> parsed_update_type -> ParsingStructure.parsed_boolean_expression -> DiscreteExpressions.update_type * DiscreteExpressions.global_expression
val convert_continuous_update : variable_infos -> parsed_scalar_or_index_update_type -> ParsingStructure.parsed_boolean_expression -> DiscreteExpressions.update_type * LinearConstraint.pxd_linear_term
val convert_conditional : variable_infos -> ParsingStructure.parsed_boolean_expression -> DiscreteExpressions.boolean_expression
val convert_seq_code_bloc : variable_infos -> ParsingStructure.parsed_seq_code_bloc -> DiscreteExpressions.seq_code_bloc
val convert_fun_definition : variable_infos -> ParsingStructure.parsed_fun_definition -> AbstractModel.fun_definition