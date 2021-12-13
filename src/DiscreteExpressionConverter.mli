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
 * Last modified     : 2021/11/20
 *
 ************************************************************)

open ParsingStructure
open DiscreteExpressions

val convert_discrete_init3 : variable_infos -> variable_name -> ParsingStructure.global_expression -> DiscreteExpressions.global_expression
val convert_discrete_constant : constants_table -> variable_name * ParsingStructure.global_expression * DiscreteType.var_type -> DiscreteExpressions.global_expression
val convert_guard : variable_infos -> ParsingStructure.guard -> AbstractModel.guard
val convert_update : variable_infos -> variable_access -> ParsingStructure.global_expression -> DiscreteExpressions.discrete_variable_access * DiscreteExpressions.global_expression
val convert_continuous_update : variable_infos -> variable_access -> ParsingStructure.global_expression -> DiscreteExpressions.discrete_variable_access * LinearConstraint.pxd_linear_term
val convert_conditional : variable_infos -> ParsingStructure.parsed_boolean_expression -> DiscreteExpressions.boolean_expression
