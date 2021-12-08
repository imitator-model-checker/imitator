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

(** Linear part **)

val linear_term_of_global_expression : variable_infos -> ParsingStructure.global_expression -> LinearConstraint.pxd_linear_term
val linear_term_of_linear_expression : variable_infos -> ParsingStructure.linear_expression -> LinearConstraint.pxd_linear_term
val linear_constraint_of_convex_predicate : variable_infos -> ParsingStructure.linear_constraint list -> LinearConstraint.pxd_linear_constraint


val convert_discrete_init3 : variable_infos -> variable_name -> ParsingStructure.global_expression -> DiscreteExpressions.global_expression
val convert_discrete_constant : constants_table -> variable_name * ParsingStructure.global_expression * DiscreteType.var_type -> DiscreteExpressions.global_expression
val convert_guard : variable_infos -> ParsingStructure.guard -> AbstractModel.guard
val convert_update : variable_infos -> variable_access -> ParsingStructure.global_expression -> DiscreteExpressions.discrete_variable_access * DiscreteExpressions.global_expression
val convert_conditional : variable_infos -> ParsingStructure.parsed_boolean_expression -> DiscreteExpressions.boolean_expression
(*val convert_discrete_bool_expr : variable_infos -> ParsingStructure.parsed_discrete_boolean_expression -> DiscreteExpressions.discrete_boolean_expression*)

val bool_expression_of_typed_discrete_boolean_expression : variable_infos -> TypeChecker2.typed_discrete_boolean_expression -> DiscreteExpressions.discrete_boolean_expression