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
(*
val convert_parsed_global_expression : variable_infos -> ParsingStructure.global_expression -> DiscreteExpressions.global_expression
val bool_expression_of_parsed_boolean_expression : variable_infos -> ParsingStructure.parsed_boolean_expression -> DiscreteExpressions.boolean_expression
val bool_expression_of_parsed_discrete_boolean_expression : variable_infos -> ParsingStructure.parsed_discrete_boolean_expression -> DiscreteExpressions.discrete_boolean_expression
val int_arithmetic_expression_of_parsed_arithmetic_expression : variable_infos -> ParsingStructure.parsed_discrete_arithmetic_expression -> DiscreteExpressions.int_arithmetic_expression
val convert_discrete_init : variable_infos -> variable_name -> ParsingStructure.global_expression -> DiscreteExpressions.global_expression
*)
val convert_discrete_init3 : variable_infos -> variable_name -> ParsingStructure.global_expression -> DiscreteExpressions.global_expression