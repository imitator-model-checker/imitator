(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Type checker module aims to type check expressions
 * Module description: Convert module aims to convert typed expressions to abstract model expressions
 *
 * File contributors : Benjamin L.
 * Created           : 2021/11/20
 *
 ************************************************************)

module rec TypeChecker : sig

open ParsingStructure
open TypedStructure

(* Check that a discrete init is well typed *)
val check_discrete_init : variable_infos -> variable_name -> parsed_boolean_expression -> typed_boolean_expression
(* Check that a constant declarations is well typed *)
val check_constant_expression : variable_infos -> variable_name * parsed_boolean_expression * DiscreteType.var_type -> typed_boolean_expression
(* Check that a guard is well typed *)
val check_guard : variable_infos -> guard -> typed_guard
(* Check that a predicate is well typed *)
val check_state_predicate : variable_infos -> parsed_state_predicate -> typed_state_predicate
(* Check that a discrete boolean expression is well typed *)
(*val check_discrete_boolean_expr : variable_infos -> parsed_discrete_boolean_expression -> typed_discrete_boolean_expression*)
(* Check whether a parsed sequential bloc definition is well typed *)
val check_seq_code_bloc : variable_infos -> parsed_seq_code_bloc -> typed_seq_code_bloc
(* Check whether a function definition is well typed *)
val check_fun_definition : variable_infos -> parsed_fun_definition -> typed_fun_definition

end

and Convert : sig

open ParsingStructure
open DiscreteExpressions
open TypedStructure
open AbstractModel

(* --- Linear part --- *)

val linear_term_of_linear_expression : variable_infos -> ParsingStructure.linear_expression -> LinearConstraint.pxd_linear_term
val linear_constraint_of_convex_predicate : variable_infos -> ParsingStructure.linear_constraint list -> LinearConstraint.pxd_linear_constraint

(* --- Discrete part --- *)

(*val linear_term_of_typed_boolean_expression : variable_infos -> typed_boolean_expression -> LinearConstraint.pxd_linear_term*)
val global_expression_of_typed_boolean_expression_by_type : variable_infos -> typed_boolean_expression -> DiscreteType.var_type_discrete -> DiscreteExpressions.global_expression
val global_expression_of_typed_boolean_expression : variable_infos -> typed_boolean_expression -> DiscreteExpressions.global_expression
val bool_expression_of_typed_boolean_expression : variable_infos -> typed_boolean_expression -> DiscreteExpressions.boolean_expression
val bool_expression_of_typed_discrete_boolean_expression : variable_infos -> typed_discrete_boolean_expression -> DiscreteExpressions.discrete_boolean_expression
val nonlinear_constraint_of_typed_nonlinear_constraint : variable_infos -> typed_discrete_boolean_expression -> DiscreteExpressions.discrete_boolean_expression

val scalar_or_index_update_type_of_typed_scalar_or_index_update_type : variable_infos -> typed_scalar_or_index_update_type -> DiscreteExpressions.scalar_or_index_update_type
(* Convert typed sequential code bloc to sequential code bloc for abstract model *)
val seq_code_bloc_of_typed_seq_code_bloc : variable_infos -> typed_seq_code_bloc -> seq_code_bloc
val clock_update_of_typed_seq_code_bloc : variable_infos -> bool -> typed_seq_code_bloc -> DiscreteExpressions.potential_clock_updates
(* Convert typed function to function for abstract model *)
val fun_definition_of_typed_fun_definition : variable_infos -> typed_fun_definition -> fun_definition

end