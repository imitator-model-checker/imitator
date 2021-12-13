module rec TypeChecker : sig

open ParsingStructure
open DiscreteType

type inner_type = var_type_discrete

type typed_global_expression =
    | Typed_global_expr of typed_boolean_expression * var_type_discrete

and typed_boolean_expression =
	| Typed_And of typed_boolean_expression * typed_boolean_expression (* implicitly bool type *)
	| Typed_Or of typed_boolean_expression * typed_boolean_expression (* implicitly bool type *)
	| Typed_discrete_bool_expr of typed_discrete_boolean_expression * var_type_discrete

and typed_discrete_boolean_expression =
    | Typed_arithmetic_expr of typed_discrete_arithmetic_expression * var_type_discrete
	| Typed_comparison of typed_discrete_boolean_expression * parsed_relop * typed_discrete_boolean_expression * var_type_discrete (* implicitly bool type *)
	| Typed_comparison_in of typed_discrete_arithmetic_expression * typed_discrete_arithmetic_expression * typed_discrete_arithmetic_expression * var_type_discrete_number (* implicitly bool type *)
	| Typed_bool_expr of typed_boolean_expression (* implicitly bool type *)
	| Typed_not_expr of typed_boolean_expression (* implicitly bool type *)

and typed_discrete_arithmetic_expression =
	| Typed_plus of typed_discrete_arithmetic_expression * typed_discrete_term * var_type_discrete_number
	| Typed_minus of typed_discrete_arithmetic_expression * typed_discrete_term * var_type_discrete_number
	| Typed_term of typed_discrete_term * var_type_discrete

and typed_discrete_term =
	| Typed_mul of typed_discrete_term * typed_discrete_factor * var_type_discrete_number
	| Typed_div of typed_discrete_term * typed_discrete_factor * var_type_discrete_number
	| Typed_factor of typed_discrete_factor * var_type_discrete

and typed_discrete_factor =
	| Typed_variable of variable_name * var_type_discrete
	| Typed_constant of DiscreteValue.discrete_value * var_type_discrete
	| Typed_array of typed_boolean_expression array * inner_type
	| Typed_list of typed_boolean_expression list * inner_type
	| Typed_expr of typed_discrete_arithmetic_expression * var_type_discrete
	| Typed_unary_min of typed_discrete_factor * var_type_discrete_number
    | Typed_access of typed_discrete_factor * typed_discrete_arithmetic_expression * var_type_discrete * inner_type
	| Typed_function_call of string * typed_boolean_expression list * var_type_discrete

type typed_variable_access =
    | Typed_variable_name of variable_name
    | Typed_variable_access of typed_variable_access * typed_discrete_arithmetic_expression * var_type_discrete


type typed_loc_predicate =
	| Typed_loc_predicate_EQ of automaton_name * location_name
	| Typed_loc_predicate_NEQ of automaton_name * location_name

type typed_simple_predicate =
	| Typed_discrete_boolean_expression of typed_discrete_boolean_expression * var_type_discrete
	| Typed_loc_predicate of typed_loc_predicate
	| Typed_state_predicate_true
	| Typed_state_predicate_false
	| Typed_state_predicate_accepting

type typed_state_predicate_factor =
	| Typed_state_predicate_factor_NOT of typed_state_predicate_factor
	| Typed_simple_predicate of typed_simple_predicate * var_type_discrete
	| Typed_state_predicate of typed_state_predicate * var_type_discrete

and typed_state_predicate_term =
	| Typed_state_predicate_term_AND of typed_state_predicate_term * typed_state_predicate_term
	| Typed_state_predicate_factor of typed_state_predicate_factor * var_type_discrete

and typed_state_predicate =
	| Typed_state_predicate_OR of typed_state_predicate * typed_state_predicate
	| Typed_state_predicate_term of typed_state_predicate_term * var_type_discrete

type typed_guard = typed_discrete_boolean_expression list

(*val string_of_typed_global_expression : variable_infos -> typed_global_expression -> string*)
val string_of_typed_discrete_boolean_expression : variable_infos -> typed_discrete_boolean_expression -> string
(*val string_of_typed_discrete_factor : variable_infos -> typed_discrete_factor -> string*)

(*val type_of_typed_discrete_boolean_expression : typed_discrete_boolean_expression -> var_type_discrete*)
(*val type_of_typed_discrete_factor : typed_discrete_factor -> var_type_discrete*)

(* Check that a discrete init is well typed *)
val check_discrete_init3 : variable_infos -> variable_name -> global_expression -> typed_global_expression
(* Check that a constant declarations is well typed *)
val check_constant_expression : variable_infos -> variable_name * global_expression * DiscreteType.var_type -> typed_global_expression
(* Check that a guard is well typed *)
val check_guard : variable_infos -> guard -> typed_guard
(* Check that an update is well typed *)
val check_update : variable_infos -> variable_access -> ParsingStructure.global_expression -> typed_variable_access * typed_global_expression
(* Check that a condition is well typed *)
val check_conditional : variable_infos -> ParsingStructure.parsed_boolean_expression -> typed_boolean_expression
(* Check that a predicate is well typed *)
val check_state_predicate : variable_infos -> parsed_state_predicate -> typed_state_predicate
(* Check that a discrete boolean expression is well typed *)
(*val check_discrete_boolean_expr : variable_infos -> parsed_discrete_boolean_expression -> typed_discrete_boolean_expression*)

end

and Convert : sig

open ParsingStructure
open DiscreteExpressions

(** Linear part **)

val linear_term_of_linear_expression : variable_infos -> ParsingStructure.linear_expression -> LinearConstraint.pxd_linear_term
val linear_constraint_of_convex_predicate : variable_infos -> ParsingStructure.linear_constraint list -> LinearConstraint.pxd_linear_constraint

val linear_term_of_typed_global_expression : variable_infos -> TypeChecker.typed_global_expression -> LinearConstraint.pxd_linear_term
val global_expression_of_typed_global_expression : variable_infos -> TypeChecker.typed_global_expression -> DiscreteExpressions.global_expression
val bool_expression_of_typed_boolean_expression : variable_infos -> TypeChecker.typed_boolean_expression -> DiscreteExpressions.boolean_expression
val bool_expression_of_typed_discrete_boolean_expression : variable_infos -> TypeChecker.typed_discrete_boolean_expression -> DiscreteExpressions.discrete_boolean_expression
val nonlinear_constraint_of_typed_nonlinear_constraint : variable_infos -> TypeChecker.typed_discrete_boolean_expression -> DiscreteExpressions.discrete_boolean_expression

val variable_access_of_typed_variable_access : variable_infos -> TypeChecker.typed_variable_access -> DiscreteExpressions.discrete_variable_access

end