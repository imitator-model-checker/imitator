open ParsingStructure
open DiscreteType

type typed_global_expression =
    | Typed_global_expr of typed_boolean_expression * var_type_discrete

and typed_boolean_expression =
	| Typed_And of typed_boolean_expression * typed_boolean_expression
	| Typed_Or of typed_boolean_expression * typed_boolean_expression
	| Typed_discrete_bool_expr of typed_discrete_boolean_expression * var_type_discrete

and typed_discrete_boolean_expression =
    | Typed_arithmetic_expr of typed_discrete_arithmetic_expression * var_type_discrete
	| Typed_comparison of typed_discrete_boolean_expression * parsed_relop * typed_discrete_boolean_expression * var_type_discrete * var_type_discrete
	| Typed_comparison_in of typed_discrete_arithmetic_expression * typed_discrete_arithmetic_expression * typed_discrete_arithmetic_expression * var_type_discrete
	| Typed_bool_expr of typed_boolean_expression * var_type_discrete
	| Typed_not_expr of typed_boolean_expression

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
	| Typed_array of typed_boolean_expression array * var_type_discrete
	| Typed_list of typed_boolean_expression list * var_type_discrete
	| Typed_expr of typed_discrete_arithmetic_expression * var_type_discrete
	| Typed_unary_min of typed_discrete_factor
    | Typed_access of typed_discrete_factor * typed_discrete_arithmetic_expression * var_type_discrete
	| Typed_function_call of string * typed_boolean_expression list * var_type_discrete

val type_of_typed_discrete_boolean_expression : typed_discrete_boolean_expression -> var_type_discrete

(*type type_tree = | Node_type of var_type_discrete * type_tree list*)
(*val split_tree : type_tree -> var_type_discrete * type_tree list*)
(*val inner_unary_node : type_tree -> type_tree*)
(*val inner_bin_node : type_tree -> type_tree * type_tree*)


(*
val type_check_global_expression : variable_infos -> global_expression -> var_type_discrete
val type_check_parsed_boolean_expression : variable_infos -> parsed_boolean_expression -> var_type_discrete
val type_check_parsed_discrete_boolean_expression : variable_infos -> parsed_discrete_boolean_expression -> var_type_discrete
val type_check_parsed_discrete_arithmetic_expression : variable_infos -> parsed_discrete_arithmetic_expression -> var_type_discrete
val type_check_parsed_discrete_term : variable_infos -> parsed_discrete_term -> var_type_discrete
val type_check_parsed_discrete_factor : variable_infos -> parsed_discrete_factor -> var_type_discrete
val type_check_nonlinear_constraint : variable_infos -> nonlinear_constraint -> var_type_discrete
*)

val string_of_typed_global_expression : variable_infos -> typed_global_expression -> string

(*val checkus_guard : variable_infos -> nonlinear_constraint list -> unit*)
(*val check_discrete_init : variable_infos -> variable_name -> global_expression -> var_type_discrete*)
val check_discrete_init3 : variable_infos -> variable_name -> global_expression -> typed_global_expression
(* Check that constant declarations are well typed *)
val check_constant_expression : variable_infos -> variable_name * global_expression * DiscreteType.var_type -> typed_global_expression