open ParsingStructure
open ImitatorUtilities
open OCamlUtilities
open DiscreteType
open CustomModules

type inner_type = var_type_discrete

type typed_variable_scope =
    | Global
    | Local

type typed_assignment_scope =
    | Ass_discrete_global
    | Ass_discrete_local
    | Ass_clock

type typed_sequence_type =
    | Typed_array
    | Typed_list
    | Typed_stack
    | Typed_queue

type typed_conj_dis =
    | Typed_and
    | Typed_or

type typed_boolean_expression =
    | Typed_conj_dis of typed_boolean_expression * typed_boolean_expression * typed_conj_dis (* implicitly bool type *)
	| Typed_discrete_bool_expr of typed_discrete_boolean_expression * var_type_discrete

and typed_discrete_boolean_expression =
    | Typed_arithmetic_expr of typed_discrete_arithmetic_expression * var_type_discrete
	| Typed_comparison of typed_discrete_boolean_expression * parsed_relop * typed_discrete_boolean_expression * var_type_discrete (* implicitly bool type *)
	| Typed_comparison_in of typed_discrete_arithmetic_expression * typed_discrete_arithmetic_expression * typed_discrete_arithmetic_expression * var_type_discrete_number (* implicitly bool type *)
	| Typed_bool_expr of typed_boolean_expression (* implicitly bool type *)
	| Typed_not_expr of typed_boolean_expression (* implicitly bool type *)

and typed_discrete_arithmetic_expression =
    | Typed_sum_diff of typed_discrete_arithmetic_expression * typed_discrete_term * var_type_discrete_number * typed_sum_diff
	| Typed_term of typed_discrete_term * var_type_discrete

and typed_sum_diff =
    | Typed_plus
    | Typed_minus

and typed_discrete_term =
    | Typed_product_quotient of typed_discrete_term * typed_discrete_factor * var_type_discrete_number * typed_product_quotient
	| Typed_factor of typed_discrete_factor * var_type_discrete

and typed_product_quotient =
    | Typed_mul
    | Typed_div

and typed_discrete_factor =
	| Typed_variable of variable_name * var_type_discrete * typed_variable_scope
	| Typed_constant of ParsedValue.parsed_value * var_type_discrete
	| Typed_sequence of typed_boolean_expression list * inner_type * typed_sequence_type
	| Typed_expr of typed_discrete_arithmetic_expression * var_type_discrete
	| Typed_unary_min of typed_discrete_factor * var_type_discrete_number
    | Typed_access of typed_discrete_factor * typed_discrete_arithmetic_expression * var_type_discrete * inner_type
	| Typed_function_call of string * typed_boolean_expression list * var_type_discrete

type typed_scalar_or_index_update_type =
    | Typed_scalar_update of variable_name
    | Typed_indexed_update of typed_scalar_or_index_update_type * typed_discrete_arithmetic_expression * var_type_discrete

type typed_update_type =
    | Typed_variable_update of typed_scalar_or_index_update_type
    | Typed_void_update

type typed_normal_update = typed_update_type * typed_boolean_expression

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

type typed_loop_dir =
    | Typed_for_loop_up
    | Typed_for_loop_down

type typed_seq_code_bloc =
    | Typed_local_decl of variable_name * var_type_discrete * typed_boolean_expression * typed_seq_code_bloc
    | Typed_assignment of typed_normal_update * typed_seq_code_bloc * typed_assignment_scope
    | Typed_instruction of typed_boolean_expression * typed_seq_code_bloc
    | Typed_for_loop of variable_name * typed_discrete_arithmetic_expression (* from *) * typed_discrete_arithmetic_expression (* to *) * typed_loop_dir (* up or down *) * typed_seq_code_bloc (* inner bloc *) * typed_seq_code_bloc (* next bloc *)
    | Typed_while_loop of typed_boolean_expression (* condition *) * typed_seq_code_bloc (* inner bloc *) * typed_seq_code_bloc (* next *)
    | Typed_if of typed_boolean_expression (* condition *) * typed_seq_code_bloc (* then bloc *) * typed_seq_code_bloc option (* else bloc *) * typed_seq_code_bloc (* next *)
    | Typed_return_expr of typed_boolean_expression
    | Typed_bloc_void

type typed_fun_definition = {
    name : variable_name; (* function name *)
    parameters : variable_name list; (* parameter names *)
    signature : var_type_discrete list; (* signature *)
    body : typed_seq_code_bloc; (* body *)
}

val label_of_typed_sequence_type : typed_sequence_type -> string
val label_of_typed_factor_constructor : typed_discrete_factor -> string

val string_of_typed_boolean_expression : variable_infos -> typed_boolean_expression -> string
val string_of_typed_discrete_boolean_expression : variable_infos -> typed_discrete_boolean_expression -> string
val string_of_typed_discrete_arithmetic_expression : variable_infos -> var_type_discrete -> typed_discrete_arithmetic_expression -> string
val string_of_typed_discrete_term : variable_infos -> var_type_discrete -> typed_discrete_term -> string
val string_of_typed_discrete_factor : variable_infos -> var_type_discrete -> typed_discrete_factor -> string
val string_of_typed_seq_code_bloc : variable_infos -> typed_seq_code_bloc -> string

val string_of_typed_state_predicate : variable_infos -> typed_state_predicate -> string

type 'a traversed_typed_seq_code_bloc =
    | Traversed_typed_local_decl of variable_name * DiscreteType.var_type_discrete * typed_boolean_expression (* init expr *) * 'a
    | Traversed_typed_assignment of typed_normal_update * 'a
    | Traversed_typed_instruction of typed_boolean_expression * 'a
    | Traversed_typed_for_loop of variable_name * typed_discrete_arithmetic_expression (* from *) * typed_discrete_arithmetic_expression (* to *) * typed_loop_dir (* up or down *) * 'a * 'a
    | Traversed_typed_while_loop of typed_boolean_expression (* condition *) * 'a (* inner bloc result *) * 'a (* next result *)
    | Traversed_typed_if of typed_boolean_expression (* condition *) * 'a (* then result *) * 'a option (* else result *) * 'a (* next result *)
    | Traversed_typed_return_expr of typed_boolean_expression
    | Traversed_typed_bloc_void

val traverse_typed_seq_code_bloc : (var_type_discrete VariableMap.t -> 'a traversed_typed_seq_code_bloc -> 'a) -> typed_seq_code_bloc -> 'a