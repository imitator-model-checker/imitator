(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Functions that extract useful information on parsing structure (get variables, is expression is constant ?...)
 *
 * File contributors : Benjamin L.
 * Created           : 2022/05/18
 *
 ************************************************************)

open ParsingStructure
open ParsingStructureUtilities
open CustomModules
open DiscreteType




(** Utils **)

val variable_name_of_parsed_scalar_or_index_update_type : parsed_scalar_or_index_update_type -> variable_name



(* Try to get value of a discrete boolean expression, if directly a constant equals to false or true *)
(* If the expression is more complex, return None *)
val discrete_boolean_expression_constant_value_opt : parsed_discrete_boolean_expression -> bool option

val is_parsed_boolean_expression_constant : variable_infos -> parsed_boolean_expression -> bool
val is_parsed_arithmetic_expression_constant : variable_infos -> parsed_discrete_arithmetic_expression -> bool

(* Check if a parsed boolean expression has side effects *)
val has_side_effect_parsed_boolean_expression : variable_infos -> parsed_boolean_expression -> bool
(* Check if a parsed discrete boolean expression has side effects *)
val has_side_effect_parsed_discrete_boolean_expression : variable_infos -> parsed_discrete_boolean_expression -> bool
(* Check if a parsed discrete arithmetic expression has side effects *)
val has_side_effect_parsed_discrete_arithmetic_expression : variable_infos -> parsed_discrete_arithmetic_expression -> bool
(* Check if a parsed normal update has side effects *)
val has_side_effect_parsed_normal_update : variable_infos -> normal_update -> bool
(* Check if a parsed state predicate has side effects *)
val has_side_effect_parsed_state_predicate : variable_infos -> parsed_state_predicate -> bool

(* Check if a parsed boolean expression contains function call(s) *)
val has_fun_call_parsed_boolean_expression : parsed_boolean_expression -> bool
(* Check if a parsed discrete arithmetic expression contains function call(s) *)
val has_fun_call_parsed_discrete_arithmetic_expression : parsed_discrete_arithmetic_expression -> bool

val is_linear_parsed_boolean_expression : variable_infos -> parsed_boolean_expression -> bool
(*
val is_linear_parsed_discrete_boolean_expression : variable_infos -> parsed_discrete_boolean_expression -> bool
val is_linear_parsed_arithmetic_expression : variable_infos -> parsed_discrete_arithmetic_expression -> bool
val is_linear_parsed_term : variable_infos -> parsed_discrete_term -> bool
val is_linear_parsed_factor : variable_infos -> parsed_discrete_factor -> bool
*)

val all_variables_defined_in_parsed_boolean_expression : variable_infos -> variable_callback -> parsed_boolean_expression -> bool
val all_variables_defined_in_parsed_boolean_expression_without_callback : variable_infos -> parsed_boolean_expression -> bool
val all_variables_defined_in_parsed_boolean_expression : variable_infos -> variable_callback -> parsed_boolean_expression -> bool
val all_variables_defined_in_parsed_discrete_boolean_expression : variable_infos -> variable_callback -> parsed_discrete_boolean_expression -> bool
val all_variables_defined_in_parsed_discrete_arithmetic_expression : variable_infos -> variable_callback -> parsed_discrete_arithmetic_expression -> bool
val all_variables_defined_in_parsed_seq_code_bloc : variable_infos -> variable_callback -> parsed_seq_code_bloc_list -> bool
val all_functions_defined_in_parsed_seq_code_bloc : variable_infos -> variable_callback -> parsed_seq_code_bloc_list -> bool
val all_variables_defined_in_parsed_fun_def : variable_infos -> variable_callback -> parsed_fun_definition -> bool
val all_functions_defined_in_parsed_fun_def : variable_infos -> variable_callback -> parsed_fun_definition -> bool
val all_variables_defined_in_parsed_normal_update : variable_infos -> variable_callback -> normal_update -> bool
val all_variables_defined_in_parsed_update : variable_infos -> variable_callback -> update -> bool
val all_variables_defined_in_linear_expression : variable_infos -> (variable_name -> unit) -> linear_expression -> bool
val all_variables_defined_in_linear_constraint : variable_infos -> (variable_name -> unit) -> linear_constraint -> bool
val all_variables_defined_in_nonlinear_constraint : variable_infos -> variable_callback -> nonlinear_constraint -> bool
val all_variables_defined_in_nonlinear_convex_predicate : variable_infos -> variable_callback -> nonlinear_constraint list -> bool

val all_variables_defined_in_parsed_state_predicate : useful_parsing_model_information -> variable_infos -> variable_callback -> (automaton_name -> unit) option -> (automaton_name -> location_name -> unit) option -> parsed_state_predicate -> bool

(* Check that there is only discrete variables in a parsed boolean expression *)
val only_discrete_in_parsed_boolean_expression : variable_infos -> (var_type -> variable_name -> unit) option -> parsed_boolean_expression -> bool
(* Check that there is only discrete variables in a parsed discrete boolean expression *)
val only_discrete_in_nonlinear_expression : variable_infos -> parsed_discrete_boolean_expression -> bool

val no_variables_in_linear_expression : variable_infos -> linear_expression -> bool

val is_parsed_linear_expression_constant : variable_infos -> linear_expression -> bool



val get_variables_in_parsed_boolean_expression_with_accumulator : StringSet.t ref -> parsed_boolean_expression -> unit
val get_variables_in_parsed_boolean_expression_with_accumulator : StringSet.t ref -> parsed_boolean_expression -> unit
val get_variables_in_parsed_discrete_boolean_expression_with_accumulator : StringSet.t ref -> parsed_discrete_boolean_expression -> unit
val get_variables_in_parsed_update_with_accumulator : StringSet.t ref -> update -> unit
val get_functions_in_parsed_update_with_accumulator : StringSet.t ref -> update -> unit
val get_variables_in_parsed_normal_update_with_accumulator : StringSet.t ref -> normal_update -> unit
val get_variables_in_parsed_simple_predicate_with_accumulator : StringSet.t ref -> parsed_simple_predicate -> unit
val get_variables_in_parsed_state_predicate_with_accumulator : StringSet.t ref -> parsed_state_predicate -> unit

val get_variables_in_parsed_boolean_expression : parsed_boolean_expression -> StringSet.t
val get_functions_in_parsed_boolean_expression : parsed_boolean_expression -> StringSet.t
val get_clocks_and_parameters_in_parsed_boolean_expression : variable_infos -> parsed_boolean_expression -> StringSet.t
val get_variables_in_parsed_discrete_boolean_expression : parsed_discrete_boolean_expression -> StringSet.t
val get_variables_in_parsed_discrete_arithmetic_expression : parsed_discrete_arithmetic_expression -> StringSet.t
val get_functions_in_parsed_discrete_arithmetic_expression : parsed_discrete_arithmetic_expression -> StringSet.t
val get_variables_in_parsed_update : update -> StringSet.t
val get_variables_in_parsed_normal_update : normal_update -> StringSet.t
val get_variables_in_linear_expression : linear_expression -> StringSet.t
val get_variables_in_linear_constraint : linear_constraint -> StringSet.t
val get_variables_in_nonlinear_constraint : nonlinear_constraint -> StringSet.t
val get_variables_in_init_state_predicate : parsed_init_state_predicate -> StringSet.t
val get_variables_in_nonlinear_convex_predicate : nonlinear_constraint list -> StringSet.t
val get_functions_in_nonlinear_convex_predicate : nonlinear_constraint list -> StringSet.t
val get_variables_in_parsed_simple_predicate : parsed_simple_predicate -> StringSet.t
val get_variables_in_parsed_state_predicate : parsed_state_predicate -> StringSet.t

(* Get pairs of left and right members of assignments (ex: i := j + 1 + k return the triple (i, [j;k], j + 1 + k) *)
val left_right_member_of_assignments_in_parsed_seq_code_bloc : parsed_seq_code_bloc_list -> (variable_name * variable_name list * parsed_boolean_expression) list
(* Check whether clock updates found in parsed sequential code bloc are only resets *)
val is_only_resets_in_parsed_seq_code_bloc : variable_infos -> parsed_seq_code_bloc_list -> bool
(* Check whether clock updates found in parsed sequential code bloc (and all called functions in bloc) are only resets *)
val is_only_resets_in_parsed_seq_code_bloc_deep : variable_infos -> parsed_functions_table -> parsed_seq_code_bloc_list -> bool
(* Get local variables of a parsed function definition *)
val local_variables_of_parsed_fun_def : parsed_fun_definition -> (variable_name * var_type_discrete * int) list
(* Get local variables of a parsed sequential code bloc *)
val local_variables_of_parsed_seq_code_bloc : parsed_seq_code_bloc_list -> (variable_name * var_type_discrete * int) list
(* Check if function has side effect recursively (through other function calls found in function body) *)
val is_function_has_side_effects : functions_meta_table -> parsed_functions_table -> parsed_fun_definition -> bool