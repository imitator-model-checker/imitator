(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: General functions for map, filter, traverse, evaluating, etc. parsing structure tree
 *
 * File contributors : Benjamin L.
 * Created           : 2021/03/05
 *
 ************************************************************)

open ParsingStructure
open DiscreteType
open CustomModules

type variable_callback = (variable_name -> unit) option

(* Leaves of parsing structure *)
type parsing_structure_leaf =
    | Leaf_variable of variable_name
    | Leaf_constant of ParsedValue.parsed_value
    | Leaf_fun of variable_name
    | Leaf_update_variable of variable_name

(* Leaves of parsed bloc *)
type parsed_seq_code_bloc_leaf =
    | Leaf_decl_variable of variable_name * var_type_discrete * variable_id

(* Leaf of linear expression *)
type linear_expression_leaf =
    | Leaf_true_linear_constraint
    | Leaf_false_linear_constraint
    | Leaf_linear_constant of NumConst.t
    | Leaf_linear_variable of NumConst.t * variable_name

val fold_parsed_function_definition : ('a -> 'a -> 'a) -> 'a -> (parsed_seq_code_bloc_leaf -> 'a) -> (parsing_structure_leaf -> 'a) -> parsed_fun_definition -> 'a
val fold_parsed_normal_update : ('a -> 'a -> 'a) -> 'a -> (parsing_structure_leaf -> 'a) -> normal_update -> 'a


(** Check if all leaf of a parsing structure satisfy the predicate **)

val for_all_in_parsed_boolean_expression : (parsing_structure_leaf -> bool) -> parsed_boolean_expression -> bool
val for_all_in_parsed_discrete_boolean_expression : (parsing_structure_leaf -> bool) -> parsed_discrete_boolean_expression -> bool
val for_all_in_parsed_discrete_arithmetic_expression : (parsing_structure_leaf -> bool) -> parsed_discrete_arithmetic_expression -> bool
val for_all_in_parsed_discrete_term : (parsing_structure_leaf -> bool) -> parsed_discrete_term -> bool
val for_all_in_parsed_discrete_factor : (parsing_structure_leaf -> bool) -> parsed_discrete_factor -> bool

(** Check if all leaf of a linear expression satisfy the predicate **)
val for_all_in_parsed_linear_expression : (linear_expression_leaf -> bool) -> linear_expression -> bool
(** Check if all leaf of a linear term satisfy the predicate **)
val for_all_in_parsed_linear_term : (linear_expression_leaf -> bool) -> linear_term -> bool
(** Check if all leaf of a linear constraint satisfy the predicate **)
val for_all_in_parsed_linear_constraint : (linear_expression_leaf -> bool) -> linear_constraint -> bool
(** Check if all leaf of a non-linear constraint satisfy the predicate **)
val for_all_in_parsed_nonlinear_constraint : (parsing_structure_leaf -> bool) -> nonlinear_constraint -> bool
(** Check if all leaf of a parsed normal update satisfy the predicate **)
val for_all_in_parsed_normal_update : (parsing_structure_leaf -> bool) -> normal_update -> bool
(** Check if all leaf of a parsed update satisfy the predicate **)
val for_all_in_parsed_update : (parsing_structure_leaf -> bool) -> update -> bool
(** Check if all leaf of a parsed sequential code bloc satisfy the predicate **)
val for_all_in_parsed_seq_code_bloc_with_local_variables : (parsed_seq_code_bloc_leaf -> bool) -> (parsing_structure_leaf -> bool) -> parsed_seq_code_bloc -> bool
(** Check if all leaf of a parsed function definition satisfy the predicate **)
val for_all_in_parsed_function_definition : (parsed_seq_code_bloc_leaf -> bool) -> (parsing_structure_leaf -> bool) -> parsed_fun_definition -> bool

(** Check if any leaf of a parsing structure satisfy the predicate **)

val exists_in_parsed_boolean_expression : (parsing_structure_leaf -> bool) -> parsed_boolean_expression -> bool
val exists_in_parsed_discrete_boolean_expression : (parsing_structure_leaf -> bool) -> parsed_discrete_boolean_expression -> bool
val exists_in_parsed_discrete_arithmetic_expression : (parsing_structure_leaf -> bool) -> parsed_discrete_arithmetic_expression -> bool
val exists_in_parsed_discrete_term : (parsing_structure_leaf -> bool) -> parsed_discrete_term -> bool
val exists_in_parsed_discrete_factor : (parsing_structure_leaf -> bool) -> parsed_discrete_factor -> bool

(** Check if any leaf of a linear expression the predicate **)
val exists_in_parsed_linear_expression : (linear_expression_leaf -> bool) -> linear_expression -> bool
(** Check if any leaf of a linear term the predicate **)
val exists_in_parsed_linear_term : (linear_expression_leaf -> bool) -> linear_term -> bool
(** Check if any leaf of a linear constraint satisfy the predicate **)
val exists_in_parsed_linear_constraint : (linear_expression_leaf -> bool) -> linear_constraint -> bool
(** Check if any leaf of a non-linear constraint satisfy the predicate **)
val exists_in_parsed_nonlinear_constraint : (parsing_structure_leaf -> bool) -> nonlinear_constraint -> bool

(** Check if any leaf of a parsed update satisfy the predicate **)
val exists_in_parsed_update : (parsing_structure_leaf -> bool) -> update -> bool
(** Check if any leaf of a parsed normal update satisfy the predicate **)
val exists_in_parsed_normal_update : (parsing_structure_leaf -> bool) -> normal_update -> bool

(** Check if any leaf of a parsed sequential code bloc satisfy the predicate **)
(*val exists_in_parsed_seq_code_bloc_with_local_variables : (parsing_structure_leaf -> bool) -> parsed_seq_code_bloc -> bool*)
(** Check if any leaf of a parsed function definition satisfy the predicate **)
val exists_in_parsed_function_definition :  (parsed_seq_code_bloc_leaf -> bool) -> (parsing_structure_leaf -> bool) -> parsed_fun_definition -> bool

(** Apply units over leaf of a parsing structure **)

val iterate_parsed_boolean_expression : (parsing_structure_leaf -> unit) -> parsed_boolean_expression -> unit
val iterate_parsed_discrete_boolean_expression : (parsing_structure_leaf -> unit) -> parsed_discrete_boolean_expression -> unit
val iterate_parsed_discrete_arithmetic_expression : (parsing_structure_leaf -> unit) -> parsed_discrete_arithmetic_expression -> unit
val iterate_parsed_discrete_term : (parsing_structure_leaf -> unit) -> parsed_discrete_term -> unit
val iterate_parsed_discrete_factor : (parsing_structure_leaf -> unit) -> parsed_discrete_factor -> unit
val iterate_parsed_update : (parsing_structure_leaf -> unit) -> update -> unit

(** Iterate over a linear expression applying a unit function **)
val iterate_parsed_linear_expression : (linear_expression_leaf -> unit) -> linear_expression -> unit
(** Iterate over a linear term applying a unit function **)
val iterate_parsed_linear_term : (linear_expression_leaf -> unit) -> linear_term -> unit
(** Iterate over a linear constraint applying a unit function **)
val iterate_parsed_linear_constraint : (linear_expression_leaf -> unit) -> linear_constraint -> unit
(** Iterate over a non-linear constraint applying a unit function **)
val iterate_parsed_nonlinear_constraint : (parsing_structure_leaf -> unit) -> nonlinear_constraint -> unit
(** Iterate over a non-linear convex predicate **)
val iterate_parsed_nonlinear_convex_predicate : (parsing_structure_leaf -> unit) -> convex_predicate -> unit

(** Iterate over a parsed sequential code bloc definition **)
(*val iterate_in_parsed_seq_code_bloc_with_local_variables : (parsing_structure_leaf -> unit) -> parsed_seq_code_bloc -> unit*)
(** Iterate over a parsed function definition **)
val iterate_in_parsed_function_definition :  (parsed_seq_code_bloc_leaf -> unit) -> (parsing_structure_leaf -> unit) -> parsed_fun_definition -> unit

val label_of_parsed_factor_constructor : parsed_discrete_factor -> string

(* Parsed expression to string *)
val function_name_of_parsed_factor : parsed_discrete_factor -> string

val string_of_assignment : string -> string -> string
val string_of_let_in : string -> string -> string -> string

val string_of_parsed_boolean_expression : variable_infos -> parsed_boolean_expression -> string
val string_of_parsed_discrete_boolean_expression : variable_infos -> parsed_discrete_boolean_expression -> string
val string_of_parsed_arithmetic_expression : variable_infos -> parsed_discrete_arithmetic_expression -> string
val string_of_parsed_term : variable_infos -> parsed_discrete_term -> string
val string_of_parsed_factor : variable_infos -> parsed_discrete_factor -> string
val string_of_parsed_relop : parsed_relop -> string -> string -> string
val string_of_parsed_fun_def : variable_infos -> parsed_fun_definition -> string
val string_of_parsed_seq_code_bloc : variable_infos -> parsed_seq_code_bloc -> string

val string_of_parsed_update : variable_infos -> update -> string
val string_of_parsed_normal_update : variable_infos -> normal_update -> string
val string_of_parsed_clock_update : variable_infos -> parsed_scalar_or_index_update_type * parsed_boolean_expression -> string
val string_of_parsed_update_type : variable_infos -> parsed_update_type -> string
val string_of_parsed_scalar_or_index_update_type : variable_infos -> parsed_scalar_or_index_update_type -> string

(* Parsed linear constraint to string *)
val string_of_parsed_linear_constraint : variable_infos -> linear_constraint -> string
val string_of_linear_expression : variable_infos -> linear_expression -> string
val string_of_linear_term : variable_infos -> linear_term -> string
val string_of_parsed_init_state_predicate : variable_infos -> parsed_init_state_predicate -> string

val string_of_parsed_nonlinear_constraint : variable_infos -> nonlinear_constraint -> string

val string_of_parsed_loc_predicate : parsed_loc_predicate -> string
val string_of_parsed_simple_predicate : variable_infos -> parsed_simple_predicate -> string
val string_of_parsed_state_predicate_factor : variable_infos -> parsed_state_predicate_factor -> string
val string_of_parsed_state_predicate_term : variable_infos -> parsed_state_predicate_term -> string
val string_of_parsed_state_predicate : variable_infos -> parsed_state_predicate -> string

val json_of_function_metadata : function_metadata -> JsonFormatter.json_element

(** Utils **)

(* Try to get value of a discrete boolean expression, if directly a constant equals to false or true *)
(* If the expression is more complex, return None *)
val discrete_boolean_expression_constant_value_opt : parsed_discrete_boolean_expression -> bool option

val is_parsed_boolean_expression_constant : variable_infos -> parsed_boolean_expression -> bool
val is_parsed_arithmetic_expression_constant : variable_infos -> parsed_discrete_arithmetic_expression -> bool

val is_linear_parsed_boolean_expression : variable_infos -> parsed_boolean_expression -> bool
val is_linear_parsed_discrete_boolean_expression : variable_infos -> parsed_discrete_boolean_expression -> bool
val is_linear_parsed_arithmetic_expression : variable_infos -> parsed_discrete_arithmetic_expression -> bool
val is_linear_parsed_term : variable_infos -> parsed_discrete_term -> bool
val is_linear_parsed_factor : variable_infos -> parsed_discrete_factor -> bool


val all_variables_defined_in_parsed_boolean_expression : variable_infos -> variable_callback -> parsed_boolean_expression -> bool
val all_variables_defined_in_parsed_boolean_expression_without_callback : variable_infos -> parsed_boolean_expression -> bool
val all_variables_defined_in_parsed_boolean_expression : variable_infos -> variable_callback -> parsed_boolean_expression -> bool
val all_variables_defined_in_parsed_discrete_boolean_expression : variable_infos -> variable_callback -> parsed_discrete_boolean_expression -> bool
val all_variables_defined_in_parsed_discrete_arithmetic_expression : variable_infos -> variable_callback -> parsed_discrete_arithmetic_expression -> bool
val all_variables_defined_in_parsed_fun_def : variable_infos -> variable_callback -> parsed_fun_definition -> bool
val all_variables_defined_in_parsed_normal_update : variable_infos -> variable_callback -> normal_update -> bool
val all_variables_defined_in_parsed_update : variable_infos -> variable_callback -> update -> bool
val all_variables_defined_in_linear_expression : variable_infos -> (variable_name -> unit) -> linear_expression -> bool
val all_variables_defined_in_linear_constraint : variable_infos -> (variable_name -> unit) -> linear_constraint -> bool
val all_variables_defined_in_nonlinear_constraint : variable_infos -> variable_callback -> nonlinear_constraint -> bool
val all_variables_defined_in_nonlinear_convex_predicate : variable_infos -> variable_callback -> nonlinear_constraint list -> bool

val all_variable_in_parsed_state_predicate : useful_parsing_model_information -> variable_infos -> variable_callback -> (automaton_name -> unit) option -> (automaton_name -> location_name -> unit) option -> parsed_state_predicate -> bool

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
val get_global_variables_in_parsed_fun_def_with_accumulator : StringSet.t ref -> parsed_fun_definition -> unit

val get_variables_in_parsed_boolean_expression : parsed_boolean_expression -> StringSet.t
val get_functions_in_parsed_boolean_expression : parsed_boolean_expression -> StringSet.t
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

val get_global_variables_in_parsed_fun_def : parsed_fun_definition -> StringSet.t

val variable_name_of_parsed_update_type_opt : parsed_update_type -> variable_name option
val variable_name_of_parsed_update_type : parsed_update_type -> variable_name
val variable_name_of_parsed_scalar_or_index_update_type : parsed_scalar_or_index_update_type -> variable_name

val linear_constraint_of_nonlinear_constraint : nonlinear_constraint -> linear_constraint

(* Gather all updates of update section (pre-updates, updates and post-updates) *)
val updates_of_update_section : update_section -> update list