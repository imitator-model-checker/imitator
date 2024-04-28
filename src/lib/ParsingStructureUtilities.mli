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

(**)
type variable_callback = (variable_name -> unit) option

(* Leaves of parsing structure *)
type parsing_structure_leaf =
    | Leaf_variable of variable_ref
    | Leaf_constant of ParsedValue.parsed_value
    | Leaf_fun of variable_name

(* Leaves of parsed bloc *)
type parsed_seq_code_bloc_leaf =
    | Leaf_update_variable of variable_ref * parsed_boolean_expression * update_mode

(** Leaf of linear expression *)
type linear_expression_leaf =
    | Leaf_true_linear_constraint
    | Leaf_false_linear_constraint
    | Leaf_linear_constant of NumConst.t
    | Leaf_linear_variable of NumConst.t * variable_name

(** Leaf of init state predicate *)
type init_state_predicate_leaf =
    | Leaf_loc_assignment of automaton_name * location_name

(* Leaf of state predicate *)
type state_predicate_leaf =
    | Leaf_predicate_true
    | Leaf_predicate_false
    | Leaf_predicate_accepting
    | Leaf_predicate_EQ of string (* automaton name *) * string (* location name *)
    | Leaf_predicate_NEQ of string (* automaton name *) * string (* location name *)

(** Type of callback function called when reach a leaf of a discrete expression *)
type 'a parsing_structure_leaf_callback = parsing_structure_leaf -> 'a

(** Type of callback function called when reach a leaf of a sequential code bloc *)
type 'a seq_code_bloc_leaf_callback = parsed_seq_code_bloc_leaf -> 'a

(** Type of callback function called when reach a leaf of a linear expression *)
type 'a linear_expression_leaf_callback = linear_expression_leaf -> 'a

type 'a variable_declaration_callback = (variable_name * var_type_discrete * int -> 'a) option

type parsing_structure_leaf_modifier = parsing_structure_leaf -> parsing_structure_leaf

val map_parsed_boolean_expression : parsing_structure_leaf_modifier -> parsed_boolean_expression -> parsed_boolean_expression
val map_parsed_discrete_boolean_expression : parsing_structure_leaf_modifier -> parsed_discrete_boolean_expression -> parsed_discrete_boolean_expression
val map_parsed_discrete_arithmetic_expression : parsing_structure_leaf_modifier -> parsed_discrete_arithmetic_expression -> parsed_discrete_arithmetic_expression
val map_parsed_discrete_term : parsing_structure_leaf_modifier -> parsed_discrete_term -> parsed_discrete_term
val map_parsed_discrete_factor : parsing_structure_leaf_modifier -> parsed_discrete_factor -> parsed_discrete_factor

val fold_parsed_boolean_expression : ('a -> 'a -> 'a) -> 'a -> 'a parsing_structure_leaf_callback -> parsed_boolean_expression -> 'a
val fold_parsed_discrete_boolean_expression : ('a -> 'a -> 'a) -> 'a -> 'a parsing_structure_leaf_callback -> parsed_discrete_boolean_expression -> 'a
val fold_parsed_discrete_arithmetic_expression : ('a -> 'a -> 'a) -> 'a -> 'a parsing_structure_leaf_callback -> parsed_discrete_arithmetic_expression -> 'a
val fold_parsed_discrete_term : ('a -> 'a -> 'a) -> 'a -> 'a parsing_structure_leaf_callback -> parsed_discrete_term -> 'a
val fold_parsed_discrete_factor : ('a -> 'a -> 'a) -> 'a -> 'a parsing_structure_leaf_callback -> parsed_discrete_factor -> 'a

val fold_parsed_seq_code_bloc : ('a -> 'a -> 'a) -> 'a -> ?decl_callback:'a variable_declaration_callback -> 'a seq_code_bloc_leaf_callback -> 'a parsing_structure_leaf_callback -> parsed_seq_code_bloc -> 'a
val fold_parsed_fun_def : ('a -> 'a -> 'a) -> 'a -> ?decl_callback:'a variable_declaration_callback -> 'a seq_code_bloc_leaf_callback -> 'a parsing_structure_leaf_callback -> parsed_fun_definition -> 'a
val fold_parsed_normal_update : ('a -> 'a -> 'a) -> 'a -> ?decl_callback:'a variable_declaration_callback -> 'a seq_code_bloc_leaf_callback -> 'a parsing_structure_leaf_callback -> normal_update -> 'a


(** Check if all leaf of a parsing structure satisfy the predicate **)

val for_all_in_parsed_boolean_expression : bool parsing_structure_leaf_callback -> parsed_boolean_expression -> bool
val for_all_in_parsed_discrete_boolean_expression : bool parsing_structure_leaf_callback -> parsed_discrete_boolean_expression -> bool
val for_all_in_parsed_discrete_arithmetic_expression : bool parsing_structure_leaf_callback -> parsed_discrete_arithmetic_expression -> bool
val for_all_in_parsed_discrete_term : bool parsing_structure_leaf_callback -> parsed_discrete_term -> bool
val for_all_in_parsed_discrete_factor : bool parsing_structure_leaf_callback -> parsed_discrete_factor -> bool

(** Check if all leaf of a linear expression satisfy the predicate **)
val for_all_in_parsed_linear_expression : bool linear_expression_leaf_callback -> linear_expression -> bool

(** Check if all leaf of a linear term satisfy the predicate **)
val for_all_in_parsed_linear_term : bool linear_expression_leaf_callback -> linear_term -> bool

(** Check if all leaf of a linear constraint satisfy the predicate **)
val for_all_in_parsed_linear_constraint : bool linear_expression_leaf_callback -> linear_constraint -> bool

(** Check if all leaf of a non-linear constraint satisfy the predicate **)
val for_all_in_parsed_nonlinear_constraint : bool parsing_structure_leaf_callback -> nonlinear_constraint -> bool

(** Check if all leaf of a parsed normal update satisfy the predicate **)
val for_all_in_parsed_normal_update : ?decl_callback:bool variable_declaration_callback -> bool seq_code_bloc_leaf_callback -> bool parsing_structure_leaf_callback -> normal_update -> bool

(** Check if all leaf of a parsed sequential code bloc satisfy the predicate **)
val for_all_in_parsed_seq_code_bloc : ?decl_callback:bool variable_declaration_callback -> bool seq_code_bloc_leaf_callback -> bool parsing_structure_leaf_callback -> parsed_seq_code_bloc -> bool

(** Check if all leaf of a parsed function definition satisfy the predicate **)
val for_all_in_parsed_fun_def : ?decl_callback:bool variable_declaration_callback -> bool seq_code_bloc_leaf_callback -> bool parsing_structure_leaf_callback -> parsed_fun_definition -> bool

val for_all_in_parsed_loc_predicate : (state_predicate_leaf -> bool) -> bool parsing_structure_leaf_callback -> parsed_loc_predicate -> bool
val for_all_in_parsed_simple_predicate : (state_predicate_leaf -> bool) -> bool parsing_structure_leaf_callback -> parsed_simple_predicate -> bool
val for_all_in_parsed_state_predicate_factor : (state_predicate_leaf -> bool) -> bool parsing_structure_leaf_callback -> parsed_state_predicate_factor -> bool
val for_all_in_parsed_state_predicate_term : (state_predicate_leaf -> bool) -> bool parsing_structure_leaf_callback -> parsed_state_predicate_term -> bool
val for_all_in_parsed_state_predicate : (state_predicate_leaf -> bool) -> bool parsing_structure_leaf_callback -> parsed_state_predicate -> bool

(** Check if any leaf of a parsing structure satisfy the predicate **)

val exists_in_parsed_boolean_expression : bool parsing_structure_leaf_callback -> parsed_boolean_expression -> bool
val exists_in_parsed_discrete_boolean_expression : bool parsing_structure_leaf_callback -> parsed_discrete_boolean_expression -> bool
val exists_in_parsed_discrete_arithmetic_expression : bool parsing_structure_leaf_callback -> parsed_discrete_arithmetic_expression -> bool
val exists_in_parsed_discrete_term : bool parsing_structure_leaf_callback -> parsed_discrete_term -> bool
val exists_in_parsed_discrete_factor : bool parsing_structure_leaf_callback -> parsed_discrete_factor -> bool

(** Check if any leaf of a linear expression the predicate **)
val exists_in_parsed_linear_expression : bool linear_expression_leaf_callback -> linear_expression -> bool

(** Check if any leaf of a linear term the predicate **)
val exists_in_parsed_linear_term : bool linear_expression_leaf_callback -> linear_term -> bool

(** Check if any leaf of a linear constraint satisfy the predicate **)
val exists_in_parsed_linear_constraint : bool linear_expression_leaf_callback -> linear_constraint -> bool

(** Check if any leaf of a non-linear constraint satisfy the predicate **)
val exists_in_parsed_nonlinear_constraint : bool parsing_structure_leaf_callback -> nonlinear_constraint -> bool

(** Check if any leaf of a parsed normal update satisfy the predicate **)
val exists_in_parsed_normal_update : ?decl_callback:bool variable_declaration_callback -> bool seq_code_bloc_leaf_callback -> bool parsing_structure_leaf_callback -> normal_update -> bool

(** Check if any leaf of a parsed state predicate satisfy the predicate **)
val exists_in_parsed_state_predicate : (state_predicate_leaf -> bool) -> bool parsing_structure_leaf_callback -> parsed_state_predicate -> bool


(** Check if any leaf of a parsed sequential code bloc satisfy the predicate **)
val exists_in_parsed_seq_code_bloc : ?decl_callback:bool variable_declaration_callback -> bool seq_code_bloc_leaf_callback -> bool parsing_structure_leaf_callback -> parsed_seq_code_bloc -> bool

(** Check if any leaf of a parsed function definition satisfy the predicate **)
val exists_in_parsed_function_definition : ?decl_callback:bool variable_declaration_callback -> bool seq_code_bloc_leaf_callback -> bool parsing_structure_leaf_callback -> parsed_fun_definition -> bool

(** Apply units over leaf of a parsing structure **)

val iterate_parsed_boolean_expression : unit parsing_structure_leaf_callback -> parsed_boolean_expression -> unit
val iterate_parsed_discrete_boolean_expression : unit parsing_structure_leaf_callback -> parsed_discrete_boolean_expression -> unit
val iterate_parsed_discrete_arithmetic_expression : unit parsing_structure_leaf_callback -> parsed_discrete_arithmetic_expression -> unit
val iterate_parsed_discrete_term : unit parsing_structure_leaf_callback -> parsed_discrete_term -> unit
val iterate_parsed_discrete_factor : unit parsing_structure_leaf_callback -> parsed_discrete_factor -> unit
val iterate_parsed_normal_update : ?decl_callback:unit variable_declaration_callback -> unit seq_code_bloc_leaf_callback -> unit parsing_structure_leaf_callback -> normal_update -> unit

val iterate_in_parsed_loc_predicate : (state_predicate_leaf -> unit) -> unit parsing_structure_leaf_callback -> parsed_loc_predicate -> unit
val iterate_in_parsed_simple_predicate : (state_predicate_leaf -> unit) -> unit parsing_structure_leaf_callback -> parsed_simple_predicate -> unit
val iterate_in_parsed_state_predicate_factor : (state_predicate_leaf -> unit) -> unit parsing_structure_leaf_callback -> parsed_state_predicate_factor -> unit
val iterate_in_parsed_state_predicate_term : (state_predicate_leaf -> unit) -> unit parsing_structure_leaf_callback -> parsed_state_predicate_term -> unit
val iterate_in_parsed_state_predicate : (state_predicate_leaf -> unit) -> unit parsing_structure_leaf_callback -> parsed_state_predicate -> unit

(** Iterate over a linear expression applying a unit function **)
val iterate_parsed_linear_expression : unit linear_expression_leaf_callback -> linear_expression -> unit

(** Iterate over a linear term applying a unit function **)
val iterate_parsed_linear_term : unit linear_expression_leaf_callback -> linear_term -> unit

(** Iterate over a linear constraint applying a unit function **)
val iterate_parsed_linear_constraint : unit linear_expression_leaf_callback -> linear_constraint -> unit

(** Iterate over a non-linear constraint applying a unit function **)
val iterate_parsed_nonlinear_constraint : unit parsing_structure_leaf_callback -> nonlinear_constraint -> unit

(** Iterate over a non-linear convex predicate **)
val iterate_parsed_nonlinear_convex_predicate : unit parsing_structure_leaf_callback -> convex_predicate -> unit

(** Iterate over a parsed sequential code bloc definition **)
val iterate_in_parsed_seq_code_bloc : ?decl_callback:unit variable_declaration_callback -> unit seq_code_bloc_leaf_callback -> unit parsing_structure_leaf_callback -> parsed_seq_code_bloc -> unit

(** Iterate over a parsed function definition **)
val iterate_in_parsed_function_definition :  ?decl_callback:unit variable_declaration_callback -> unit seq_code_bloc_leaf_callback -> unit parsing_structure_leaf_callback -> parsed_fun_definition -> unit

val label_of_parsed_factor_constructor : parsed_discrete_factor -> string

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
val string_of_parsed_instruction : variable_infos -> parsed_instruction -> string

val string_of_parsed_normal_update : variable_infos -> normal_update -> string
val string_of_parsed_clock_update : variable_infos -> normal_update -> string
val string_of_parsed_scalar_or_index_update_type : variable_infos -> parsed_scalar_or_index_update_type -> string

(* Parsed linear constraint to string *)
val string_of_parsed_linear_constraint : variable_infos -> linear_constraint -> string
val string_of_linear_expression : variable_infos -> linear_expression -> string
(* val string_of_linear_term : linear_term -> string *)
val string_of_parsed_init_state_predicate : variable_infos -> parsed_init_state_predicate -> string

val string_of_parsed_nonlinear_constraint : variable_infos -> nonlinear_constraint -> string

val string_of_parsed_loc_predicate : parsed_loc_predicate -> string
val string_of_parsed_simple_predicate : variable_infos -> parsed_simple_predicate -> string
val string_of_parsed_state_predicate_factor : variable_infos -> parsed_state_predicate_factor -> string
val string_of_parsed_state_predicate_term : variable_infos -> parsed_state_predicate_term -> string
val string_of_parsed_state_predicate : variable_infos -> parsed_state_predicate -> string

val json_of_function_metadata : function_metadata -> JsonFormatter.json_element
val link_variables_in_parsed_model : parsed_model -> parsed_model * (variable_name * variable_id, var_type) Hashtbl.t

val gen_access_id : string -> int -> string

val string_of_list_of_name_or_access_with_sep : string -> name_or_access list -> string
