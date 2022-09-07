(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Functions that extract information on parsed model (dependency graph of variables / functions, etc.)
 *
 * File contributors : Benjamin L.
 * Created           : 2022/05/18
 *
 ************************************************************)

open ParsingStructure
open ParsingStructureUtilities
open CustomModules
open DiscreteType

type automaton_name = string
type variable_name = string
type param_name = string
type fun_name = string
type id = int

type local_variable_ref = variable_name * fun_name * id
type param_ref = param_name * fun_name

(* Reference to a program component *)
type component =
    | System_ref
    | Automaton_ref of automaton_name
    | Global_variable_ref of variable_name
    | Local_variable_ref of local_variable_ref
    | Param_ref of param_ref
    | Fun_ref of fun_name

(* A components set *)
module ComponentSet : Set.S with type elt = component

(* Relation between two components a -> b mean a use b *)
type relation = component * component
(* Dependency graph as a list of relations between the components *)
type dependency_graph = component list (* declared components *) * relation list


(* Get a dependency graph as a list of relations between variables and functions *)
(* Each relation is a pair representing a ref to a variable / function using another variable / function *)
val dependency_graph : ?no_var_autoremove:bool -> parsed_model -> dependency_graph

(* Get dependency graph as string (dot graphviz format) *)
val string_of_dependency_graph : dependency_graph -> string

(* Get all declared components of model *)
val components_of_model : dependency_graph -> ComponentSet.t

(* Get all components that are effectively used by automatons of the model *)
(* It mean all components that are reachable starting from the system reference *)
val used_components_of_model : dependency_graph -> ComponentSet.t

(* Get all components that are not used by automatons of the model *)
(* It mean all components that are not reachable starting from the system reference *)
val unused_components_of_model : dependency_graph -> ComponentSet.t

val used_functions_of_model : dependency_graph -> StringSet.t
val unused_functions_of_model : dependency_graph -> StringSet.t
val used_variables_of_model : dependency_graph -> StringSet.t
val unused_variables_of_model : dependency_graph -> StringSet.t


val model_cycle_infos : dependency_graph -> (bool * string) list

(* Get all variables (local and global) at the left side of an assignment in a function body implementation *)
val left_variables_of_assignments_in : parsed_fun_definition -> ComponentSet.t
(* Get all variables (local and global) at the right side of an assignment in a function body implementation *)
val right_variables_of_assignments_in : parsed_fun_definition -> ComponentSet.t



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

val variable_name_of_parsed_update_type_opt : parsed_update_type -> variable_name option
val variable_name_of_parsed_update_type : parsed_update_type -> variable_name
val variable_name_of_parsed_scalar_or_index_update_type : parsed_scalar_or_index_update_type -> variable_name

val linear_constraint_of_nonlinear_constraint : nonlinear_constraint -> linear_constraint

(* Gather all updates of update section (pre-updates, updates and post-updates) *)
val updates_of_update_section : update_section -> update list