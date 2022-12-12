(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Dependency graph of the parsed model (variables / functions, etc.)
 *
 * File contributors : Benjamin L.
 * Created           : 2022/09/07
 *
 ************************************************************)

open ParsingStructure
open ParsingStructureUtilities
open ParsingStructureMeta
open CustomModules

type automaton_name = string
type variable_name = string
type param_name = string
type fun_name = string
type id = int

type local_variable_ref = variable_name * fun_name * id
type param_ref = param_name * fun_name

(* Reference to a program component *)
type component =
    | System_component
    | Automaton_component of automaton_name
    | Global_variable_component of variable_name
    | Local_variable_component of local_variable_ref
    | Param_component of param_ref
    | Fun_component of fun_name

(* A components set *)
module ComponentSet : Set.S with type elt = component

(* Relation between two components a -> b mean a use b *)
type relation = component * component
(* Dependency graph as a list of relations between the components *)
type dependency_graph = component list (* declared components *) * relation list


(* Get a dependency graph as a list of relations between variables and functions *)
(* Each relation is a pair representing a ref to a variable / function using another variable / function *)
val dependency_graph : ?no_var_autoremove:bool -> declarations_info -> parsed_model -> parsed_property option -> dependency_graph

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
val used_global_variables_of_model : dependency_graph -> StringSet.t
val unused_global_variables_of_model : dependency_graph -> StringSet.t

(* Remove all unused assignments in sequential code bloc *)
val remove_unused_assignments_in_updates : declarations_info -> dependency_graph -> parsed_seq_code_bloc_list -> parsed_seq_code_bloc_list
(* Remove all unused assignments in function definition *)
val remove_unused_assignments_in_fun_def : declarations_info -> dependency_graph -> parsed_fun_definition -> parsed_fun_definition
(* Filter a init_definition (init_state_predicate list) to keep only init definition of used variables *)
val remove_unused_inits : dependency_graph -> init_definition -> init_definition

val model_cycle_infos : dependency_graph -> (bool * string) list
