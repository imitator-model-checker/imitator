(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Utils functions that works on variable / constant
 *
 * File contributors : lbinria
 * Created           : 2022/05/10
 *
 ************************************************************)

open Exceptions
open ParsingStructure
open DiscreteType
open ParsedValue
open OCamlUtilities

(* Variable kind type represent a variable or a constant kind *)
type variable_kind =
    | Variable_kind of int
    | Constant_kind of AbstractValue.abstract_value

val variable_name_of_index : variable_infos -> variable_index -> variable_name
val index_of_variable_name : variable_infos -> variable_name -> variable_index
val index_of_variable_name_opt : variable_infos -> variable_name -> variable_index option
val value_of_constant_name : variable_infos -> variable_name -> AbstractValue.abstract_value

val is_variable_is_defined : variable_infos -> variable_name -> bool
val is_variable_removed : variable_infos -> variable_name -> bool
val is_variable_declared : variable_infos -> variable_name -> bool
val is_constant_is_defined : variable_infos -> variable_name -> bool
val is_variable_or_constant_defined : variable_infos -> variable_name -> bool
val is_variable_or_constant_declared : variable_infos -> variable_name -> bool

val var_type_of_variable_index : variable_infos -> variable_index -> var_type
val var_type_of_variable_name : variable_infos -> variable_name -> var_type
val var_type_of_variable_or_constant : variable_infos -> variable_name -> var_type
val var_type_of_variable_or_constant_opt : variable_infos -> variable_name -> var_type option
val discrete_type_of_variable_or_constant : variable_infos -> variable_name -> var_type_discrete
val discrete_type_of_variable_or_constant_opt : variable_infos -> variable_name -> var_type_discrete option

(* Know if variable with a given name is a variable or a constant *)
val variable_kind_of_variable_name : variable_infos -> variable_name -> variable_kind
(* Know if variable with a given name is a variable or a constant, if variable not found, return None *)
val variable_kind_of_variable_name_opt : variable_infos -> variable_name -> variable_kind option

(* Check if variable is a discrete variable given it's name *)
val is_discrete_variable : variable_infos -> variable_name -> bool

(* Check if variable is a clock *)
val is_clock : variable_infos -> variable_name -> bool


(* Get function metadata given it's name *)
val function_metadata_by_name : variable_infos -> variable_name -> function_metadata