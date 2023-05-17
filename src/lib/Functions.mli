(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: This module enable definition of customs IMITATOR functions
 * In order to define a new IMITATOR function, just add it's signature and tag to know if it is subject to side effects
 *
 * File contributors : Benjamin L.
 * Created           : 2021/11/20
 *
 ************************************************************)

(* Parsing structure modules *)
open ParsingStructure
open DiscreteType
open ParsedValue

(* Abstract modules *)
open FunctionSig

(* Get builtin functions meta *)
val builtin_functions_metadata : function_metadata list
(* Get builtin function metadata as a table *)
val builtin_functions_metadata_table : (string, function_metadata) Hashtbl.t

(* Get builtin function implementations *)
val builtin_function_bodies : AbstractModel.fun_definition list
(* Get builtin function implementations as a table *)
val builtin_function_bodies_table : (string, AbstractModel.fun_definition) Hashtbl.t

(* Remove the declarations of unused local variables from function body *)
(*val fun_def_without_unused_local_vars : (string * int) list -> parsed_fun_definition -> parsed_fun_definition*)

(* Compute metadata of a user function definition *)
val metadata_of_parsed_function_definition : functions_meta_table -> parsed_functions_table -> parsed_fun_definition -> function_metadata


(* Get arity of a function given it's name *)
val arity_of_function : variable_infos -> string -> int

