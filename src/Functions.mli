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

open ParsingStructure
open DiscreteType
open DiscreteValue
open FunctionSig

(* Get builtin functions meta *)
val builtin_functions : function_metadata list
val fun_definitions_table : (string, AbstractModel.fun_definition) Hashtbl.t

(* Get function metadata of a given parsed function definition *)
val metadata_of_function_definition : parsed_fun_definition -> function_metadata

(* Get function metadata given it's name *)
val function_metadata_by_name : variable_infos -> variable_name -> function_metadata
(* Get arity of a function given it's name *)
val arity_of_function : variable_infos -> string -> int

