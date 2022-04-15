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

open DiscreteType
open DiscreteValue
open FunctionSig

val fun_definitions_table : (string, AbstractModel.fun_definition) Hashtbl.t ref

(* Get signature constraint of a function given it's name *)
val signature_constraint_of_function : string -> signature_constraint
(* Get if function is subject to side-effects *)
val is_function_subject_to_side_effect : string -> bool
(* Get arity of a function given it's name *)
val arity_of_function : string -> int
(* String representation of the function signature constraint *)
val string_of_function_signature_constraint : string -> string