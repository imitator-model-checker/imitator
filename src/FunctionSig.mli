(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: This module enable definition of customs IMITATOR functions signature_constraint template
 * For example : list_cons : 'a -> 'a list -> 'a list
 * Theses templates can after be resolved according to the type of arguments of the function call
 * For example : list_cons(r, lr) would be resolved as rat -> rat list -> rat list for r : rat.
 *
 * File contributors : Benjamin L.
 * Created           : 2021/11/20
 * Last modified     : 2021/11/20
 *
 ************************************************************)

open ParsingStructure
open DiscreteType
open DiscreteValue

type constraint_name = string

type length_constraint =
    | Length_constraint of int
    | Length_constraint_expression of length_constraint_expression

and length_constraint_expression =
    | Length_scalar_constraint of constraint_name
    | Length_plus_constraint of constraint_name * length_constraint

type int_type_constraint =
    | Int_type_constraint
    | Int_name_constraint of constraint_name (* Dependent type *)

type defined_type_number_constraint =
    | Int_constraint of int_type_constraint
    | Rat_constraint

type type_number_constraint =
    | Defined_type_number_constraint of defined_type_number_constraint
    | Number_type_name_constraint of constraint_name

type type_constraint =
    | Defined_type_constraint of defined_type_constraint
    | Type_name_constraint of constraint_name

and defined_type_constraint =
    | Number_constraint of type_number_constraint
    | Bool_constraint
    | Binary_constraint of length_constraint
    | Array_constraint of type_constraint * length_constraint
    | List_constraint of type_constraint

(* Signature constraint is a list of type constraint *)
type signature_constraint = type_constraint list
(* Signature is a list of discrete type *)
type signature = var_type_discrete list

(*val signature_constraint_of_function : string -> var_type_discrete list*)


val string_of_signature : signature -> string
val string_of_signature_constraint : signature_constraint -> string

(*val constraint_signature : var_type_discrete -> var_type_discrete list -> var_type_discrete list*)

(*val signature_constraint_of_signature : signature -> signature_constraint*)




val split_signature : 'a list -> 'a list * 'a

val is_signature_compatible_with_signature_constraint : signature -> signature_constraint -> bool