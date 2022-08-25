(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: This module enable definition of customs IMITATOR functions signature template
 * For example : list_cons : 'a -> 'a list -> 'a list
 * Theses templates can after be resolved according to the type of arguments of the function call
 * For example : list_cons(r, lr) would be resolved as rat -> rat list -> rat list for r : rat.
 *
 * File contributors : Benjamin L.
 * Created           : 2021/11/20
 *
 ************************************************************)

(* Utils modules *)
open Exceptions

(* Parsing structure modules *)
open DiscreteType
open ParsedValue

(* Abstract modules *)
open DiscreteExpressions

(* Name of constraint *)
type constraint_name = string

(* Length constraint, like array length, binary word length *)
type length_constraint =
    | Length_constraint of int
    | Length_constraint_expression of length_constraint_expression

(* Length constraint expression *)
and length_constraint_expression =
    | Length_scalar_constraint of constraint_name
    | Length_plus_constraint of constraint_name * length_constraint

(* Constraint on int type *)
type int_type_constraint =
    | Int_type_constraint
    | Int_name_constraint of constraint_name (* Dependent type *)

(* Constraint on concrete number type *)
type defined_type_number_constraint =
    | Int_constraint of int_type_constraint
    | Rat_constraint

(* Constraint on number type *)
type type_number_constraint =
    | Defined_type_number_constraint of defined_type_number_constraint
    | Number_type_name_constraint of constraint_name

(* Constraint on any type *)
type type_constraint =
    | Defined_type_constraint of defined_type_constraint
    | Type_name_constraint of constraint_name

(* Constraint on concrete type *)
and defined_type_constraint =
    | Void_constraint
    | Number_constraint of type_number_constraint
    | Bool_constraint
    | Binary_constraint of length_constraint
    | Array_constraint of type_constraint * length_constraint
    | List_constraint of type_constraint
    | Stack_constraint of type_constraint
    | Queue_constraint of type_constraint

(* Signature constraint is a list of type constraint *)
type signature_constraint = type_constraint list
(* Signature is a list of discrete type *)
type signature = var_type_discrete list

(** -------------------- **)
(** Utils **)
(** -------------------- **)

(* Split signature into signature of parameters and signature of return type *)
let split_signature =
    OCamlUtilities.list_split_last

(** -------------------- **)
(** Strings **)
(** -------------------- **)

let rec string_of_length_constraint = function
    | Length_constraint_expression length_constraint_expr -> string_of_length_constraint_expression length_constraint_expr
    | Length_constraint length -> string_of_int length

and string_of_length_constraint_expression = function
    | Length_scalar_constraint constraint_name -> constraint_name
    | Length_plus_constraint (constraint_name, length_constraint) -> constraint_name ^ " + " ^ string_of_length_constraint length_constraint

let string_of_int_type_constraint = function
    | Int_type_constraint -> "int"
    | Int_name_constraint constraint_name -> constraint_name ^ ":int"

let string_of_type_number = function
    | Int_constraint int_constraint -> string_of_int_type_constraint int_constraint
    | Rat_constraint -> "rat"

let string_of_type_number_constraint = function
    | Number_type_name_constraint constraint_name -> "'" ^ constraint_name ^ " number"
    | Defined_type_number_constraint defined_type_constraint_number -> string_of_type_number defined_type_constraint_number

let rec string_of_defined_type_constraint = function
    | Void_constraint ->
        "void"
    | Number_constraint type_number_constraint ->
        string_of_type_number_constraint type_number_constraint
    | Bool_constraint ->
        "bool"
    | Binary_constraint length_constraint ->
        "binary(" ^ string_of_length_constraint length_constraint ^ ")"
    | Array_constraint (type_constraint, length_constraint) ->
        string_of_type_constraint type_constraint ^ " array(" ^ string_of_length_constraint length_constraint ^ ")"
    | List_constraint type_constraint ->
        string_of_type_constraint type_constraint ^ " list"
    | Stack_constraint type_constraint ->
        string_of_type_constraint type_constraint ^ " stack"
    | Queue_constraint type_constraint ->
        string_of_type_constraint type_constraint ^ " queue"

and string_of_type_constraint = function
    | Type_name_constraint constraint_name -> "'" ^ constraint_name
    | Defined_type_constraint defined_type_constraint -> string_of_defined_type_constraint defined_type_constraint

(* String representation of a signature constraint *)
let string_of_signature_constraint signature_constraint =
    let str_type_constraints = List.map string_of_type_constraint signature_constraint in
    let str_param_type_constraints, str_return_type_constraint = split_signature str_type_constraints in
    let l_par_del, r_par_del = Constants.default_paren_delimiter in
    l_par_del ^ OCamlUtilities.string_of_list_of_string_with_sep ", " str_param_type_constraints ^ r_par_del ^ " : " ^ str_return_type_constraint

(* String representation of a signature *)
let string_of_signature signature =
    let str_signature_types_list = List.map (DiscreteType.string_of_var_type_discrete) signature in
    let str_param_types, str_return_type = split_signature str_signature_types_list in
    let l_par_del, r_par_del = Constants.default_paren_delimiter in
    l_par_del ^ OCamlUtilities.string_of_list_of_string_with_sep ", " str_param_types ^ r_par_del ^ " : " ^ str_return_type

(** -------------------- **)
(** Compatibility **)
(** -------------------- **)

let rec is_discrete_type_compatible_with_length_constraint length = function
    | Length_constraint length_value -> length = length_value
    (* In theory a length constraint expression can be incompatible with some length *)
    (* but may be compatible too, eg : 1 + 1 with 2, so we consider as compatible anyway *)
    | Length_constraint_expression _ -> true

let is_discrete_type_compatible_with_defined_type_number_constraint discrete_number_type defined_type_number_constraint =
    match discrete_number_type, defined_type_number_constraint with
    | Var_type_discrete_int, Int_constraint _
    | Var_type_discrete_rat, Rat_constraint
    | Var_type_discrete_weak_number, _ -> true
    | _ -> false

let is_discrete_type_compatible_with_type_number_constraint discrete_number_type = function
    | Number_type_name_constraint _ -> true
    | Defined_type_number_constraint defined_type_number_constraint ->
        is_discrete_type_compatible_with_defined_type_number_constraint discrete_number_type defined_type_number_constraint

let rec is_discrete_type_compatible_with_type_constraint discrete_type = function
    | Type_name_constraint _ -> true
    | Defined_type_constraint defined_type_constraint ->
        is_discrete_type_compatible_with_defined_type_constraint discrete_type defined_type_constraint

and is_discrete_type_compatible_with_defined_type_constraint discrete_type defined_type_constraint =
    match discrete_type, defined_type_constraint with
    | Var_type_void, Void_constraint -> true
    | Var_type_discrete_number discrete_number_type, Number_constraint type_number_constraint ->
        is_discrete_type_compatible_with_type_number_constraint discrete_number_type type_number_constraint
    | Var_type_discrete_bool, Bool_constraint -> true
    | Var_type_discrete_binary_word length, Binary_constraint length_constraint ->
        is_discrete_type_compatible_with_length_constraint length length_constraint
    | Var_type_discrete_array (inner_type, length), Array_constraint (type_constraint, length_constraint) ->
        is_discrete_type_compatible_with_type_constraint inner_type type_constraint
        && is_discrete_type_compatible_with_length_constraint length length_constraint
    | Var_type_discrete_list inner_type, List_constraint type_constraint
    | Var_type_discrete_stack inner_type, Stack_constraint type_constraint
    | Var_type_discrete_queue inner_type, Queue_constraint type_constraint ->
        is_discrete_type_compatible_with_type_constraint inner_type type_constraint
    | _ -> false

(* Check whether a signature is compatible with a signature constraint (for type checking) *)
let is_signature_compatible_with_signature_constraint signature signature_constraint =
    let signature_with_signature_constraint = List.combine signature signature_constraint in
    List.for_all (fun (s_type, sc_type) -> is_discrete_type_compatible_with_type_constraint s_type sc_type) signature_with_signature_constraint

(** -------------------- **)
(** Conversion **)
(** -------------------- **)

let type_number_constraint_of_discrete_number_type = function
    | Var_type_discrete_rat -> Rat_constraint
    | Var_type_discrete_int -> Int_constraint Int_type_constraint
    (* Note benjamin: a case that never happen shouldn't appear, due to a bad modeling, to fix *)
    | Var_type_discrete_weak_number -> raise (InternalError "should never happen")

let type_constraint_of_discrete_type discrete_type =
    let rec type_constraint_of_discrete_type_rec = function
        | Var_type_void -> Void_constraint
        | Var_type_discrete_number discrete_number_type ->
            let type_number_constraint = type_number_constraint_of_discrete_number_type discrete_number_type in
            Number_constraint (Defined_type_number_constraint type_number_constraint)
        | Var_type_discrete_bool -> Bool_constraint
        | Var_type_discrete_binary_word length -> Binary_constraint (Length_constraint length)
        | Var_type_discrete_array (inner_type, length) ->
            let inner_type_constraint = Defined_type_constraint (type_constraint_of_discrete_type_rec inner_type) in
            Array_constraint (inner_type_constraint, Length_constraint length)
        | Var_type_discrete_list inner_type ->
            let inner_type_constraint = Defined_type_constraint (type_constraint_of_discrete_type_rec inner_type) in
            List_constraint inner_type_constraint
        | Var_type_discrete_stack inner_type ->
            let inner_type_constraint = Defined_type_constraint (type_constraint_of_discrete_type_rec inner_type) in
            Stack_constraint inner_type_constraint
        | Var_type_discrete_queue inner_type ->
            let inner_type_constraint = Defined_type_constraint (type_constraint_of_discrete_type_rec inner_type) in
            Queue_constraint inner_type_constraint
        (* Note benjamin: a case that never happen shouldn't appear, due to a bad modeling, to fix *)
        | Var_type_weak -> raise (InternalError "should never happen")
    in
    Defined_type_constraint (type_constraint_of_discrete_type_rec discrete_type)

(* Get signature constraint of signature *)
let signature_constraint_of_signature = List.map type_constraint_of_discrete_type

