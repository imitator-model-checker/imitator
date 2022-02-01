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
 * Last modified     : 2022/02/01
 *
 ************************************************************)

open Exceptions
open DiscreteType
open DiscreteValue
open DiscreteExpressions
open FunctionSig

type side_effect_marker =
    | No_side_effect
    | Side_effect

(* binary(l) -> l -> binary(l) *)
let shift_signature =
    [
        Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_scalar_constraint "a")));
        Defined_type_constraint (Number_constraint (Defined_type_number_constraint (Int_constraint Int_type_constraint)));
        Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_scalar_constraint "a")))
    ]

(* binary(l) -> l':int -> binary(l + l') *)
let fill_signature =
    [
        Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_scalar_constraint "l1")));
        Defined_type_constraint (Number_constraint (Defined_type_number_constraint (Int_constraint (Int_name_constraint "l"))));
        Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_plus_constraint ("l", Length_constraint_expression (Length_scalar_constraint "l1")))))
    ]

(* binary(l) -> binary(l) *)
let unary_log_signature =
    [
        Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_scalar_constraint "a")));
        Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_scalar_constraint "a")))
    ]

(* binary(l) -> binary(l) -> binary(l) *)
let binary_log_signature =
    [
        Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_scalar_constraint "a")));
        Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_scalar_constraint "a")));
        Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_scalar_constraint "a")))
    ]

let function_by_name = function
    | "pow" ->
        [
            Defined_type_constraint (Number_constraint (Number_type_name_constraint "a"));
            Defined_type_constraint (Number_constraint (Defined_type_number_constraint (Int_constraint Int_type_constraint)));
            Defined_type_constraint (Number_constraint (Number_type_name_constraint "a"))
        ], No_side_effect
    | "rational_of_int" ->
        [Defined_type_constraint (Number_constraint (Defined_type_number_constraint (Int_constraint Int_type_constraint))); Defined_type_constraint (Number_constraint (Defined_type_number_constraint Rat_constraint))], No_side_effect
    | "shift_left" -> shift_signature, No_side_effect
    | "shift_right" -> shift_signature, No_side_effect
    | "fill_left" -> fill_signature, No_side_effect
    | "fill_right" -> fill_signature, No_side_effect
    | "logand" -> binary_log_signature, No_side_effect
    | "logor" -> binary_log_signature, No_side_effect
    | "logxor" -> binary_log_signature, No_side_effect
    | "lognot" -> unary_log_signature, No_side_effect
    | "array_append" ->
        [
            Defined_type_constraint (Array_constraint (Type_name_constraint "a", Length_constraint_expression (Length_scalar_constraint "l1")));
            Defined_type_constraint (Array_constraint (Type_name_constraint "a", Length_constraint_expression (Length_scalar_constraint "l2")));
            Defined_type_constraint (Array_constraint (Type_name_constraint "a", Length_constraint_expression (Length_plus_constraint ("l1", Length_constraint_expression (Length_scalar_constraint "l2")))));
        ], No_side_effect
    | "array_mem" ->
        [
            Type_name_constraint "a";
            Defined_type_constraint (Array_constraint (Type_name_constraint "a", Length_constraint_expression (Length_scalar_constraint "l")));
            Defined_type_constraint Bool_constraint
        ], No_side_effect
    | "array_length" ->
        [
            Defined_type_constraint (Array_constraint (Type_name_constraint "a", Length_constraint_expression (Length_scalar_constraint "l")));
            Defined_type_constraint (Number_constraint (Defined_type_number_constraint (Int_constraint Int_type_constraint)))
        ], No_side_effect
    | "list_cons" ->
        [
            Type_name_constraint "a";
            Defined_type_constraint (List_constraint (Type_name_constraint "a"));
            Defined_type_constraint (List_constraint (Type_name_constraint "a"))
        ], No_side_effect
    | "list_hd" ->
        [
            Defined_type_constraint (List_constraint (Type_name_constraint "a"));
            Type_name_constraint "a";
        ], No_side_effect
    | "list_tl" ->
        [
            Defined_type_constraint (List_constraint (Type_name_constraint "a"));
            Defined_type_constraint (List_constraint (Type_name_constraint "a"));
        ], No_side_effect
    | "list_rev" ->
        [
            Defined_type_constraint (List_constraint (Type_name_constraint "a"));
            Defined_type_constraint (List_constraint (Type_name_constraint "a"));
        ], No_side_effect
    | "list_mem" ->
        [
            Type_name_constraint "a";
            Defined_type_constraint (List_constraint (Type_name_constraint "a"));
            Defined_type_constraint Bool_constraint
        ], No_side_effect
    | "list_length" ->
        [
            Defined_type_constraint (List_constraint (Type_name_constraint "a"));
            Defined_type_constraint (Number_constraint (Defined_type_number_constraint (Int_constraint Int_type_constraint)))
        ], No_side_effect
    | "stack_push" ->
        [
            Type_name_constraint "a";
            Defined_type_constraint (Stack_constraint (Type_name_constraint "a"));
            Defined_type_constraint (Stack_constraint (Type_name_constraint "a"))
        ], Side_effect
    | "stack_pop" ->
        [
            Defined_type_constraint (Stack_constraint (Type_name_constraint "a"));
            Type_name_constraint "a"
        ], Side_effect
    | "stack_top" ->
        [
            Defined_type_constraint (Stack_constraint (Type_name_constraint "a"));
            Type_name_constraint "a"
        ], No_side_effect
    | "stack_clear" ->
        [
            Defined_type_constraint (Stack_constraint (Type_name_constraint "a"));
            Defined_type_constraint (Stack_constraint (Type_name_constraint "a"))
        ], Side_effect
    | "stack_is_empty" ->
        [
            Defined_type_constraint (Stack_constraint (Type_name_constraint "a"));
            Defined_type_constraint Bool_constraint
        ], No_side_effect
    | "stack_length" ->
        [
            Defined_type_constraint (Stack_constraint (Type_name_constraint "a"));
            Defined_type_constraint (Number_constraint (Defined_type_number_constraint (Int_constraint Int_type_constraint)))
        ], No_side_effect
    | "queue_push" ->
        [
            Type_name_constraint "a";
            Defined_type_constraint (Queue_constraint (Type_name_constraint "a"));
            Defined_type_constraint (Queue_constraint (Type_name_constraint "a"))
        ], Side_effect
    | "queue_pop" ->
        [
            Defined_type_constraint (Queue_constraint (Type_name_constraint "a"));
            Type_name_constraint "a"
        ], Side_effect
    | "queue_top" ->
        [
            Defined_type_constraint (Queue_constraint (Type_name_constraint "a"));
            Type_name_constraint "a"
        ], No_side_effect
    | "queue_clear" ->
        [
            Defined_type_constraint (Queue_constraint (Type_name_constraint "a"));
            Defined_type_constraint (Queue_constraint (Type_name_constraint "a"))
        ], Side_effect
    | "queue_is_empty" ->
        [
            Defined_type_constraint (Queue_constraint (Type_name_constraint "a"));
            Defined_type_constraint Bool_constraint
        ], No_side_effect
    | "queue_length" ->
        [
            Defined_type_constraint (Queue_constraint (Type_name_constraint "a"));
            Defined_type_constraint (Number_constraint (Defined_type_number_constraint (Int_constraint Int_type_constraint)))
        ], No_side_effect

    | "fake" ->
        [
            Defined_type_constraint (List_constraint (Defined_type_constraint (List_constraint (Type_name_constraint "a"))));
            Defined_type_constraint (List_constraint (Defined_type_constraint (List_constraint (Type_name_constraint "a"))));
        ], No_side_effect
    | function_name -> raise (UndefinedFunction function_name)

(* Get signature constraint of a function given it's name *)
let signature_constraint_of_function function_name =
    let signature, _ = function_by_name function_name in signature

(* Get if function is subject to side-effects *)
let is_function_subject_to_side_effect function_name =
    let _, side_effect_marker = function_by_name function_name in
    match side_effect_marker with
    | No_side_effect -> false
    | Side_effect -> true

(* Get arity of a function given it's name *)
let arity_of_function function_name =
    (List.length (signature_constraint_of_function function_name)) - 1

(* String representation of the function signature constraint *)
let string_of_function_signature_constraint function_name =
     string_of_signature_constraint (signature_constraint_of_function function_name)