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

open Exceptions
open DiscreteType
open DiscreteValue
open DiscreteExpressions
open FunctionSig
open ParsingStructure
open OCamlUtilities

type side_effect_marker =
    | No_side_effect
    | Side_effect

(* binary(l) -> l -> binary(l) *)
let shift_signature2 =
    [
        ("a", Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_scalar_constraint "a"))));
        ("b", Defined_type_constraint (Number_constraint (Defined_type_number_constraint (Int_constraint Int_type_constraint))));
        ("c", Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_scalar_constraint "a"))))
    ]

let shift_signature =
    [
        Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_scalar_constraint "a")));
        Defined_type_constraint (Number_constraint (Defined_type_number_constraint (Int_constraint Int_type_constraint)));
        Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_scalar_constraint "a")))
    ]

(* binary(l) -> l':int -> binary(l + l') *)
let fill_signature2 =
    [
        ("a", Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_scalar_constraint "l1"))));
        ("b", Defined_type_constraint (Number_constraint (Defined_type_number_constraint (Int_constraint (Int_name_constraint "l")))));
        ("c", Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_plus_constraint ("l", Length_constraint_expression (Length_scalar_constraint "l1"))))))
    ]

let fill_signature =
    [
        Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_scalar_constraint "l1")));
        Defined_type_constraint (Number_constraint (Defined_type_number_constraint (Int_constraint (Int_name_constraint "l"))));
        Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_plus_constraint ("l", Length_constraint_expression (Length_scalar_constraint "l1")))))
    ]

(* binary(l) -> binary(l) *)
let unary_log_signature2 =
    [
        ("a", Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_scalar_constraint "a"))));
        ("b", Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_scalar_constraint "a"))))
    ]

let unary_log_signature =
    [
        Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_scalar_constraint "a")));
        Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_scalar_constraint "a")))
    ]

(* binary(l) -> binary(l) -> binary(l) *)
let binary_log_signature2 =
    [
        ("a", Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_scalar_constraint "a"))));
        ("b", Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_scalar_constraint "a"))));
        ("c", Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_scalar_constraint "a"))))
    ]

let binary_log_signature =
    [
        Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_scalar_constraint "a")));
        Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_scalar_constraint "a")));
        Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_scalar_constraint "a")))
    ]

let fun_definitions_table : (string, AbstractModel.fun_definition) Hashtbl.t = Hashtbl.create 0

let builtin_functions : ParsingStructure.function_metadata list =
    [
        {
            name = "pow";
            signature_constraint = [
                Defined_type_constraint (Number_constraint (Number_type_name_constraint "a"));
                Defined_type_constraint (Number_constraint (Defined_type_number_constraint (Int_constraint Int_type_constraint)));
                Defined_type_constraint (Number_constraint (Number_type_name_constraint "a"))
            ];
            side_effect = false
        };
        {
            name = "rational_of_int";
            signature_constraint = [
                Defined_type_constraint (Number_constraint (Defined_type_number_constraint (Int_constraint Int_type_constraint)));
                Defined_type_constraint (Number_constraint (Defined_type_number_constraint Rat_constraint))
            ];
            side_effect = false
        };
        {
            name = "shift_left";
            signature_constraint = shift_signature;
            side_effect = false
        };
        {
            name = "shift_right";
            signature_constraint = shift_signature;
            side_effect = false
        };
        {
            name = "fill_left";
            signature_constraint = fill_signature;
            side_effect = false
        };
        {
            name = "fill_right";
            signature_constraint = fill_signature;
            side_effect = false
        };
        {
            name = "logand";
            signature_constraint = binary_log_signature;
            side_effect = false
        };
        {
            name = "logor";
            signature_constraint = binary_log_signature;
            side_effect = false
        };
        {
            name = "logxor";
            signature_constraint = binary_log_signature;
            side_effect = false
        };
        {
            name = "lognot";
            signature_constraint = unary_log_signature;
            side_effect = false
        };
        {
            name = "array_append";
            signature_constraint = [
                Defined_type_constraint (Array_constraint (Type_name_constraint "a", Length_constraint_expression (Length_scalar_constraint "l1")));
                Defined_type_constraint (Array_constraint (Type_name_constraint "a", Length_constraint_expression (Length_scalar_constraint "l2")));
                Defined_type_constraint (Array_constraint (Type_name_constraint "a", Length_constraint_expression (Length_plus_constraint ("l1", Length_constraint_expression (Length_scalar_constraint "l2")))));
            ];
            side_effect = false
        };
        {
            name = "array_mem";
            signature_constraint = [
                Type_name_constraint "a";
                Defined_type_constraint (Array_constraint (Type_name_constraint "a", Length_constraint_expression (Length_scalar_constraint "l")));
                Defined_type_constraint Bool_constraint
            ];
            side_effect = false
        };
        {
            name = "list_is_empty";
            signature_constraint = [
                Defined_type_constraint (List_constraint (Type_name_constraint "a"));
                Defined_type_constraint Bool_constraint
            ];
            side_effect = false
        };
        {
            name = "array_length";
            signature_constraint = [
                Defined_type_constraint (Array_constraint (Type_name_constraint "a", Length_constraint_expression (Length_scalar_constraint "l")));
                Defined_type_constraint (Number_constraint (Defined_type_number_constraint (Int_constraint Int_type_constraint)))
           ];
            side_effect = false
        };
        {
            name = "list_cons";
            signature_constraint = [
                Type_name_constraint "a";
                Defined_type_constraint (List_constraint (Type_name_constraint "a"));
                Defined_type_constraint (List_constraint (Type_name_constraint "a"))
            ];
            side_effect = false
        };
        {
            name = "list_hd";
            signature_constraint = [
                Defined_type_constraint (List_constraint (Type_name_constraint "a"));
                Type_name_constraint "a";
            ];
            side_effect = false
        };
        {
            name = "list_tl";
            signature_constraint = [
                Defined_type_constraint (List_constraint (Type_name_constraint "a"));
                Defined_type_constraint (List_constraint (Type_name_constraint "a"));
            ];
            side_effect = false
        };
        {
            name = "list_rev";
            signature_constraint = [
                Defined_type_constraint (List_constraint (Type_name_constraint "a"));
                Defined_type_constraint (List_constraint (Type_name_constraint "a"));
            ];
            side_effect = false
        };
        {
            name = "list_mem";
            signature_constraint = [
                Type_name_constraint "a";
                Defined_type_constraint (List_constraint (Type_name_constraint "a"));
                Defined_type_constraint Bool_constraint
            ];
            side_effect = false
        };
        {
            name = "list_length";
            signature_constraint = [
                Defined_type_constraint (List_constraint (Type_name_constraint "a"));
                Defined_type_constraint (Number_constraint (Defined_type_number_constraint (Int_constraint Int_type_constraint)))
            ];
            side_effect = false
        };
        {
            name = "stack_push";
            signature_constraint = [
                Type_name_constraint "a";
                Defined_type_constraint (Stack_constraint (Type_name_constraint "a"));
                Defined_type_constraint (Stack_constraint (Type_name_constraint "a"))
            ];
            side_effect = true
        };
        {
            name = "stack_pop";
            signature_constraint = [
                Defined_type_constraint (Stack_constraint (Type_name_constraint "a"));
                Type_name_constraint "a"
            ];
            side_effect = true
        };
        {
            name = "stack_top";
            signature_constraint = [
                Defined_type_constraint (Stack_constraint (Type_name_constraint "a"));
                Type_name_constraint "a"
            ];
            side_effect = false
        };
        {
            name = "stack_clear";
            signature_constraint = [
                Defined_type_constraint (Stack_constraint (Type_name_constraint "a"));
                Defined_type_constraint (Stack_constraint (Type_name_constraint "a"))
            ];
            side_effect = true
        };
        {
            name = "stack_is_empty";
            signature_constraint = [
                Defined_type_constraint (Stack_constraint (Type_name_constraint "a"));
                Defined_type_constraint Bool_constraint
            ];
            side_effect = false
        };
        {
            name = "stack_length";
            signature_constraint = [
                Defined_type_constraint (Stack_constraint (Type_name_constraint "a"));
                Defined_type_constraint (Number_constraint (Defined_type_number_constraint (Int_constraint Int_type_constraint)))
            ];
            side_effect = false
        };
        {
            name = "queue_push";
            signature_constraint = [
                Type_name_constraint "a";
                Defined_type_constraint (Queue_constraint (Type_name_constraint "a"));
                Defined_type_constraint (Queue_constraint (Type_name_constraint "a"))
            ];
            side_effect = true
        };
        {
            name = "queue_pop";
            signature_constraint = [
                Defined_type_constraint (Queue_constraint (Type_name_constraint "a"));
                Type_name_constraint "a"
            ];
            side_effect = true
        };
        {
            name = "queue_top";
            signature_constraint = [
                Defined_type_constraint (Queue_constraint (Type_name_constraint "a"));
                Type_name_constraint "a"
            ];
            side_effect = false
        };
        {
            name = "queue_clear";
            signature_constraint = [
                Defined_type_constraint (Queue_constraint (Type_name_constraint "a"));
                Defined_type_constraint (Queue_constraint (Type_name_constraint "a"))
            ];
            side_effect = true
        };
        {
            name = "queue_is_empty";
            signature_constraint = [
                Defined_type_constraint (Queue_constraint (Type_name_constraint "a"));
                Defined_type_constraint Bool_constraint
            ];
            side_effect = false
        };
        {
            name = "queue_length";
            signature_constraint = [
                Defined_type_constraint (Queue_constraint (Type_name_constraint "a"));
                Defined_type_constraint (Number_constraint (Defined_type_number_constraint (Int_constraint Int_type_constraint)))
            ];
            side_effect = false
        };
        {
            name = "fake";
            signature_constraint = [
                Defined_type_constraint (List_constraint (Defined_type_constraint (List_constraint (Type_name_constraint "a"))));
                Defined_type_constraint (List_constraint (Defined_type_constraint (List_constraint (Type_name_constraint "a"))));
            ];
            side_effect = false
        };
    ]

let metadata_of_function_definition (fun_def : parsed_fun_definition) =
    (* Concat parameters type and return type *)
    let signature = List.map second_of_tuple fun_def.parameters @ [fun_def.return_type] in
    {
        name = fun_def.name;
        signature_constraint = FunctionSig.signature_constraint_of_signature signature;
        side_effect = false; (* TODO benjamin IMPLEMENT check if body contain side effect expr *)
    }

let function_metadata_by_name (variable_infos : variable_infos) function_name =
    let fun_definition_opt = Hashtbl.find_opt variable_infos.functions function_name in
    match fun_definition_opt with
    | Some fun_definition -> fun_definition
    | None ->
        raise (UndefinedFunction function_name)

(* Get arity of a function given it's name *)
let arity_of_function variable_infos function_name =
    let function_metadata = function_metadata_by_name variable_infos function_name in
    (List.length (function_metadata.signature_constraint)) - 1