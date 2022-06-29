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
open ParsingStructureUtilities
open OCamlUtilities

(* Shortcuts to hash table types *)
type fun_metadata_table = (string, function_metadata) Hashtbl.t
type fun_definitions_table = (string, parsed_fun_definition) Hashtbl.t

(* Infer whether a user function is subject to side effects *)
let rec is_function_has_side_effects builtin_functions_metadata_table user_function_definitions_table (fun_def : parsed_fun_definition) =

    (* Check if a tree leaf has side effect *)
    let is_leaf_has_side_effects = function
        | Leaf_fun function_name ->
            (* Is call found is a call to a builtin function ? *)
            if Hashtbl.mem builtin_functions_metadata_table function_name then (
                let function_metadata = Hashtbl.find builtin_functions_metadata_table function_name in
                function_metadata.side_effect
            )
            (* Is call found is a call to a user function ? *)
            else if Hashtbl.mem user_function_definitions_table function_name then (
                let found_function_def = Hashtbl.find user_function_definitions_table function_name in
                is_function_has_side_effects builtin_functions_metadata_table user_function_definitions_table found_function_def
            )
            else
                raise (UndefinedFunction fun_def.name);

        | _ -> false
    in
    (* Loop into function body - OK *)
    (* For each expression in body : *)
    (* check for leaf_fun in init_expr, expr *)
    (* Search fun in builtin, if found get side_effect property *)
    (* If not found search into user_function_def, and call recursively this function *)
    (* if no function found -> undefined function *)
    let rec is_next_expr_has_side_effects = function
        | Parsed_fun_local_decl (_, _, init_expr, next_expr, _) ->
            (* Check if init expression has side-effects *)
            let has_init_expr_side_effects = ParsingStructureUtilities.exists_in_parsed_global_expression is_leaf_has_side_effects init_expr in
            (* Check if next expressions has side-effects *)
            let has_next_expr_side_effects = is_next_expr_has_side_effects next_expr in
            (* Check if any has side-effects *)
            has_init_expr_side_effects || has_next_expr_side_effects

        | Parsed_fun_instruction ((parsed_update_type, update_expr), next_expr) ->
            (match parsed_update_type with
            (* When any variable is assigned (written) the function is subject to side-effects *)
            | Parsed_variable_update _ -> true
            (* If the instruction is not an assignment *)
            | Parsed_void_update ->
                (* Check if the update expression has side-effects *)
                let has_update_expr_side_effects = ParsingStructureUtilities.exists_in_parsed_global_expression is_leaf_has_side_effects update_expr in
                (* Check if next expressions has side-effects *)
                let has_next_expr_side_effects = is_next_expr_has_side_effects next_expr in
                (* Check if any has side-effects *)
                has_update_expr_side_effects || has_next_expr_side_effects
            )

        | Parsed_fun_expr expr ->
            (* Check if expression has side-effects *)
            ParsingStructureUtilities.exists_in_parsed_global_expression is_leaf_has_side_effects expr
    in
    is_next_expr_has_side_effects fun_def.body

(* Remove the declarations of unused local variables from function body *)
let fun_def_without_unused_local_vars unused_local_vars (fun_def : parsed_fun_definition) =

    let rec next_expr_without_unused = function
        | Parsed_fun_local_decl (variable_name, discrete_type, init_expr, next_expr, id) ->
            (* If current declaration is found in unused local variable, just remove by skipping *)
            if List.mem (variable_name, id) unused_local_vars then
                next_expr_without_unused next_expr
            (* Else,  *)
            else (
                let new_next_expr_without_unused = next_expr_without_unused next_expr in
                Parsed_fun_local_decl (variable_name, discrete_type, init_expr, new_next_expr_without_unused, id)
            )

        | Parsed_fun_instruction (normal_update, next_expr) ->
            let new_next_expr_without_unused = next_expr_without_unused next_expr in
            Parsed_fun_instruction (normal_update, new_next_expr_without_unused)

        | Parsed_fun_expr _ as expr -> expr

    in
    { fun_def with body = next_expr_without_unused fun_def.body }

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

(* Table of converted functions *)
let fun_definitions_table : (string, AbstractModel.fun_definition) Hashtbl.t = Hashtbl.create 0

(* List of builtin functions metadata *)
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

(* Compute metadata of a user defined function definition *)
let metadata_of_function_definition builtin_functions_metadata_table user_function_definitions_table (fun_def : parsed_fun_definition) =
    (* Concat parameters type and return type *)
    let signature = List.map second_of_tuple fun_def.parameters @ [fun_def.return_type] in
    {
        name = fun_def.name;
        signature_constraint = FunctionSig.signature_constraint_of_signature signature;
        side_effect = is_function_has_side_effects builtin_functions_metadata_table user_function_definitions_table fun_def;
    }

(* Get function meta given it's name, raise an error if the function doesn't exists *)
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