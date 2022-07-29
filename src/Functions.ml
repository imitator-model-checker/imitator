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

(* Utils modules *)
open Exceptions
open OCamlUtilities

(* Parsing structure modules *)
open DiscreteType
open DiscreteValue
open ParsingStructure
open ParsingStructureUtilities

(* Abstract model modules *)
open DiscreteExpressions
open FunctionSig



(* Shortcuts to hash table types *)
type functions_meta_table = (string, function_metadata) Hashtbl.t
type parsed_functions_table = (string, parsed_fun_definition) Hashtbl.t

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

    (* Return body of function without unused expression *)
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

(* Extract metadata from abstract model function definition *)
let metadata_of_function_definition (fun_def : AbstractModel.fun_definition) : ParsingStructure.function_metadata =
    {
        name = fun_def.name;
        parameter_names = fun_def.parameter_names;
        signature_constraint = fun_def.signature_constraint;
        side_effect = fun_def.side_effect;
    }

(* Get builtin function implementations *)
let builtin_function_bodies : AbstractModel.fun_definition list =
    [
        {
            name = "pow";
            parameter_names = ["x"; "exp"];
            signature_constraint = [
                Defined_type_constraint (Number_constraint (Number_type_name_constraint "a"));
                Defined_type_constraint (Number_constraint (Defined_type_number_constraint (Int_constraint Int_type_constraint)));
                Defined_type_constraint (Number_constraint (Number_type_name_constraint "a"))
            ];
            body = Fun_builtin DiscreteExpressionEvaluator.eval_pow;
            side_effect = false;
        };
        {
            name = "rational_of_int";
            parameter_names = ["r"];
            signature_constraint = [
                Defined_type_constraint (Number_constraint (Defined_type_number_constraint (Int_constraint Int_type_constraint)));
                Defined_type_constraint (Number_constraint (Defined_type_number_constraint Rat_constraint))
            ];
            body = Fun_builtin DiscreteExpressionEvaluator.eval_rational_of_int;
            side_effect = false
        };
        {
            name = "shift_left";
            parameter_names = ["b"; "i"];
            signature_constraint = shift_signature;
            body = Fun_builtin DiscreteExpressionEvaluator.eval_shift_left;
            side_effect = false
        };
        {
            name = "shift_right";
            parameter_names = ["b"; "i"];
            signature_constraint = shift_signature;
            body = Fun_builtin DiscreteExpressionEvaluator.eval_shift_right;
            side_effect = false
        };
        {
            name = "fill_left";
            parameter_names = ["b"; "i"];
            signature_constraint = fill_signature;
            body = Fun_builtin DiscreteExpressionEvaluator.eval_fill_left;
            side_effect = false
        };
        {
            name = "fill_right";
            parameter_names = ["b"; "i"];
            signature_constraint = fill_signature;
            body = Fun_builtin DiscreteExpressionEvaluator.eval_fill_right;
            side_effect = false
        };
        {
            name = "logand";
            parameter_names = ["b1"; "b2"];
            signature_constraint = binary_log_signature;
            body = Fun_builtin DiscreteExpressionEvaluator.eval_log_and;
            side_effect = false
        };
        {
            name = "logor";
            parameter_names = ["b1"; "b2"];
            signature_constraint = binary_log_signature;
            body = Fun_builtin DiscreteExpressionEvaluator.eval_log_or;
            side_effect = false
        };
        {
            name = "logxor";
            parameter_names = ["b1"; "b2"];
            signature_constraint = binary_log_signature;
            body = Fun_builtin DiscreteExpressionEvaluator.eval_log_xor;
            side_effect = false
        };
        {
            name = "lognot";
            parameter_names = ["b1"];
            signature_constraint = unary_log_signature;
            body = Fun_builtin DiscreteExpressionEvaluator.eval_log_not;
            side_effect = false
        };
        {
            name = "array_append";
            parameter_names = ["a1"; "a2"];
            signature_constraint = [
                Defined_type_constraint (Array_constraint (Type_name_constraint "a", Length_constraint_expression (Length_scalar_constraint "l1")));
                Defined_type_constraint (Array_constraint (Type_name_constraint "a", Length_constraint_expression (Length_scalar_constraint "l2")));
                Defined_type_constraint (Array_constraint (Type_name_constraint "a", Length_constraint_expression (Length_plus_constraint ("l1", Length_constraint_expression (Length_scalar_constraint "l2")))));
            ];
            body = Fun_builtin DiscreteExpressionEvaluator.eval_array_append;
            side_effect = false
        };
        {
            name = "array_mem";
            parameter_names = ["a"; "e"];
            signature_constraint = [
                Type_name_constraint "a";
                Defined_type_constraint (Array_constraint (Type_name_constraint "a", Length_constraint_expression (Length_scalar_constraint "l")));
                Defined_type_constraint Bool_constraint
            ];
            body = Fun_builtin DiscreteExpressionEvaluator.eval_array_mem;
            side_effect = false
        };
        {
            name = "array_length";
            parameter_names = ["a"];
            signature_constraint = [
                Defined_type_constraint (Array_constraint (Type_name_constraint "a", Length_constraint_expression (Length_scalar_constraint "l")));
                Defined_type_constraint (Number_constraint (Defined_type_number_constraint (Int_constraint Int_type_constraint)))
           ];
            body = Fun_builtin DiscreteExpressionEvaluator.eval_array_length;
            side_effect = false
        };
        {
            name = "list_is_empty";
            parameter_names = ["l"];
            signature_constraint = [
                Defined_type_constraint (List_constraint (Type_name_constraint "a"));
                Defined_type_constraint Bool_constraint
            ];
            body = Fun_builtin DiscreteExpressionEvaluator.eval_list_is_empty;
            side_effect = false
        };
        {
            name = "list_cons";
            parameter_names = ["l"; "e"];
            signature_constraint = [
                Type_name_constraint "a";
                Defined_type_constraint (List_constraint (Type_name_constraint "a"));
                Defined_type_constraint (List_constraint (Type_name_constraint "a"))
            ];
            body = Fun_builtin DiscreteExpressionEvaluator.eval_list_cons;
            side_effect = false
        };
        {
            name = "list_hd";
            parameter_names = ["l"];
            signature_constraint = [
                Defined_type_constraint (List_constraint (Type_name_constraint "a"));
                Type_name_constraint "a";
            ];
            body = Fun_builtin DiscreteExpressionEvaluator.eval_list_hd;
            side_effect = false
        };
        {
            name = "list_tl";
            parameter_names = ["l"];
            signature_constraint = [
                Defined_type_constraint (List_constraint (Type_name_constraint "a"));
                Defined_type_constraint (List_constraint (Type_name_constraint "a"));
            ];
            body = Fun_builtin DiscreteExpressionEvaluator.eval_list_tl;
            side_effect = false
        };
        {
            name = "list_rev";
            parameter_names = ["l"];
            signature_constraint = [
                Defined_type_constraint (List_constraint (Type_name_constraint "a"));
                Defined_type_constraint (List_constraint (Type_name_constraint "a"));
            ];
            body = Fun_builtin DiscreteExpressionEvaluator.eval_list_rev;
            side_effect = false
        };
        {
            name = "list_mem";
            parameter_names = ["e"; "l"];
            signature_constraint = [
                Type_name_constraint "a";
                Defined_type_constraint (List_constraint (Type_name_constraint "a"));
                Defined_type_constraint Bool_constraint
            ];
            body = Fun_builtin DiscreteExpressionEvaluator.eval_list_mem;
            side_effect = false
        };
        {
            name = "list_length";
            parameter_names = ["l"];
            signature_constraint = [
                Defined_type_constraint (List_constraint (Type_name_constraint "a"));
                Defined_type_constraint (Number_constraint (Defined_type_number_constraint (Int_constraint Int_type_constraint)))
            ];
            body = Fun_builtin DiscreteExpressionEvaluator.eval_list_length;
            side_effect = false
        };
        {
            name = "stack_push";
            parameter_names = ["s"; "e"];
            signature_constraint = [
                Type_name_constraint "a";
                Defined_type_constraint (Stack_constraint (Type_name_constraint "a"));
                Defined_type_constraint (Stack_constraint (Type_name_constraint "a"))
            ];
            body = Fun_builtin DiscreteExpressionEvaluator.eval_stack_push;
            side_effect = true
        };
        {
            name = "stack_pop";
            parameter_names = ["s"];
            signature_constraint = [
                Defined_type_constraint (Stack_constraint (Type_name_constraint "a"));
                Type_name_constraint "a"
            ];
            body = Fun_builtin DiscreteExpressionEvaluator.eval_stack_pop;
            side_effect = true
        };
        {
            name = "stack_top";
            parameter_names = ["s"];
            signature_constraint = [
                Defined_type_constraint (Stack_constraint (Type_name_constraint "a"));
                Type_name_constraint "a"
            ];
            body = Fun_builtin DiscreteExpressionEvaluator.eval_stack_top;
            side_effect = false
        };
        {
            name = "stack_clear";
            parameter_names = ["s"];
            signature_constraint = [
                Defined_type_constraint (Stack_constraint (Type_name_constraint "a"));
                Defined_type_constraint (Stack_constraint (Type_name_constraint "a"))
            ];
            body = Fun_builtin DiscreteExpressionEvaluator.eval_stack_clear;
            side_effect = true
        };
        {
            name = "stack_is_empty";
            parameter_names = ["s"];
            signature_constraint = [
                Defined_type_constraint (Stack_constraint (Type_name_constraint "a"));
                Defined_type_constraint Bool_constraint
            ];
            body = Fun_builtin DiscreteExpressionEvaluator.eval_stack_is_empty;
            side_effect = false
        };
        {
            name = "stack_length";
            parameter_names = ["s"];
            signature_constraint = [
                Defined_type_constraint (Stack_constraint (Type_name_constraint "a"));
                Defined_type_constraint (Number_constraint (Defined_type_number_constraint (Int_constraint Int_type_constraint)))
            ];
            body = Fun_builtin DiscreteExpressionEvaluator.eval_stack_length;
            side_effect = false
        };
        {
            name = "queue_push";
            parameter_names = ["e"; "q"];
            signature_constraint = [
                Type_name_constraint "a";
                Defined_type_constraint (Queue_constraint (Type_name_constraint "a"));
                Defined_type_constraint (Queue_constraint (Type_name_constraint "a"))
            ];
            body = Fun_builtin DiscreteExpressionEvaluator.eval_stack_push;
            side_effect = true
        };
        {
            name = "queue_pop";
            parameter_names = ["q"];
            signature_constraint = [
                Defined_type_constraint (Queue_constraint (Type_name_constraint "a"));
                Type_name_constraint "a"
            ];
            body = Fun_builtin DiscreteExpressionEvaluator.eval_stack_pop;
            side_effect = true
        };
        {
            name = "queue_top";
            parameter_names = ["q"];
            signature_constraint = [
                Defined_type_constraint (Queue_constraint (Type_name_constraint "a"));
                Type_name_constraint "a"
            ];
            body = Fun_builtin DiscreteExpressionEvaluator.eval_stack_top;
            side_effect = false
        };
        {
            name = "queue_clear";
            parameter_names = ["q"];
            signature_constraint = [
                Defined_type_constraint (Queue_constraint (Type_name_constraint "a"));
                Defined_type_constraint (Queue_constraint (Type_name_constraint "a"))
            ];
            body = Fun_builtin DiscreteExpressionEvaluator.eval_stack_clear;
            side_effect = true
        };
        {
            name = "queue_is_empty";
            parameter_names = ["q"];
            signature_constraint = [
                Defined_type_constraint (Queue_constraint (Type_name_constraint "a"));
                Defined_type_constraint Bool_constraint
            ];
            body = Fun_builtin DiscreteExpressionEvaluator.eval_stack_is_empty;
            side_effect = false
        };
        {
            name = "queue_length";
            parameter_names = ["q"];
            signature_constraint = [
                Defined_type_constraint (Queue_constraint (Type_name_constraint "a"));
                Defined_type_constraint (Number_constraint (Defined_type_number_constraint (Int_constraint Int_type_constraint)))
            ];
            body = Fun_builtin DiscreteExpressionEvaluator.eval_stack_length;
            side_effect = false
        };
        {
            name = "fake";
            parameter_names = ["e"];
            signature_constraint = [
                Defined_type_constraint (List_constraint (Defined_type_constraint (List_constraint (Type_name_constraint "a"))));
                Defined_type_constraint (List_constraint (Defined_type_constraint (List_constraint (Type_name_constraint "a"))));
            ];
            body = Fun_builtin (fun _ -> raise (Exceptions.InternalError "implement function"));
            side_effect = false
        };
    ]

(* List of builtin functions metadata *)
let builtin_functions_metadata : ParsingStructure.function_metadata list =
    List.map metadata_of_function_definition builtin_function_bodies

(* Get builtin function metadata as a table *)
let builtin_functions_metadata_table =
    builtin_functions_metadata
    |> List.map (fun (fun_def : function_metadata) -> fun_def.name, fun_def)
    |> OCamlUtilities.hashtbl_of_tuples

(* Get builtin function implementations as a table *)
let builtin_function_bodies_table =
    builtin_function_bodies
    |> List.map (fun (fun_def : AbstractModel.fun_definition) -> fun_def.name, fun_def)
    |> OCamlUtilities.hashtbl_of_tuples

(* Compute metadata of a user defined function definition *)
let metadata_of_parsed_function_definition builtin_functions_metadata_table user_function_definitions_table (fun_def : parsed_fun_definition) =
    (* Concat parameters type and return type *)
    let signature = List.map second_of_tuple fun_def.parameters @ [fun_def.return_type] in
    {
        name = fun_def.name;
        parameter_names = List.map first_of_tuple fun_def.parameters;
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