(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Functions that extract useful information on parsing structure (get variables, is expression is constant ?...)
 *
 * File contributors : Benjamin L.
 * Created           : 2022/05/18
 *
 ************************************************************)

open ParsingStructure
open ParsingStructureUtilities
open OCamlUtilities
open ImitatorUtilities
open CustomModules
open DiscreteType
open VariableInfo
open Exceptions


(** Utils **)

(* Get variable name from a variable access *)
(* ex : my_var[0][0] -> my_var *)
let rec variable_name_of_parsed_scalar_or_index_update_type = function
    | Parsed_scalar_update variable_name -> variable_name
    | Parsed_indexed_update (parsed_scalar_or_index_update_type, _) -> variable_name_of_parsed_scalar_or_index_update_type parsed_scalar_or_index_update_type

(* Get variable name if any *)
let rec variable_name_of_parsed_update_type_opt = function
    | Parsed_variable_update parsed_scalar_or_index_update_type ->
        Some (variable_name_of_parsed_scalar_or_index_update_type parsed_scalar_or_index_update_type)
    | Parsed_void_update -> None

let variable_name_of_parsed_update_type parsed_update_type =
    let variable_name_opt = variable_name_of_parsed_update_type_opt parsed_update_type in
    match variable_name_opt with
    | Some variable_name -> variable_name
    | None -> raise (InternalError "Unable to get variable name of an update.")

(* Gather all updates of update section (pre-updates, updates and post-updates) *)
let updates_of_update_section update_section =
    let seq_updates, updates, _ = update_section in
    seq_updates @ updates

(* Try to get value of a discrete boolean expression, if directly a constant equals to false or true *)
(* If the expression is more complex, return None *)
let discrete_boolean_expression_constant_value_opt = function
    | Parsed_arithmetic_expression (Parsed_DAE_term (Parsed_DT_factor (Parsed_DF_constant (Bool_value v)))) -> Some v
    | _ -> None

(* Tree leaf functions *)

(* Check if leaf is a constant *)
let is_constant variable_infos = function
    | Leaf_variable variable_name -> is_constant_is_defined variable_infos variable_name
    | Leaf_constant _ -> true
    (* TODO benjamin IMPROVE not always true, a function can be constant *)
    | Leaf_fun _ -> false

(* Check if leaf has side effects *)
let has_side_effects variable_infos = function
    | Leaf_fun function_name ->
        let function_metadata = Functions.function_metadata_by_name variable_infos function_name in
        function_metadata.side_effect
    | Leaf_variable _
    | Leaf_constant _ -> false

(* TODO benjamin REFACTOR rename *)
let has_side_effects_2 variable_infos local_variables = function
    | Leaf_update_variable variable_name ->
        (* TODO benjamin IMPORTANT below false is a wrong value *)
        (* TODO benjamin IMPORTANT This function tends to disapear when removing old updates *)
        false
        (* Side effect only occurs if the updated variable is global *)
(*        not (VariableMap.mem variable_name local_variables)*)
    | Leaf_decl_variable _ -> false

(* Check if linear leaf is a constant *)
let is_linear_constant variable_infos = function
    | Leaf_linear_variable (_, variable_name) -> is_constant_is_defined variable_infos variable_name
    | Leaf_linear_constant _
    | Leaf_false_linear_constraint
    | Leaf_true_linear_constraint -> true

(* Check if leaf is a variable that is defined *)
(* A given callback is executed if it's not a defined variable *)
let is_variable_defined_with_callback variable_infos local_variables_opt variable_not_defined_callback_opt = function
    | Leaf_variable variable_name ->

        let is_defined_global = is_variable_or_constant_declared variable_infos variable_name in

        let is_defined_local =
            match local_variables_opt with
            | Some local_variables -> StringSet.mem variable_name local_variables
            | None -> false
        in

        let is_defined = is_defined_global || is_defined_local in

        if not is_defined then (
            match variable_not_defined_callback_opt with
            | Some variable_not_defined_callback -> variable_not_defined_callback variable_name
            | None -> ()
        );

        is_defined
    | Leaf_fun _ -> true
    | Leaf_constant _ -> true

let is_variable_defined_with_callback_2 variable_infos local_variables_opt variable_not_defined_callback_opt local_variables = function
    | Leaf_update_variable variable_name ->

        let is_defined_global = is_variable_or_constant_declared variable_infos variable_name in

        let is_defined_local =
            match local_variables_opt with
            | Some local_variables -> StringSet.mem variable_name local_variables
            | None -> false
        in

        let is_defined = is_defined_global || is_defined_local in

        if not is_defined then (
            match variable_not_defined_callback_opt with
            | Some variable_not_defined_callback -> variable_not_defined_callback variable_name
            | None -> ()
        );

        is_defined
    | Leaf_decl_variable _ -> true

let is_variable_defined variable_infos local_variables_opt = is_variable_defined_with_callback variable_infos local_variables_opt None

(* Check if linear expression leaf is a variable that is defined *)
let is_variable_defined_in_linear_expression variable_infos callback_fail = function
    | Leaf_linear_variable (_, variable_name) ->
        if not (List.mem variable_name variable_infos.variable_names) && not (is_constant_is_defined variable_infos variable_name) then(
            callback_fail variable_name; false
        )
        else
            true

    | Leaf_linear_constant _
    | Leaf_false_linear_constraint
    | Leaf_true_linear_constraint -> true

(* Check if a state predicate leaf has it's automaton / location defined *)
let is_automaton_defined_in_parsed_state_predicate_with_callbacks parsing_info undefined_automaton_callback_opt undefined_loc_callback_opt = function
    | Leaf_predicate_false
    | Leaf_predicate_true
    | Leaf_predicate_accepting -> true
    | Leaf_predicate_EQ (automaton_name, location_name)
    | Leaf_predicate_NEQ (automaton_name, location_name) ->
        (* Find the automaton *)
        if not (Hashtbl.mem parsing_info.index_of_automata automaton_name) then (
            (match undefined_automaton_callback_opt with
            | Some callback -> callback automaton_name
            | None -> ()
            );
            false
        ) else (
            let automaton_index : Automaton.automaton_index = Hashtbl.find parsing_info.index_of_automata automaton_name in
            (* Find the location *)
            if not (Hashtbl.mem parsing_info.index_of_locations.(automaton_index) location_name) then (
                (match undefined_loc_callback_opt with
                | Some callback -> callback automaton_name location_name
                | None -> ()
                );
                false
            ) else (
                (* Both checks passed *)
                true
            )
        )

(* Check if leaf is only a discrete variable *)
let is_only_discrete variable_infos clock_or_param_found_callback_opt = function
    | Leaf_variable variable_name ->
        let var_type = var_type_of_variable_or_constant variable_infos variable_name in
        (match var_type with
        | Var_type_clock
        | Var_type_parameter as var_type ->
            (match clock_or_param_found_callback_opt with
            | Some clock_or_param_found_callback -> clock_or_param_found_callback var_type variable_name
            | None -> ()
            ); false
        | Var_type_discrete _ -> true
        )
    (* Constants can only be discrete *)
    | Leaf_constant _
    (* As long as function can only return discrete and can't manipulate clocks and parameters *)
    | Leaf_fun _ -> true

(* Check if leaf isn't a variable *)
let no_variables variable_infos = function
    | Leaf_linear_variable (_, variable_name) ->
        (* Constants are allowed *)
        (is_constant_is_defined variable_infos variable_name)
        (* Or parameter *)
        ||
        let variable_index = index_of_variable_name variable_infos variable_name in
        variable_infos.type_of_variables variable_index = Var_type_parameter

    | Leaf_linear_constant _
    | Leaf_false_linear_constraint
    | Leaf_true_linear_constraint -> true

(* Check if a parsed boolean expression is constant *)
let is_parsed_boolean_expression_constant variable_infos =
    for_all_in_parsed_boolean_expression (is_constant variable_infos)

(* Check if a parsed arithmetic expression is constant *)
let is_parsed_arithmetic_expression_constant variable_infos =
    for_all_in_parsed_discrete_arithmetic_expression (is_constant variable_infos)

(* Check if a parsed term is constant *)
let is_parsed_term_constant variable_infos = for_all_in_parsed_discrete_term (is_constant variable_infos)
(* Check if a parsed term is constant *)
let is_parsed_factor_constant variable_infos = for_all_in_parsed_discrete_factor (is_constant variable_infos)

(* Check if a parsed boolean expression has side effects *)
let has_side_effect_parsed_boolean_expression variable_infos = exists_in_parsed_boolean_expression (has_side_effects variable_infos)
(* Check if a parsed discrete boolean expression has side effects *)
let has_side_effect_parsed_discrete_boolean_expression variable_infos = exists_in_parsed_discrete_boolean_expression (has_side_effects variable_infos)
(* Check if a parsed discrete arithmetic expression has side effects *)
let has_side_effect_parsed_discrete_arithmetic_expression variable_infos = exists_in_parsed_discrete_arithmetic_expression (has_side_effects variable_infos)
(* Check if a parsed normal update has side effects *)
let has_side_effect_parsed_normal_update variable_infos = exists_in_parsed_normal_update (has_side_effects_2 variable_infos) (has_side_effects variable_infos)
(* Check if a parsed state predicate has side effects *)
let has_side_effect_parsed_state_predicate variable_infos = exists_in_parsed_state_predicate (function _ -> false) (has_side_effects variable_infos)
(* Check if a parsed sequential code bloc has side effects *)
let has_side_effect_parsed_seq_code_bloc variable_infos (* seq_code_bloc *) = exists_in_parsed_seq_code_bloc (has_side_effects_2 variable_infos) (has_side_effects variable_infos)

(* Check if a parsed boolean expression is linear *)
let rec is_linear_parsed_boolean_expression variable_infos = function
    | Parsed_conj_dis _ -> false
    | Parsed_Discrete_boolean_expression expr ->
        is_linear_parsed_discrete_boolean_expression variable_infos expr

(* Check if a parsed discrete boolean expression is linear *)
and is_linear_parsed_discrete_boolean_expression variable_infos = function
    | Parsed_arithmetic_expression expr ->
        is_linear_parsed_arithmetic_expression variable_infos expr
    | Parsed_boolean_expression expr ->
        is_linear_parsed_boolean_expression variable_infos expr
    | Parsed_comparison _
    | Parsed_comparison_in _
    | Parsed_Not _ -> false

(* Check if a parsed arithmetic expression is linear *)
and is_linear_parsed_arithmetic_expression variable_infos = function
    | Parsed_sum_diff (expr, term, _) ->
        is_linear_parsed_arithmetic_expression variable_infos expr &&
        is_linear_parsed_term variable_infos term
    | Parsed_DAE_term term ->
        is_linear_parsed_term variable_infos term

(* Check if a parsed term is linear *)
and is_linear_parsed_term variable_infos = function
    | Parsed_product_quotient (term, factor, parsed_product_quotient) ->

        (* Check both term and factor are linear *)
        let is_linear_term = is_linear_parsed_term variable_infos term in
        let is_linear_factor = is_linear_parsed_factor variable_infos factor in
        let is_linear = is_linear_term && is_linear_factor in

        (* Check if term, factor are constant *)
        let is_term_constant = is_parsed_term_constant variable_infos term in
        let is_factor_constant = is_parsed_factor_constant variable_infos factor in

        let is_linear_product_quotient =
            match parsed_product_quotient with
            (* k*k, v*k or k*v are linear, but not v*v (with k a constant and v a variable) *)
            | Parsed_mul -> is_term_constant || is_factor_constant
            (* v/k, k/k is linear *)
            | Parsed_div -> is_factor_constant
        in
        is_linear && is_linear_product_quotient

    | Parsed_DT_factor factor ->
        is_linear_parsed_factor variable_infos factor

(* Check if a parsed factor is linear *)
and is_linear_parsed_factor variable_infos = function
    (* only rational variable *)
    | Parsed_DF_variable variable_name ->
        let var_type = VariableInfo.var_type_of_variable_or_constant variable_infos variable_name in

        (match var_type with
        | Var_type_clock
        | Var_type_parameter
        | Var_type_discrete (Var_type_discrete_number Var_type_discrete_rat)
        | Var_type_discrete (Var_type_discrete_number Var_type_discrete_weak_number) -> true
        | Var_type_discrete _ -> false
        )
    (* only rational constant *)
    | Parsed_DF_constant value ->
        (match value with
        | Rational_value _
        | Weak_number_value _ -> true
        | _ -> false
        )
    | Parsed_DF_expression expr ->
        is_linear_parsed_arithmetic_expression variable_infos expr
    | Parsed_DF_unary_min factor ->
        is_linear_parsed_factor variable_infos factor
    | Parsed_sequence _
    | Parsed_DF_access _
    | Parsed_function_call _ -> false

let all_variables_defined_in_parsed_boolean_expression_without_callback variable_infos expr =
    for_all_in_parsed_boolean_expression (is_variable_defined variable_infos None) expr

(* Check that all variables in a parsed boolean expression are effectively be defined *)
let all_variables_defined_in_parsed_boolean_expression variable_infos callback expr =
    for_all_in_parsed_boolean_expression (is_variable_defined_with_callback variable_infos None callback) expr

(* Check that all variables in a parsed discrete boolean expression are effectively be defined *)
let all_variables_defined_in_parsed_discrete_boolean_expression variable_infos callback expr =
    for_all_in_parsed_discrete_boolean_expression (is_variable_defined_with_callback variable_infos None callback) expr

(* Check that all variables in a parsed discrete arithmetic expression are effectively be defined *)
let all_variables_defined_in_parsed_discrete_arithmetic_expression variable_infos callback expr =
    for_all_in_parsed_discrete_arithmetic_expression (is_variable_defined_with_callback variable_infos None callback) expr

(* Check that all variables in a parsed normal update are effectively be defined *)
let all_variables_defined_in_parsed_normal_update variable_infos undefined_variable_callback expr =
    for_all_in_parsed_normal_update
        (is_variable_defined_with_callback_2 variable_infos None undefined_variable_callback)
        (is_variable_defined_with_callback variable_infos None undefined_variable_callback)
        expr

(* Check that all variables in a parsed update are effectively be defined *)
let all_variables_defined_in_parsed_update variable_infos undefined_variable_callback expr =
    for_all_in_parsed_update
        (is_variable_defined_with_callback_2 variable_infos None undefined_variable_callback)
        (is_variable_defined_with_callback variable_infos None undefined_variable_callback)
        expr

(* TODO benjamin REFACTOR replace by a general function in ParsingStructureUtilities *)
(* Check if all variables defined in user function body using local variables set *)
let all_variables_defined_in_parsed_seq_code_bloc_with_local_variables local_variables variable_infos undefined_variable_callback seq_code_bloc =

    (* Overwrite function `all_variables_defined_in_parsed_boolean_expression` adding a parameter for taking into account local variables set *)
    let all_variables_defined_in_parsed_boolean_expression local_variables (* expr *) =
        for_all_in_parsed_boolean_expression (is_variable_defined_with_callback variable_infos (Some local_variables) undefined_variable_callback) (* expr *)
    in

    (* Overwrite function `all_variables_defined_in_parsed_discrete_arithmetic_expression` adding a parameter for taking into account local variables set *)
    let all_variables_defined_in_parsed_discrete_arithmetic_expression local_variables (* expr *) =
        for_all_in_parsed_discrete_arithmetic_expression (is_variable_defined_with_callback variable_infos (Some local_variables) undefined_variable_callback) (* expr *)
    in

    (* Overwrite function `all_variables_defined_in_parsed_normal_update` adding a parameter for taking into account local variables set *)
    let all_variables_defined_in_parsed_normal_update local_variables (* expr *) =
        let leaf_seq_code_bloc_fun = is_variable_defined_with_callback_2 variable_infos (Some local_variables) undefined_variable_callback in
        let leaf_fun = is_variable_defined_with_callback variable_infos (Some local_variables) undefined_variable_callback in
        for_all_in_parsed_normal_update leaf_seq_code_bloc_fun leaf_fun (* expr *)
    in

    let rec all_variables_defined_in_parsed_seq_code_bloc_rec local_variables = function
        | Parsed_local_decl (variable_name, _, init_expr, next_expr, _) ->
            let all_variables_defined_in_init_expr = all_variables_defined_in_parsed_boolean_expression local_variables init_expr in
            (* Add the new declared local variable to set *)
            let local_variables = StringSet.add variable_name local_variables in
            all_variables_defined_in_parsed_seq_code_bloc_rec local_variables next_expr && all_variables_defined_in_init_expr

        | Parsed_assignment (normal_update, next_expr) ->
            (* Check if variables defined in normal update *)
            let all_variables_defined_in_normal_update = all_variables_defined_in_parsed_normal_update local_variables normal_update in
            (* Check if variables defined in next expressions *)
            let all_variables_defined_in_next_expr = all_variables_defined_in_parsed_seq_code_bloc_rec local_variables next_expr in
            (* Is all defined ? *)
            all_variables_defined_in_normal_update && all_variables_defined_in_next_expr

        | Parsed_for_loop (variable_name, from_expr, to_expr, _, inner_bloc, next_expr, _) ->
            (* Check if variables defined in from expr *)
            let all_variables_defined_in_from_expr = all_variables_defined_in_parsed_discrete_arithmetic_expression local_variables from_expr in
            (* Check if variables defined in to expr *)
            let all_variables_defined_in_to_expr = all_variables_defined_in_parsed_discrete_arithmetic_expression local_variables to_expr in
            (* Add the new declared local variable to set *)
            let local_variables_of_loop = StringSet.add variable_name local_variables in
            (* Check if variables defined in inner expressions *)
            let all_variables_defined_in_inner_bloc = all_variables_defined_in_parsed_seq_code_bloc_rec local_variables_of_loop inner_bloc in
            (* Check if variables defined in next expressions *)
            let all_variables_defined_in_next_expr = all_variables_defined_in_parsed_seq_code_bloc_rec local_variables next_expr in
            (* Is all defined ? *)
            all_variables_defined_in_inner_bloc && all_variables_defined_in_from_expr && all_variables_defined_in_to_expr && all_variables_defined_in_next_expr

        | Parsed_while_loop (condition_expr, inner_bloc, next_expr) ->
            (* Check if variables defined in from expr *)
            let all_variables_defined_in_condition_expr = all_variables_defined_in_parsed_boolean_expression local_variables condition_expr in
            (* Check if variables defined in inner expressions *)
            let all_variables_defined_in_inner_bloc = all_variables_defined_in_parsed_seq_code_bloc_rec local_variables inner_bloc in
            (* Check if variables defined in next expressions *)
            let all_variables_defined_in_next_expr = all_variables_defined_in_parsed_seq_code_bloc_rec local_variables next_expr in
            (* Is all defined ? *)
            all_variables_defined_in_condition_expr && all_variables_defined_in_inner_bloc && all_variables_defined_in_next_expr

        | Parsed_if (condition_expr, then_bloc, else_bloc_opt, next_expr) ->
            (* Check if variables defined in from expr *)
            let all_variables_defined_in_condition_expr = all_variables_defined_in_parsed_boolean_expression local_variables condition_expr in
            (* Check if variables defined in then expressions *)
            let all_variables_defined_in_then_bloc = all_variables_defined_in_parsed_seq_code_bloc_rec local_variables then_bloc in
            (* Check if variables defined in else expressions *)
            let all_variables_defined_in_else_bloc =
                match else_bloc_opt with
                | Some else_bloc ->
                    all_variables_defined_in_parsed_seq_code_bloc_rec local_variables else_bloc
                | None ->
                    true
            in
            (* Check if variables defined in next expressions *)
            let all_variables_defined_in_next_expr = all_variables_defined_in_parsed_seq_code_bloc_rec local_variables next_expr in
            (* Is all defined ? *)
            all_variables_defined_in_condition_expr && all_variables_defined_in_then_bloc && all_variables_defined_in_else_bloc && all_variables_defined_in_next_expr

        | Parsed_return_expr expr ->
            all_variables_defined_in_parsed_boolean_expression local_variables expr
        | Parsed_bloc_void -> true
    in
    all_variables_defined_in_parsed_seq_code_bloc_rec local_variables seq_code_bloc

let all_variables_defined_in_parsed_seq_code_bloc = all_variables_defined_in_parsed_seq_code_bloc_with_local_variables StringSet.empty

(* Check that all variables in a parsed fun declaration are effectively be defined *)
let all_variables_defined_in_parsed_fun_def variable_infos undefined_variable_callback (fun_def : parsed_fun_definition) =
    (* Add parameters as local variables *)
    let parameter_names = List.map first_of_tuple fun_def.parameters in
    let local_variables = List.fold_right StringSet.add parameter_names StringSet.empty in
    all_variables_defined_in_parsed_seq_code_bloc_with_local_variables local_variables variable_infos undefined_variable_callback fun_def.body

(* Check that all variables in a linear expression are effectively be defined *)
let all_variables_defined_in_linear_expression variable_infos callback_fail expr =
    for_all_in_parsed_linear_expression (is_variable_defined_in_linear_expression variable_infos callback_fail) expr

(* Check that all variables in a linear constraint are effectively be defined *)
let all_variables_defined_in_linear_constraint variable_infos callback_fail expr =
    for_all_in_parsed_linear_constraint
        (is_variable_defined_in_linear_expression variable_infos callback_fail) expr

(* Check that all variables in a non-linear constraint are effectively be defined *)
let all_variables_defined_in_nonlinear_constraint variable_infos callback expr =
    for_all_in_parsed_nonlinear_constraint
        (is_variable_defined_with_callback variable_infos None callback)
        expr

(* Check that all variables in a non-linear convex predicate (non-linear constraint list) are effectively be defined *)
let all_variables_defined_in_nonlinear_convex_predicate variable_infos callback non_linear_convex_predicate =
  List.fold_left
    (fun all_defined nonlinear_constraint ->
       OCamlUtilities.evaluate_and all_defined (all_variables_defined_in_nonlinear_constraint variable_infos callback nonlinear_constraint)
    )
    true
    non_linear_convex_predicate

(* Check that all variables in a state predicate are effectively be defined *)
let all_variable_in_parsed_state_predicate parsing_infos variable_infos undefined_variable_callback_opt undefined_automaton_callback_opt undefined_loc_callback_opt expr =
    for_all_in_parsed_state_predicate
        (is_automaton_defined_in_parsed_state_predicate_with_callbacks parsing_infos undefined_automaton_callback_opt undefined_loc_callback_opt)
        (is_variable_defined_with_callback variable_infos None undefined_variable_callback_opt)
        expr

(* Check that there is only discrete variables in a parsed boolean expression *)
let only_discrete_in_parsed_boolean_expression variable_infos clock_or_param_found_callback_opt expr =
    for_all_in_parsed_boolean_expression (is_only_discrete variable_infos clock_or_param_found_callback_opt) expr

(* Check that there is only discrete variables in a parsed discrete boolean expression *)
let only_discrete_in_nonlinear_expression variable_infos expr =
    for_all_in_parsed_discrete_boolean_expression (is_only_discrete variable_infos None) expr

(* Check if there is no variables in a linear expression *)
let no_variables_in_linear_expression variable_infos expr =
    for_all_in_parsed_linear_expression (no_variables variable_infos) expr

(* Check if a linear expression is constant *)
let is_parsed_linear_expression_constant variable_infos expr =
    for_all_in_parsed_linear_expression (is_linear_constant variable_infos) expr

(* Gather all variable names used in a linear_expression *)
let add_variable_of_linear_expression variables_used_ref = function
    | Leaf_linear_variable (_, variable_name) ->
        (* Add the variable name to the set and update the reference *)
        variables_used_ref := StringSet.add variable_name !variables_used_ref

    | Leaf_linear_constant _
    | Leaf_false_linear_constraint
    | Leaf_true_linear_constraint -> ()

(* Gather all variable names used in a discrete boolean expression *)
let add_variable_of_discrete_boolean_expression variables_used_ref = function
    | Leaf_constant _
    | Leaf_fun _ -> ()
    | Leaf_variable variable_name ->
        (* Add the variable name to the set and update the reference *)
        variables_used_ref := StringSet.add variable_name !variables_used_ref

(* Gather all function names used in a discrete boolean expression *)
let add_function_of_discrete_boolean_expression function_used_ref = function
    | Leaf_constant _
    | Leaf_variable _ -> ()
    | Leaf_fun function_name ->
        (* Add the variable name to the set and update the reference *)
        function_used_ref := StringSet.add function_name !function_used_ref

let get_functions_in_parsed_boolean_expression_with_accumulator function_used_ref =
    iterate_parsed_boolean_expression (add_function_of_discrete_boolean_expression function_used_ref)

(* Gather all variable names used in a parsed boolean expression in a given accumulator *)
let get_variables_in_parsed_boolean_expression_with_accumulator variables_used_ref =
    iterate_parsed_boolean_expression (add_variable_of_discrete_boolean_expression variables_used_ref)

(* Gather all variable names used in a parsed discrete boolean expression in a given accumulator *)
let get_variables_in_parsed_discrete_boolean_expression_with_accumulator variables_used_ref =
    iterate_parsed_discrete_boolean_expression (add_variable_of_discrete_boolean_expression variables_used_ref)

(* Gather all function names used in a parsed discrete boolean expression in a given accumulator *)
let get_functions_in_parsed_discrete_boolean_expression_with_accumulator variables_used_ref =
    iterate_parsed_discrete_boolean_expression (add_function_of_discrete_boolean_expression variables_used_ref)

(* Gather all variable names used in a parsed discrete arithmetic expression in a given accumulator *)
let get_variables_in_parsed_discrete_arithmetic_expression_with_accumulator variables_used_ref =
    iterate_parsed_discrete_arithmetic_expression (add_variable_of_discrete_boolean_expression variables_used_ref)

(* Gather all function names used in a parsed discrete arithmetic expression in a given accumulator *)
let get_functions_in_parsed_discrete_arithmetic_expression_with_accumulator functions_used_ref =
    iterate_parsed_discrete_arithmetic_expression (add_function_of_discrete_boolean_expression functions_used_ref)

(* Gather all variable names used in a linear expression in a given accumulator *)
let get_variables_in_linear_expression_with_accumulator variables_used_ref =
    iterate_parsed_linear_expression (add_variable_of_linear_expression variables_used_ref)

(* Gather all variable names used in a linear constraint in a given accumulator *)
let get_variables_in_linear_constraint_with_accumulator variables_used_ref =
    iterate_parsed_linear_constraint (add_variable_of_linear_expression variables_used_ref)

(* Gather all variable names used in a non-linear constraint in a given accumulator *)
let get_variables_in_nonlinear_constraint_with_accumulator = get_variables_in_parsed_discrete_boolean_expression_with_accumulator

(* Gather all function names used in a non-linear constraint in a given accumulator *)
let get_functions_in_nonlinear_constraint_with_accumulator = get_functions_in_parsed_discrete_boolean_expression_with_accumulator

(* Gather all variable names used in an update in a given accumulator *)
let get_variables_in_parsed_update_with_accumulator variables_used_ref =
    iterate_parsed_update
        (fun _ _ -> ())
        (add_variable_of_discrete_boolean_expression variables_used_ref)

(* Gather all function names used in an update in a given accumulator *)
let get_functions_in_parsed_update_with_accumulator variables_used_ref =
    iterate_parsed_update
        (fun _ _ -> ())
        (add_function_of_discrete_boolean_expression variables_used_ref)

(* Gather all variable names used in a normal update in a given accumulator *)
let get_variables_in_parsed_normal_update_with_accumulator variables_used_ref =
    iterate_parsed_normal_update
        (fun _ _ -> ())
        (add_variable_of_discrete_boolean_expression variables_used_ref)

(* Gather all variable names used in a parsed simple predicate in a given accumulator *)
let get_variables_in_parsed_simple_predicate_with_accumulator variables_used_ref =
    iterate_in_parsed_simple_predicate
        (function _ -> ())
        (add_variable_of_discrete_boolean_expression variables_used_ref)

(* Gather all variable names used in a parsed state predicate in a given accumulator *)
let get_variables_in_parsed_state_predicate_with_accumulator variables_used_ref =
    iterate_in_parsed_state_predicate
        (function _ -> ())
        (add_variable_of_discrete_boolean_expression variables_used_ref)

(* Create and wrap an accumulator then return result directly *)
let wrap_accumulator f expr =
    let variables_used_ref = ref StringSet.empty in
    f variables_used_ref expr;
    !variables_used_ref

(* Gather all variable names used in a parsed boolean expression *)
let get_functions_in_parsed_boolean_expression =
    wrap_accumulator get_functions_in_parsed_boolean_expression_with_accumulator

(* Gather all variable names used in a parsed boolean expression *)
let get_variables_in_parsed_boolean_expression =
    wrap_accumulator get_variables_in_parsed_boolean_expression_with_accumulator

(* Gather all variable names used in a parsed discrete boolean expression *)
let get_variables_in_parsed_discrete_boolean_expression =
    wrap_accumulator get_variables_in_parsed_discrete_boolean_expression_with_accumulator

(* Gather all variable names used in a parsed discrete arithmetic expression *)
let get_variables_in_parsed_discrete_arithmetic_expression =
    wrap_accumulator get_variables_in_parsed_discrete_arithmetic_expression_with_accumulator

(* Gather all function names used in a parsed discrete arithmetic expression *)
let get_functions_in_parsed_discrete_arithmetic_expression =
    wrap_accumulator get_functions_in_parsed_discrete_arithmetic_expression_with_accumulator

(* Gather all variable names used in a parsed update expression *)
let get_variables_in_parsed_update =
    wrap_accumulator get_variables_in_parsed_update_with_accumulator

(* Gather all variable names used in a parsed normal update expression *)
let get_variables_in_parsed_normal_update =
    wrap_accumulator get_variables_in_parsed_normal_update_with_accumulator

(* Gather all variable names used in a linear expression *)
let get_variables_in_linear_expression =
    wrap_accumulator get_variables_in_linear_expression_with_accumulator

(* Gather all variable names used in a linear constraint *)
let get_variables_in_linear_constraint =
    wrap_accumulator get_variables_in_linear_constraint_with_accumulator

(* Gather all variable names used in a non-linear constraint *)
let get_variables_in_nonlinear_constraint =
    wrap_accumulator get_variables_in_nonlinear_constraint_with_accumulator

(* Gather all function names used in a non-linear constraint *)
let get_functions_in_nonlinear_constraint =
    wrap_accumulator get_functions_in_nonlinear_constraint_with_accumulator

(* Gather all variable names used in a parsed init state predicate *)
let get_variables_in_init_state_predicate = function
	| Parsed_loc_assignment _ -> StringSet.empty
	| Parsed_linear_predicate linear_constraint -> get_variables_in_linear_constraint linear_constraint
	| Parsed_discrete_predicate (_, expr) -> get_variables_in_parsed_boolean_expression expr

(* Gather all variable names used in a non-linear convex predicate (non-linear constraint list) *)
let get_variables_in_nonlinear_convex_predicate convex_predicate =
    List.map (get_variables_in_nonlinear_constraint) convex_predicate |>
    List.fold_left (fun variables acc -> StringSet.union acc variables) StringSet.empty

(* Gather all function names used in a non-linear convex predicate (non-linear constraint list) *)
let get_functions_in_nonlinear_convex_predicate convex_predicate =
    List.map (get_functions_in_nonlinear_constraint) convex_predicate |>
    List.fold_left (fun variables acc -> StringSet.union acc variables) StringSet.empty

let get_variables_in_parsed_simple_predicate =
    wrap_accumulator get_variables_in_parsed_simple_predicate_with_accumulator

(**)
let get_variables_in_parsed_state_predicate =
    wrap_accumulator get_variables_in_parsed_state_predicate_with_accumulator

let left_right_member_of_assignments_in_parsed_seq_code_bloc (* seq_code_bloc *) =

    let left_right_member_of_assignments local_variables = function
        | Traversed_parsed_for_loop (_, _, _, _, inner_result, next_result)
        | Traversed_parsed_while_loop (_, inner_result, next_result) -> inner_result @ next_result
        | Traversed_parsed_if (_, then_result, else_result_opt, next_result) ->
            then_result @ (match else_result_opt with Some else_result -> else_result | None -> []) @ next_result
        | Traversed_parsed_local_decl (_, _, _, next_result) -> next_result

        | Traversed_parsed_assignment ((parsed_update_type, expr), next_result) ->

            let right_variable_names = string_set_to_list (get_variables_in_parsed_boolean_expression expr) in

            let variable_name_opt = variable_name_of_parsed_update_type_opt parsed_update_type in
            let left_variable_name =
                match variable_name_opt with
                | Some variable_name -> variable_name
                | None -> ""
            in

            [left_variable_name, right_variable_names] @ next_result
        | Traversed_parsed_return_expr _
        | Traversed_parsed_bloc_void -> []
    in
    ParsingStructureUtilities.traverse_parsed_seq_code_bloc left_right_member_of_assignments (* seq_code_bloc *)

(* Get local variables of a parsed function definition *)
let local_variables_of_parsed_fun_def (fun_def : parsed_fun_definition) =
    (* Concat all local variables found when traversing the function body *)
    ParsingStructureUtilities.fold_parsed_fun_def
        (@) (* concat operator *)
        [] (* base *)
        (fun _ leaf -> match leaf with Leaf_decl_variable (variable_name, discrete_type, _) -> [variable_name, discrete_type] | Leaf_update_variable _ -> [])
        (function _ -> [])
        fun_def

(* Get local variables of a parsed sequential code bloc *)
let local_variables_of_parsed_seq_code_bloc seq_code_bloc =
    (* Concat all local variables found when traversing the function body *)
    ParsingStructureUtilities.fold_parsed_seq_code_bloc
        (@) (* concat operator *)
        [] (* base *)
        (fun _ leaf -> match leaf with Leaf_decl_variable (variable_name, discrete_type, _) -> [variable_name, discrete_type] | Leaf_update_variable _ -> [])
        (function _ -> [])
        seq_code_bloc