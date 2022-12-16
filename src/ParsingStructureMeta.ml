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
    | Parsed_scalar_update (variable_name, _ (* id *)) -> variable_name
    | Parsed_indexed_update (parsed_scalar_or_index_update_type, _) -> variable_name_of_parsed_scalar_or_index_update_type parsed_scalar_or_index_update_type

(* Get variable ref (variable_name * variable_id) from a variable access *)
(* ex : my_var[0][0] -> my_var *)let rec variable_ref_of_parsed_scalar_or_index_update_type = function
    | Parsed_scalar_update variable_ref -> variable_ref
    | Parsed_indexed_update (parsed_scalar_or_index_update_type, _) -> variable_ref_of_parsed_scalar_or_index_update_type parsed_scalar_or_index_update_type


(* Try to get value of a discrete boolean expression, if directly a constant equals to false or true *)
(* If the expression is more complex, return None *)
let discrete_boolean_expression_constant_value_opt = function
    | Parsed_arithmetic_expr (Parsed_term (Parsed_factor (Parsed_constant (Bool_value v)))) -> Some v
    | _ -> None

(* Tree leaf functions *)

(* Check if leaf is a constant *)
let is_constant variable_infos = function
    | Leaf_variable (variable_name, _) -> is_constant_is_defined variable_infos variable_name
    | Leaf_constant _ -> true
    (* TODO benjamin IMPROVE not always true, a function can be constant if it's content is constant too *)
    | Leaf_fun _ -> false

(* Check if leaf is a function call *)
let has_fun_call = function
    | Leaf_fun _ -> true
    | Leaf_variable _
    | Leaf_constant _ -> false

(* Check if leaf has side effects *)
let has_side_effects variable_infos = function
    | Leaf_fun function_name ->
        let function_metadata = VariableInfo.function_metadata_by_name variable_infos function_name in
        function_metadata.side_effect
    | Leaf_variable _
    | Leaf_constant _ -> false

(* Check if linear leaf is a constant *)
let is_linear_constant variable_infos = function
    | Leaf_linear_variable (_, variable_name) -> is_constant_is_defined variable_infos variable_name
    | Leaf_linear_constant _
    | Leaf_false_linear_constraint
    | Leaf_true_linear_constraint -> true


(* Check if leaf is a variable that is defined *)
(* A given callback is executed if it's not a defined variable *)
let is_variable_defined_with_callback variable_infos variable_not_defined_callback_opt = function
    | Leaf_variable ((variable_name, _) as variable_ref) ->

        let is_defined = is_variable_or_constant_declared variable_infos variable_ref in

        if not is_defined then (
            match variable_not_defined_callback_opt with
            | Some variable_not_defined_callback -> variable_not_defined_callback variable_name
            | None -> ()
        );

        is_defined
    | Leaf_fun _ -> true
    | Leaf_constant _ -> true

let is_variable_defined_on_update_with_callback variable_infos variable_not_defined_callback_opt = function
        | Leaf_update_variable ((variable_name, _) as variable_ref, _) ->

        let is_defined = is_variable_or_constant_declared variable_infos variable_ref in

        if not is_defined then (
            match variable_not_defined_callback_opt with
            | Some variable_not_defined_callback -> variable_not_defined_callback variable_name
            | None -> ()
        );

        is_defined

let is_variable_defined variable_infos = is_variable_defined_with_callback variable_infos None

let is_function_defined_with_callback variable_infos function_not_defined_callback_opt = function
    | Leaf_fun function_name ->

        let is_defined = Hashtbl.mem variable_infos.fun_meta function_name in

        if not is_defined then (
            match function_not_defined_callback_opt with
            | Some function_not_defined_callback -> function_not_defined_callback function_name
            | None -> ()
        );

        is_defined
    | Leaf_variable _ -> true
    | Leaf_constant _ -> true

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
    | Leaf_variable ((variable_name, _) as variable_ref) ->
        let var_type = var_type_of_variable_or_constant variable_infos variable_ref in
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
(* Check if a parsed state predicate has side effects *)
let has_side_effect_parsed_state_predicate variable_infos = exists_in_parsed_state_predicate (function _ -> false) (has_side_effects variable_infos)

(* Check if a parsed boolean expression contains function call(s) *)
let has_fun_call_parsed_boolean_expression = exists_in_parsed_boolean_expression has_fun_call
(* Check if a parsed discrete arithmetic expression contains function call(s) *)
let has_fun_call_parsed_discrete_arithmetic_expression = exists_in_parsed_discrete_arithmetic_expression has_fun_call

(* Check if a parsed boolean expression is linear *)
let rec is_linear_parsed_boolean_expression variable_infos = function
    | Parsed_conj_dis _ -> false
    | Parsed_discrete_bool_expr expr ->
        is_linear_parsed_discrete_boolean_expression variable_infos expr

(* Check if a parsed discrete boolean expression is linear *)
and is_linear_parsed_discrete_boolean_expression variable_infos = function
    | Parsed_arithmetic_expr expr ->
        is_linear_parsed_arithmetic_expression variable_infos expr
    | Parsed_nested_bool_expr expr ->
        is_linear_parsed_boolean_expression variable_infos expr
    | Parsed_comparison _
    | Parsed_comparison_in _
    | Parsed_not _ -> false

(* Check if a parsed arithmetic expression is linear *)
and is_linear_parsed_arithmetic_expression variable_infos = function
    | Parsed_sum_diff (expr, term, _) ->
        is_linear_parsed_arithmetic_expression variable_infos expr &&
        is_linear_parsed_term variable_infos term
    | Parsed_term term ->
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

    | Parsed_factor factor ->
        is_linear_parsed_factor variable_infos factor

(* Check if a parsed factor is linear *)
and is_linear_parsed_factor variable_infos = function
    (* only rational variable *)
    | Parsed_variable _ -> true
    (* only rational constant *)
    | Parsed_constant value ->
        (match value with
        | Rat_value _
        | Weak_number_value _ -> true
        | _ -> false
        )
    | Parsed_nested_expr expr ->
        is_linear_parsed_arithmetic_expression variable_infos expr
    | Parsed_unary_min factor ->
        is_linear_parsed_factor variable_infos factor
    | Parsed_sequence _
    | Parsed_access _
    | Parsed_function_call _ -> false

let all_variables_defined_in_parsed_boolean_expression_without_callback variable_infos expr =
    for_all_in_parsed_boolean_expression (is_variable_defined variable_infos) expr

(* Check that all variables in a parsed boolean expression are effectively be defined *)
let all_variables_defined_in_parsed_boolean_expression variable_infos callback expr =
    for_all_in_parsed_boolean_expression (is_variable_defined_with_callback variable_infos callback) expr

(* Check that all variables in a parsed discrete boolean expression are effectively be defined *)
let all_variables_defined_in_parsed_discrete_boolean_expression variable_infos callback expr =
    for_all_in_parsed_discrete_boolean_expression (is_variable_defined_with_callback variable_infos callback) expr

(* Check that all variables in a parsed discrete arithmetic expression are effectively be defined *)
let all_variables_defined_in_parsed_discrete_arithmetic_expression variable_infos callback expr =
    for_all_in_parsed_discrete_arithmetic_expression (is_variable_defined_with_callback variable_infos callback) expr

(* Check that all variables in a parsed normal update are effectively be defined *)
let all_variables_defined_in_parsed_normal_update variable_infos undefined_variable_callback expr =
    for_all_in_parsed_normal_update
        (is_variable_defined_on_update_with_callback variable_infos undefined_variable_callback)
        (is_variable_defined_with_callback variable_infos undefined_variable_callback)
        expr

(* Check that all variables in a parsed sequential code bloc are effectively be defined *)
let all_variables_defined_in_parsed_seq_code_bloc variable_infos undefined_variable_callback seq_code_bloc =
    ParsingStructureUtilities.for_all_in_parsed_seq_code_bloc
        (is_variable_defined_on_update_with_callback variable_infos undefined_variable_callback)
        (is_variable_defined_with_callback variable_infos undefined_variable_callback)
        seq_code_bloc

(* Check that all functions called in a parsed sequential code bloc are effectively be defined *)
let all_functions_defined_in_parsed_seq_code_bloc variable_infos undefined_function_callback seq_code_bloc =
    ParsingStructureUtilities.for_all_in_parsed_seq_code_bloc
        (fun _ -> true)
        (is_function_defined_with_callback variable_infos undefined_function_callback)
        seq_code_bloc

(* Check that all variables in a parsed fun declaration are effectively be defined *)
let all_variables_defined_in_parsed_fun_def variable_infos undefined_variable_callback (fun_def : parsed_fun_definition) =
    ParsingStructureUtilities.for_all_in_parsed_fun_def
        (is_variable_defined_on_update_with_callback variable_infos undefined_variable_callback)
        (is_variable_defined_with_callback variable_infos undefined_variable_callback)
        fun_def

(* Check that all functions called in a parsed fun declaration are effectively be defined *)
let all_functions_defined_in_parsed_fun_def variable_infos undefined_function_callback (fun_def : parsed_fun_definition) =
    ParsingStructureUtilities.for_all_in_parsed_fun_def
        (fun _ -> true)
        (is_function_defined_with_callback variable_infos undefined_function_callback)
        fun_def

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
        (is_variable_defined_with_callback variable_infos callback)
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
let all_variables_defined_in_parsed_state_predicate parsing_infos variable_infos undefined_variable_callback_opt undefined_automaton_callback_opt undefined_loc_callback_opt expr =
    for_all_in_parsed_state_predicate
        (is_automaton_defined_in_parsed_state_predicate_with_callbacks parsing_infos undefined_automaton_callback_opt undefined_loc_callback_opt)
        (is_variable_defined_with_callback variable_infos undefined_variable_callback_opt)
        expr

(* Check that there is only discrete variables in a parsed boolean expression *)
let only_discrete_in_parsed_boolean_expression variable_infos clock_or_param_found_callback_opt expr =
    for_all_in_parsed_boolean_expression (is_only_discrete variable_infos clock_or_param_found_callback_opt) expr

(* Check that there is only discrete variables in a parsed arithmetic expression *)
let only_discrete_in_parsed_discrete_arithmetic_expression variable_infos clock_or_param_found_callback_opt expr =
    for_all_in_parsed_discrete_arithmetic_expression (is_only_discrete variable_infos clock_or_param_found_callback_opt) expr

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
    | Leaf_variable (variable_name, _) ->
        (* Add the variable name to the set and update the reference *)
        variables_used_ref := StringSet.add variable_name !variables_used_ref

(* Gather all variable refs used in a discrete boolean expression *)
let add_variable_ref_of_discrete_boolean_expression variables_used_ref = function
    | Leaf_constant _
    | Leaf_fun _ -> ()
    | Leaf_variable variable_ref ->
        (* Add the variable name to the set and update the reference *)
        variables_used_ref := VarSet.add variable_ref !variables_used_ref

(* Gather all function names used in a discrete boolean expression *)
let add_function_of_discrete_boolean_expression function_used_ref = function
    | Leaf_constant _
    | Leaf_variable _ -> ()
    | Leaf_fun function_name ->
        (* Add the variable name to the set and update the reference *)
        function_used_ref := StringSet.add function_name !function_used_ref

(* Gather all variable and function names used in a discrete boolean expression *)
let add_variable_and_function_of_discrete_boolean_expression variables_used_ref = function
    | Leaf_constant _ -> ()
    | Leaf_variable (variable_name, _)
    | Leaf_fun variable_name ->
         (* Add the variable name to the set and update the reference *)
         variables_used_ref := StringSet.add variable_name !variables_used_ref

(* Gather all clock and parameter names used in a discrete boolean expression *)
let add_clock_or_parameter_of_discrete_boolean_expression variable_infos variables_used_ref = function
    | Leaf_constant _
    | Leaf_fun _ -> ()
    | Leaf_variable ((variable_name, _) as variable_ref) ->
        (* Only add if it's a clock or parameter *)
        if (VariableInfo.is_clock_or_param variable_infos variable_ref) then (
            (* Add the variable name to the set and update the reference *)
            variables_used_ref := StringSet.add variable_name !variables_used_ref;
        )

(* Gather all variable names used in a parsed boolean expression in a given accumulator *)
let get_variables_in_parsed_boolean_expression_with_accumulator variables_used_ref =
    iterate_parsed_boolean_expression (add_variable_of_discrete_boolean_expression variables_used_ref)

(* Gather all variable refs used in a parsed boolean expression in a given accumulator *)
let get_variable_refs_in_parsed_boolean_expression_with_accumulator variables_used_ref =
    iterate_parsed_boolean_expression (add_variable_ref_of_discrete_boolean_expression variables_used_ref)

let get_functions_in_parsed_boolean_expression_with_accumulator function_used_ref =
    iterate_parsed_boolean_expression (add_function_of_discrete_boolean_expression function_used_ref)

(* Gather all variable and function names used in a parsed boolean expression in a given accumulator *)
let get_variables_and_functions_in_parsed_boolean_expression_with_accumulator variables_used_ref =
    iterate_parsed_boolean_expression (add_variable_and_function_of_discrete_boolean_expression variables_used_ref)

(* Gather all clock and parameter names used in a parsed boolean expression in a given accumulator *)
let get_clocks_and_parameters_in_parsed_boolean_expression_with_accumulator variable_infos variables_used_ref =
    iterate_parsed_boolean_expression (add_clock_or_parameter_of_discrete_boolean_expression variable_infos variables_used_ref)

(* Gather all variable names used in a parsed discrete boolean expression in a given accumulator *)
let get_variables_in_parsed_discrete_boolean_expression_with_accumulator variables_used_ref =
    iterate_parsed_discrete_boolean_expression (add_variable_of_discrete_boolean_expression variables_used_ref)

(* Gather all function names used in a parsed discrete boolean expression in a given accumulator *)
let get_functions_in_parsed_discrete_boolean_expression_with_accumulator variables_used_ref =
    iterate_parsed_discrete_boolean_expression (add_function_of_discrete_boolean_expression variables_used_ref)

(* Gather all variable names used in a parsed discrete arithmetic expression in a given accumulator *)
let get_variables_in_parsed_discrete_arithmetic_expression_with_accumulator variables_used_ref =
    iterate_parsed_discrete_arithmetic_expression (add_variable_of_discrete_boolean_expression variables_used_ref)

(* Gather all variable refs used in a parsed discrete arithmetic expression in a given accumulator *)
let get_variable_refs_in_parsed_discrete_arithmetic_expression_with_accumulator variables_used_ref =
    iterate_parsed_discrete_arithmetic_expression (add_variable_ref_of_discrete_boolean_expression variables_used_ref)

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

(* Gather all variable names used in a normal update in a given accumulator *)
let get_variables_in_parsed_normal_update_with_accumulator variables_used_ref =
    iterate_parsed_normal_update
        (fun _ -> ())
        (add_variable_of_discrete_boolean_expression variables_used_ref)

(* Gather all function names used in a parsed sequential code bloc in a given accumulator *)
let get_functions_in_parsed_seq_code_bloc_with_accumulator function_used_ref =
    iterate_in_parsed_seq_code_bloc (fun _ -> ()) (add_function_of_discrete_boolean_expression function_used_ref)

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

(* Create and wrap a variable ref accumulator then return result directly *)
let wrap_var_ref_accumulator f expr =
    let variables_used_ref = ref VarSet.empty in
    f variables_used_ref expr;
    !variables_used_ref

(* Gather all variable names used in a parsed boolean expression *)
let get_variables_in_parsed_boolean_expression =
    wrap_accumulator get_variables_in_parsed_boolean_expression_with_accumulator

(* Gather all variable refs used in a parsed boolean expression *)
let get_variable_refs_in_parsed_boolean_expression =
    wrap_var_ref_accumulator get_variable_refs_in_parsed_boolean_expression_with_accumulator

(* Gather all variable names used in a parsed boolean expression *)
let get_functions_in_parsed_boolean_expression =
    wrap_accumulator get_functions_in_parsed_boolean_expression_with_accumulator

(* Gather all variable and function names used in a parsed boolean expression *)
let get_variables_and_functions_in_parsed_boolean_expression =
    wrap_accumulator get_variables_and_functions_in_parsed_boolean_expression_with_accumulator

(* Gather all clock and parameter names used in a parsed boolean expression *)
let get_clocks_and_parameters_in_parsed_boolean_expression variable_infos =
    wrap_accumulator (get_clocks_and_parameters_in_parsed_boolean_expression_with_accumulator variable_infos)

(* Gather all variable names used in a parsed discrete boolean expression *)
let get_variables_in_parsed_discrete_boolean_expression =
    wrap_accumulator get_variables_in_parsed_discrete_boolean_expression_with_accumulator

(* Gather all variable names used in a parsed discrete arithmetic expression *)
let get_variables_in_parsed_discrete_arithmetic_expression =
    wrap_accumulator get_variables_in_parsed_discrete_arithmetic_expression_with_accumulator

(* Gather all variable refs used in a parsed discrete arithmetic expression *)
let get_variable_refs_in_parsed_discrete_arithmetic_expression =
    wrap_var_ref_accumulator get_variable_refs_in_parsed_discrete_arithmetic_expression_with_accumulator

(* Gather all function names used in a parsed discrete arithmetic expression *)
let get_functions_in_parsed_discrete_arithmetic_expression =
    wrap_accumulator get_functions_in_parsed_discrete_arithmetic_expression_with_accumulator

(* Gather all variable names used in a parsed normal update expression *)
let get_variables_in_parsed_normal_update =
    wrap_accumulator get_variables_in_parsed_normal_update_with_accumulator

(* Gather all function names used in a parsed sequential code bloc *)
let get_functions_in_parsed_seq_code_bloc =
    wrap_accumulator get_functions_in_parsed_seq_code_bloc_with_accumulator

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

(* Get pairs of left and right members of assignments (ex: i := j + 1 + k return the triple (i, [j;k], j + 1 + k) *)
let left_right_member_of_assignments_in_parsed_seq_code_bloc (* parsed_seq_code_bloc *) =

    let rec left_right_member_of_assignments_in_parsed_seq_code_bloc_rec parsed_seq_code_bloc =
        let left_right_members_nested = List.map left_right_member_of_assignments_in_instruction parsed_seq_code_bloc in
        List.concat left_right_members_nested

    and left_right_member_of_assignments_in_instruction = function
        | Parsed_for_loop (_, _, _, _, inner_bloc)
        | Parsed_while_loop (_, inner_bloc) ->
            left_right_member_of_assignments_in_parsed_seq_code_bloc_rec inner_bloc

        | Parsed_if (_, then_bloc, else_bloc_opt) ->
            let then_bloc_results = left_right_member_of_assignments_in_parsed_seq_code_bloc_rec then_bloc in

            let else_bloc_results =
                match else_bloc_opt with
                | Some else_bloc -> left_right_member_of_assignments_in_parsed_seq_code_bloc_rec else_bloc
                | None -> []
            in
            then_bloc_results @ else_bloc_results

        | Parsed_assignment (parsed_scalar_or_index_update_type, expr) ->

            let right_variable_names = get_variable_refs_in_parsed_boolean_expression expr |> VarSet.to_seq |> List.of_seq in
            let left_variable_name = variable_ref_of_parsed_scalar_or_index_update_type parsed_scalar_or_index_update_type in

            [left_variable_name, right_variable_names, expr]

        | Parsed_instruction _
        | Parsed_local_decl _ -> []
    in
    left_right_member_of_assignments_in_parsed_seq_code_bloc_rec (* parsed_seq_code_bloc *)

(* Check whether clock updates found in parsed sequential code bloc are only resets *)
let is_only_resets_in_parsed_seq_code_bloc variable_infos (* seq_code_bloc *) =
    ParsingStructureUtilities.for_all_in_parsed_seq_code_bloc
        (function
            (* If found clock update and assigned value is not zero so there is not only resets *)
            | Leaf_update_variable ((variable_name, _) as variable_ref, update_expr) ->

                let is_clock = VariableInfo.is_clock variable_infos variable_ref in
                let is_reset_value =
                    match update_expr with
                    | Parsed_discrete_bool_expr (Parsed_arithmetic_expr (Parsed_term (Parsed_factor (Parsed_constant value)))) when ParsedValue.is_zero value -> true
                    | _ -> false
                in

                not is_clock || is_clock && is_reset_value
        )
        (fun _ -> true) (* seq_code_bloc *)

(* Check whether clock updates found in parsed sequential code bloc (and all called functions in bloc) are only resets *)
let rec is_only_resets_in_parsed_seq_code_bloc_deep variable_infos user_function_definitions_table seq_code_bloc =

    (* Function that check if a function body contains only reset (recursively through function calls) *)
    let is_only_resets_in_called_function function_name =
        (* Is a call to a user function ? *)
        if Hashtbl.mem user_function_definitions_table function_name then (
            let found_function_def = Hashtbl.find user_function_definitions_table function_name in
            let code_bloc, _ = found_function_def.body in
            is_only_resets_in_parsed_seq_code_bloc_deep variable_infos user_function_definitions_table code_bloc
        )
        else
            true
    in

    (* Search for called functions in sequential code bloc *)
    let called_functions = get_functions_in_parsed_seq_code_bloc seq_code_bloc in
    let called_functions_list = OCamlUtilities.string_set_to_list called_functions in
    (* Check if there is only resets in called functions *)
    let is_only_resets_in_called_functions = List.map is_only_resets_in_called_function called_functions_list in
    (* Check if only reset in current code bloc and called functions *)
    (is_only_resets_in_parsed_seq_code_bloc variable_infos seq_code_bloc) && (List.fold_left (&&) true is_only_resets_in_called_functions)

(* Get local variables of a parsed function definition *)
let local_variables_of_parsed_fun_def (fun_def : parsed_fun_definition) =
    ParsingStructureUtilities.fold_parsed_fun_def
        (@)
        []
        ~decl_callback:(Some (fun (variable_name, discrete_type, id) -> [variable_name, discrete_type, id]))
        (fun _ -> [])
        (fun _ -> [])
        fun_def

(* Get local variables of a parsed sequential code bloc *)
let local_variables_of_parsed_seq_code_bloc seq_code_bloc =
    ParsingStructureUtilities.fold_parsed_seq_code_bloc
        (@)
        []
        ~decl_callback:(Some (fun (variable_name, discrete_type, id) -> [variable_name, discrete_type, id]))
        (fun _ -> [])
        (fun _ -> [])
        seq_code_bloc

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
                raise (UndefinedFunction function_name);
        | _ -> false
    in

    let is_seq_code_bloc_leaf_has_side_effects = function
        | Leaf_update_variable (variable_ref, _) ->
            (* Side effect occurs only when update a global variable *)
            VariableInfo.is_global variable_ref
    in

    ParsingStructureUtilities.exists_in_parsed_function_definition
        is_seq_code_bloc_leaf_has_side_effects (* Check if leaf of sequential code bloc has side effect *)
        is_leaf_has_side_effects (* Check if leaf has side effect *)
        fun_def