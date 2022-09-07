(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Functions that extract useful information on parsed model (dependency graph of variables / functions, etc.)
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
open VariableInfo
open Exceptions

type automaton_name = string
type variable_name = string
type param_name = string
type fun_name = string
type id = int

type local_variable_ref = variable_name * fun_name * id
type param_ref = param_name * fun_name

(* Reference to a program component *)
type component =
    | System_ref
    | Automaton_ref of automaton_name
    | Global_variable_ref of variable_name
    | Local_variable_ref of local_variable_ref
    | Param_ref of param_ref
    | Fun_ref of fun_name

(* Relation between two components a -> b mean a use b *)
type relation = component * component
(* Dependency graph as a list of relations between the components *)
type dependency_graph = component list (* declared components *) * relation list

(* A relations set *)
module RelationSet = Set.Make(struct
    type t = relation
    let compare a b = if a = b then 0 else (if a < b then -1 else 1)
end)

(* A components set *)
module ComponentSet = Set.Make(struct
    type t = component
    let compare a b = if a = b then 0 else (if a < b then -1 else 1)
end)



(** Utils **)

(* Try to get value of a discrete boolean expression, if directly a constant equals to false or true *)
(* If the expression is more complex, return None *)
let discrete_boolean_expression_constant_value_opt = function
    | Parsed_arithmetic_expression (Parsed_DAE_term (Parsed_DT_factor (Parsed_DF_constant (Bool_value v)))) -> Some v
    | _ -> None

(* Check if leaf is a constant *)
let is_constant variable_infos = function
    | Leaf_variable variable_name -> is_constant_is_defined variable_infos variable_name
    | Leaf_constant _ -> true
    | Leaf_fun _
    | Leaf_update_variable _ -> false

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
    | Leaf_update_variable _ -> true

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
        let variable_index = index_of_variable_name variable_infos variable_name in
        let discrete_type = variable_infos.type_of_variables variable_index in
        (match discrete_type with
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
    for_all_in_parsed_normal_update (is_variable_defined_with_callback variable_infos None undefined_variable_callback) expr

(* Check that all variables in a parsed update are effectively be defined *)
let all_variables_defined_in_parsed_update variable_infos undefined_variable_callback expr =
    for_all_in_parsed_update (is_variable_defined_with_callback variable_infos None undefined_variable_callback) expr

(* Check that all variables in a parsed fun declaration are effectively be defined *)
let all_variables_defined_in_parsed_fun_def variable_infos undefined_variable_callback (fun_def : parsed_fun_definition) =

    (* Add parameters as local variables *)
    let parameter_names = List.map first_of_tuple fun_def.parameters in
    let local_variables = List.fold_right StringSet.add parameter_names StringSet.empty in

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
        let leaf_fun = is_variable_defined_with_callback variable_infos (Some local_variables) undefined_variable_callback in
        for_all_in_parsed_normal_update leaf_fun (* expr *)
    in

    (* TODO benjamin REFACTOR replace by a general function in ParsingStructureUtilities *)
    (* Check if all variables defined in user function body using local variables set *)
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

        | Parsed_loop (variable_name, from_expr, to_expr, _, inner_bloc, next_expr, _) ->
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

        | Parsed_bloc_expr expr ->
            all_variables_defined_in_parsed_boolean_expression local_variables expr
        | Parsed_bloc_void -> true
    in
    all_variables_defined_in_parsed_seq_code_bloc_rec local_variables fun_def.body

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
    | Leaf_fun _
    | Leaf_update_variable _ -> ()
    | Leaf_variable variable_name ->
        (* Add the variable name to the set and update the reference *)
        variables_used_ref := StringSet.add variable_name !variables_used_ref

(* Gather all function names used in a discrete boolean expression *)
let add_function_of_discrete_boolean_expression function_used_ref = function
    | Leaf_constant _
    | Leaf_variable _
    | Leaf_update_variable _ -> ()
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
        (add_variable_of_discrete_boolean_expression variables_used_ref)

(* Gather all function names used in an update in a given accumulator *)
let get_functions_in_parsed_update_with_accumulator variables_used_ref =
    iterate_parsed_update
        (add_function_of_discrete_boolean_expression variables_used_ref)

(* Gather all variable names used in a normal update in a given accumulator *)
let get_variables_in_parsed_normal_update_with_accumulator variables_used_ref =
    iterate_parsed_normal_update
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

let get_variables_in_parsed_state_predicate =
    wrap_accumulator get_variables_in_parsed_state_predicate_with_accumulator

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

(* - --- - -- - *)


(*------------------------------------------------------------*)
(* Try to convert a non-linear expression to a linear *)
(* If it's not possible (due to non-linear expression involving clocks or parameters *)
(* we raise an InvalidExpression exception *)
(*------------------------------------------------------------*)

(* Try to convert parsed discrete term to a linear term *)
(* If it's not possible, we raise an InvalidExpression exception *)
let rec try_convert_linear_term_of_parsed_discrete_term = function
    | Parsed_product_quotient (term, factor, Parsed_mul) ->
        (* Check consistency of multiplication, if it keep constant we can convert to a linear term *)
        let linear_term, linear_factor =
            try_convert_linear_term_of_parsed_discrete_term term,
            try_convert_linear_term_of_parsed_discrete_factor factor
        in
        (match linear_term, linear_factor with
            (* Constant multiplied by constant, it's ok*)
            | Constant l_const_value, Constant r_const_value ->
                let value = NumConst.mul l_const_value r_const_value in
                Constant value
            (* Constant multiplied by a variable (commutative), it's ok *)
            | Variable (var_value, variable_name), Constant const_value
            | Constant const_value, Variable (var_value, variable_name) ->
                let value = NumConst.mul var_value const_value in
                Variable (value, variable_name)
            (* Other cases are non-linears, so it's impossible to make the conversion, we raise an exception *)
            | _ ->
                raise (InvalidExpression ("A non-linear arithmetic expression involve clock(s) / parameter(s)"))
        )
    | Parsed_product_quotient (term, factor, Parsed_div) ->
        (* Check consistency of division, if it keep constants we can convert to a linear term *)
        let linear_term, linear_factor =
        try_convert_linear_term_of_parsed_discrete_term term,
        try_convert_linear_term_of_parsed_discrete_factor factor
        in
        (match linear_term, linear_factor with
            (* Constant divided by constant, it's ok*)
            | Constant l_const_value, Constant r_const_value ->
                let value = NumConst.div l_const_value r_const_value in
                Constant value
            (* Other cases are non-linear, so it's impossible to make the conversion, we raise an exception *)
            | _ ->
                raise (InvalidExpression ("A non-linear arithmetic expression involve clock(s) / parameter(s)"))
        )
    (* Try to convert factor *)
    | Parsed_DT_factor parsed_discrete_factor -> try_convert_linear_term_of_parsed_discrete_factor parsed_discrete_factor

(* Try to convert parsed discrete arithmetic expression (non-linear expression) to a linear expression *)
(* If it's not possible, we raise an InvalidExpression exception *)
and try_convert_linear_expression_of_parsed_discrete_arithmetic_expression = function
    | Parsed_sum_diff (expr, term, sum_diff) ->
        let linear_expr, linear_term =
            try_convert_linear_expression_of_parsed_discrete_arithmetic_expression expr,
            try_convert_linear_term_of_parsed_discrete_term term
        in
        (match sum_diff with
        | Parsed_plus -> Linear_plus_expression (linear_expr, linear_term)
        | Parsed_minus ->  Linear_minus_expression (linear_expr, linear_term)
        )
    | Parsed_DAE_term term ->
        Linear_term (try_convert_linear_term_of_parsed_discrete_term term)

(* Try to convert parsed discrete factor to a linear term *)
(* If it's not possible, we raise an InvalidExpression exception *)
and try_convert_linear_term_of_parsed_discrete_factor = function
        | Parsed_DF_variable variable_name -> Variable(NumConst.one, variable_name)
        | Parsed_DF_constant value -> Constant (ParsedValue.to_numconst_value value)
        | Parsed_DF_unary_min parsed_discrete_factor ->
            (* Check for unary min, negate variable and constant *)
            (match parsed_discrete_factor with
                | Parsed_DF_variable variable_name -> Variable(NumConst.minus_one, variable_name)
                | Parsed_DF_constant value ->
                    let numconst_value = ParsedValue.to_numconst_value value in
                    Constant (NumConst.neg numconst_value)
                | _ -> try_convert_linear_term_of_parsed_discrete_factor parsed_discrete_factor
            )

        (* Nested expression used in a linear expression ! So it's difficult to make the conversion, we raise an exception *)
        | Parsed_DF_expression expr ->
            raise (InvalidExpression "A linear arithmetic expression has invalid format, maybe caused by nested expression(s)")

        | _ as factor ->
            raise (InvalidExpression ("Use of \"" ^ label_of_parsed_factor_constructor factor ^ "\" is forbidden in an expression involving clock(s) or parameter(s)"))

let try_convert_linear_expression_of_parsed_discrete_boolean_expression = function
    | Parsed_arithmetic_expression _ ->
        raise (InvalidExpression "An expression that involve clock(s) / parameter(s) contains a boolean variable")
    | Parsed_comparison (Parsed_arithmetic_expression l_expr, relop, Parsed_arithmetic_expression r_expr) ->
        Parsed_linear_constraint (
            try_convert_linear_expression_of_parsed_discrete_arithmetic_expression l_expr,
            relop,
            try_convert_linear_expression_of_parsed_discrete_arithmetic_expression r_expr
        )
    | Parsed_comparison (l_expr, relop, r_expr) ->
        raise (InvalidExpression "Use of non arithmetic comparison is forbidden in an expression that involve clock(s) / parameter(s)")
    (* Expression in used ! So it's impossible to make the conversion, we raise an exception*)
    | Parsed_comparison_in (_, _, _) -> raise (InvalidExpression "A boolean 'in' expression involve clock(s) / parameter(s)")
    | Parsed_boolean_expression _ -> raise (InvalidExpression "A non-convex predicate involve clock(s) / parameter(s)")
    | Parsed_Not _ -> raise (InvalidExpression "A not expression involve clock(s) / parameter(s)")

let linear_constraint_of_nonlinear_constraint = try_convert_linear_expression_of_parsed_discrete_boolean_expression

(* Gather all updates of update section (pre-updates, updates and post-updates) *)
let updates_of_update_section update_section =
    let seq_updates, updates = update_section in
    seq_updates @ updates



(* String of component *)
let string_of_component = function
    | System_ref -> "sys"
    | Automaton_ref x -> "auto_" ^ x
    | Global_variable_ref x -> x
    | Local_variable_ref (x, _, id) -> x ^ "_" ^ string_of_int id
    | Fun_ref x -> "fun_" ^ x
    | Param_ref (x, function_name) -> "param_" ^ x ^ "_of_" ^ function_name

(* String of component with attributes for DOT format *)
let string_of_component_with_attr = function
    | System_ref -> "sys [color=gray]"
    | Automaton_ref x -> "auto_" ^ x ^ "[color=red]"
    | Global_variable_ref x -> x
    | Local_variable_ref (x, _, id) -> x ^ "_" ^ string_of_int id ^ "[color=darkseagreen2]"
    | Fun_ref x -> "fun_" ^ x ^ "[color=darkseagreen][label=\"" ^ x ^ ":fun\"]"
    | Param_ref (x, function_name) -> "param_" ^ x ^ "_of_" ^ function_name ^ "[color=darkseagreen2][label=\"" ^ x ^ ":param\"]"

(* String of component name only *)
let string_of_component_name = function
    | System_ref -> ""
    | Automaton_ref x -> x
    | Global_variable_ref x -> x
    | Local_variable_ref (x, _, _) -> x
    | Fun_ref x -> x
    | Param_ref (x, _) -> x

(* Get the set of all variable names used in the parsed model *)
let all_components_used_in_automatons (parsed_model : ParsingStructure.parsed_model) =
	(* Create a set for components *)
	let all_relations = ref RelationSet.empty in

	(*** NOTE: we pass this set by reference ***)

	(* Gather in each automaton *)
	List.iter (fun (automaton_name, sync_name_list, locations) ->
		print_message Verbose_total ("      Gathering variables used in automaton " ^ automaton_name);
        let automaton_ref = Automaton_ref automaton_name in
        all_relations := RelationSet.add (System_ref, automaton_ref) !all_relations;
		(* Gather in each location *)
		List.iter (fun (location : parsed_location) ->
			print_message Verbose_total ("        Gathering variables used in location " ^ location.name);

			(* Gather in the cost *)
			begin
				match location.cost with
				| Some cost ->
				print_message Verbose_total ("          Gathering variables in used cost");
				ParsingStructureUtilities.iterate_parsed_linear_expression (function
                    | Leaf_linear_variable (_, variable_name) ->
                        all_relations := RelationSet.add (automaton_ref, Global_variable_ref variable_name) !all_relations
                    | Leaf_linear_constant _
                    | Leaf_false_linear_constraint
                    | Leaf_true_linear_constraint -> ()

				) cost;

				| None -> ()
			end;

			(* Gather in the stopwatches *)
			print_message Verbose_total ("          Gathering variables used in possible stopwatches");
			List.iter (fun stopwatch_name ->
				all_relations := RelationSet.add (automaton_ref, Global_variable_ref stopwatch_name) !all_relations
            ) location.stopped;

			(* Gather in the flows *)
			print_message Verbose_total ("          Gathering variables used in possible flows");
			List.iter (fun (clock_name, _) ->
				all_relations := RelationSet.add (automaton_ref, Global_variable_ref clock_name) !all_relations
            ) location.flow;

			(* Gather in the convex predicate *)
			print_message Verbose_total ("          Gathering variables in convex predicate");
			ParsingStructureUtilities.iterate_parsed_nonlinear_convex_predicate (function
                | Leaf_variable variable_name ->
                    all_relations := RelationSet.add (automaton_ref, Global_variable_ref variable_name) !all_relations
                | Leaf_fun function_name ->
                    all_relations := RelationSet.add (automaton_ref, Fun_ref function_name) !all_relations
                | Leaf_constant _
                | Leaf_update_variable _ -> ()
			) location.invariant;

			(* Gather in transitions *)
			print_message Verbose_total ("          Gathering variables in transitions");
			List.iter (fun (convex_predicate, update_section, (*sync*)_, (*target_location_name*)_) ->
				(* Gather in the convex predicate (guard) *)
				print_message Verbose_total ("            Gathering variables in convex predicate");
				ParsingStructureUtilities.iterate_parsed_nonlinear_convex_predicate (function
                    | Leaf_variable variable_name ->
                        all_relations := RelationSet.add (automaton_ref, Global_variable_ref variable_name) !all_relations
                    | Leaf_fun function_name ->
                        all_relations := RelationSet.add (automaton_ref, Fun_ref function_name) !all_relations
                    | Leaf_constant _
                    | Leaf_update_variable _ -> ()
                ) convex_predicate;

				(* Gather in the updates *)
				print_message Verbose_total ("            Gathering variables in updates");
				let updates = updates_of_update_section update_section in

				List.iter (fun update_expression ->
					(*** NOTE: let us NOT consider that a reset is a 'use' of a variable; it must still be used in a guard, an invariant, in the right-hand side term of a reset, or a property, to be considered 'used' in the model ***)
					ParsingStructureUtilities.iterate_parsed_update (function
                        | Leaf_variable variable_name ->
                            all_relations := RelationSet.add (automaton_ref, Global_variable_ref variable_name) !all_relations
                        | Leaf_fun function_name ->
                            all_relations := RelationSet.add (automaton_ref, Fun_ref function_name) !all_relations
                        | Leaf_constant _
                        | Leaf_update_variable _ -> ()
					) update_expression;

                ) updates;

                (* Only for seq updates *)
                let seq_updates, _ = update_section in

                (* In seq update an assigned variable is considered as used (even if not used in any guard / invariant) *)
                (* For example r := <anything>, r is considered as used *)
                (* We make this choice for sake of simplicity, because seq updates can contains side effect functions that should be executed *)
                (* eg: r := stack_pop(s) *)
				List.iter (fun update_expression ->
					ParsingStructureUtilities.iterate_parsed_update
					    (function
                            | Leaf_variable _
                            | Leaf_constant _
                            | Leaf_fun _ -> ()
					        | Leaf_update_variable variable_name ->
                                all_relations := RelationSet.add (automaton_ref, Global_variable_ref variable_name) !all_relations
                        )
                    update_expression;
                ) seq_updates;

            ) location.transitions;
        ) locations;

    ) parsed_model.automata;

    RelationSet.to_seq !all_relations |> List.of_seq

(**)
let declared_components_of_model parsed_model =

    (* Get all declared variables in model *)
    let all_declared_variables_in_model =
        List.map (fun (_, variables_list) ->
            List.map (fun (variable_name, _) -> Global_variable_ref variable_name) variables_list
        ) parsed_model.variable_declarations
        (* Flatten list of list of variable components *)
        |> List.flatten
    in

    (* Get all declared function in model *)
    let all_declared_functions_in_model =
        List.map (fun (fun_def : parsed_fun_definition) -> Fun_ref fun_def.name) parsed_model.fun_definitions
    in

    (* Get all declared local variables in a given function definition *)
    let all_declared_local_variables_in_fun_def (fun_def : parsed_fun_definition) =
        ParsingStructureUtilities.fold_parsed_function_definition
            (@) (* operator concat list *)
            [] (* base *)
            (function Leaf_decl_variable (variable_name, _, id) -> [Local_variable_ref (variable_name, fun_def.name, id)])
            (function _ -> [])
            fun_def
    in

    (* Get all declared parameters in a given function definition *)
    let all_declared_params_in_fun_def (fun_def : parsed_fun_definition) =
        List.fold_left (fun acc (variable_name, _) -> Param_ref (variable_name, fun_def.name) :: acc) [] fun_def.parameters
    in

    let all_declared_local_variables_in_model =
        List.fold_left (fun acc fun_def -> all_declared_local_variables_in_fun_def fun_def @ acc) [] parsed_model.fun_definitions
    in

    let ss = List.map (function Local_variable_ref (variable_name, _, _) -> variable_name) all_declared_local_variables_in_model in
    let sss = OCamlUtilities.string_of_list_of_string_with_sep "," ss in
    print_standard_message ("ParsingStructureMeta: " ^ sss);

    let all_declared_params_in_model =
        List.fold_left (fun acc fun_def -> all_declared_params_in_fun_def fun_def @ acc) [] parsed_model.fun_definitions
    in

    all_declared_variables_in_model @ all_declared_functions_in_model @ all_declared_local_variables_in_model @ all_declared_params_in_model

(* Get a dependency graph as a list of relations between variables and functions *)
(* Each relation is a pair representing a ref to a variable / function using another variable / function *)
let dependency_graph ?(no_var_autoremove=false) parsed_model =

    (* Function that return all component relations of a given function definition *)
    let function_relations fun_def =

        (* Get parameter names *)
        let parameter_names = List.map first_of_tuple fun_def.parameters in
        (* Add parameter names to local variables of function *)
        let local_variables = List.fold_right (fun parameter_name acc -> StringMap.add parameter_name (Param_ref (parameter_name, fun_def.name)) acc) parameter_names StringMap.empty in
        (* Ref to function *)
        let fun_ref = Fun_ref fun_def.name in

        (* Functions that return ref of a variable, if variable is found in local variable table *)
        (* It return a Local_variable_ref, else a Global_variable_ref *)
        let get_variable_ref local_variables used_variable_name =
            let used_variable_ref_opt = StringMap.find_opt used_variable_name local_variables in
            match used_variable_ref_opt with
            | Some used_variable_ref -> used_variable_ref
            | None -> Global_variable_ref used_variable_name
        in

        (* Create relations between a set of variables used by another variable reference *)
        let variable_to_variable_relations local_variables variable_ref variables_used =
            StringSet.fold (fun used_variable_name acc ->
                let used_variable_ref = get_variable_ref local_variables used_variable_name in
                (variable_ref, used_variable_ref) :: acc
            ) variables_used []
        in

        (* Function that return component reference found in a parsed global expression *)
        let get_variable_and_function_refs_in_parsed_boolean_expression local_variables expr =
            (* Get variables used in the local init expression of the variable *)
            let variables_used = string_set_to_list (get_variables_in_parsed_boolean_expression expr) in
            let variables_used_refs = List.map (get_variable_ref local_variables) variables_used in
            (* Get functions used in the local init expression of the variable *)
            let functions_used = string_set_to_list (get_functions_in_parsed_boolean_expression expr) in
            let functions_used_refs = List.map (fun function_name -> Fun_ref function_name) functions_used in
            (* Get refs *)
            variables_used_refs, functions_used_refs
        in

        (* Function that return component reference found in a parsed arithmetic expression *)
        let get_variable_and_function_refs_in_parsed_arithmetic_expression local_variables expr =
            (* Get variables used in the local init expression of the variable *)
            let variables_used = string_set_to_list (get_variables_in_parsed_discrete_arithmetic_expression expr) in
            let variables_used_refs = List.map (get_variable_ref local_variables) variables_used in
            (* Get functions used in the local init expression of the variable *)
            let functions_used = string_set_to_list (get_functions_in_parsed_discrete_arithmetic_expression expr) in
            let functions_used_refs = List.map (fun function_name -> Fun_ref function_name) functions_used in
            (* Get refs *)
            variables_used_refs, functions_used_refs
        in

        (* Function that return all component relations of a given function expression *)
        let rec function_relations_in_parsed_seq_code_bloc_rec local_variables = function
            | Parsed_local_decl (variable_name, _, init_expr, next_expr, id) ->

                (* Create local variable ref representing a unique variable ref *)
                let variable_ref = Local_variable_ref (variable_name, fun_def.name, id) in
                (* Add the new declared local variable (or update if the new declaration shadows a previous one) *)
                let local_variables = StringMap.update variable_name (function None -> Some variable_ref | Some _ -> Some variable_ref) local_variables in

                (* Get variable and function refs used in the local init expression *)
                let variables_used_refs, functions_used_refs = get_variable_and_function_refs_in_parsed_boolean_expression local_variables init_expr in
                let all_refs = variables_used_refs @ functions_used_refs in
                let relations = List.map (fun _ref -> (variable_ref, _ref)) all_refs in

                (* Add a relation between current function and declared variable *)
                (* The declared variable will always be considered as used here *)
                let relations = (fun_ref, variable_ref) :: relations in
                (* Old behavior (deprecated) *)
                (*
                (* Add a relation between current function and declared variable, if -no-var-autoremove option is set *)
                let relations =
                    if no_var_autoremove then
                        (fun_ref, variable_ref) :: relations
                    else
                        relations
                in
                *)

                (* Get list of relations for the next expression / declaration *)
                let next_declaration_relations = function_relations_in_parsed_seq_code_bloc_rec local_variables next_expr in
                (* Concat current relations with next relations *)
                next_declaration_relations @ relations

            | Parsed_assignment ((parsed_update_type, expr), next_expr) ->

                let rec relations_of_scalar_or_index_update_type = function
                    | Parsed_scalar_update variable_name ->
                        (* Updated variable use all variables found in expression *)
                        (* For example x := a + b, x use a, b *)
                        (* and current function use x *)

                        (* Create local variable ref representing a unique variable ref *)
                        let variable_ref = get_variable_ref local_variables variable_name in

                        (* Get variable and function refs used in the update expression *)
                        let variables_used_refs, functions_used_refs = get_variable_and_function_refs_in_parsed_boolean_expression local_variables expr in
                        let all_refs = variables_used_refs @ functions_used_refs in
                        let relations = List.map (fun _ref -> (variable_ref, _ref)) all_refs in

                        (* Here for sake of simplicity, we consider that all global variables modified in function are used (it's not always the case) *)

                        (* Create relation between current function and assigned variable *)
                        let cur_fun_used_variable_relation = fun_ref, variable_ref in
                        (* Concat all relations *)
                        cur_fun_used_variable_relation :: relations

                    | Parsed_indexed_update (inner_scalar_or_index_update_type, index_expr) ->
                        (* Updated variable use all variables found in expression, and all variables found in index *)
                        (* For example x[y + z] = a + b, x use y, z, a, b *)
                        (* and current function use x *)
                        let variable_name = variable_name_of_parsed_scalar_or_index_update_type inner_scalar_or_index_update_type in

                        (* Create local variable ref representing a unique variable ref *)
                        let variable_ref = get_variable_ref local_variables variable_name in

                        (* Get variables / functions used in the indexed expression of the variable *)
                        let variables_used = string_set_to_list (get_variables_in_parsed_discrete_arithmetic_expression index_expr) in
                        (* Get functions used in the local init expression of the variable *)
                        let functions_used = string_set_to_list (get_functions_in_parsed_discrete_arithmetic_expression index_expr) in

                        let variables_used_refs = List.map (get_variable_ref local_variables) variables_used in
                        let functions_used_refs = List.map (fun function_name -> Fun_ref function_name) functions_used in
                        let all_refs = variables_used_refs @ functions_used_refs in
                        let relations = List.map (fun _ref -> (variable_ref, _ref)) all_refs in

                        let variable_use_variables_relations = relations_of_scalar_or_index_update_type inner_scalar_or_index_update_type in
                        relations @ variable_use_variables_relations


                in
                let relations_of_update_type = function
                    | Parsed_variable_update parsed_scalar_or_index_update_type ->
                        relations_of_scalar_or_index_update_type parsed_scalar_or_index_update_type
                    | Parsed_void_update ->
                        (* All variables found in expression are used by current function *)
                        (* For example: stack_pop(s) + x + y *)
                        (* Current function 'f' use s, x, y *)

                        (* Get variables used in update expression *)
                        let variables_used = get_variables_in_parsed_boolean_expression expr in
                        let functions_used = get_functions_in_parsed_boolean_expression expr in

                        let fun_use_variables_relations = variable_to_variable_relations local_variables fun_ref variables_used in

                        let fun_use_fun_relations = StringSet.fold (fun used_function_name acc ->
                            (fun_ref, Fun_ref used_function_name) :: acc
                        ) functions_used []
                        in
                        fun_use_variables_relations @ fun_use_fun_relations

                in
                let relations = relations_of_update_type parsed_update_type in

                (* Get list of relations for the next expression / declaration *)
                let next_declaration_relations = function_relations_in_parsed_seq_code_bloc_rec local_variables next_expr in
                (* Concat current relations with next relations *)
                relations @ next_declaration_relations

            | Parsed_loop (variable_name, from_expr, to_expr, _, inner_bloc, next_expr, id) ->
                (* Create local variable ref representing a unique variable ref *)
                let variable_ref = Local_variable_ref (variable_name, fun_def.name, id) in
                (* Add the new declared local variable (or update if the new declaration shadows a previous one) *)
                let local_variables_of_loop = StringMap.update variable_name (function None -> Some variable_ref | Some _ -> Some variable_ref) local_variables in

                (* Get variable and function refs used in the from expression *)
                let variables_used_refs, functions_used_refs = get_variable_and_function_refs_in_parsed_arithmetic_expression local_variables_of_loop from_expr in
                let all_refs = variables_used_refs @ functions_used_refs in
                (* Get variable and function refs used in the to expression *)
                let variables_used_refs, functions_used_refs = get_variable_and_function_refs_in_parsed_arithmetic_expression local_variables to_expr in
                let all_refs = all_refs @ variables_used_refs @ functions_used_refs in

                let relations = List.map (fun _ref -> (variable_ref, _ref)) all_refs in

                (* Add a relation between current function and declared variable *)
                (* The declared variable will always be considered as used here *)
                let relations = (fun_ref, variable_ref) :: relations in

                (* Get list of relations for the inner expressions *)
                let inner_declaration_relations = function_relations_in_parsed_seq_code_bloc_rec local_variables inner_bloc in
                (* Get list of relations for the next expression / declaration *)
                let next_declaration_relations = function_relations_in_parsed_seq_code_bloc_rec local_variables next_expr in
                (* Concat current relations with next relations *)
                inner_declaration_relations @ next_declaration_relations @ relations

            | Parsed_bloc_expr expr ->
                (* Get variable and function refs used in the expression *)
                let variables_used_refs, functions_used_refs = get_variable_and_function_refs_in_parsed_boolean_expression local_variables expr in
                let all_refs = variables_used_refs @ functions_used_refs in
                List.map (fun _ref -> (fun_ref, _ref)) all_refs

            | Parsed_bloc_void -> []
        in
        (* Get all component relations of current function body *)
        function_relations_in_parsed_seq_code_bloc_rec local_variables fun_def.body
    in
    (* Get variables and functions used by automatons *)
    let automatons_relations =
        all_components_used_in_automatons parsed_model
    in
    (* Get variables and functions used in all declared functions *)
    let system_functions_relations =
        List.fold_left (fun acc fun_def -> function_relations fun_def @ acc) [] parsed_model.fun_definitions
    in

    (* Get variables and functions dependencies in init *)
    let init_relations =
        List.fold_left (fun acc init ->
            match init with
            (* `loc[automaton] = location`: no variable => nothing to do *)
            | Parsed_loc_assignment _
            (* Linear predicate are true or false constraint => nothing to do*)
            | Parsed_linear_predicate Parsed_true_constraint
            | Parsed_linear_predicate Parsed_false_constraint
            (* Special form `variable ~ constant` => in this case we assume NOT used *)
            | Parsed_linear_predicate (Parsed_linear_constraint (Linear_term (Variable _), _, Linear_term (Constant _))) -> acc
            (* *)
            | Parsed_discrete_predicate (variable_name, expr) ->
                let used_variable_names = get_variables_in_parsed_boolean_expression expr in
                let used_variable_names_list = string_set_to_list used_variable_names in
                List.map (fun used_variable_name -> Global_variable_ref variable_name, Global_variable_ref used_variable_name) used_variable_names_list
            (* Linear constraint: get variables *)
            | Parsed_linear_predicate (Parsed_linear_constraint (l_expr, _, r_expr)) ->
                let left_hand_variables = string_set_to_list (get_variables_in_linear_expression l_expr) in
                let right_hand_variables = string_set_to_list (get_variables_in_linear_expression r_expr) in
                let left_and_right_variables = left_hand_variables @ right_hand_variables in
                let combination = OCamlUtilities.list_combination left_and_right_variables left_and_right_variables in
                List.map (fun (l_variable_name, r_variable_name) -> Global_variable_ref l_variable_name, Global_variable_ref r_variable_name) combination @ acc
        ) [] parsed_model.init_definition
    in
    (* Concat all relations, to get overall relations of the model *)
    let all_model_relations = automatons_relations @ system_functions_relations @ init_relations in
    (* Remove variable to variable relations when it's an auto reference *)
    let all_model_relations_without_variable_autoref = List.filter (function
        | (Global_variable_ref _ as a, (Global_variable_ref _ as b)) -> a <> b
        | (Local_variable_ref _ as a, (Local_variable_ref _ as b)) -> a <> b
        | _ -> true
    ) all_model_relations in
    (* Return dependency graph of the model *)
    declared_components_of_model parsed_model, all_model_relations_without_variable_autoref

(* Get all components that are effectively used by automatons of the model *)
(* It mean all components that are reachable starting from the system reference *)
let used_components_of_model_list (_, component_relations) =

    (* A set that will contain already processed references, to avoid circular *)
    let already_processed_ref = ref RelationSet.empty in

    (* Function that compute all reachable refs given a source ref *)
    let rec all_reachable_ref relation =

        (* Decompose relation *)
        let variable_ref, used_variable_ref = relation in

        let already_processed = !already_processed_ref in

        (* Check if relation was already processed *)
        (* Avoid that circular references make an infinite loop *)
        if RelationSet.mem relation already_processed then [] else (
            already_processed_ref := RelationSet.add relation already_processed;
            (* Get destination refs as new source starting from the current ref *)
            let source_refs = List.filter (fun (a, b) -> a = used_variable_ref) component_relations in
            (* Compute destination refs *)
            let dest_refs = List.fold_left (fun acc s -> all_reachable_ref s @ acc) [] source_refs in
            (* Add current ref with computed destination refs *)
            used_variable_ref :: dest_refs
        )
    in

    (* Get system refs *)
    let system_refs = List.filter (fun (s, d) -> match s with Automaton_ref _ -> true | _ -> false) component_relations in
    (* Find all reachable refs (variables / functions) from system refs... *)
    List.fold_left (fun acc system_ptr -> all_reachable_ref system_ptr @ acc) [] system_refs

(* Get all components that are effectively used by automatons of the model *)
(* It mean all components that are reachable starting from the system reference *)
let used_components_of_model dependency_graph = used_components_of_model_list dependency_graph |> List.to_seq |> ComponentSet.of_seq

(* Get all declared components of model *)
let components_of_model (components, _) = components |> List.to_seq |> ComponentSet.of_seq

(* Get all components that are not used by automatons of the model *)
(* It mean all components that are not reachable starting from the system reference *)
let unused_components_of_model dependency_graph =
    (* Get declared components from dependency graph *)
    let declared_components, _ = dependency_graph in
    (* Compute used components from dependency graph *)
    let used_components = used_components_of_model_list dependency_graph in
    (* Convert to sets *)
    let declared_components_set = declared_components |> List.to_seq |> ComponentSet.of_seq in
    let used_components_set = used_components |> List.to_seq |> ComponentSet.of_seq in
    (* Make the diff *)
    let unused_components = ComponentSet.diff
        (declared_components_set)
        (used_components_set)
    in
    (* Remove parameters and local variables from unused components if the function to which they belong is unused *)
    (* Note that it's useless to print message `param x of f` is unused if the function `f` is unused *)
    ComponentSet.filter (function
        | Param_ref (_, function_name)
        | Local_variable_ref (_, function_name, _) ->
            not (ComponentSet.mem (Fun_ref function_name) unused_components)
        | _ -> true
    ) unused_components


let unused_components_of_model_list dependency_graph =
    unused_components_of_model dependency_graph |> ComponentSet.to_seq |> List.of_seq

(* General function to filter and map components used in model *)
let filter_map_components_from component_function dependency_graph filter_map =
    (* Get all used components *)
    let used_components = component_function dependency_graph in
    (* Extract something from all used components *)
    let used_component_names = List.filter_map filter_map used_components in
    (* Convert to StringSet, remove duplicates *)
    list_to_string_set used_component_names

(* General function to filter and map components used in model *)
let filter_map_components_used_in_model = filter_map_components_from used_components_of_model_list
(* General function to filter and map components unused in model *)
let filter_map_components_unused_in_model = filter_map_components_from unused_components_of_model_list

(* Get the names of all used global variable *)
let used_variables_of_model dependency_graph =
    filter_map_components_used_in_model dependency_graph (function Global_variable_ref variable_name -> Some variable_name | _ -> None)

(* Get the names of all unused global variable *)
let unused_variables_of_model dependency_graph =
    filter_map_components_unused_in_model dependency_graph (function Global_variable_ref variable_name -> Some variable_name | _ -> None)

(* Get the names of all function used *)
let used_functions_of_model dependency_graph =
    filter_map_components_used_in_model dependency_graph (function Fun_ref name -> Some name | _ -> None)

(* Get the names of all function unused *)
let unused_functions_of_model dependency_graph =
    filter_map_components_unused_in_model dependency_graph (function Fun_ref name -> Some name | _ -> None)



let model_cycle_infos (_, model_relations) =

    let rec is_cycle_in already_seen c =

        let is_fun_ref = function
            | Fun_ref _ -> true
            | _ -> false
        in

        if List.mem c already_seen then (
            if is_fun_ref c then (
                [true, c :: already_seen]
            )
            else (
                []
            )
        )
        else (
            let next_components = List.filter_map (function (src, dst) when src = c -> Some dst | _ -> None) model_relations in
            List.map (is_cycle_in (c :: already_seen)) next_components |> List.flatten
        )
    in

    let system_components = List.filter_map (function (System_ref, dst) -> Some dst | _ -> None) model_relations in
    let cycle_infos = List.map (is_cycle_in []) system_components |> List.flatten in
    (* Transform path list to str path *)
    List.map (fun (has_cycle, path) ->
        let str_path_list = List.map string_of_component_name path |> List.rev in
        let str_path = OCamlUtilities.string_of_list_of_string_with_sep " -> " str_path_list in
        has_cycle, str_path
    ) cycle_infos

(* Get dependency graph as string (dot graphviz format) (generate png of this graph with the cmd: dot -Tpng file.dot file.png) *)
let string_of_dependency_graph (components, component_relations) =
    (* String of each components *)
    let str_components_list = List.map string_of_component_with_attr components in
    let str_components = OCamlUtilities.string_of_list_of_string_with_sep ";" str_components_list in
    (* String of relation between two components *)
    let string_of_component_relation (a, b) = string_of_component a ^ " -> " ^ string_of_component b in
    (* Map all relations to strings *)
    let str_relations_list = List.map string_of_component_relation component_relations in
    (* Concat to string *)
    let str_component_relations = OCamlUtilities.string_of_list_of_string_with_sep "; " str_relations_list in
    (* Dependency graph as dot format *)
    "digraph dependency_graph {" ^ str_components ^ ";" ^ str_component_relations ^ "}"

(* Utils function for traversing a parsed_fun_definition *)
let traverse_function operator f base (fun_def : parsed_fun_definition) =
    (* Add parameters as local variables *)
    let parameter_refs = List.map (fun (param_name, _) -> Param_ref (param_name, fun_def.name)) fun_def.parameters in
    let local_variable_components = List.fold_right ComponentSet.add parameter_refs ComponentSet.empty in
    (* Create ref of local variable components set *)
    let local_variable_components_ref = ref local_variable_components in

    (* TODO benjamin see if can replace by general function *)
    (* Function that traverse function body expression *)
    let rec traverse_parsed_seq_code_bloc = function
        | Parsed_local_decl (variable_name, _, _, next_expr, id) as expr ->
            (* Add the new declared local variable to set *)
            let local_variable_ref = Local_variable_ref (variable_name, fun_def.name, id) in
            local_variable_components_ref := ComponentSet.add local_variable_ref !local_variable_components_ref;

            operator
                (f !local_variable_components_ref expr)
                (traverse_parsed_seq_code_bloc next_expr)

        | Parsed_loop (variable_name, _, _, _, inner_bloc, next_expr, id) as expr ->
            (* Add the new declared local variable to set *)
            let local_variable_ref = Local_variable_ref (variable_name, fun_def.name, id) in
            local_variable_components_ref := ComponentSet.add local_variable_ref !local_variable_components_ref;

            operator
                (operator
                    (f !local_variable_components_ref expr)
                    (traverse_parsed_seq_code_bloc inner_bloc)
                )
                (traverse_parsed_seq_code_bloc next_expr)

        | Parsed_assignment (_, next_expr) as expr ->
            operator
                (f !local_variable_components_ref expr)
                (traverse_parsed_seq_code_bloc next_expr)

        | Parsed_bloc_expr _ as expr ->
            f !local_variable_components_ref expr
        | Parsed_bloc_void -> base
    in
    traverse_parsed_seq_code_bloc fun_def.body



(* Get all variables (local and global) at the left side of an assignment in a function body implementation *)
let left_variables_of_assignments_in (fun_def : parsed_fun_definition) =

    (* Create set of assigned variables *)
    let components_ref = ref ComponentSet.empty in

    (* Function that get assigned variable in function body expression *)
    let rec left_variables_of_assignments_in_parsed_seq_code_bloc local_variable_components = function
        | Parsed_loop _
        | Parsed_local_decl _ -> ()

        | Parsed_assignment ((parsed_update_type, _), next_expr) ->

            let variable_name_opt = variable_name_of_parsed_update_type_opt parsed_update_type in
            (match variable_name_opt with
            | Some variable_name ->
                (* Check existence of any local variable / parameter that may shadow global variable of the same name *)
                let local_variable_component_opt = ComponentSet.find_first_opt (function
                    | Param_ref (v, _)
                    | Local_variable_ref (v, _, _) -> variable_name = v
                    | _ -> false
                ) local_variable_components
                in

                let component =
                    match local_variable_component_opt with
                    (* If any local variable shadow a global variable, get it's reference *)
                    | Some local_variable_component -> local_variable_component
                    (* Else it's possibly an update of a global variable (or an nonexistent variable) *)
                    | None -> Global_variable_ref variable_name
                in
                components_ref := ComponentSet.add component !components_ref

            | None -> ()
            );
        | Parsed_bloc_expr _
        | Parsed_bloc_void -> ()
    in
    traverse_function bin_unit left_variables_of_assignments_in_parsed_seq_code_bloc () fun_def;
    !components_ref

let variable_ref_of local_variable_components variable_name =
    (* Check existence of any local variable / parameter that may shadow global variable of the same name *)
    let local_variable_component_opt = ComponentSet.find_first_opt (function
        | Param_ref (v, _)
        | Local_variable_ref (v, _, _) -> variable_name = v
        | _ -> false
    ) local_variable_components
    in

    match local_variable_component_opt with
    (* If any local variable shadow a global variable, get it's reference *)
    | Some local_variable_component -> local_variable_component
    (* Else it's possibly an update of a global variable (or an nonexistent variable) *)
    | None -> Global_variable_ref variable_name

(* Get all variables (local and global) at the right side of an assignment in a function body implementation *)
let right_variables_of_assignments_in (fun_def : parsed_fun_definition) =

    (* Create set of assigned variables *)
    let component_refs = ref ComponentSet.empty in

    (* Function that get assigned variable in function body expression *)
    let right_variables_of_assignments_in_parsed_seq_code_bloc local_variable_components = function
        | Parsed_local_decl (_, _, expr, _, _)
        | Parsed_assignment ((_, expr), _) ->
            let variable_names = string_set_to_list (get_variables_in_parsed_boolean_expression expr) in
            let variable_refs = List.map (variable_ref_of local_variable_components) variable_names in
            component_refs := List.fold_left (Fun.flip ComponentSet.add) !component_refs variable_refs

        | Parsed_loop _
        | Parsed_bloc_expr _
        | Parsed_bloc_void -> ()
    in
    traverse_function bin_unit right_variables_of_assignments_in_parsed_seq_code_bloc () fun_def;
    !component_refs



