(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: This module aims to convert a parsed expression to a abstract typed expression
 *
 * File contributors : Benjamin L.
 * Created           : 2021/11/20
 *
 ************************************************************)

(* Utils modules *)
open Constants
open Exceptions
open OCamlUtilities
open ImitatorUtilities

(* Parsing structure modules *)
open ParsingStructure
open ParsedModelMetadata
open DiscreteType

(* Abstract model modules *)
open AbstractModel
open DiscreteExpressions
open ExpressionConverter.Convert


let convert_discrete_init variable_infos variable_name expr =
    (* Get typed expression *)
    let typed_expr = ExpressionConverter.TypeChecker.check_discrete_init variable_infos variable_name expr in
    (* Print *)
(*    ImitatorUtilities.print_message Verbose_standard (ExpressionConverter.TypeChecker.string_of_typed_global_expression variable_infos typed_expr);*)
    (* Convert *)
    ExpressionConverter.Convert.global_expression_of_typed_global_expression variable_infos typed_expr

let convert_discrete_constant initialized_constants (name, expr, var_type) =
    (* Create fake variable_infos containing just initialized constants *)
    let variable_infos = {
        constants = initialized_constants;
        variables = [||];
        variable_names = [];
        index_of_variables = Hashtbl.create 0;
        removed_variable_names = [];
        type_of_variables = (fun _ -> raise (TypeError "oops!"));
        discrete = [];
        functions = Hashtbl.create 0;
    }
    in

    let typed_expr = ExpressionConverter.TypeChecker.check_constant_expression variable_infos (name, expr, var_type) in
    ExpressionConverter.Convert.global_expression_of_typed_global_expression variable_infos typed_expr



(*------------------------------------------------------------*)
(* Convert a guard *)
(*------------------------------------------------------------*)
let nonlinear_constraint_of_convex_predicate variable_infos guard =
    (* Type check guard *)
    let typed_guard = ExpressionConverter.TypeChecker.check_guard variable_infos guard in

(*    let str_typed_nonlinear_constraints = List.map (string_of_typed_discrete_boolean_expression variable_infos) typed_guard in*)
(*    let str = OCamlUtilities.string_of_list_of_string_with_sep "\n & " str_typed_nonlinear_constraints in*)
(*    ImitatorUtilities.print_message Verbose_standard str;*)

    (* Convert *)
    let converted_nonlinear_constraints = List.rev_map (ExpressionConverter.Convert.nonlinear_constraint_of_typed_nonlinear_constraint variable_infos) typed_guard in

    (* Try reduce *)
    converted_nonlinear_constraints
    (* TODO benjamin IMPORTANT here add reducing with Some *)
    (*
    let reduced_nonlinear_constraints = converted_nonlinear_constraints in
    (match reduced_nonlinear_constraints with
    | Some true -> True_guard
    | Some false -> False_guard
    | None -> Discrete_guard (reduced_nonlinear_constraints)
    )
    *)

(* Split convex_predicate into two lists *)
(* One only contain discrete expression to nonlinear_constraint *)
(* One that doesn't only contain discrete expression to linear_constraint *)
let split_convex_predicate_into_discrete_and_continuous variable_infos convex_predicate =
  (* Compute a list of inequalities *)
  let partitions = List.partition
    (fun nonlinear_inequality ->
       match nonlinear_inequality with
       (* TODO benjamin REFACTOR, in ParsingStructureUtilities create a function that check if a nonlinear constraint is true or false *)
       | Parsed_arithmetic_expression (Parsed_DAE_term (Parsed_DT_factor (Parsed_DF_constant v))) when DiscreteValue.bool_value v = true -> true
       | Parsed_arithmetic_expression (Parsed_DAE_term (Parsed_DT_factor (Parsed_DF_constant v))) when DiscreteValue.bool_value v = false -> raise False_exception
       | nonlinear_constraint -> ParsingStructureUtilities.only_discrete_in_nonlinear_expression variable_infos nonlinear_constraint
    ) convex_predicate
    in
    (* Get discrete part as a nonlinear constraint but convert back continuous part to a linear constraint *)
    let discrete_part, continuous_part = partitions in
        discrete_part,
        List.map (fun nonlinear_constraint -> ParsingStructureUtilities.linear_constraint_of_nonlinear_constraint nonlinear_constraint) continuous_part


let convert_guard variable_infos guard_convex_predicate =
    try (

        (* Separate the guard into a discrete guard (on discrete variables) and a continuous guard (on all variables) *)
        let discrete_guard_convex_predicate, continuous_guard_convex_predicate = split_convex_predicate_into_discrete_and_continuous variable_infos guard_convex_predicate in

        match discrete_guard_convex_predicate, continuous_guard_convex_predicate with
        (* No inequalities: true *)
        | [] , [] -> True_guard
        (* Only discrete inequalities: discrete *)
        | discrete_guard_convex_predicate , [] ->
            Discrete_guard (
                nonlinear_constraint_of_convex_predicate variable_infos discrete_guard_convex_predicate
            )

        (* Only continuous inequalities: continuous *)
        | [] , continuous_guard_convex_predicate ->
            Continuous_guard (
                ExpressionConverter.Convert.linear_constraint_of_convex_predicate variable_infos continuous_guard_convex_predicate
            )

        (* Otherwise: both *)
        | discrete_guard_convex_predicate , continuous_guard_convex_predicate ->
            (* Convert both parts *)
            let discrete_guard = nonlinear_constraint_of_convex_predicate variable_infos discrete_guard_convex_predicate in
            let continuous_guard = linear_constraint_of_convex_predicate variable_infos continuous_guard_convex_predicate in

            (* TODO benjamin, check if optimization is possible *)
            (* NOTE : This optimization (below) was possible when discrete part use only rational-valued variables
               I don't think that it's possible anymore *)

            (*** NOTE: try to simplify a bit if possible (costly, but would save a lot of time later if checks are successful) ***)
            (*      let intersection = LinearConstraint.pxd_intersection_with_d continuous_guard discrete_guard in*)

            (*      if LinearConstraint.pxd_is_true intersection then True_guard*)
            (*      else if LinearConstraint.pxd_is_false intersection then False_guard*)
            (*      else*)
            (* Else create mixed guard as planned *)
            Discrete_continuous_guard
            {
                discrete_guard = discrete_guard;
                continuous_guard = continuous_guard;
            }

    (* If some false construct found: false guard *)
    ) with False_exception -> False_guard

let convert_update variable_infos updates_type parsed_update_type expr =
    let typed_update_type, typed_expr = ExpressionConverter.TypeChecker.check_update variable_infos updates_type parsed_update_type expr in
    ExpressionConverter.Convert.update_type_of_typed_update_type variable_infos typed_update_type,
    ExpressionConverter.Convert.global_expression_of_typed_global_expression variable_infos typed_expr

let convert_continuous_update variable_infos parsed_scalar_or_index_update_type expr =
    let parsed_update_type = Parsed_variable_update parsed_scalar_or_index_update_type in
    let typed_update_type, typed_expr = ExpressionConverter.TypeChecker.check_update variable_infos Parsed_std_updates parsed_update_type expr in
    ExpressionConverter.Convert.update_type_of_typed_update_type variable_infos typed_update_type,
    ExpressionConverter.Convert.linear_term_of_typed_global_expression variable_infos typed_expr

let convert_conditional variable_infos expr =
    (* Check *)
    let typed_expr = ExpressionConverter.TypeChecker.check_conditional variable_infos expr in
    (* Convert *)
    ExpressionConverter.Convert.bool_expression_of_typed_boolean_expression variable_infos typed_expr

(* Check if user function definition is well formed *)
let check_fun_definition variable_infos (fun_def : parsed_fun_definition) =

    (* Check if there isn't duplicate parameter with inconsistent types *)
    let is_consistent_duplicate_parameters =

        (* Message to display when duplicate parameters found *)
        let duplicate_parameter_message parameter_name =
            "Duplicate parameter `"
            ^ parameter_name
            ^ "` in function `"
            ^ fun_def.name
            ^ "`"
        in

        (* Check that each parameter have different name *)
        (* Group parameters by their names *)
        let parameters_by_names = OCamlUtilities.group_by first_of_tuple fun_def.parameters in
        (* If for one parameter name, their is more than one parameter, there is duplicates *)
        let duplicate_parameters = List.filter (fun (parameter_name, group) -> List.length group > 1) parameters_by_names in

        (* For each parameter get if duplicate definitions are consistent or not *)
        (* Ex: for fn f (a : int, a : rat), duplicate definition of `a` isn't consistent *)
        List.iter (fun (parameter_name, group) ->
            (* Remove parameter duplicates *)
            let group_without_duplicates = OCamlUtilities.list_only_once group in
            (* Prepare message *)
            let current_duplicate_parameter_message = duplicate_parameter_message parameter_name in
            (* If duplicates remain greater than 1, there is inconsistent definitions *)
            if List.length group_without_duplicates = 1 then (
                print_error (current_duplicate_parameter_message ^ ".");
            ) else (
                let str_parameters_list = List.map (fun (parameter_name, discrete_type) -> parameter_name ^ " : " ^ DiscreteType.string_of_var_type_discrete discrete_type) group_without_duplicates in
                let str_parameters = OCamlUtilities.string_of_list_of_string_with_sep ", " str_parameters_list in
                print_error (current_duplicate_parameter_message ^ "` does not have consistent definitions: `" ^ str_parameters ^ "`.");
            )
        ) duplicate_parameters;

        (* Check if it exist duplicate parameters *)
        List.length duplicate_parameters = 0
    in

    (* Check if all variables in function definition are defined *)
    let is_all_variables_defined =

        (* Prepare callback function that print error message when undeclared variable is found *)
        let print_variable_in_fun_not_declared variable_name =
            print_error (
                "Variable `"
                ^ variable_name
                ^ "` used in function `"
                ^ fun_def.name
                ^ "` was not declared."
            )
        in

        let print_variable_in_fun_not_declared_opt = Some print_variable_in_fun_not_declared in
        ParsingStructureUtilities.all_variables_defined_in_parsed_fun_def variable_infos print_variable_in_fun_not_declared_opt print_variable_in_fun_not_declared_opt fun_def
    in

    (* Check if assignments found in function body are allowed *)
    let is_assignments_are_allowed =

        (* Check for assigned variables (local and global) in a function implementation *)
        let left_variable_refs = ParsedModelMetadata.left_variables_of_assignments_in fun_def |> ComponentSet.elements in
        (* Check for variables (local and global) at the right side of an assignment in a function implementation *)
        let right_variable_refs = ParsedModelMetadata.right_variables_of_assignments_in fun_def |> ComponentSet.elements in

        (* Check that no local variable are updated *)
        let assigned_local_variable_names = List.filter_map (function
            | Local_variable_ref (variable_name, _, _) -> Some variable_name
            | _ -> None
        ) left_variable_refs in

        (* Check that no parameter are updated *)
        let assigned_parameter_names = List.filter_map (function
            | Param_ref (param_name, _) -> Some param_name
            | _ -> None
        ) left_variable_refs in

        (* Check that no clocks are updated *)
        (* Get only clock update and map to a clock names list *)
        let assigned_clock_type_names = List.filter_map (function
            | Global_variable_ref variable_name ->
                (* Get eventual var type (or none if variable was not declared or removed) *)
                let var_type_opt = VariableInfo.var_type_of_variable_or_constant_opt variable_infos variable_name in
                (match var_type_opt with
                | Some (Var_type_clock as var_type)
                | Some (Var_type_parameter as var_type) -> Some (var_type, variable_name)
                | _ -> None
                )
            | _ -> None

        ) left_variable_refs in

        let right_variable_clock_type_names = List.filter_map (function
            | Global_variable_ref variable_name ->
                (* Get eventual var type (or none if variable was not declared or removed) *)
                let var_type_opt = VariableInfo.var_type_of_variable_or_constant_opt variable_infos variable_name in
                (match var_type_opt with
                | Some (Var_type_clock as var_type)
                | Some (Var_type_parameter as var_type) -> Some (var_type, variable_name)
                | _ -> None
                )
            | _ -> None
        ) right_variable_refs in

        (* Is any local variable modifications in user function ? *)
        let has_parameter_modifications = List.length assigned_parameter_names > 0 in
        (* Is any local variable modifications in user function ? *)
        let has_local_variable_modifications = List.length assigned_local_variable_names > 0 in
        (* Is any clock modifications in user function ? *)
        let has_clock_param_modifications = List.length assigned_clock_type_names > 0 in
        (* Is any discrete is updated by a clock or parameter ? *)
        let was_updated_by_clock_param = List.length right_variable_clock_type_names > 0 in

        (* Print possible errors *)
        List.iter (fun param_name ->
            print_error (
                "Trying to update function parameter `"
                ^ param_name
                ^ "` in `"
                ^ fun_def.name ^
                "`. Parameters are immutables."
            );
        ) assigned_parameter_names;

        List.iter (fun variable_name ->
            print_error (
                "Trying to update local variable `"
                ^ variable_name
                ^ "` in `"
                ^ fun_def.name ^
                "`. Local variables are immutables."
            );
        ) assigned_local_variable_names;

        List.iter (fun (var_type, variable_name) ->
            let str_var_type = string_of_var_type var_type in
            let capitalized_str_var_type = String.capitalize_ascii str_var_type in
            print_error (
                "Trying to update "
                ^ str_var_type
                ^ " `"
                ^ variable_name
                ^ "` in `"
                ^ fun_def.name
                ^ "`. "
                ^ capitalized_str_var_type
                ^ " cannot be updated in user defined functions."
            );
        ) assigned_clock_type_names;

        List.iter (fun (var_type, variable_name) ->
            let str_var_type = string_of_var_type var_type in
            let capitalized_str_var_type = String.capitalize_ascii str_var_type in
            print_error (
                "Trying to update a discrete variable with "
                ^ str_var_type
                ^ " `"
                ^ variable_name
                ^ "` in `"
                ^ fun_def.name
                ^ "`. "
                ^ capitalized_str_var_type
                ^ " cannot be used for updating discrete variable."
            );
        ) right_variable_clock_type_names;

        not (has_parameter_modifications || has_local_variable_modifications || has_clock_param_modifications || was_updated_by_clock_param)
    in

    (* Return *)
    is_consistent_duplicate_parameters
    && is_assignments_are_allowed
    && is_all_variables_defined

let convert_fun_definition variable_infos (fun_definition : parsed_fun_definition) =

    (* Some checks *)
    let well_formed_user_function = check_fun_definition variable_infos fun_definition in

    (* Not well formed, raise an error *)
    if not well_formed_user_function then
        raise InvalidModel;

    (* Type check *)
    let typed_fun_definition = ExpressionConverter.TypeChecker.check_fun_definition variable_infos fun_definition in
    (* Convert *)
    ExpressionConverter.Convert.fun_definition_of_typed_fun_definition variable_infos typed_fun_definition