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
open ParsingStructureMeta
open ParsingStructureGraph
open DiscreteType

(* Abstract model modules *)
open AbstractModel
open DiscreteExpressions
open ExpressionConverter.Convert


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
            raise (InvalidExpression ("Use of \"" ^ ParsingStructureUtilities.label_of_parsed_factor_constructor factor ^ "\" is forbidden in an expression involving clock(s) or parameter(s)"))

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

let check_seq_code_bloc_assignments variable_infos code_bloc_name seq_code_bloc =

    (* If code bloc is named, we put name of location in messages *)
    let str_location =  " in `" ^ code_bloc_name ^ "`" in

    let is_assignments_are_allowed =
        (* Check for variables (local and global) at the left and right side of an assignment *)
        let left_right_variable_names = ParsingStructureMeta.left_right_member_of_assignments_in_parsed_seq_code_bloc seq_code_bloc in
        let left_variable_names = List.map first_of_tuple left_right_variable_names in

        (* Check that no constants are updated *)
        let assigned_constant_names = List.filter_map (fun variable_name ->
            let variable_kind_opt = VariableInfo.variable_kind_of_variable_name_opt variable_infos variable_name in
            match variable_kind_opt with
            | Some (VariableInfo.Constant_kind _) -> Some variable_name
            | _ -> None
        ) left_variable_names in

        (* Check that no params are updated *)
        let assigned_param_names = List.filter_map (fun variable_name ->
            let var_type_opt = VariableInfo.var_type_of_variable_or_constant_opt variable_infos variable_name in
            match var_type_opt with
            | Some Var_type_parameter -> Some variable_name
            | _ -> None
        ) left_variable_names in

        (* Check that no variable (clocks, discrete) was updated by a param *)
        let variable_names_updated_by_params = List.fold_left (fun acc (left_variable_name, right_variable_names) ->
            (* Get eventual var type (or none if variable was not declared or removed) *)
            let right_params =
                List.filter_map (fun right_variable_name ->
                    let var_type_opt = VariableInfo.var_type_of_variable_or_constant_opt variable_infos right_variable_name in
                    match var_type_opt with
                    | Some Var_type_parameter -> Some right_variable_name
                    | _ -> None
                ) right_variable_names
            in
            let has_right_params = List.length right_params > 0 in
            if has_right_params then (left_variable_name, right_params) :: acc else acc

        ) [] left_right_variable_names in

        (* Get only discrete variable names *)
        let left_discrete_variable_names = List.filter (fun (variable_name, _) ->
            let var_type_opt = VariableInfo.var_type_of_variable_or_constant_opt variable_infos variable_name in
            match var_type_opt with
            | None
            | Some (Var_type_discrete _) -> true
            | Some _ -> false
        ) left_right_variable_names in

        (* Check that no discrete variable was updated by a clock *)
        let discrete_variable_names_updated_by_clocks = List.fold_left (fun acc (left_variable_name, right_variable_names) ->
            (* Get eventual var type (or none if variable was not declared or removed) *)
            let right_clocks =
                List.filter_map (fun right_variable_name ->
                    let var_type_opt = VariableInfo.var_type_of_variable_or_constant_opt variable_infos right_variable_name in
                    match var_type_opt with
                    | Some Var_type_clock -> Some right_variable_name
                    | _ -> None
                ) right_variable_names
            in
            let has_right_clocks = List.length right_clocks > 0 in
            if has_right_clocks then (left_variable_name, right_clocks) :: acc else acc

        ) [] left_discrete_variable_names in


        (* Is any constant modifications found in user function ? *)
        let has_assigned_constant_modifications = List.length assigned_constant_names > 0 in
        (* Is any param modifications found in user function ? *)
        let has_assigned_param_modifications = List.length assigned_param_names > 0 in
        (* Is any variable (clock, discrete) is updated by a parameter ? *)
        let has_variable_updated_with_params = List.length variable_names_updated_by_params > 0 in
        (* Is any discrete is updated by a clock ? *)
        let has_discrete_updated_with_clocks = List.length discrete_variable_names_updated_by_clocks > 0 in

        (* Print errors *)

        List.iter (fun variable_name ->
            print_error (
                "Trying to update constant `"
                ^ variable_name
                ^ "` "
                ^ str_location
                ^ ". Constants are immutables."
            );
        ) assigned_constant_names;

        List.iter (fun variable_name ->
            print_error (
                "Trying to update parameter `"
                ^ variable_name
                ^ "` "
                ^ str_location
                ^ ". Parameters cannot be updated."
            );
        ) assigned_param_names;

        List.iter (fun (variable_name, param_names) ->
            print_error (
                "Trying to update variable `"
                ^ variable_name
                ^ "` with parameter(s) "
                ^ " `"
                ^ OCamlUtilities.string_of_list_of_string_with_sep ", " param_names
                ^ "`"
                ^ str_location
                ^ ". Parameters cannot be used for updating variable."
            );
        ) variable_names_updated_by_params;

        List.iter (fun (variable_name, clock_names) ->
            print_error (
                "Trying to update discrete variable `"
                ^ variable_name
                ^ "` with clock(s) "
                ^ " `"
                ^ OCamlUtilities.string_of_list_of_string_with_sep ", " clock_names
                ^ "`"
                ^ str_location
                ^ ". Clocks cannot be used for updating discrete variable."
            );
        ) discrete_variable_names_updated_by_clocks;

        (* Return is assignment is allowed *)
        not (has_assigned_constant_modifications || has_assigned_param_modifications || has_variable_updated_with_params || has_discrete_updated_with_clocks )
    in

    (* TODO benjamin REFACTOR check that in type checking, not here *)
    (* Check if there isn't any void typed variable or parameter *)
    let is_any_void_local_variable =
        (* Get local variables / parameters of parsed sequential code bloc *)
        let local_variables = ParsingStructureMeta.local_variables_of_parsed_seq_code_bloc seq_code_bloc in
        (* Check if exist any void variable *)
        List.exists (fun (variable_name, discrete_type, _) ->
            match discrete_type with
            | Var_type_void -> print_error ("Local variable or formal parameter `" ^ variable_name ^ "` " ^ str_location ^ " was declared as `void`. A variable cannot be declared as `void`."); true
            | _ -> false
        ) local_variables
    in
    (* Return *)
    is_assignments_are_allowed && not is_any_void_local_variable

(* Check whether a bloc of sequential code is well formed *)
let check_seq_code_bloc variable_infos code_bloc_name seq_code_bloc =

    (* If code bloc is named, we put name of location in messages *)
    let str_location =  " in `" ^ code_bloc_name ^ "`" in

    (* Check if all variables in function definition are defined *)
    let is_all_variables_defined =

        (* Prepare callback function that print error message when undeclared variable is found *)
        let print_variable_in_fun_not_declared variable_name =
            print_error (
                "Variable `"
                ^ variable_name
                ^ str_location
                ^ " was not declared."
            )
        in

        ParsingStructureMeta.all_variables_defined_in_parsed_seq_code_bloc variable_infos (Some print_variable_in_fun_not_declared) seq_code_bloc
    in

    let is_all_functions_defined =

        (* Prepare callback function that print error message when undeclared function is found *)
        let print_function_in_fun_not_declared variable_name =
            print_error (
                "Function `"
                ^ variable_name
                ^ str_location
                ^ " was not declared."
            )
        in

        ParsingStructureMeta.all_functions_defined_in_parsed_seq_code_bloc variable_infos (Some print_function_in_fun_not_declared) seq_code_bloc

    in

    (* Return *)
    check_seq_code_bloc_assignments variable_infos code_bloc_name seq_code_bloc && is_all_variables_defined && is_all_functions_defined


(* Check if user function definition is well formed *)
let check_fun_definition variable_infos (fun_def : parsed_fun_definition) =

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
        ParsingStructureMeta.all_variables_defined_in_parsed_fun_def variable_infos print_variable_in_fun_not_declared_opt fun_def
    in

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
    (* Return *)
     check_seq_code_bloc_assignments variable_infos fun_def.name fun_def.body && is_all_variables_defined && is_consistent_duplicate_parameters

(* Convert the init expression (parsed boolean expression) to a global expression *)
let convert_discrete_init variable_infos variable_name expr =
    (* Get typed expression *)
    let typed_expr = ExpressionConverter.TypeChecker.check_discrete_init variable_infos variable_name expr in
    (* Print *)
(*    ImitatorUtilities.print_message Verbose_standard (ExpressionConverter.TypeChecker.string_of_typed_boolean_expression variable_infos typed_expr);*)
    (* Convert *)
    ExpressionConverter.Convert.global_expression_of_typed_boolean_expression variable_infos typed_expr

(* Convert the init expression (parsed boolean expression) to a global expression *)
let convert_discrete_constant initialized_constants (name, expr, var_type) =
    (* Create fake variable_infos containing just initialized constants *)
    let dummy_variable_infos = {
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

    let typed_expr = ExpressionConverter.TypeChecker.check_constant_expression dummy_variable_infos (name, expr, var_type) in
    ExpressionConverter.Convert.global_expression_of_typed_boolean_expression dummy_variable_infos typed_expr

(* Split convex_predicate into two lists *)
(* One only contain discrete expression to nonlinear_constraint *)
(* One that doesn't only contain discrete expression to linear_constraint *)
let split_convex_predicate_into_discrete_and_continuous variable_infos convex_predicate =
  (* Compute a list of inequalities *)
  let partitions = List.partition
    (fun nonlinear_inequality ->
        (* Try to get value if it's a simple value (True / False) *)
        let value_opt = ParsingStructureMeta.discrete_boolean_expression_constant_value_opt nonlinear_inequality in

        match value_opt with
        | Some true -> true
        | Some false -> raise False_exception
        | None -> ParsingStructureMeta.only_discrete_in_nonlinear_expression variable_infos nonlinear_inequality

    ) convex_predicate
    in
    (* Get discrete part as a nonlinear constraint but convert back continuous part to a linear constraint *)
    let discrete_part, continuous_part = partitions in
        discrete_part,
        List.map (fun nonlinear_constraint -> linear_constraint_of_nonlinear_constraint nonlinear_constraint) continuous_part

(* Convert a convex predicate (list of parsed discrete boolean expression) to non-linear constraint for abstract model *)
let nonlinear_constraint_of_convex_predicate variable_infos guard =
    (* Type check guard *)
    let typed_guard = ExpressionConverter.TypeChecker.check_guard variable_infos guard in
    (* Convert *)
    List.rev_map (ExpressionConverter.Convert.nonlinear_constraint_of_typed_nonlinear_constraint variable_infos) typed_guard

(* Convert a parsed guard (list of parsed discrete boolean expression) to guard for abstract model *)
let convert_guard variable_infos guard_convex_predicate =
    try (

        (* Separate the guard into a discrete guard (on discrete variables) and a continuous guard (on all variables) *)
        let discrete_guard_convex_predicate, continuous_guard_convex_predicate = split_convex_predicate_into_discrete_and_continuous variable_infos guard_convex_predicate in

        match discrete_guard_convex_predicate, continuous_guard_convex_predicate with
        (* No inequalities: true *)
        | [] , [] -> True_guard
        (* Only discrete inequalities: discrete *)
        | discrete_guard_convex_predicate , [] ->

            (* Get the converted non-linear constraint *)
            let converted_nonlinear_constraint = nonlinear_constraint_of_convex_predicate variable_infos discrete_guard_convex_predicate in

            (* Try to eval without context, if it fails return None *)
            let reduced_nonlinear_constraint_opt = DiscreteExpressionEvaluator.eval_nonlinear_constraint_opt None converted_nonlinear_constraint in

            (* Little optimization here, it's not mandatory to work properly *)
            (* We can directly convert to Discrete_guard converted_nonlinear_constraint *)
            (match reduced_nonlinear_constraint_opt with
            | Some true -> True_guard
            | Some false -> False_guard
            | None -> Discrete_guard converted_nonlinear_constraint
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

(* Convert a parsed update to update for abstract model *)
let convert_update variable_infos updates_type parsed_update_type expr =
    let typed_update_type, typed_expr = ExpressionConverter.TypeChecker.check_update variable_infos updates_type parsed_update_type expr in
    ExpressionConverter.Convert.update_type_of_typed_update_type variable_infos typed_update_type,
    ExpressionConverter.Convert.global_expression_of_typed_boolean_expression variable_infos typed_expr

(* Convert a parsed continuous update to continuous update for abstract model *)
let convert_continuous_update variable_infos parsed_scalar_or_index_update_type expr =
    let parsed_update_type = Parsed_variable_update parsed_scalar_or_index_update_type in
    let typed_update_type, typed_expr = ExpressionConverter.TypeChecker.check_update variable_infos Parsed_std_updates parsed_update_type expr in
    ExpressionConverter.Convert.update_type_of_typed_update_type variable_infos typed_update_type,
    ExpressionConverter.Convert.linear_term_of_typed_boolean_expression variable_infos typed_expr

(* Convert a parsed boolean expression to boolean expression for abstract model *)
let convert_conditional variable_infos expr =
    (* Check *)
    let typed_expr = ExpressionConverter.TypeChecker.check_conditional variable_infos expr in
    (* Convert *)
    ExpressionConverter.Convert.bool_expression_of_typed_boolean_expression variable_infos typed_expr

(* Convert a parsed function definition to function definition for abstract model *)
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

(* Convert a parsed sequential code bloc to sequential code bloc for abstract model *)
let convert_seq_code_bloc variable_infos seq_code_bloc =
    (* Some checks *)
    let well_formed_user_function = check_seq_code_bloc variable_infos "update" seq_code_bloc in

    (* Not well formed, raise an error *)
    if not well_formed_user_function then
        raise InvalidModel;

    (* Type check *)
    let typed_seq_code_bloc = ExpressionConverter.TypeChecker.check_seq_code_bloc variable_infos seq_code_bloc in
    ExpressionConverter.Convert.seq_code_bloc_of_typed_seq_code_bloc variable_infos typed_seq_code_bloc