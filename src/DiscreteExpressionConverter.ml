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
open Constants
open Exceptions
open ParsingStructure
open AbstractModel
open DiscreteExpressions
open DiscreteType
open ExpressionConverter.Convert

let convert_discrete_init variable_infos variable_name expr =
    (* Get typed expression *)
    let typed_expr = ExpressionConverter.TypeChecker.check_discrete_init variable_infos variable_name expr in
    (* Print *)
(*    ImitatorUtilities.print_message Verbose_standard (ExpressionConverter.TypeChecker.string_of_typed_global_expression variable_infos typed_expr);*)
    (* Convert *)
    ExpressionConverter.Convert.global_expression_of_typed_global_expression variable_infos typed_expr

let convert_discrete_constant initialized_constants (name, expr, var_type) =

    let variable_infos = {
        constants = initialized_constants;
        variable_names = [];
        index_of_variables = Hashtbl.create 0;
        removed_variable_names = [];
        type_of_variables = (fun _ -> raise (TypeError "oops!"));
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

let convert_update variable_infos updates_type parsed_variable_update_type expr =
    let typed_variable_update_type, typed_expr = ExpressionConverter.TypeChecker.check_update variable_infos updates_type parsed_variable_update_type expr in
    ExpressionConverter.Convert.parsed_variable_update_type_of_typed_variable_update_type variable_infos typed_variable_update_type,
    ExpressionConverter.Convert.global_expression_of_typed_global_expression variable_infos typed_expr

let convert_continuous_update variable_infos parsed_variable_update_type expr =
    let typed_variable_update_type, typed_expr = ExpressionConverter.TypeChecker.check_update variable_infos Parsed_updates parsed_variable_update_type expr in
    ExpressionConverter.Convert.parsed_variable_update_type_of_typed_variable_update_type variable_infos typed_variable_update_type,
    ExpressionConverter.Convert.linear_term_of_typed_global_expression variable_infos typed_expr


let convert_conditional variable_infos expr =
    (* Check *)
    let typed_expr = ExpressionConverter.TypeChecker.check_conditional variable_infos expr in
    (* Convert *)
    ExpressionConverter.Convert.bool_expression_of_typed_boolean_expression variable_infos typed_expr
