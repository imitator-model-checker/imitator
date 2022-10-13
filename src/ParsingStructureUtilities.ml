(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: General functions for map, filter, traverse, evaluating, etc. parsing structure tree
 *
 * File contributors : Benjamin L.
 * Created           : 2021/03/05
 *
 ************************************************************)

open Exceptions
open ParsingStructure
open VariableInfo
open DiscreteType
open OCamlUtilities
open CustomModules

(* Map of declared local variables *)
type local_variables_map = (var_type_discrete * int) VariableMap.t
(**)
type variable_callback = (variable_name -> unit) option

(* Leaves of parsing structure *)
type parsing_structure_leaf =
    | Leaf_variable of variable_name
    | Leaf_constant of ParsedValue.parsed_value
    | Leaf_fun of variable_name

(* Leaves of parsed bloc *)
type parsed_seq_code_bloc_leaf =
    | Leaf_decl_variable of variable_name * var_type_discrete * variable_id
    | Leaf_update_variable of variable_name

(* Leaf of linear expression *)
type linear_expression_leaf =
    | Leaf_true_linear_constraint
    | Leaf_false_linear_constraint
    | Leaf_linear_constant of NumConst.t
    | Leaf_linear_variable of NumConst.t * variable_name

(* Leaf of init state predicate *)
type init_state_predicate_leaf =
    | Leaf_loc_assignment of automaton_name * location_name

(* Leaf of state predicate *)
type state_predicate_leaf =
    | Leaf_predicate_true
    | Leaf_predicate_false
    | Leaf_predicate_accepting
    | Leaf_predicate_EQ of string (* automaton name *) * string (* location name *)
    | Leaf_predicate_NEQ of string (* automaton name *) * string (* location name *)

(* Type of callback function called when reach a leaf of a discrete expression *)
type 'a parsing_structure_leaf_callback = local_variables_map -> parsing_structure_leaf -> 'a
(* Type of callback function called when reach a leaf of a linear expression *)
type 'a linear_expression_leaf_callback = linear_expression_leaf -> 'a

(* Extract function name from parsed factor *)
let function_name_of_parsed_factor = function
	| Parsed_DF_variable name -> name
    | factor -> raise (TypeError "Trying to make a call on a non-function.")

(** Fold a parsing structure using operator applying custom function on leafs **)

let rec fold_parsed_boolean_expression_with_local_variables local_variables operator base leaf_fun = function
    | Parsed_conj_dis (l_expr, r_expr, _) ->
	    operator
	        (fold_parsed_boolean_expression_with_local_variables local_variables operator base leaf_fun l_expr)
	        (fold_parsed_boolean_expression_with_local_variables local_variables operator base leaf_fun r_expr)
	| Parsed_Discrete_boolean_expression expr ->
	    fold_parsed_discrete_boolean_expression_with_local_variables local_variables operator base leaf_fun expr

and fold_parsed_discrete_boolean_expression_with_local_variables local_variables operator base leaf_fun = function
    | Parsed_arithmetic_expression expr ->
        fold_parsed_discrete_arithmetic_expression_with_local_variables local_variables operator base leaf_fun expr
	| Parsed_comparison (l_expr, _, r_expr) ->
	    operator
	        (fold_parsed_discrete_boolean_expression_with_local_variables local_variables operator base leaf_fun l_expr)
	        (fold_parsed_discrete_boolean_expression_with_local_variables local_variables operator base leaf_fun r_expr)
	| Parsed_comparison_in (lower_expr, expr, upper_expr) ->
	    operator
	        (fold_parsed_discrete_arithmetic_expression_with_local_variables local_variables operator base leaf_fun expr)
            (operator
                (fold_parsed_discrete_arithmetic_expression_with_local_variables local_variables operator base leaf_fun lower_expr)
                (fold_parsed_discrete_arithmetic_expression_with_local_variables local_variables operator base leaf_fun upper_expr))
	| Parsed_boolean_expression expr
	| Parsed_Not expr ->
        fold_parsed_boolean_expression_with_local_variables local_variables operator base leaf_fun expr

and fold_parsed_discrete_arithmetic_expression_with_local_variables local_variables operator base leaf_fun = function
	| Parsed_sum_diff (expr, term, _) ->
        operator
            (fold_parsed_discrete_arithmetic_expression_with_local_variables local_variables operator base leaf_fun expr)
            (fold_parsed_discrete_term_with_local_variables local_variables operator base leaf_fun term)
	| Parsed_DAE_term term ->
        fold_parsed_discrete_term_with_local_variables local_variables operator base leaf_fun term

and fold_parsed_discrete_term_with_local_variables local_variables operator base leaf_fun = function
	| Parsed_product_quotient (term, factor, _) ->
	    operator
	        (fold_parsed_discrete_term_with_local_variables local_variables operator base leaf_fun term)
	        (fold_parsed_discrete_factor_with_local_variables local_variables operator base leaf_fun factor)
	| Parsed_DT_factor factor ->
        fold_parsed_discrete_factor_with_local_variables local_variables operator base leaf_fun factor

and fold_parsed_discrete_factor_with_local_variables local_variables operator base leaf_fun = function
	| Parsed_DF_variable variable_name -> leaf_fun local_variables (Leaf_variable variable_name)
	| Parsed_DF_constant value -> leaf_fun local_variables (Leaf_constant value)
	| Parsed_sequence (expr_list, _) -> List.fold_left (fun acc expr -> operator acc (fold_parsed_boolean_expression_with_local_variables local_variables operator base leaf_fun expr)) base expr_list
	| Parsed_DF_expression expr ->
        fold_parsed_discrete_arithmetic_expression_with_local_variables local_variables operator base leaf_fun expr
    | Parsed_function_call (factor_name, argument_expressions) ->
        let function_name = function_name_of_parsed_factor factor_name in
        operator
            (leaf_fun local_variables (Leaf_fun function_name))
            (List.fold_left (fun acc expr -> operator (fold_parsed_boolean_expression_with_local_variables local_variables operator base leaf_fun expr) acc) base argument_expressions)
    | Parsed_DF_access (factor, _)
	(* | Parsed_log_not factor *)
	| Parsed_DF_unary_min factor ->
	    fold_parsed_discrete_factor_with_local_variables local_variables operator base leaf_fun factor

and fold_parsed_scalar_or_index_update_type_with_local_variables local_variables operator base seq_code_bloc_leaf_fun leaf_fun = function
    | Parsed_scalar_update variable_name -> seq_code_bloc_leaf_fun local_variables (Leaf_update_variable variable_name)
    | Parsed_indexed_update (parsed_scalar_or_index_update_type, index_expr) ->
        operator
            (fold_parsed_scalar_or_index_update_type_with_local_variables local_variables operator base seq_code_bloc_leaf_fun leaf_fun parsed_scalar_or_index_update_type)
            (fold_parsed_discrete_arithmetic_expression_with_local_variables local_variables operator base leaf_fun index_expr)

and fold_parsed_update_type_with_local_variables local_variables operator base seq_code_bloc_leaf_fun leaf_fun = function
    | Parsed_variable_update parsed_scalar_or_index_update_type ->
        fold_parsed_scalar_or_index_update_type_with_local_variables local_variables operator base seq_code_bloc_leaf_fun leaf_fun parsed_scalar_or_index_update_type
    | Parsed_void_update -> base

and fold_parsed_seq_code_bloc_with_local_variables local_variables operator base seq_code_bloc_leaf_fun leaf_fun (* seq_code_bloc *) =
    let rec fold_parsed_seq_code_bloc_rec local_variables = function
        | Parsed_local_decl (variable_name, discrete_type, init_expr, next_expr, id) ->
            (* Add new declared local variable *)
            let new_local_variables = VariableMap.add variable_name (discrete_type, id) local_variables in

            seq_code_bloc_leaf_fun local_variables (Leaf_decl_variable (variable_name, discrete_type, id))
            |> operator (fold_parsed_boolean_expression_with_local_variables local_variables operator base leaf_fun init_expr)
            |> operator (fold_parsed_seq_code_bloc_rec new_local_variables next_expr)

        | Parsed_for_loop (variable_name, from_expr, to_expr, _, inner_bloc, next_expr, id) ->
            (* Add local variable used for loop *)
            let inner_local_variables = VariableMap.add variable_name (Var_type_discrete_number Var_type_discrete_int, id) local_variables in

            seq_code_bloc_leaf_fun local_variables (Leaf_decl_variable (variable_name, (Var_type_discrete_number Var_type_discrete_int), id))
            |> operator (fold_parsed_discrete_arithmetic_expression_with_local_variables local_variables operator base leaf_fun from_expr)
            |> operator (fold_parsed_discrete_arithmetic_expression_with_local_variables local_variables operator base leaf_fun to_expr)
            |> operator (fold_parsed_seq_code_bloc_rec inner_local_variables inner_bloc)
            |> operator (fold_parsed_seq_code_bloc_rec local_variables next_expr)

        | Parsed_while_loop (condition_expr, inner_bloc, next_expr) ->
            fold_parsed_boolean_expression_with_local_variables local_variables operator base leaf_fun condition_expr
            |> operator (fold_parsed_seq_code_bloc_rec local_variables inner_bloc)
            |> operator (fold_parsed_seq_code_bloc_rec local_variables next_expr)

        | Parsed_if (condition_expr, then_bloc, else_bloc_opt, next_expr) ->
            let else_bloc_result =
                match else_bloc_opt with
                | Some else_bloc -> fold_parsed_seq_code_bloc_rec local_variables else_bloc
                | None -> base
            in

            fold_parsed_boolean_expression_with_local_variables local_variables operator base leaf_fun condition_expr
            |> operator (fold_parsed_seq_code_bloc_rec local_variables then_bloc)
            |> operator else_bloc_result
            |> operator (fold_parsed_seq_code_bloc_rec local_variables next_expr)

        | Parsed_assignment (normal_update, next_expr) ->
            operator
                (fold_parsed_normal_update_with_local_variables local_variables operator base seq_code_bloc_leaf_fun leaf_fun normal_update)
                (fold_parsed_seq_code_bloc_rec local_variables next_expr)

        | Parsed_return_expr expr ->
            fold_parsed_boolean_expression_with_local_variables local_variables operator base leaf_fun expr

        | Parsed_bloc_void ->
            base
    in
    fold_parsed_seq_code_bloc_rec local_variables (* seq_code_bloc *)

and fold_parsed_normal_update_with_local_variables local_variables operator base seq_code_bloc_leaf_fun leaf_fun (update_type, expr) =
    operator
        (fold_parsed_update_type_with_local_variables local_variables operator base seq_code_bloc_leaf_fun leaf_fun update_type)
        (fold_parsed_boolean_expression_with_local_variables local_variables operator base leaf_fun expr)

(** Fold a parsed update expression using operator applying custom function on leaves **)
and fold_parsed_update operator base seq_code_bloc_leaf_fun leaf_fun = function
	| Normal normal_update ->
	    fold_parsed_normal_update_with_local_variables VariableMap.empty operator base seq_code_bloc_leaf_fun leaf_fun normal_update
	| Condition (bool_expr, update_list_if, update_list_else) ->
        let all_updates = update_list_if@update_list_else in
        let fold_updates = List.fold_left (fun acc normal_update ->
            operator acc (fold_parsed_normal_update_with_local_variables VariableMap.empty operator base seq_code_bloc_leaf_fun leaf_fun normal_update)
        ) base all_updates
        in
        operator fold_updates (fold_parsed_boolean_expression_with_local_variables VariableMap.empty operator base leaf_fun bool_expr)

let rec fold_parsed_linear_constraint operator leaf_fun = function
    | Parsed_true_constraint -> leaf_fun Leaf_true_linear_constraint
    | Parsed_false_constraint -> leaf_fun Leaf_false_linear_constraint
    | Parsed_linear_constraint (l_expr, _, r_expr) ->
        operator
            (fold_parsed_linear_expression operator leaf_fun l_expr)
            (fold_parsed_linear_expression operator leaf_fun r_expr)

(** Fold a parsed linear expression using operator applying custom function on leafs **)
and fold_parsed_linear_expression operator leaf_fun = function
    | Linear_term term ->
        fold_parsed_linear_term operator leaf_fun term
    | Linear_plus_expression (expr, term)
    | Linear_minus_expression (expr, term) ->
        operator
            (fold_parsed_linear_expression operator leaf_fun expr)
            (fold_parsed_linear_term operator leaf_fun term)

(** Fold a parsed linear term using operator applying custom function on leafs **)
and fold_parsed_linear_term operator leaf_fun = function
    | Constant value -> leaf_fun (Leaf_linear_constant value)
    | Variable (value, variable_name) -> leaf_fun (Leaf_linear_variable (value, variable_name))

let fold_parsed_boolean_expression operator = fold_parsed_boolean_expression_with_local_variables VariableMap.empty operator
let fold_parsed_discrete_boolean_expression operator = fold_parsed_discrete_boolean_expression_with_local_variables VariableMap.empty operator
let fold_parsed_discrete_arithmetic_expression operator = fold_parsed_discrete_arithmetic_expression_with_local_variables VariableMap.empty operator
let fold_parsed_discrete_term operator = fold_parsed_discrete_term_with_local_variables VariableMap.empty operator
let fold_parsed_discrete_factor operator = fold_parsed_discrete_factor_with_local_variables VariableMap.empty operator
let fold_parsed_seq_code_bloc operator = fold_parsed_seq_code_bloc_with_local_variables VariableMap.empty operator

(** Fold a parsed linear constraint using operator applying custom function on leafs **)
let fold_parsed_nonlinear_constraint = fold_parsed_discrete_boolean_expression


let fold_init_state_predicate operator base loc_assignment_leaf_fun linear_expression_leaf_fun leaf_fun = function
	| Parsed_loc_assignment (automaton_name, loc_name) -> loc_assignment_leaf_fun (automaton_name, loc_name)
	| Parsed_linear_predicate linear_constraint -> fold_parsed_linear_constraint operator linear_expression_leaf_fun linear_constraint
	| Parsed_discrete_predicate (_, expr) -> fold_parsed_boolean_expression operator base leaf_fun expr

let fold_parsed_loc_predicate operator base predicate_leaf_fun leaf_fun = function
    | Parsed_loc_predicate_EQ (automaton_name, loc_name) ->
        predicate_leaf_fun (Leaf_predicate_EQ (automaton_name, loc_name))
    | Parsed_loc_predicate_NEQ (automaton_name, loc_name) ->
        predicate_leaf_fun (Leaf_predicate_NEQ (automaton_name, loc_name))

let fold_parsed_simple_predicate operator base predicate_leaf_fun leaf_fun = function
	| Parsed_discrete_boolean_expression expr ->
        fold_parsed_discrete_boolean_expression operator base leaf_fun expr
	| Parsed_loc_predicate predicate ->
		fold_parsed_loc_predicate operator base predicate_leaf_fun leaf_fun predicate
	| Parsed_state_predicate_true -> predicate_leaf_fun Leaf_predicate_true
	| Parsed_state_predicate_false -> predicate_leaf_fun Leaf_predicate_false
	| Parsed_state_predicate_accepting -> predicate_leaf_fun Leaf_predicate_accepting

let rec fold_parsed_state_predicate_factor operator base predicate_leaf_fun leaf_fun = function
	| Parsed_state_predicate predicate ->
	    fold_parsed_state_predicate operator base predicate_leaf_fun leaf_fun predicate
	| Parsed_state_predicate_factor_NOT predicate ->
	    fold_parsed_state_predicate_factor operator base predicate_leaf_fun leaf_fun predicate
	| Parsed_simple_predicate predicate ->
	    fold_parsed_simple_predicate operator base predicate_leaf_fun leaf_fun predicate

and fold_parsed_state_predicate_term operator base predicate_leaf_fun leaf_fun = function
	| Parsed_state_predicate_term_AND (l_predicate, r_predicate) ->
	    operator
	        (fold_parsed_state_predicate_term operator base predicate_leaf_fun leaf_fun l_predicate)
	        (fold_parsed_state_predicate_term operator base predicate_leaf_fun leaf_fun r_predicate)
	| Parsed_state_predicate_factor predicate ->
	    fold_parsed_state_predicate_factor operator base predicate_leaf_fun leaf_fun predicate

and fold_parsed_state_predicate operator base predicate_leaf_fun leaf_fun = function
	| Parsed_state_predicate_OR (l_predicate, r_predicate) ->
        operator
            (fold_parsed_state_predicate operator base predicate_leaf_fun leaf_fun l_predicate)
            (fold_parsed_state_predicate operator base predicate_leaf_fun leaf_fun r_predicate)
	| Parsed_state_predicate_term predicate_term ->
	    fold_parsed_state_predicate_term operator base predicate_leaf_fun leaf_fun predicate_term

(**)
let fold_parsed_fun_def operator base seq_code_bloc_leaf_fun leaf_fun (fun_def : parsed_fun_definition) =
    let local_variables = List.fold_left (fun acc (param_name, param_type) -> VariableMap.add param_name (param_type, 0) acc) VariableMap.empty fun_def.parameters in
    (* Apply seq_code_leaf_fun function on each parameters of the function and fold with operator *)
    List.fold_left (fun acc (param_name, param_type) -> operator acc (seq_code_bloc_leaf_fun VariableMap.empty (Leaf_decl_variable (param_name, param_type, -1)))) base fun_def.parameters
    |> operator (fold_parsed_seq_code_bloc_with_local_variables local_variables operator base seq_code_bloc_leaf_fun leaf_fun fun_def.body)

type 'a traversed_parsed_seq_code_bloc =
    | Traversed_parsed_local_decl of variable_name * DiscreteType.var_type_discrete * parsed_boolean_expression (* init expr *) * 'a
    | Traversed_parsed_assignment of normal_update * 'a
    | Traversed_parsed_for_loop of variable_name * parsed_discrete_arithmetic_expression (* from *) * parsed_discrete_arithmetic_expression (* to *) * parsed_loop_dir (* up or down *) * 'a * 'a
    | Traversed_parsed_while_loop of parsed_boolean_expression (* condition *) * 'a (* inner bloc result *) * 'a (* next result *)
    | Traversed_parsed_if of parsed_boolean_expression (* condition *) * 'a (* then result *) * 'a option (* else result *) * 'a (* next result *)
    | Traversed_parsed_return_expr of parsed_boolean_expression
    | Traversed_parsed_bloc_void

(* Traverse a bloc of sequential code using a callback function *)
(* When traversing, all local variables are automatically computed *)
(* Callback function give as parameters:
 - local variables at the current point of path,
 - a list of values returned by previously traversed branch
 - Current expression to process
*)
let traverse_parsed_seq_code_bloc traverse_fun (* seq_code_bloc *) =

    let rec traverse_parsed_seq_code_bloc_rec local_variables = function
        | Parsed_local_decl (variable_name, discrete_type, init_expr, next_expr, _) ->
            (* Add the new declared local variable to map *)
            let new_local_variables = VariableMap.add variable_name discrete_type local_variables in

            let next_result = traverse_parsed_seq_code_bloc_rec new_local_variables next_expr in

            let traversed_element = Traversed_parsed_local_decl (variable_name, discrete_type, init_expr, next_result) in
            traverse_fun new_local_variables traversed_element

        | Parsed_for_loop (variable_name, from_expr, to_expr, loop_dir, inner_bloc, next_expr, _) ->
            (* Add the new declared local variable to map *)
            let inner_local_variables = VariableMap.add variable_name (Var_type_discrete_number Var_type_discrete_int) local_variables in

            let inner_bloc_result = traverse_parsed_seq_code_bloc_rec inner_local_variables inner_bloc in
            let next_result = traverse_parsed_seq_code_bloc_rec local_variables next_expr in

            let traversed_element = Traversed_parsed_for_loop (variable_name, from_expr, to_expr, loop_dir, inner_bloc_result, next_result) in
            traverse_fun local_variables traversed_element

        | Parsed_while_loop (condition_expr, inner_bloc, next_expr) ->

            let inner_bloc_result = traverse_parsed_seq_code_bloc_rec local_variables inner_bloc in
            let next_result = traverse_parsed_seq_code_bloc_rec local_variables next_expr in

            let traversed_element = Traversed_parsed_while_loop (condition_expr, inner_bloc_result, next_result) in
            traverse_fun local_variables traversed_element

        | Parsed_if (condition_expr, then_bloc, else_bloc_opt, next_expr) ->

            let then_result = traverse_parsed_seq_code_bloc_rec local_variables then_bloc in

            let else_result_opt =
                match else_bloc_opt with
                | Some else_bloc -> Some (traverse_parsed_seq_code_bloc_rec local_variables else_bloc)
                | None -> None
            in

            let next_result = traverse_parsed_seq_code_bloc_rec local_variables next_expr in

            let traversed_element = Traversed_parsed_if (condition_expr, then_result, else_result_opt, next_result) in
            traverse_fun local_variables traversed_element

        | Parsed_assignment (normal_update, next_expr) ->
            let next_result = traverse_parsed_seq_code_bloc_rec local_variables next_expr in
            let traversed_element = Traversed_parsed_assignment (normal_update, next_result) in
            traverse_fun local_variables traversed_element

        | Parsed_return_expr (expr) ->
            traverse_fun local_variables (Traversed_parsed_return_expr expr)

        | Parsed_bloc_void ->
            traverse_fun local_variables Traversed_parsed_bloc_void
    in
    traverse_parsed_seq_code_bloc_rec VariableMap.empty (* seq_code_bloc *)



let flat_map_parsed_boolean_expression = fold_parsed_boolean_expression (@) []
(** Check if all leaf of a parsing structure satisfy the predicate **)

(* Apply to a fold function the standard parameters for evaluate AND *)
let apply_evaluate_and fold_func = fold_func (OCamlUtilities.evaluate_and)
let apply_evaluate_and_with_base fold_func = fold_func (OCamlUtilities.evaluate_and) true

let for_all_in_parsed_boolean_expression = apply_evaluate_and_with_base fold_parsed_boolean_expression
let for_all_in_parsed_discrete_boolean_expression = apply_evaluate_and_with_base fold_parsed_discrete_boolean_expression
let for_all_in_parsed_discrete_arithmetic_expression = apply_evaluate_and_with_base fold_parsed_discrete_arithmetic_expression
let for_all_in_parsed_discrete_term = apply_evaluate_and_with_base fold_parsed_discrete_term
let for_all_in_parsed_discrete_factor = apply_evaluate_and_with_base fold_parsed_discrete_factor

(** Check if all leaf of a linear expression satisfy the predicate **)
let for_all_in_parsed_linear_expression = apply_evaluate_and fold_parsed_linear_expression
(** Check if all leaf of a linear term satisfy the predicate **)
let for_all_in_parsed_linear_term = apply_evaluate_and fold_parsed_linear_term
(** Check if all leaf of a linear constraint satisfy the predicate **)
let for_all_in_parsed_linear_constraint = apply_evaluate_and fold_parsed_linear_constraint
(** Check if all leaf of a non-linear constraint satisfy the predicate **)
let for_all_in_parsed_nonlinear_constraint = apply_evaluate_and_with_base fold_parsed_nonlinear_constraint
(** Check if all leaf of a parsed update satisfy the predicate **)
let for_all_in_parsed_normal_update = apply_evaluate_and_with_base (fold_parsed_normal_update_with_local_variables VariableMap.empty)
(** Check if all leaf of a parsed update satisfy the predicate **)
let for_all_in_parsed_update = apply_evaluate_and_with_base fold_parsed_update
(** Check if all leaf of a parsed normal update satisfy the predicate **)
let for_all_in_parsed_normal_update = apply_evaluate_and_with_base (fold_parsed_normal_update_with_local_variables VariableMap.empty)

let for_all_in_parsed_loc_predicate = apply_evaluate_and_with_base fold_parsed_loc_predicate
let for_all_in_parsed_simple_predicate = apply_evaluate_and_with_base fold_parsed_simple_predicate
let for_all_in_parsed_state_predicate_factor = apply_evaluate_and_with_base fold_parsed_state_predicate_factor
let for_all_in_parsed_state_predicate_term = apply_evaluate_and_with_base fold_parsed_state_predicate_term
let for_all_in_parsed_state_predicate = apply_evaluate_and_with_base fold_parsed_state_predicate

(** Check if all leaf of a parsed sequential code bloc satisfy the predicate **)
let for_all_in_parsed_seq_code_bloc = apply_evaluate_and_with_base fold_parsed_seq_code_bloc
(** Check if all leaf of a parsed function definition satisfy the predicate **)
let for_all_in_parsed_function_definition = apply_evaluate_and_with_base fold_parsed_fun_def

(** Check if any leaf of a parsing structure satisfy the predicate **)

(* Apply to a fold function the standard parameters for evaluate OR *)
let apply_evaluate_or fold_func = fold_func (||)
let apply_evaluate_or_with_base fold_func = fold_func (||) false

let exists_in_parsed_boolean_expression = apply_evaluate_or_with_base fold_parsed_boolean_expression
let exists_in_parsed_discrete_boolean_expression = apply_evaluate_or_with_base fold_parsed_discrete_boolean_expression
let exists_in_parsed_discrete_arithmetic_expression = apply_evaluate_or_with_base fold_parsed_discrete_arithmetic_expression
let exists_in_parsed_discrete_term = apply_evaluate_or_with_base fold_parsed_discrete_term
let exists_in_parsed_discrete_factor = apply_evaluate_or_with_base fold_parsed_discrete_factor

(** Check if any leaf of a linear expression satisfy the predicate **)
let exists_in_parsed_linear_expression = apply_evaluate_or fold_parsed_linear_expression
(** Check if any leaf of a linear term satisfy the predicate **)
let exists_in_parsed_linear_term = apply_evaluate_or fold_parsed_linear_term
(** Check if any leaf of a linear constraint satisfy the predicate **)
let exists_in_parsed_linear_constraint = apply_evaluate_or fold_parsed_linear_constraint
(** Check if any leaf of a non-linear constraint satisfy the predicate **)
let exists_in_parsed_nonlinear_constraint = apply_evaluate_or_with_base fold_parsed_nonlinear_constraint

(** Check if any leaf of a parsed update satisfy the predicate **)
let exists_in_parsed_update = apply_evaluate_or_with_base fold_parsed_update
(** Check if any leaf of a parsed normal update satisfy the predicate **)
let exists_in_parsed_normal_update = apply_evaluate_or_with_base (fold_parsed_normal_update_with_local_variables VariableMap.empty)

let exists_in_parsed_loc_predicate = apply_evaluate_or_with_base fold_parsed_loc_predicate
let exists_in_parsed_simple_predicate = apply_evaluate_or_with_base fold_parsed_simple_predicate
let exists_in_parsed_state_predicate_factor = apply_evaluate_or_with_base fold_parsed_state_predicate_factor
let exists_in_parsed_state_predicate_term = apply_evaluate_or_with_base fold_parsed_state_predicate_term
let exists_in_parsed_state_predicate = apply_evaluate_or_with_base fold_parsed_state_predicate

(** Check if any leaf of a parsed sequential code bloc satisfy the predicate **)
let exists_in_parsed_seq_code_bloc_with_local_variables = apply_evaluate_or_with_base fold_parsed_seq_code_bloc
(** Check if any leaf of a parsed sequential code bloc satisfy the predicate **)
let exists_in_parsed_seq_code_bloc = apply_evaluate_or_with_base fold_parsed_seq_code_bloc
(** Check if any leaf of a parsed function definition satisfy the predicate **)
let exists_in_parsed_function_definition = apply_evaluate_or_with_base fold_parsed_fun_def

(** Iterate over a parsing structure **)

(* Apply to a fold function the standard parameters for evaluate unit *)
let apply_evaluate_unit fold_func = fold_func bin_unit
let apply_evaluate_unit_with_base fold_func = fold_func bin_unit ()


let iterate_parsed_boolean_expression = apply_evaluate_unit_with_base fold_parsed_boolean_expression
let iterate_parsed_discrete_boolean_expression = apply_evaluate_unit_with_base fold_parsed_discrete_boolean_expression
let iterate_parsed_discrete_arithmetic_expression = apply_evaluate_unit_with_base fold_parsed_discrete_arithmetic_expression
let iterate_parsed_discrete_term  = apply_evaluate_unit_with_base fold_parsed_discrete_term
let iterate_parsed_discrete_factor = apply_evaluate_unit_with_base fold_parsed_discrete_factor

(** Iterate over a linear expression **)
let iterate_parsed_linear_expression = apply_evaluate_unit fold_parsed_linear_expression
(** Iterate over a linear term **)
let iterate_parsed_linear_term = apply_evaluate_unit fold_parsed_linear_term
(** Iterate over a linear constraint **)
let iterate_parsed_linear_constraint = apply_evaluate_unit fold_parsed_linear_constraint
(** Iterate over a non-linear constraint **)
let iterate_parsed_nonlinear_constraint = apply_evaluate_unit_with_base fold_parsed_nonlinear_constraint
(** Iterate over a non-linear convex predicate **)
let iterate_parsed_nonlinear_convex_predicate leaf_fun convex_predicate =
    List.iter (iterate_parsed_nonlinear_constraint leaf_fun) convex_predicate

let iterate_parsed_update = apply_evaluate_unit_with_base fold_parsed_update
let iterate_parsed_normal_update = apply_evaluate_unit_with_base (fold_parsed_normal_update_with_local_variables VariableMap.empty)
let iterate_in_parsed_loc_predicate = apply_evaluate_unit_with_base fold_parsed_loc_predicate
let iterate_in_parsed_simple_predicate = apply_evaluate_unit_with_base fold_parsed_simple_predicate
let iterate_in_parsed_state_predicate_factor = apply_evaluate_unit_with_base fold_parsed_state_predicate_factor
let iterate_in_parsed_state_predicate_term = apply_evaluate_unit_with_base fold_parsed_state_predicate_term
let iterate_in_parsed_state_predicate = apply_evaluate_unit_with_base fold_parsed_state_predicate

(** Iterate over a parsed sequential code bloc definition **)
let iterate_in_parsed_seq_code_bloc_with_local_variables = apply_evaluate_unit_with_base fold_parsed_seq_code_bloc
(** Iterate over a parsed function definition **)
let iterate_in_parsed_function_definition = apply_evaluate_unit_with_base fold_parsed_fun_def

let label_of_parsed_sequence_type = function
    | Parsed_array -> "array"
    | Parsed_list -> "list"
    | Parsed_stack -> Constants.stack_string
    | Parsed_queue -> "queue"

let label_of_parsed_factor_constructor = function
	| Parsed_DF_variable _ -> "variable"
	| Parsed_DF_constant _ -> "constant"
	| Parsed_sequence (_, seq_type) -> label_of_parsed_sequence_type seq_type
	| Parsed_DF_access _ -> "access"
	| Parsed_DF_expression _ -> "expression"
	| Parsed_DF_unary_min _ -> "minus"
    | Parsed_function_call (variable, _) -> function_name_of_parsed_factor variable



(* String of a parsed expression *)
(* Used when needed to display an error message (ie : error on type checking) *)

(* String of relational operator *)
let string_of_parsed_relop relop value_1 value_2 =
    match relop with
    | PARSED_OP_L		-> value_1 ^ " < " ^ value_2
    | PARSED_OP_LEQ	    -> value_1 ^ " <= " ^ value_2
    | PARSED_OP_EQ		-> value_1 ^ " = " ^ value_2
    | PARSED_OP_NEQ	    -> value_1 ^ " <> " ^ value_2
    | PARSED_OP_GEQ	    -> value_1 ^ " >= " ^ value_2
    | PARSED_OP_G		-> value_1 ^ " > " ^ value_2

let string_of_parsed_sum_diff = function
    | Parsed_plus -> Constants.default_arithmetic_string.plus_string
    | Parsed_minus -> Constants.default_arithmetic_string.minus_string

let string_of_parsed_product_quotient = function
    | Parsed_mul -> Constants.default_arithmetic_string.mul_string
    | Parsed_div -> Constants.default_arithmetic_string.div_string

let string_of_parsed_conj_dis = function
    | Parsed_and -> Constants.default_string.and_operator
    | Parsed_or -> Constants.default_string.or_operator

(* String representation of an assignment *)
let string_of_assignment str_left_member str_right_member =
    str_left_member
    ^ (if str_left_member <> "" then " := " else "")
    ^ str_right_member

(* String representation of let-in structure *)
let string_of_let_in variable_name str_discrete_type str_init_expr =
    "var "
    ^ variable_name
    ^ " : "
    ^ str_discrete_type
    ^ " = "
    ^ str_init_expr
    ^ ";"

let rec string_of_parsed_arithmetic_expression variable_infos = function
    | Parsed_sum_diff (arithmetic_expr, term, sum_diff) ->
            string_of_parsed_arithmetic_expression variable_infos arithmetic_expr
            ^ string_of_parsed_sum_diff sum_diff
            ^ string_of_parsed_term variable_infos term

    | Parsed_DAE_term term ->
        string_of_parsed_term variable_infos term

and string_of_parsed_term variable_infos = function
    | Parsed_product_quotient (term, factor, parsed_product_quotient) ->
        string_of_parsed_term variable_infos term
        ^ string_of_parsed_product_quotient parsed_product_quotient
        ^ string_of_parsed_factor variable_infos factor

    | Parsed_DT_factor factor ->
        string_of_parsed_factor variable_infos factor

and string_of_parsed_factor variable_infos = function
    | Parsed_DF_variable variable_name ->
        if (is_constant_is_defined variable_infos variable_name) then (
            (* Retrieve the value of the global constant *)
            let value = value_of_constant_name variable_infos variable_name in
            variable_name
            ^ "="
            ^ AbstractValue.string_of_value value
        ) else
            variable_name
    | Parsed_DF_constant value -> ParsedValue.string_of_value value
    | Parsed_sequence (expr_list, seq_type) as seq ->
        let str_elements = List.map (string_of_parsed_boolean_expression variable_infos) expr_list in
        let str_array = "[" ^ OCamlUtilities.string_of_list_of_string_with_sep ", " str_elements ^ "]" in
        (match seq_type with
        | Parsed_array -> str_array
        | Parsed_list
        | Parsed_stack
        | Parsed_queue -> label_of_parsed_factor_constructor seq ^ "(" ^ str_array ^ ")"
        )
    | Parsed_DF_access (factor, expr) ->
        string_of_parsed_factor variable_infos factor ^ "[" ^ string_of_parsed_arithmetic_expression variable_infos expr ^ "]"
    | Parsed_DF_expression arithmetic_expr -> string_of_parsed_arithmetic_expression variable_infos arithmetic_expr
    | Parsed_DF_unary_min factor ->
        "-(" ^ (string_of_parsed_factor variable_infos factor) ^ ")"
    | Parsed_function_call (_, argument_expressions) as func ->
        let str_arguments_list = List.map (string_of_parsed_boolean_expression variable_infos) argument_expressions in
        let str_arguments = OCamlUtilities.string_of_list_of_string_with_sep ", " str_arguments_list in
        label_of_parsed_factor_constructor func ^ "(" ^ str_arguments ^ ")"


and string_of_parsed_boolean_expression variable_infos = function
    | Parsed_conj_dis (l_expr, r_expr, parsed_conj_dis) ->
            string_of_parsed_boolean_expression variable_infos l_expr
            ^ string_of_parsed_conj_dis parsed_conj_dis
            ^ string_of_parsed_boolean_expression variable_infos r_expr

    | Parsed_Discrete_boolean_expression expr ->
        string_of_parsed_discrete_boolean_expression variable_infos expr

and string_of_parsed_discrete_boolean_expression variable_infos = function
    | Parsed_arithmetic_expression expr ->
        string_of_parsed_arithmetic_expression variable_infos expr
    | Parsed_comparison (l_expr, relop, r_expr) ->
        string_of_parsed_relop
            relop
            (string_of_parsed_discrete_boolean_expression variable_infos l_expr)
            (string_of_parsed_discrete_boolean_expression variable_infos r_expr)
    | Parsed_comparison_in (expr1, expr2, expr3) ->
        (* Compute the first one to avoid redundancy *)
        let str_expr1 = string_of_parsed_arithmetic_expression variable_infos expr1 in
        let str_expr2 = string_of_parsed_arithmetic_expression variable_infos expr2 in
        let str_expr3 = string_of_parsed_arithmetic_expression variable_infos expr3 in
        str_expr1 ^ " in [" ^ str_expr2 ^ ".." ^ str_expr3 ^ "]"
    | Parsed_boolean_expression expr ->
        string_of_parsed_boolean_expression variable_infos expr
    | Parsed_Not expr ->
            "not (" ^ (string_of_parsed_boolean_expression variable_infos expr) ^ ")"

and string_of_parsed_seq_code_bloc variable_infos = function
        | Parsed_local_decl (variable_name, discrete_type, init_expr, next_expr, _) ->
            string_of_let_in
                variable_name
                (DiscreteType.string_of_var_type_discrete discrete_type)
                (string_of_parsed_boolean_expression variable_infos init_expr)
            ^ "\n"
            ^ string_of_parsed_seq_code_bloc variable_infos next_expr

        | Parsed_assignment (normal_update, next_expr) ->
            string_of_parsed_normal_update variable_infos normal_update
            ^ string_of_parsed_seq_code_bloc variable_infos next_expr

        | Parsed_for_loop (variable_name, from_expr, to_expr, loop_dir, inner_bloc, next_expr, _) ->
            "for "
            ^ variable_name
            ^ " = "
            ^ string_of_parsed_arithmetic_expression variable_infos from_expr
            ^ (match loop_dir with Parsed_for_loop_up -> " to " | Parsed_for_loop_down -> " downto ")
            ^ string_of_parsed_arithmetic_expression variable_infos to_expr
            ^ " do\n"
            ^ string_of_parsed_seq_code_bloc variable_infos inner_bloc
            ^ "\ndone\n"
            ^ string_of_parsed_seq_code_bloc variable_infos next_expr

        | Parsed_while_loop (condition_expr, inner_bloc, next_expr) ->
            "while "
            ^ string_of_parsed_boolean_expression variable_infos condition_expr
            ^ " do\n"
            ^ string_of_parsed_seq_code_bloc variable_infos inner_bloc
            ^ "\ndone\n"
            ^ string_of_parsed_seq_code_bloc variable_infos next_expr

        | Parsed_if (condition_expr, then_bloc, else_bloc_opt, next_expr) ->
            (* string representation of else bloc if defined *)
            let str_else =
                match else_bloc_opt with
                | Some else_bloc ->
                    " else " ^ string_of_parsed_seq_code_bloc variable_infos else_bloc
                | None -> ""
            in

            "if "
            ^ string_of_parsed_boolean_expression variable_infos condition_expr
            ^ " then "
            ^ string_of_parsed_seq_code_bloc variable_infos then_bloc
            ^ str_else
            ^ " end\n\n"
            ^ string_of_parsed_seq_code_bloc variable_infos next_expr

        | Parsed_return_expr expr ->
            "return " ^ string_of_parsed_boolean_expression variable_infos expr
        | Parsed_bloc_void -> ""

and string_of_parsed_normal_update variable_infos (update_type, expr) =
    let str_left_member = string_of_parsed_update_type variable_infos update_type in
    let str_right_member = string_of_parsed_boolean_expression variable_infos expr in
    string_of_assignment str_left_member str_right_member

and string_of_parsed_clock_update variable_infos (scalar_or_index_update_type, expr) =
    let str_left_member = string_of_parsed_scalar_or_index_update_type variable_infos scalar_or_index_update_type in
    let str_right_member = string_of_parsed_boolean_expression variable_infos expr in
    string_of_assignment str_left_member str_right_member

and string_of_parsed_update variable_infos = function
	| Normal normal_update ->
        string_of_parsed_normal_update variable_infos normal_update
	| Condition (bool_expr, update_list_if, update_list_else) ->
	    let str_update_if_list = List.map (string_of_parsed_normal_update variable_infos) update_list_if in
	    let str_update_else_list = List.map (string_of_parsed_normal_update variable_infos) update_list_else in
	    let count_update_in_else = List.length str_update_else_list in
	    "if "
        ^ string_of_parsed_boolean_expression variable_infos bool_expr
        ^ " then "
        ^ OCamlUtilities.string_of_list_of_string_with_sep ", " str_update_if_list
        ^ (if count_update_in_else > 0 then " else " ^ OCamlUtilities.string_of_list_of_string_with_sep "," str_update_else_list else "")
        ^ " end"

(* Get variable name from a variable access *)
(* ex : my_var[0][0] -> my_var *)
and string_of_parsed_scalar_or_index_update_type variable_infos = function
    | Parsed_scalar_update variable_name -> variable_name
    | Parsed_indexed_update (parsed_scalar_or_index_update_type, expr) ->
        let l_del, r_del = Constants.default_array_string.array_access_delimiter in
        string_of_parsed_scalar_or_index_update_type variable_infos parsed_scalar_or_index_update_type
        ^ l_del ^ string_of_parsed_arithmetic_expression variable_infos expr ^ r_del

(* Get variable name if any *)
and string_of_parsed_update_type variable_infos = function
    | Parsed_variable_update parsed_scalar_or_index_update_type ->
        string_of_parsed_scalar_or_index_update_type variable_infos parsed_scalar_or_index_update_type
    | Parsed_void_update -> ""

let string_of_parsed_fun_def variable_infos fun_def =
    (* Format each parameters to string *)
    let str_parameters_list = List.map (fun (parameter_name, parameter_type) -> parameter_name ^ " : " ^ DiscreteType.string_of_var_type_discrete parameter_type) fun_def.parameters in
    (* Format all parameters to string *)
    let str_parameters = OCamlUtilities.string_of_list_of_string_with_sep ", " str_parameters_list in
    (* Format function definition to string *)
    "fn " ^ fun_def.name ^ " (" ^ str_parameters ^ ") : " ^ DiscreteType.string_of_var_type_discrete fun_def.return_type ^ "\n"
    ^ string_of_parsed_seq_code_bloc variable_infos fun_def.body ^ "\n"
    ^ "end\n"

let rec string_of_parsed_linear_constraint variable_infos = function
	| Parsed_true_constraint -> "True"
	| Parsed_false_constraint -> "False"
	| Parsed_linear_constraint (l_expr, relop, r_expr) ->
	    string_of_parsed_relop
            relop
            (string_of_linear_expression variable_infos l_expr)
            (string_of_linear_expression variable_infos r_expr)

and string_of_linear_expression variable_infos = function
	| Linear_term term -> string_of_linear_term variable_infos term
	| Linear_plus_expression (expr, term) ->
	    string_of_linear_expression variable_infos expr
	    ^ " + "
	    ^ string_of_linear_term variable_infos term
	| Linear_minus_expression (expr, term) ->
	    string_of_linear_expression variable_infos expr
	    ^ " - "
	    ^ string_of_linear_term variable_infos term

and string_of_linear_term variable_infos = function
	| Constant c -> NumConst.string_of_numconst c
	| Variable (coef, variable_name) when NumConst.equal NumConst.one coef -> variable_name
	| Variable (coef, variable_name) -> (NumConst.string_of_numconst coef)

let string_of_parsed_init_state_predicate variable_infos = function
	| Parsed_loc_assignment (automaton_name, location_name) -> "loc[" ^ automaton_name ^ "] = " ^ location_name
	| Parsed_linear_predicate linear_constraint -> string_of_parsed_linear_constraint variable_infos linear_constraint
	| Parsed_discrete_predicate (variable_name, expr) ->
	    variable_name
	    ^ " = "
	    ^ string_of_parsed_boolean_expression variable_infos expr


let string_of_parsed_nonlinear_constraint = string_of_parsed_discrete_boolean_expression



let string_of_parsed_loc_predicate = function
	| Parsed_loc_predicate_EQ (automaton_name, location_name) ->
	    automaton_name ^ " = " ^ location_name
	| Parsed_loc_predicate_NEQ (automaton_name, location_name) ->
	    automaton_name ^ " <> " ^ location_name

let string_of_parsed_simple_predicate variable_infos = function
	| Parsed_discrete_boolean_expression expr -> string_of_parsed_discrete_boolean_expression variable_infos expr
	| Parsed_loc_predicate parsed_loc_predicate -> string_of_parsed_loc_predicate parsed_loc_predicate
	| Parsed_state_predicate_true -> Constants.default_string.true_string
	| Parsed_state_predicate_false -> Constants.default_string.false_string
	| Parsed_state_predicate_accepting -> "accepting"

let rec string_of_parsed_state_predicate_factor variable_infos = function
	| Parsed_state_predicate_factor_NOT predicate_factor ->
	    Constants.default_string.not_operator
	    ^ "(" ^ string_of_parsed_state_predicate_factor variable_infos predicate_factor ^ ")"
	| Parsed_simple_predicate simple_predicate ->
	    string_of_parsed_simple_predicate variable_infos simple_predicate
	| Parsed_state_predicate state_predicate ->
	    string_of_parsed_state_predicate variable_infos state_predicate

and string_of_parsed_state_predicate_term variable_infos = function
	| Parsed_state_predicate_term_AND (l_predicate_term, r_predicate_term) ->
	    string_of_parsed_state_predicate_term variable_infos l_predicate_term
	    ^ Constants.default_string.and_operator
	    ^ string_of_parsed_state_predicate_term variable_infos r_predicate_term

	| Parsed_state_predicate_factor predicate_factor ->
	    string_of_parsed_state_predicate_factor variable_infos predicate_factor

and string_of_parsed_state_predicate variable_infos = function
	| Parsed_state_predicate_OR (l_predicate_term, r_predicate_term) ->
	    string_of_parsed_state_predicate variable_infos l_predicate_term
	    ^ Constants.default_string.or_operator
	    ^ string_of_parsed_state_predicate variable_infos r_predicate_term

	| Parsed_state_predicate_term predicate_term ->
	    string_of_parsed_state_predicate_term variable_infos predicate_term

let json_of_function_metadata (fm : function_metadata) =
    JsonFormatter.Json_struct [
        "name", JsonFormatter.Json_string fm.name;
        "side-effects", JsonFormatter.Json_bool fm.side_effect
    ]
