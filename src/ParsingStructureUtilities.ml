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

(**)
type variable_callback = (variable_name -> unit) option

(* Leaves of parsing structure *)
type parsing_structure_leaf =
    | Leaf_variable of variable_ref
    | Leaf_constant of ParsedValue.parsed_value
    | Leaf_fun of variable_name

(* Leaves of parsed bloc *)
type parsed_seq_code_bloc_leaf =
    | Leaf_update_variable of variable_ref * parsed_boolean_expression

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
type 'a parsing_structure_leaf_callback = parsing_structure_leaf -> 'a
(* Type of callback function called when reach a leaf of a sequential code bloc *)
type 'a seq_code_bloc_leaf_callback = parsed_seq_code_bloc_leaf -> 'a
(* Type of callback function called when reach a leaf of a linear expression *)
type 'a linear_expression_leaf_callback = linear_expression_leaf -> 'a

type 'a variable_declaration_callback = (variable_name * var_type_discrete * int -> 'a) option

(** Fold a parsing structure using operator applying custom function on leafs **)

let rec fold_parsed_boolean_expression operator base leaf_fun = function
    | Parsed_conj_dis (l_expr, r_expr, _) ->
	    operator
	        (fold_parsed_boolean_expression operator base leaf_fun l_expr)
	        (fold_parsed_boolean_expression operator base leaf_fun r_expr)
	| Parsed_discrete_bool_expr expr ->
	    fold_parsed_discrete_boolean_expression operator base leaf_fun expr

and fold_parsed_discrete_boolean_expression operator base leaf_fun = function
    | Parsed_arithmetic_expr expr ->
        fold_parsed_discrete_arithmetic_expression operator base leaf_fun expr
	| Parsed_comparison (l_expr, _, r_expr) ->
	    operator
	        (fold_parsed_discrete_boolean_expression operator base leaf_fun l_expr)
	        (fold_parsed_discrete_boolean_expression operator base leaf_fun r_expr)
	| Parsed_comparison_in (lower_expr, expr, upper_expr) ->
	    operator
	        (fold_parsed_discrete_arithmetic_expression operator base leaf_fun expr)
            (operator
                (fold_parsed_discrete_arithmetic_expression operator base leaf_fun lower_expr)
                (fold_parsed_discrete_arithmetic_expression operator base leaf_fun upper_expr))
	| Parsed_nested_bool_expr expr
	| Parsed_not expr ->
        fold_parsed_boolean_expression operator base leaf_fun expr

and fold_parsed_discrete_arithmetic_expression operator base leaf_fun = function
	| Parsed_sum_diff (expr, term, _) ->
        operator
            (fold_parsed_discrete_arithmetic_expression operator base leaf_fun expr)
            (fold_parsed_discrete_term operator base leaf_fun term)
	| Parsed_term term ->
        fold_parsed_discrete_term operator base leaf_fun term

and fold_parsed_discrete_term operator base leaf_fun = function
	| Parsed_product_quotient (term, factor, _) ->
	    operator
	        (fold_parsed_discrete_term operator base leaf_fun term)
	        (fold_parsed_discrete_factor operator base leaf_fun factor)
	| Parsed_factor factor ->
        fold_parsed_discrete_factor operator base leaf_fun factor

and fold_parsed_discrete_factor operator base leaf_fun = function
	| Parsed_variable variable_ref ->
	    leaf_fun (Leaf_variable variable_ref)

	| Parsed_constant value -> leaf_fun (Leaf_constant value)
	| Parsed_sequence (expr_list, _) -> List.fold_left (fun acc expr -> operator acc (fold_parsed_boolean_expression operator base leaf_fun expr)) base expr_list
	| Parsed_nested_expr expr ->
        fold_parsed_discrete_arithmetic_expression operator base leaf_fun expr
    | Parsed_function_call (function_name, argument_expressions) ->
        operator
            (leaf_fun (Leaf_fun function_name))
            (List.fold_left (fun acc expr -> operator (fold_parsed_boolean_expression operator base leaf_fun expr) acc) base argument_expressions)
    | Parsed_access (factor, _)
	(* | Parsed_log_not factor *)
	| Parsed_unary_min factor ->
	    fold_parsed_discrete_factor operator base leaf_fun factor

and fold_parsed_seq_code_bloc operator base ?(decl_callback=None) seq_code_bloc_leaf_fun leaf_fun (* parsed_seq_code_bloc *) =


    let rec fold_parsed_seq_code_bloc_rec parsed_seq_code_bloc =
        List.fold_left (fun acc instruction -> operator (fold_parsed_instruction instruction) acc) base parsed_seq_code_bloc

    and fold_parsed_instruction = function
        | Parsed_local_decl ((variable_name, id), discrete_type, init_expr) ->

            (* Fold init expr *)
            let init_expr_result = fold_parsed_boolean_expression operator base leaf_fun init_expr in

            let decl_callback_result =
                match decl_callback with
                | Some decl_callback -> decl_callback (variable_name, discrete_type, id)
                | None -> base
            in

            init_expr_result
            |> operator decl_callback_result

        | Parsed_for_loop ((variable_name, id), from_expr, to_expr, _, inner_bloc) ->

            let decl_callback_result =
                match decl_callback with
                | Some decl_callback -> decl_callback (variable_name, (Dt_number Dt_int), id)
                | None -> base
            in

            (fold_parsed_discrete_arithmetic_expression operator base leaf_fun from_expr)
            |> operator (fold_parsed_discrete_arithmetic_expression operator base leaf_fun to_expr)
            |> operator (fold_parsed_seq_code_bloc_rec inner_bloc)
            |> operator decl_callback_result

        | Parsed_while_loop (condition_expr, inner_bloc) ->
            fold_parsed_boolean_expression operator base leaf_fun condition_expr
            |> operator (fold_parsed_seq_code_bloc_rec inner_bloc)

        | Parsed_if (condition_expr, then_bloc, else_bloc_opt) ->

            let else_bloc_result =
                match else_bloc_opt with
                | Some else_bloc -> fold_parsed_seq_code_bloc_rec else_bloc
                | None -> base
            in

            fold_parsed_boolean_expression operator base leaf_fun condition_expr
            |> operator (fold_parsed_seq_code_bloc_rec then_bloc)
            |> operator else_bloc_result

        | Parsed_assignment normal_update ->
            fold_parsed_normal_update operator base ~decl_callback:decl_callback seq_code_bloc_leaf_fun leaf_fun normal_update

        | Parsed_instruction expr ->
            fold_parsed_boolean_expression operator base leaf_fun expr

    in
    fold_parsed_seq_code_bloc_rec (* parsed_seq_code_bloc *)

and fold_parsed_normal_update operator base ?(decl_callback=None) seq_code_bloc_leaf_fun leaf_fun (parsed_scalar_or_index_update_type, expr) =

    let rec fold_parsed_scalar_or_index_update_type_with_local_variables operator base ?(decl_callback=None) seq_code_bloc_leaf_fun leaf_fun = function
        | Parsed_scalar_update variable_ref ->
            seq_code_bloc_leaf_fun (Leaf_update_variable (variable_ref, expr))

        | Parsed_indexed_update (parsed_scalar_or_index_update_type, index_expr) ->
            operator
                (fold_parsed_scalar_or_index_update_type_with_local_variables operator base ~decl_callback:decl_callback seq_code_bloc_leaf_fun leaf_fun parsed_scalar_or_index_update_type)
                (fold_parsed_discrete_arithmetic_expression operator base leaf_fun index_expr)
    in
    operator
        (fold_parsed_scalar_or_index_update_type_with_local_variables operator base ~decl_callback:decl_callback seq_code_bloc_leaf_fun leaf_fun parsed_scalar_or_index_update_type)
        (fold_parsed_boolean_expression operator base leaf_fun expr)

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
let fold_parsed_fun_def operator base ?(decl_callback=None) seq_code_bloc_leaf_fun leaf_fun (fun_def : parsed_fun_definition) =

    let decl_callback_result =
        match decl_callback with
        | Some decl_callback -> List.fold_left (fun acc ((param_name, id), param_type) -> operator acc (decl_callback (param_name, param_type, id))) base fun_def.parameters
        | None -> base
    in

    let code_bloc, return_expr_opt = fun_def.body in

    let parsed_seq_code_bloc_result = fold_parsed_seq_code_bloc operator base ~decl_callback:decl_callback seq_code_bloc_leaf_fun leaf_fun code_bloc in

    let return_expr_result =
        match return_expr_opt with
        | Some return_expr -> fold_parsed_boolean_expression operator base leaf_fun return_expr
        | None -> base
    in

    parsed_seq_code_bloc_result |> operator return_expr_result |> operator decl_callback_result

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
let for_all_in_parsed_normal_update = apply_evaluate_and_with_base fold_parsed_normal_update
(** Check if all leaf of a parsed normal update satisfy the predicate **)
let for_all_in_parsed_normal_update = apply_evaluate_and_with_base fold_parsed_normal_update

let for_all_in_parsed_loc_predicate = apply_evaluate_and_with_base fold_parsed_loc_predicate
let for_all_in_parsed_simple_predicate = apply_evaluate_and_with_base fold_parsed_simple_predicate
let for_all_in_parsed_state_predicate_factor = apply_evaluate_and_with_base fold_parsed_state_predicate_factor
let for_all_in_parsed_state_predicate_term = apply_evaluate_and_with_base fold_parsed_state_predicate_term
let for_all_in_parsed_state_predicate = apply_evaluate_and_with_base fold_parsed_state_predicate

(** Check if all leaf of a parsed sequential code bloc satisfy the predicate **)
let for_all_in_parsed_seq_code_bloc = apply_evaluate_and_with_base fold_parsed_seq_code_bloc
(** Check if all leaf of a parsed function definition satisfy the predicate **)
let for_all_in_parsed_fun_def = apply_evaluate_and_with_base fold_parsed_fun_def

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
(** Check if any leaf of a parsed normal update satisfy the predicate **)
let exists_in_parsed_normal_update = apply_evaluate_or_with_base fold_parsed_normal_update

let exists_in_parsed_loc_predicate = apply_evaluate_or_with_base fold_parsed_loc_predicate
let exists_in_parsed_simple_predicate = apply_evaluate_or_with_base fold_parsed_simple_predicate
let exists_in_parsed_state_predicate_factor = apply_evaluate_or_with_base fold_parsed_state_predicate_factor
let exists_in_parsed_state_predicate_term = apply_evaluate_or_with_base fold_parsed_state_predicate_term
let exists_in_parsed_state_predicate = apply_evaluate_or_with_base fold_parsed_state_predicate

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

let iterate_parsed_normal_update = apply_evaluate_unit_with_base fold_parsed_normal_update
let iterate_in_parsed_loc_predicate = apply_evaluate_unit_with_base fold_parsed_loc_predicate
let iterate_in_parsed_simple_predicate = apply_evaluate_unit_with_base fold_parsed_simple_predicate
let iterate_in_parsed_state_predicate_factor = apply_evaluate_unit_with_base fold_parsed_state_predicate_factor
let iterate_in_parsed_state_predicate_term = apply_evaluate_unit_with_base fold_parsed_state_predicate_term
let iterate_in_parsed_state_predicate = apply_evaluate_unit_with_base fold_parsed_state_predicate

(** Iterate over a parsed sequential code bloc definition **)
let iterate_in_parsed_seq_code_bloc = apply_evaluate_unit_with_base fold_parsed_seq_code_bloc
(** Iterate over a parsed function definition **)
let iterate_in_parsed_function_definition = apply_evaluate_unit_with_base fold_parsed_fun_def

let label_of_parsed_sequence_type = function
    | Parsed_array -> "array"
    | Parsed_list -> "list"
    | Parsed_stack -> Constants.stack_string
    | Parsed_queue -> "queue"

let label_of_parsed_factor_constructor = function
	| Parsed_variable _ -> "variable"
	| Parsed_constant _ -> "constant"
	| Parsed_sequence (_, seq_type) -> label_of_parsed_sequence_type seq_type
	| Parsed_access _ -> "access"
	| Parsed_nested_expr _ -> "expression"
	| Parsed_unary_min _ -> "minus"
    | Parsed_function_call (function_name, _) -> function_name



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
let string_of_assignment str_left_member str_right_member = str_left_member ^ " := " ^ str_right_member

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

    | Parsed_term term ->
        string_of_parsed_term variable_infos term

and string_of_parsed_term variable_infos = function
    | Parsed_product_quotient (term, factor, parsed_product_quotient) ->
        (* TODO benjamin IMPROVE Check if there is not only product or quotient in term add parenthesis *)
        (* TODO benjamin IMPROVE Check if there is not only product or quotient in factor add parenthesis *)
        string_of_parsed_term variable_infos term
        ^ string_of_parsed_product_quotient parsed_product_quotient
        ^ string_of_parsed_factor variable_infos factor

    | Parsed_factor factor ->
        string_of_parsed_factor variable_infos factor

and string_of_parsed_factor variable_infos = function
    | Parsed_variable (variable_name, _) ->
        if (is_constant_is_defined variable_infos variable_name) then (
            (* Retrieve the value of the global constant *)
            let value = value_of_constant_name variable_infos variable_name in
            variable_name
            ^ "="
            ^ AbstractValue.string_of_value value
        ) else
            variable_name
    | Parsed_constant value -> ParsedValue.string_of_value value
    | Parsed_sequence (expr_list, seq_type) as seq ->
        let str_elements = List.map (string_of_parsed_boolean_expression variable_infos) expr_list in
        let str_array = "[" ^ OCamlUtilities.string_of_list_of_string_with_sep ", " str_elements ^ "]" in
        (match seq_type with
        | Parsed_array -> str_array
        | Parsed_list
        | Parsed_stack
        | Parsed_queue -> label_of_parsed_factor_constructor seq ^ "(" ^ str_array ^ ")"
        )
    | Parsed_access (factor, expr) ->
        string_of_parsed_factor variable_infos factor ^ "[" ^ string_of_parsed_arithmetic_expression variable_infos expr ^ "]"
    | Parsed_nested_expr expr ->
        let l_paren, r_paren = Constants.default_paren_delimiter in
        l_paren ^ string_of_parsed_arithmetic_expression variable_infos expr ^ r_paren

    | Parsed_unary_min factor ->
        (* Only add parenthesis if factor is a nested expression *)
        let l_paren, r_paren =
            match factor with
            | Parsed_nested_expr _ -> Constants.default_paren_delimiter
            | _ -> "", ""
        in
        "-" ^ l_paren ^ string_of_parsed_factor variable_infos factor ^ r_paren

    | Parsed_function_call (_, argument_expressions) as func ->
        let str_arguments_list = List.map (string_of_parsed_boolean_expression variable_infos) argument_expressions in
        let str_arguments = OCamlUtilities.string_of_list_of_string_with_sep ", " str_arguments_list in
        label_of_parsed_factor_constructor func ^ "(" ^ str_arguments ^ ")"


and string_of_parsed_boolean_expression variable_infos = function
    | Parsed_conj_dis (l_expr, r_expr, parsed_conj_dis) ->
        string_of_parsed_boolean_expression variable_infos l_expr
        ^ string_of_parsed_conj_dis parsed_conj_dis
        ^ string_of_parsed_boolean_expression variable_infos r_expr

    | Parsed_discrete_bool_expr expr ->
        string_of_parsed_discrete_boolean_expression variable_infos expr

and string_of_parsed_discrete_boolean_expression variable_infos = function
    | Parsed_arithmetic_expr expr ->
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
    | Parsed_nested_bool_expr expr ->
        string_of_parsed_boolean_expression variable_infos expr
    | Parsed_not expr ->
            "not (" ^ (string_of_parsed_boolean_expression variable_infos expr) ^ ")"

and string_of_parsed_seq_code_bloc variable_infos parsed_seq_code_bloc =
    let str_instructions = List.map (string_of_parsed_instruction variable_infos) parsed_seq_code_bloc in
    OCamlUtilities.string_of_list_of_string_with_sep "\n" str_instructions

and string_of_parsed_instruction variable_infos = function
    | Parsed_local_decl ((variable_name, _ (* id *)), discrete_type, init_expr) ->
        string_of_let_in
            variable_name
            (DiscreteType.string_of_var_type_discrete discrete_type)
            (string_of_parsed_boolean_expression variable_infos init_expr)

    | Parsed_assignment normal_update ->
        string_of_parsed_normal_update variable_infos normal_update

    | Parsed_instruction expr ->
        string_of_parsed_boolean_expression variable_infos expr

    | Parsed_for_loop ((variable_name, _ (* id *)), from_expr, to_expr, loop_dir, inner_bloc) ->
        "for "
        ^ variable_name
        ^ " = "
        ^ string_of_parsed_arithmetic_expression variable_infos from_expr
        ^ (match loop_dir with Parsed_for_loop_up -> " to " | Parsed_for_loop_down -> " downto ")
        ^ string_of_parsed_arithmetic_expression variable_infos to_expr
        ^ " do\n"
        ^ string_of_parsed_seq_code_bloc variable_infos inner_bloc
        ^ "\ndone"

    | Parsed_while_loop (condition_expr, inner_bloc) ->
        "while "
        ^ string_of_parsed_boolean_expression variable_infos condition_expr
        ^ " do\n"
        ^ string_of_parsed_seq_code_bloc variable_infos inner_bloc
        ^ "\ndone"

    | Parsed_if (condition_expr, then_bloc, else_bloc_opt) ->
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
        ^ " end\n"

and string_of_parsed_normal_update variable_infos (parsed_scalar_or_index_update_type, expr) =
    let str_left_member = string_of_parsed_scalar_or_index_update_type variable_infos parsed_scalar_or_index_update_type in
    let str_right_member = string_of_parsed_boolean_expression variable_infos expr in
    string_of_assignment str_left_member str_right_member

and string_of_parsed_clock_update variable_infos (scalar_or_index_update_type, expr) =
    let str_left_member = string_of_parsed_scalar_or_index_update_type variable_infos scalar_or_index_update_type in
    let str_right_member = string_of_parsed_boolean_expression variable_infos expr in
    string_of_assignment str_left_member str_right_member

(* Get variable name from a variable access *)
(* ex : my_var[0][0] -> my_var *)
and string_of_parsed_scalar_or_index_update_type variable_infos = function
    | Parsed_scalar_update (variable_name, _ (* id *)) -> variable_name
    | Parsed_indexed_update (parsed_scalar_or_index_update_type, expr) ->
        let l_del, r_del = Constants.default_array_string.array_access_delimiter in
        string_of_parsed_scalar_or_index_update_type variable_infos parsed_scalar_or_index_update_type
        ^ l_del ^ string_of_parsed_arithmetic_expression variable_infos expr ^ r_del

let string_of_parsed_fun_def_body variable_infos (code_bloc, return_expr_opt) =
    let str_return_expr =
        match return_expr_opt with
        | Some return_expr -> "\n" ^ string_of_parsed_boolean_expression variable_infos return_expr
        | None -> ""
    in
    string_of_parsed_seq_code_bloc variable_infos code_bloc ^ str_return_expr

let string_of_parsed_fun_def variable_infos fun_def =
    (* Format each parameters to string *)
    let str_parameters_list = List.map (fun ((parameter_name, _ (* id *)), parameter_type) -> parameter_name ^ " : " ^ DiscreteType.string_of_var_type_discrete parameter_type) fun_def.parameters in
    (* Format all parameters to string *)
    let str_parameters = OCamlUtilities.string_of_list_of_string_with_sep ", " str_parameters_list in
    (* Format function definition to string *)
    "fn " ^ fun_def.name ^ " (" ^ str_parameters ^ ") : " ^ DiscreteType.string_of_var_type_discrete fun_def.return_type ^ "\n"
    ^ string_of_parsed_fun_def_body variable_infos fun_def.body ^ "\n"
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

(* Functions below, reconstruct the parsed model by making the link between variables occurrence and their declarations *)
(* I would like to do it more simply, but it was impossible to do directly in the parser (action )*)
(* These functions are really important because it allow to retrieve variables *)

(* Set to variable ref, variable id *)
let link_variables_in_parsed_model parsed_model =

    (* Accumulator that will contains all local declared variables *)
    let local_variables_accumulator = Hashtbl.create 0 in

    let rec link_variables_in_parsed_boolean_expression local_variables = function
        | Parsed_conj_dis (l_expr, r_expr, parsed_conj_dis) ->
            Parsed_conj_dis (
                link_variables_in_parsed_boolean_expression local_variables l_expr,
                link_variables_in_parsed_boolean_expression local_variables r_expr,
                parsed_conj_dis
            )

        | Parsed_discrete_bool_expr expr ->
            Parsed_discrete_bool_expr (
                link_variables_in_parsed_discrete_boolean_expression local_variables expr
            )

    and link_variables_in_parsed_discrete_boolean_expression local_variables = function
        | Parsed_arithmetic_expr expr ->
            Parsed_arithmetic_expr (
                link_variables_in_parsed_arithmetic_expression local_variables expr
            )

        | Parsed_comparison (l_expr, relop, r_expr) ->
            Parsed_comparison (
                link_variables_in_parsed_discrete_boolean_expression local_variables l_expr,
                relop,
                link_variables_in_parsed_discrete_boolean_expression local_variables r_expr
            )

        | Parsed_comparison_in (lw_expr, md_expr, up_expr) ->
            Parsed_comparison_in (
                link_variables_in_parsed_arithmetic_expression local_variables lw_expr,
                link_variables_in_parsed_arithmetic_expression local_variables md_expr,
                link_variables_in_parsed_arithmetic_expression local_variables up_expr
            )

        | Parsed_nested_bool_expr expr ->
            Parsed_nested_bool_expr (
                link_variables_in_parsed_boolean_expression local_variables expr
            )

        | Parsed_not expr ->
            Parsed_not (
                link_variables_in_parsed_boolean_expression local_variables expr
            )

    and link_variables_in_parsed_arithmetic_expression local_variables = function
        | Parsed_sum_diff (expr, term, parsed_sum_diff) ->
            Parsed_sum_diff (
                link_variables_in_parsed_arithmetic_expression local_variables expr,
                link_variables_in_parsed_discrete_term local_variables term,
                parsed_sum_diff
            )

        | Parsed_term term ->
            Parsed_term (
                link_variables_in_parsed_discrete_term local_variables term
            )

    and link_variables_in_parsed_discrete_term local_variables = function
        | Parsed_product_quotient (term, factor, parsed_product_quotient) ->
            Parsed_product_quotient (
                link_variables_in_parsed_discrete_term local_variables term,
                link_variables_in_parsed_discrete_factor local_variables factor,
                parsed_product_quotient
            )

        | Parsed_factor factor ->
            Parsed_factor (
                link_variables_in_parsed_discrete_factor local_variables factor
            )

    and link_variables_in_parsed_discrete_factor local_variables = function
        | Parsed_variable (variable_name, _ (* init id *)) ->
            (* Found variable id in local variables context *)
            let variable_id =
                let variable_opt = Hashtbl.find_opt local_variables variable_name in
                match variable_opt with
                (* If found in local variables get id *)
                | Some (discrete_type, id) -> id
                | None -> 0 (* global variable id *)
            in
            (* Print linking info *)
            ImitatorUtilities.print_message_lazy Verbose_high (lazy ("  link variable ref `(" ^ variable_name ^ "," ^ string_of_int variable_id ^ ")`"));
            Parsed_variable (variable_name, variable_id)

        | Parsed_constant _ as constant -> constant
        | Parsed_sequence (expressions, parsed_sequence_type) ->
            Parsed_sequence (
                List.map (link_variables_in_parsed_boolean_expression local_variables) expressions,
                parsed_sequence_type
            )

        | Parsed_access (element, index_expr) ->
            Parsed_access (
                link_variables_in_parsed_discrete_factor local_variables element,
                link_variables_in_parsed_arithmetic_expression local_variables index_expr
            )

        | Parsed_nested_expr expr ->
            Parsed_nested_expr (
                link_variables_in_parsed_arithmetic_expression local_variables expr
            )

        | Parsed_unary_min factor ->
            Parsed_unary_min (
                link_variables_in_parsed_discrete_factor local_variables factor
            )

        | Parsed_function_call (function_name, arguments) ->
            Parsed_function_call (
                function_name,
                List.map (link_variables_in_parsed_boolean_expression local_variables) arguments
            )
    in

    let rec link_variables_in_parsed_scalar_or_index_update_type local_variables = function
        | Parsed_scalar_update (variable_name, _) ->
            (* Found variable id *)
            let variable_id =
                let variable_opt = Hashtbl.find_opt local_variables variable_name in
                match variable_opt with
                (* If found in local variables get id *)
                | Some (discrete_type, id) -> id
                | None -> 0 (* global variable id *)
            in
            (* Print linking info *)
            ImitatorUtilities.print_message_lazy Verbose_high (lazy ("  link assigned variable `(" ^ variable_name ^ "," ^ string_of_int variable_id ^ ")`"));

            Parsed_scalar_update (variable_name, variable_id)

        | Parsed_indexed_update (parsed_scalar_or_index_update_type, expr) ->
            let linked_parsed_scalar_or_index_update_type = link_variables_in_parsed_scalar_or_index_update_type local_variables parsed_scalar_or_index_update_type in
            let linked_expr = link_variables_in_parsed_arithmetic_expression local_variables expr in
            Parsed_indexed_update (linked_parsed_scalar_or_index_update_type, linked_expr)
    in

    let link_variables_in_parsed_seq_code_bloc local_variables (* seq_code_bloc *) =

        let rec link_variables_in_parsed_seq_code_bloc local_variables seq_code_bloc =
            List.map (link_variables_in_parsed_instruction local_variables) seq_code_bloc

        and link_variables_in_parsed_instruction local_variables = function
            | Parsed_local_decl ((variable_name, id) as variable_ref, discrete_type, expr) ->
                let linked_expr = link_variables_in_parsed_boolean_expression local_variables expr in
                Hashtbl.replace local_variables variable_name (discrete_type, id);
                Hashtbl.add local_variables_accumulator variable_ref (Var_type_discrete discrete_type);
                Parsed_local_decl (variable_ref, discrete_type, linked_expr)

            | Parsed_assignment (parsed_scalar_or_index_update_type, expr) ->
                let linked_parsed_scalar_or_index_update_type = link_variables_in_parsed_scalar_or_index_update_type local_variables parsed_scalar_or_index_update_type in
                let linked_expr = link_variables_in_parsed_boolean_expression local_variables expr in
                Parsed_assignment (linked_parsed_scalar_or_index_update_type, linked_expr)

            | Parsed_instruction expr ->
                Parsed_instruction (link_variables_in_parsed_boolean_expression local_variables expr)

            | Parsed_for_loop ((variable_name, id) as variable_ref, from_expr, to_expr, parsed_loop_dir, inner_bloc) ->
                let linked_from_expr = link_variables_in_parsed_arithmetic_expression local_variables from_expr in
                let linked_to_expr = link_variables_in_parsed_arithmetic_expression local_variables to_expr in
                (* Add variable used for loop to inner local variables scope *)
                let inner_local_variables = Hashtbl.copy local_variables in
                let discrete_type = Dt_number Dt_int in
                Hashtbl.replace inner_local_variables variable_name (discrete_type, id);
                Hashtbl.add local_variables_accumulator variable_ref (Var_type_discrete discrete_type);
                let linked_inner_bloc = link_variables_in_parsed_seq_code_bloc inner_local_variables inner_bloc in
                Parsed_for_loop (variable_ref, linked_from_expr, linked_to_expr, parsed_loop_dir, linked_inner_bloc)

            | Parsed_while_loop (condition_expr, inner_bloc) ->
                let linked_condition_expr = link_variables_in_parsed_boolean_expression local_variables condition_expr in
                (* Add variable used for loop to inner local variables scope *)
                let inner_local_variables = Hashtbl.copy local_variables in
                let linked_inner_bloc = link_variables_in_parsed_seq_code_bloc inner_local_variables inner_bloc in
                Parsed_while_loop (linked_condition_expr, linked_inner_bloc)

            | Parsed_if (condition_expr, then_bloc, else_bloc_opt) ->
                let then_local_variables = Hashtbl.copy local_variables in
                let linked_condition_expr = link_variables_in_parsed_boolean_expression local_variables condition_expr in
                let linked_then_bloc = link_variables_in_parsed_seq_code_bloc then_local_variables then_bloc in
                let linked_else_bloc_opt =
                    match else_bloc_opt with
                    | Some else_bloc ->
                        let else_local_variables = Hashtbl.copy local_variables in
                        Some (link_variables_in_parsed_seq_code_bloc else_local_variables else_bloc)
                    | None -> None
                in
                Parsed_if (linked_condition_expr, linked_then_bloc, linked_else_bloc_opt)

        in
        link_variables_in_parsed_seq_code_bloc local_variables (* seq_code_bloc *)
    in

    (* Link variables in function definition *)
    let link_variables_in_fun_defs fun_defs =
        let link_variables_in_fun_def fun_def =

            let code_bloc, return_expr_opt = fun_def.body in

            (* Create local variable table *)
            let local_variables = Hashtbl.create (List.length fun_def.parameters) in
            (* Add parameters to local variables *)
            List.iter (fun (((parameter_name, id) as variable_ref), discrete_type) ->
                Hashtbl.replace local_variables parameter_name (discrete_type, id);
                Hashtbl.add local_variables_accumulator variable_ref (Var_type_discrete discrete_type);
            ) fun_def.parameters;

            (* Link variables in sequential code bloc *)
            let linked_code_bloc = link_variables_in_parsed_seq_code_bloc local_variables code_bloc in
            (* Link variables in return expression (if any) *)
            let linked_return_expr_opt =
                match return_expr_opt with
                | Some return_expr -> Some (link_variables_in_parsed_boolean_expression local_variables return_expr)
                | None -> None
            in
            (* Return new function definition with linked variables *)
            { fun_def with body = linked_code_bloc, linked_return_expr_opt }

        in
        List.map link_variables_in_fun_def fun_defs
    in

    let link_variables_in_automata automata =
        let link_variables_in_automaton automaton =
            let automaton_name, sync_names, locations = automaton in

            let link_variables_in_locations locations =
                let link_variables_in_location location =

                    let link_variables_in_transitions transitions =
                        let link_variables_in_transition transition =

                            let guard, code_bloc, sync, location_name = transition in

                            let link_variables_in_guard = List.map (link_variables_in_parsed_discrete_boolean_expression (Hashtbl.create 0)) in

                            link_variables_in_guard guard, link_variables_in_parsed_seq_code_bloc (Hashtbl.create 0) code_bloc, sync, location_name
                        in
                        List.map link_variables_in_transition transitions
                    in

                    {location with transitions = link_variables_in_transitions location.transitions }
                in
                List.map link_variables_in_location locations
            in
            automaton_name, sync_names, link_variables_in_locations locations
        in
        List.map link_variables_in_automaton automata
    in

    (* Add global declared variables to *)
    List.iter (fun (var_type, variables_list) ->
        List.iter (fun (variable_name, _) -> Hashtbl.add local_variables_accumulator (variable_name, 0) var_type) variables_list
    ) parsed_model.variable_declarations;

    (* Link variables to their declaration in automata and function definitions *)
    {
        parsed_model with
        automata = link_variables_in_automata parsed_model.automata;
        fun_definitions = link_variables_in_fun_defs parsed_model.fun_definitions;
    } , local_variables_accumulator
