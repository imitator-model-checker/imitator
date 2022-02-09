(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: General fonctions for map, filter, traverse, evaluating, etc. parsing structure tree
 *
 * File contributors : Benjamin L.
 * Created           : 2021/03/05
 * Last modified     : 2021/09/29
 *
 ************************************************************)

open Exceptions
open ParsingStructure
open CustomModules

type variable_name = string

(* Leaf of parsing structure *)
type parsing_structure_leaf =
    | Leaf_variable of variable_name
    | Leaf_constant of DiscreteValue.discrete_value

(* Leaf of linear expression *)
type linear_expression_leaf =
    | Leaf_linear_constant of NumConst.t
    | Leaf_linear_variable of NumConst.t * variable_name

(* Leaf of linear constraint *)
type linear_constraint_leaf =
    | Leaf_true_linear_constraint
    | Leaf_false_linear_constraint

(* Leaf of non-linear constraint *)
type nonlinear_constraint_leaf =
    | Leaf_true_nonlinear_constraint
    | Leaf_false_nonlinear_constraint

type init_state_predicate_leaf =
    | Leaf_loc_assignment of automaton_name * location_name

type state_predicate_leaf =
    | Leaf_predicate_true
    | Leaf_predicate_false
    | Leaf_predicate_accepting
    | Leaf_predicate_EQ of string (* automaton name *) * string (* location name *)
    | Leaf_predicate_NEQ of string (* automaton name *) * string (* location name *)

(** Fold a parsing structure using operator applying custom function on leafs **)

let rec fold_parsed_global_expression operator base leaf_fun = function
     | Parsed_global_expression expr -> fold_parsed_boolean_expression operator base leaf_fun expr

and fold_parsed_boolean_expression operator base leaf_fun = function
	| Parsed_And (l_expr, r_expr)
	| Parsed_Or (l_expr, r_expr) ->
	    operator
	        (fold_parsed_boolean_expression operator base leaf_fun l_expr)
	        (fold_parsed_boolean_expression operator base leaf_fun r_expr)
	| Parsed_Discrete_boolean_expression expr ->
	    fold_parsed_discrete_boolean_expression operator base leaf_fun expr

and fold_parsed_discrete_boolean_expression operator base leaf_fun = function
    | Parsed_arithmetic_expression expr ->
        fold_parsed_discrete_arithmetic_expression operator base leaf_fun expr
	| Parsed_expression (l_expr, _, r_expr) ->
	    operator
	        (fold_parsed_discrete_boolean_expression operator base leaf_fun l_expr)
	        (fold_parsed_discrete_boolean_expression operator base leaf_fun r_expr)
	| Parsed_expression_in (lower_expr, expr, upper_expr) ->
	    operator
	        (fold_parsed_discrete_arithmetic_expression operator base leaf_fun expr)
            (operator
                (fold_parsed_discrete_arithmetic_expression operator base leaf_fun lower_expr)
                (fold_parsed_discrete_arithmetic_expression operator base leaf_fun upper_expr))
	| Parsed_boolean_expression expr
	| Parsed_Not expr ->
        fold_parsed_boolean_expression operator base leaf_fun expr

and fold_parsed_discrete_arithmetic_expression operator base leaf_fun = function
	| Parsed_sum_diff (expr, term, _) ->
        operator
            (fold_parsed_discrete_arithmetic_expression operator base leaf_fun expr)
            (fold_parsed_discrete_term operator base leaf_fun term)
	| Parsed_DAE_term term ->
        fold_parsed_discrete_term operator base leaf_fun term

and fold_parsed_discrete_term operator base leaf_fun = function
	| Parsed_product_quotient (term, factor, _) ->
	    operator
	        (fold_parsed_discrete_term operator base leaf_fun term)
	        (fold_parsed_discrete_factor operator base leaf_fun factor)
	| Parsed_DT_factor factor ->
        fold_parsed_discrete_factor operator base leaf_fun factor

and fold_parsed_discrete_factor operator base leaf_fun = function
	| Parsed_DF_variable variable_name -> leaf_fun (Leaf_variable variable_name)
	| Parsed_DF_constant value -> leaf_fun (Leaf_constant value)
	| Parsed_sequence (expr_list, _) -> List.fold_left (fun acc expr -> operator acc (fold_parsed_boolean_expression operator base leaf_fun expr)) base expr_list
	| Parsed_DF_expression expr ->
        fold_parsed_discrete_arithmetic_expression operator base leaf_fun expr
    | Parsed_function_call (_, argument_expressions) ->
        List.fold_left (fun acc expr -> operator (fold_parsed_boolean_expression operator base leaf_fun expr) acc) base argument_expressions
    | Parsed_DF_access (factor, _)
	(* | Parsed_log_not factor *)
	| Parsed_DF_unary_min factor ->
	    fold_parsed_discrete_factor operator base leaf_fun factor


let rec fold_parsed_linear_constraint operator leaf_fun linear_constraint_leaf_fun = function
    | Parsed_true_constraint -> linear_constraint_leaf_fun Leaf_true_linear_constraint
    | Parsed_false_constraint -> linear_constraint_leaf_fun Leaf_false_linear_constraint
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

(** Fold a parsed update expression using operator applying custom function on leafs **)
(** As update expression contain list of leaf, it return list of result from function applications **)
let fold_map_parsed_update operator base leaf_fun = function
	| Normal (_, expr) ->
	    [fold_parsed_global_expression operator base leaf_fun expr]
	| Condition (bool_expr, update_list_if, update_list_else) ->
	        (fold_parsed_boolean_expression operator base leaf_fun bool_expr) ::
	        (List.map (fun (_, expr) -> fold_parsed_global_expression operator base leaf_fun expr) (update_list_if@update_list_else))

(** Fold a parsed update expression using operator applying custom function on leafs **)
(** And fold the list of leaf using base **)
let fold_parsed_update operator base leaf_fun expr =
    let elements = fold_map_parsed_update operator base leaf_fun expr in
    List.fold_left operator base elements

let fold_init_state_predicate operator base loc_assignment_leaf_fun linear_expression_leaf_fun linear_constraint_leaf_fun leaf_fun = function
	| Parsed_loc_assignment (automaton_name, loc_name) -> loc_assignment_leaf_fun (automaton_name, loc_name)
	| Parsed_linear_predicate linear_constraint -> fold_parsed_linear_constraint operator linear_expression_leaf_fun linear_constraint_leaf_fun linear_constraint
	| Parsed_discrete_predicate (_, expr) -> fold_parsed_global_expression operator base leaf_fun expr

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

(** Check if all leaf of a parsing structure satisfy the predicate **)

(* Apply to a fold function the standard parameters for evaluate AND *)
let apply_evaluate_and fold_func = fold_func (OCamlUtilities.evaluate_and)
let apply_evaluate_and_with_base fold_func = fold_func (OCamlUtilities.evaluate_and) true

let for_all_in_parsed_global_expression = apply_evaluate_and_with_base fold_parsed_global_expression
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
let for_all_in_parsed_update = apply_evaluate_and_with_base fold_parsed_update

let for_all_in_parsed_loc_predicate = apply_evaluate_and_with_base fold_parsed_loc_predicate
let for_all_in_parsed_simple_predicate = apply_evaluate_and_with_base fold_parsed_simple_predicate
let for_all_in_parsed_state_predicate_factor = apply_evaluate_and_with_base fold_parsed_state_predicate_factor
let for_all_in_parsed_state_predicate_term = apply_evaluate_and_with_base fold_parsed_state_predicate_term
let for_all_in_parsed_state_predicate = apply_evaluate_and_with_base fold_parsed_state_predicate

(** Check if any leaf of a parsing structure satisfy the predicate **)

(* Apply to a fold function the standard parameters for evaluate OR *)
let apply_evaluate_or fold_func = fold_func (||)
let apply_evaluate_or_with_base fold_func = fold_func (||) false

let exists_in_parsed_global_expression = apply_evaluate_or_with_base fold_parsed_global_expression
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

let exists_in_parsed_loc_predicate = apply_evaluate_or_with_base fold_parsed_loc_predicate
let exists_in_parsed_simple_predicate = apply_evaluate_or_with_base fold_parsed_simple_predicate
let exists_in_parsed_state_predicate_factor = apply_evaluate_or_with_base fold_parsed_state_predicate_factor
let exists_in_parsed_state_predicate_term = apply_evaluate_or_with_base fold_parsed_state_predicate_term
let exists_in_parsed_state_predicate = apply_evaluate_or_with_base fold_parsed_state_predicate

(** Iterate over a parsing structure **)

let binunit (a : unit) (b : unit) = a; b; ()
(* Apply to a fold function the standard parameters for evaluate unit *)
let apply_evaluate_unit fold_func = fold_func binunit
let apply_evaluate_unit_with_base fold_func = fold_func binunit ()


let iterate_parsed_global_expression = apply_evaluate_unit_with_base fold_parsed_global_expression
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

let iterate_parsed_update = apply_evaluate_unit_with_base fold_parsed_update

let iterate_in_parsed_loc_predicate = apply_evaluate_unit_with_base fold_parsed_loc_predicate
let iterate_in_parsed_simple_predicate = apply_evaluate_unit_with_base fold_parsed_simple_predicate
let iterate_in_parsed_state_predicate_factor = apply_evaluate_unit_with_base fold_parsed_state_predicate_factor
let iterate_in_parsed_state_predicate_term = apply_evaluate_unit_with_base fold_parsed_state_predicate_term
let iterate_in_parsed_state_predicate = apply_evaluate_unit_with_base fold_parsed_state_predicate

(* Extract function name from parsed factor *)
let function_name_of_parsed_factor = function
	| Parsed_DF_variable name -> name
    | factor -> raise (TypeError "Trying to make a call on a non-function.")

(* Labels of a parsed factors *)
(*
let label_of_parsed_shift_function_type = function
	| Parsed_shift_left -> "shift_left"
	| Parsed_shift_right -> "shift_right"
	| Parsed_fill_left -> "fill_left"
	| Parsed_fill_right -> "fill_right"

let label_of_parsed_bin_log_function_type = function
    | Parsed_log_and -> "logand"
    | Parsed_log_or -> "logor"
    | Parsed_log_xor -> "logxor"
*)

let label_of_parsed_sequence_type = function
    | Parsed_array -> "array"
    | Parsed_list -> "list"
    | Parsed_stack -> "stack"
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

let rec string_of_parsed_global_expression variable_infos = function
    | Parsed_global_expression expr -> string_of_parsed_boolean_expression variable_infos expr

and string_of_parsed_arithmetic_expression variable_infos = function
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
        if (Hashtbl.mem variable_infos.constants variable_name) then (
            (* Retrieve the value of the global constant *)
            let value = Hashtbl.find variable_infos.constants variable_name in
            variable_name
            ^ "="
            ^ DiscreteValue.string_of_value value
        ) else
            variable_name
    | Parsed_DF_constant value -> DiscreteValue.string_of_value value
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
    (*
    | Parsed_rational_of_int_function arithmetic_expr as factor ->
        label_of_parsed_factor_constructor factor
        ^ "(" ^ string_of_parsed_arithmetic_expression variable_infos arithmetic_expr ^ ")"
    | Parsed_pow_function (expr, exp_expr) as factor ->
        label_of_parsed_factor_constructor factor
        ^ "("
        ^ string_of_parsed_arithmetic_expression variable_infos expr
        ^ ","
        ^ string_of_parsed_arithmetic_expression variable_infos exp_expr
        ^ ")"
    | Parsed_shift_function (_, factor, expr) as shift ->
        label_of_parsed_factor_constructor shift
        ^ "("
        ^ string_of_parsed_factor variable_infos factor
        ^ ", "
        ^ string_of_parsed_arithmetic_expression variable_infos expr
        ^ ")"
    | Parsed_bin_log_function (_, l_factor, r_factor)
    | Parsed_array_append (l_factor, r_factor) as func ->
        label_of_parsed_factor_constructor func
        ^ "("
        ^ string_of_parsed_factor variable_infos l_factor
        ^ ", "
        ^ string_of_parsed_factor variable_infos r_factor
        ^ ")"
    | Parsed_list_cons (expr, factor) as func ->
        label_of_parsed_factor_constructor func
        ^ "("
        ^ string_of_parsed_boolean_expression variable_infos expr
        ^ ", "
        ^ string_of_parsed_factor variable_infos factor
        ^ ")"
    *)
    | Parsed_function_call (_, argument_expressions) as func ->
        let str_arguments_list = List.map (string_of_parsed_boolean_expression variable_infos) argument_expressions in
        let str_arguments = OCamlUtilities.string_of_list_of_string_with_sep ", " str_arguments_list in
        label_of_parsed_factor_constructor func ^ "(" ^ str_arguments ^ ")"
    (*
    | Parsed_log_not factor as func ->
        label_of_parsed_factor_constructor func
        ^ "("
        ^ string_of_parsed_factor variable_infos factor
        ^ ")"
    *)

and string_of_parsed_boolean_expression variable_infos = function
    | Parsed_And (l_expr, r_expr) ->
            (string_of_parsed_boolean_expression variable_infos l_expr) ^
            " & " ^
            (string_of_parsed_boolean_expression variable_infos r_expr)
    | Parsed_Or (l_expr, r_expr) ->
            (string_of_parsed_boolean_expression variable_infos l_expr) ^
            " | " ^
            (string_of_parsed_boolean_expression variable_infos r_expr)
    | Parsed_Discrete_boolean_expression expr ->
        string_of_parsed_discrete_boolean_expression variable_infos expr

and string_of_parsed_discrete_boolean_expression variable_infos = function
    | Parsed_arithmetic_expression expr ->
        string_of_parsed_arithmetic_expression variable_infos expr
    | Parsed_expression (l_expr, relop, r_expr) ->
        string_of_parsed_relop
            relop
            (string_of_parsed_discrete_boolean_expression variable_infos l_expr)
            (string_of_parsed_discrete_boolean_expression variable_infos r_expr)
    | Parsed_expression_in (expr1, expr2, expr3) ->
        (* Compute the first one to avoid redundancy *)
        let str_expr1 = string_of_parsed_arithmetic_expression variable_infos expr1 in
        let str_expr2 = string_of_parsed_arithmetic_expression variable_infos expr2 in
        let str_expr3 = string_of_parsed_arithmetic_expression variable_infos expr3 in
        str_expr1 ^ " in [" ^ str_expr2 ^ ".." ^ str_expr3 ^ "]"
    | Parsed_boolean_expression expr ->
        string_of_parsed_boolean_expression variable_infos expr
    | Parsed_Not expr ->
            "not (" ^ (string_of_parsed_boolean_expression variable_infos expr) ^ ")"

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

(* Get variable name from a variable access *)
(* ex : my_var[0][0] -> my_var *)
let rec string_of_variable_access variable_infos = function
    | Parsed_variable_update variable_name -> variable_name
    | Parsed_indexed_update (variable_access, expr) ->
        let l_del, r_del = Constants.default_array_string.array_access_delimiter in
        string_of_variable_access variable_infos variable_access
        ^ l_del ^ string_of_parsed_arithmetic_expression variable_infos expr ^ r_del
    | Parsed_void_update -> ""

let string_of_parsed_init_state_predicate variable_infos = function
	| Parsed_loc_assignment (automaton_name, location_name) -> "loc[" ^ automaton_name ^ "] = " ^ location_name
	| Parsed_linear_predicate linear_constraint -> string_of_parsed_linear_constraint variable_infos linear_constraint
	| Parsed_discrete_predicate (variable_name, expr) ->
	    variable_name
	    ^ " = "
	    ^ string_of_parsed_global_expression variable_infos expr


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

(** Utils **)

(* Check if leaf is a constant *)
let is_constant variable_infos = function
    | Leaf_variable variable_name -> Hashtbl.mem variable_infos.constants variable_name
    | Leaf_constant _ -> true

(* Check if linear leaf is a constant *)
let is_linear_constant variable_infos = function
    | Leaf_linear_variable (_, variable_name) -> Hashtbl.mem variable_infos.constants variable_name
    | Leaf_linear_constant _ -> true

(* Check if leaf is a variable that is defined *)
(* A given callback is executed if it's not a defined variable *)
let is_variable_defined_with_callback variable_infos callback = function
    | Leaf_variable variable_name ->
        if not (List.mem variable_name variable_infos.variable_names) && not (Hashtbl.mem variable_infos.constants variable_name) then(
            (
            match callback with
            | Some func -> func variable_name
            | None -> ()
            );
            false
        )
        else
            true
    | Leaf_constant _ -> true

let is_variable_defined variable_infos = is_variable_defined_with_callback variable_infos None

(* Check if linear expression leaf is a variable that is defined *)
let is_variable_defined_in_linear_expression variable_infos callback_fail = function
    | Leaf_linear_constant _ -> true
    | Leaf_linear_variable (_, variable_name) ->
        if not (List.mem variable_name variable_infos.variable_names) && not (Hashtbl.mem variable_infos.constants variable_name) then(
            callback_fail variable_name; false
        )
        else
            true

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
let is_only_discrete variable_infos = function
    | Leaf_constant _ -> true
    | Leaf_variable variable_name ->
        (* Constants are allowed *)
        (Hashtbl.mem variable_infos.constants variable_name)
        (* Or discrete *)
        ||
        try(
            let variable_index = Hashtbl.find variable_infos.index_of_variables variable_name in
            DiscreteType.is_discrete_type (variable_infos.type_of_variables variable_index)
        ) with Not_found -> (
            (* Variable not found! *)
            (*** TODO: why is this checked here…? It should have been checked before ***)
            ImitatorUtilities.print_error ("The variable `" ^ variable_name ^ "` used in an update was not declared.");
            false
        )

(* Check if leaf isn't a variable *)
let no_variables variable_infos = function
    | Leaf_linear_constant _ -> true
    | Leaf_linear_variable (_, variable_name) ->
        (* Constants are allowed *)
        (Hashtbl.mem variable_infos.constants variable_name)
        (* Or parameter *)
        ||
        let variable_index = Hashtbl.find variable_infos.index_of_variables variable_name in
        variable_infos.type_of_variables variable_index = DiscreteType.Var_type_parameter

(* Variable kind type represent a variable or a constant kind *)
type variable_kind =
    | Variable_kind of int
    | Constant_kind of DiscreteValue.discrete_value

(* Know if variable with a given name is a variable or a constant *)
let variable_kind_of_variable_name variable_infos variable_name =

    (* First check whether this is a constant *)
    if Hashtbl.mem variable_infos.constants variable_name then (
        let value = Hashtbl.find variable_infos.constants variable_name in
        Constant_kind value
    )
    (* Otherwise: a variable *)
    else
        Variable_kind (Hashtbl.find variable_infos.index_of_variables variable_name)


(* Check if a parsed global expression is constant *)
let is_parsed_global_expression_constant variable_infos =
    for_all_in_parsed_global_expression (is_constant variable_infos)

(* Check if a parsed boolean expression is constant *)
let is_parsed_boolean_expression_constant variable_infos =
    for_all_in_parsed_boolean_expression (is_constant variable_infos)

(* Check if a parsed arithmetic expression is constant *)
let is_parsed_arithmetic_expression_constant variable_infos =
    for_all_in_parsed_discrete_arithmetic_expression (is_constant variable_infos)

(* Check that all variables in a parsed global expression are effectively be defined *)
let all_variables_defined_in_parsed_global_expression variable_infos expr =
    for_all_in_parsed_global_expression (is_variable_defined variable_infos) expr

(* Check that all variables in a parsed boolean expression are effectively be defined *)
let all_variables_defined_in_parsed_boolean_expression variable_infos callback expr =
    for_all_in_parsed_boolean_expression (is_variable_defined_with_callback variable_infos callback) expr

(* Check that all variables in a parsed discrete boolean expression are effectively be defined *)
let all_variables_defined_in_parsed_discrete_boolean_expression variable_infos callback expr =
    for_all_in_parsed_discrete_boolean_expression (is_variable_defined_with_callback variable_infos callback) expr

(* Check that all variables in a linear expression are effectively be defined *)
let all_variables_defined_in_linear_expression variable_infos callback_fail expr =
    for_all_in_parsed_linear_expression (is_variable_defined_in_linear_expression variable_infos callback_fail) expr

(* Check that all variables in a linear constraint are effectively be defined *)
let all_variables_defined_in_linear_constraint variable_infos callback_fail expr =
    for_all_in_parsed_linear_constraint
        (is_variable_defined_in_linear_expression variable_infos callback_fail)
        (function | Leaf_false_linear_constraint | Leaf_true_linear_constraint -> true) expr

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
let all_variable_in_parsed_state_predicate parsing_infos variable_infos undefined_variable_callback_opt undefined_automaton_callback_opt undefined_loc_callback_opt expr =
    for_all_in_parsed_state_predicate
        (is_automaton_defined_in_parsed_state_predicate_with_callbacks parsing_infos undefined_automaton_callback_opt undefined_loc_callback_opt)
        (is_variable_defined_with_callback variable_infos undefined_variable_callback_opt)
        expr

(* Check that there is only discrete variables in a parsed global expression *)
let only_discrete_in_parsed_global_expression variable_infos expr =
    for_all_in_parsed_global_expression (is_only_discrete variable_infos) expr

(* Check that there is only discrete variables in a parsed discrete boolean expression *)
let only_discrete_in_nonlinear_expression variable_infos expr =
    for_all_in_parsed_discrete_boolean_expression (is_only_discrete variable_infos) expr

(* Check if there is no variables in a linear expression *)
let no_variables_in_linear_expression variable_infos expr =
    for_all_in_parsed_linear_expression (no_variables variable_infos) expr

(* Check if a linear expression is constant *)
let is_parsed_linear_expression_constant variable_infos expr =
    for_all_in_parsed_linear_expression (is_linear_constant variable_infos) expr

(* Gather all variable names used in a linear_expression *)
let add_variable_of_linear_expression variables_used_ref = function
    | Leaf_linear_constant _ -> ()
    | Leaf_linear_variable (_, variable_name) ->
        (* Add the variable name to the set and update the reference *)
        variables_used_ref := StringSet.add variable_name !variables_used_ref

(* Gather all variable names used in a discrete boolean expression *)
let add_variable_of_discrete_boolean_expression variables_used_ref = function
    | Leaf_constant _ -> ()
    | Leaf_variable variable_name ->
        (* Add the variable name to the set and update the reference *)
        variables_used_ref := StringSet.add variable_name !variables_used_ref

(* Gather all variable names used in a global expression in a given accumulator *)
let get_variables_in_parsed_global_expression_with_accumulator variables_used_ref =
    iterate_parsed_global_expression (add_variable_of_discrete_boolean_expression variables_used_ref)

(* Gather all variable names used in a parsed boolean expression in a given accumulator *)
let get_variables_in_parsed_boolean_expression_with_accumulator variables_used_ref =
    iterate_parsed_boolean_expression (add_variable_of_discrete_boolean_expression variables_used_ref)

(* Gather all variable names used in a parsed discrete boolean expression in a given accumulator *)
let get_variables_in_parsed_discrete_boolean_expression_with_accumulator variables_used_ref =
    iterate_parsed_discrete_boolean_expression (add_variable_of_discrete_boolean_expression variables_used_ref)

(* Gather all variable names used in a linear expression in a given accumulator *)
let get_variables_in_linear_expression_with_accumulator variables_used_ref =
    iterate_parsed_linear_expression (add_variable_of_linear_expression variables_used_ref)

(* Gather all variable names used in a linear constraint in a given accumulator *)
let get_variables_in_linear_constraint_with_accumulator variables_used_ref =
    iterate_parsed_linear_constraint
        (add_variable_of_linear_expression variables_used_ref)
        (function | Leaf_true_linear_constraint | Leaf_false_linear_constraint -> ())

(* Gather all variable names used in a non-linear constraint in a given accumulator *)
let get_variables_in_nonlinear_constraint_with_accumulator = get_variables_in_parsed_discrete_boolean_expression_with_accumulator

(* Gather all variable names used in an update in a given accumulator *)
let get_variables_in_parsed_update_with_accumulator variables_used_ref =
    iterate_parsed_update
        (add_variable_of_discrete_boolean_expression variables_used_ref)

(* Create and wrap an accumulator then return result directly *)
let wrap_accumulator f expr =
    let variables_used_ref = ref StringSet.empty in
    f variables_used_ref expr;
    !variables_used_ref

(* Gather all variable names used in a global expression *)
let get_variables_in_parsed_global_expression =
    wrap_accumulator get_variables_in_parsed_global_expression_with_accumulator

(* Gather all variable names used in a parsed discrete boolean expression *)
let get_variables_in_parsed_discrete_boolean_expression =
    wrap_accumulator get_variables_in_parsed_discrete_boolean_expression_with_accumulator

(* Gather all variable names used in a linear expression *)
let get_variables_in_linear_expression =
    wrap_accumulator get_variables_in_linear_expression_with_accumulator

(* Gather all variable names used in a linear constraint *)
let get_variables_in_linear_constraint =
    wrap_accumulator get_variables_in_linear_constraint_with_accumulator

(* Gather all variable names used in a non-linear constraint *)
let get_variables_in_nonlinear_constraint =
    wrap_accumulator get_variables_in_nonlinear_constraint_with_accumulator

(* Gather all variable names used in a parsed init state predicate *)
let get_variables_in_init_state_predicate = function
	| Parsed_loc_assignment _ -> StringSet.empty
	| Parsed_linear_predicate linear_constraint -> get_variables_in_linear_constraint linear_constraint
	| Parsed_discrete_predicate (_, expr) -> get_variables_in_parsed_global_expression expr

(* Gather all variable names used in a non-linear convex predicate (non-linear constraint list) *)
let get_variables_in_nonlinear_convex_predicate convex_predicate =
    List.map (get_variables_in_nonlinear_constraint) convex_predicate |>
    List.fold_left (fun variables acc -> StringSet.union acc variables) StringSet.empty

(* Get variable name from a variable access *)
(* ex : my_var[0][0] -> my_var *)
let rec variable_name_of_variable_access = function
    | Parsed_variable_update variable_name -> Some variable_name
    | Parsed_indexed_update (variable_access, _) -> variable_name_of_variable_access variable_access
    | Parsed_void_update -> None

(* Check if variable access is a variable name directly *)
(* ex : my_var -> true, my_var[i] -> false *)
let is_variable_access_is_a_variable_name = function
    | Parsed_variable_update _ -> true
    | Parsed_void_update
    | Parsed_indexed_update _ -> false



(* - --- - -- - *)


(*------------------------------------------------------------*)
(* Try to convert a non-linear expression to a linear *)
(* If it's not possible (due to non-linear expression involving clocks or parameters *)
(* we raise an InvalidExpression exception *)
(*------------------------------------------------------------*)

(* Try to convert parsed discrete term to a linear term *)
(* If it's not possible, we raise an InvalidExpression exception *)
let rec try_convert_linear_term_of_parsed_discrete_term = function
    (* TODO benjamin reduction should be made before *)
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
        | Parsed_DF_constant value -> Constant (DiscreteValue.to_numconst_value value)
        | Parsed_DF_unary_min parsed_discrete_factor ->
            (* Check for unary min, negate variable and constant *)
            (match parsed_discrete_factor with
                | Parsed_DF_variable variable_name -> Variable(NumConst.minus_one, variable_name)
                | Parsed_DF_constant value ->
                    let numconst_value = DiscreteValue.to_numconst_value value in
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
    | Parsed_expression (Parsed_arithmetic_expression l_expr, relop, Parsed_arithmetic_expression r_expr) ->
        Parsed_linear_constraint (
            try_convert_linear_expression_of_parsed_discrete_arithmetic_expression l_expr,
            relop,
            try_convert_linear_expression_of_parsed_discrete_arithmetic_expression r_expr
        )
    | Parsed_expression (l_expr, relop, r_expr) ->
        raise (InvalidExpression "Use of non arithmetic comparison is forbidden in an expression that involve clock(s) / parameter(s)")
    (* Expression in used ! So it's impossible to make the conversion, we raise an exception*)
    | Parsed_expression_in (_, _, _) -> raise (InvalidExpression "A boolean 'in' expression involve clock(s) / parameter(s)")
    | Parsed_boolean_expression _ -> raise (InvalidExpression "A non-convex predicate involve clock(s) / parameter(s)")
    | Parsed_Not _ -> raise (InvalidExpression "A not expression involve clock(s) / parameter(s)")

let linear_constraint_of_nonlinear_constraint = try_convert_linear_expression_of_parsed_discrete_boolean_expression




(* Extract variable infos from useful_parsing_model_information *)
let variable_infos_of_parsed_model (parsed_model : useful_parsing_model_information) =
    {
        constants = parsed_model.constants;
        variable_names = parsed_model.variable_names;
        index_of_variables = parsed_model.index_of_variables;
        type_of_variables = parsed_model.type_of_variables;
        removed_variable_names = parsed_model.removed_variable_names;
    }


