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
 * Last modified     : 2021/07/29
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

(** Fold a parsing structure using operator applying custom function on leafs **)

let rec fold_parsed_global_expression (operator : 'a -> 'a -> 'a) leaf_fun = function
     | Parsed_global_expression expr -> fold_parsed_boolean_expression operator leaf_fun expr

and fold_parsed_boolean_expression operator leaf_fun = function
	| Parsed_True -> leaf_fun (Leaf_constant (DiscreteValue.Bool_value true))
	| Parsed_False -> leaf_fun (Leaf_constant (DiscreteValue.Bool_value false))
	| Parsed_And (l_expr, r_expr)
	| Parsed_Or (l_expr, r_expr) ->
	    operator
	        (fold_parsed_boolean_expression operator leaf_fun l_expr)
	        (fold_parsed_boolean_expression operator leaf_fun r_expr)
	| Parsed_Discrete_boolean_expression expr ->
	    fold_parsed_discrete_boolean_expression operator leaf_fun expr

and fold_parsed_discrete_boolean_expression operator leaf_fun = function
    | Parsed_arithmetic_expression expr ->
        fold_parsed_discrete_arithmetic_expression operator leaf_fun expr
	| Parsed_expression (l_expr, _, r_expr) ->
	    operator
	        (fold_parsed_discrete_boolean_expression operator leaf_fun l_expr)
	        (fold_parsed_discrete_boolean_expression operator leaf_fun r_expr)
	| Parsed_expression_in (lower_expr, expr, upper_expr) ->
	    operator
	        (fold_parsed_discrete_arithmetic_expression operator leaf_fun expr)
            (operator
                (fold_parsed_discrete_arithmetic_expression operator leaf_fun lower_expr)
                (fold_parsed_discrete_arithmetic_expression operator leaf_fun upper_expr))
	| Parsed_boolean_expression expr
	| Parsed_Not expr ->
        fold_parsed_boolean_expression operator leaf_fun expr

and fold_parsed_discrete_arithmetic_expression operator leaf_fun = function
	| Parsed_DAE_plus (expr, term)
	| Parsed_DAE_minus (expr, term) ->
        operator
            (fold_parsed_discrete_arithmetic_expression operator leaf_fun expr)
            (fold_parsed_discrete_term operator leaf_fun term)
	| Parsed_DAE_term term ->
        fold_parsed_discrete_term operator leaf_fun term

and fold_parsed_discrete_term operator leaf_fun = function
	| Parsed_DT_mul (term, factor)
	| Parsed_DT_div (term, factor) ->
	    operator
	        (fold_parsed_discrete_term operator leaf_fun term)
	        (fold_parsed_discrete_factor operator leaf_fun factor)
	| Parsed_DT_factor factor ->
        fold_parsed_discrete_factor operator leaf_fun factor

and fold_parsed_discrete_factor operator leaf_fun = function
	| Parsed_DF_variable variable_name -> leaf_fun (Leaf_variable variable_name)
	| Parsed_DF_constant value -> leaf_fun (Leaf_constant value)
	| Parsed_DF_expression expr
	| Parsed_rational_of_int_function expr ->
        fold_parsed_discrete_arithmetic_expression operator leaf_fun expr
	| Parsed_pow_function (expr_0, expr_1) ->
        operator
            (fold_parsed_discrete_arithmetic_expression operator leaf_fun expr_0)
            (fold_parsed_discrete_arithmetic_expression operator leaf_fun expr_1)
	| Parsed_shift_left (factor, expr)
	| Parsed_shift_right (factor, expr)
	| Parsed_fill_left (factor, expr)
	| Parsed_fill_right (factor, expr) ->
        operator
            (fold_parsed_discrete_factor operator leaf_fun factor)
            (fold_parsed_discrete_arithmetic_expression operator leaf_fun expr)
	| Parsed_log_and (factor_0, factor_1)
	| Parsed_log_or (factor_0, factor_1)
	| Parsed_log_xor (factor_0, factor_1) ->
        operator
            (fold_parsed_discrete_factor operator leaf_fun factor_0)
            (fold_parsed_discrete_factor operator leaf_fun factor_1)
	| Parsed_log_not factor
	| Parsed_DF_unary_min factor ->
	    fold_parsed_discrete_factor operator leaf_fun factor


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

and fold_parsed_linear_term operator leaf_fun = function
    | Constant value -> leaf_fun (Leaf_linear_constant value)
    | Variable (value, variable_name) -> leaf_fun (Leaf_linear_variable (value, variable_name))


let fold_parsed_nonlinear_constraint operator leaf_fun nonlinear_constraint_leaf_fun = function
    | Parsed_false_nonlinear_constraint ->
        nonlinear_constraint_leaf_fun Leaf_false_nonlinear_constraint
    | Parsed_true_nonlinear_constraint ->
        nonlinear_constraint_leaf_fun Leaf_true_nonlinear_constraint
    | Parsed_nonlinear_constraint expr ->
        fold_parsed_discrete_boolean_expression operator leaf_fun expr

(** Check if all leaf of a parsing structure satisfy the predicate **)

let for_all_in_parsed_global_expression = fold_parsed_global_expression (&&)
let for_all_in_parsed_boolean_expression = fold_parsed_boolean_expression (&&)
let for_all_in_parsed_discrete_boolean_expression = fold_parsed_discrete_boolean_expression (&&)
let for_all_in_parsed_discrete_arithmetic_expression = fold_parsed_discrete_arithmetic_expression (&&)
let for_all_in_parsed_discrete_term = fold_parsed_discrete_term (&&)
let for_all_in_parsed_discrete_factor = fold_parsed_discrete_factor (&&)

(** Check if all leaf of a linear expression satisfy the predicate **)
let for_all_in_parsed_linear_expression = fold_parsed_linear_expression (&&)
(** Check if all leaf of a linear term satisfy the predicate **)
let for_all_in_parsed_linear_term = fold_parsed_linear_term (&&)
(** Check if all leaf of a linear constraint satisfy the predicate **)
let for_all_in_parsed_linear_constraint = fold_parsed_linear_constraint (&&)
(** Check if all leaf of a non-linear constraint satisfy the predicate **)
let for_all_in_parsed_nonlinear_constraint = fold_parsed_nonlinear_constraint (&&)

(** Check if any leaf of a parsing structure satisfy the predicate **)

let exists_in_parsed_global_expression = fold_parsed_global_expression (||)
let exists_in_parsed_boolean_expression = fold_parsed_boolean_expression (||)
let exists_in_parsed_discrete_boolean_expression = fold_parsed_discrete_boolean_expression (||)
let exists_in_parsed_discrete_arithmetic_expression = fold_parsed_discrete_arithmetic_expression (||)
let exists_in_parsed_discrete_term = fold_parsed_discrete_term (||)
let exists_in_parsed_discrete_factor = fold_parsed_discrete_factor (||)

(** Check if any leaf of a linear expression satisfy the predicate **)
let exists_in_parsed_linear_expression = fold_parsed_linear_expression (||)
(** Check if any leaf of a linear term satisfy the predicate **)
let exists_in_parsed_linear_term = fold_parsed_linear_term (||)
(** Check if any leaf of a linear constraint satisfy the predicate **)
let exists_in_parsed_linear_constraint = fold_parsed_linear_constraint (||)
(** Check if any leaf of a non-linear constraint satisfy the predicate **)
let exists_in_parsed_nonlinear_constraint = fold_parsed_nonlinear_constraint (||)

(** Iterate over a parsing structure **)

let binunit (a : unit) (b : unit) = a; b; ()
let iterate_parsed_global_expression = fold_parsed_global_expression binunit
let iterate_parsed_boolean_expression = fold_parsed_boolean_expression binunit
let iterate_parsed_discrete_boolean_expression = fold_parsed_discrete_boolean_expression binunit
let iterate_parsed_discrete_arithmetic_expression = fold_parsed_discrete_arithmetic_expression binunit
let iterate_parsed_discrete_term  = fold_parsed_discrete_term binunit
let iterate_parsed_discrete_factor = fold_parsed_discrete_factor binunit

(** Iterate over a linear expression **)
let iterate_parsed_linear_expression = fold_parsed_linear_expression binunit
(** Iterate over a linear term **)
let iterate_parsed_linear_term = fold_parsed_linear_term binunit
(** Iterate over a linear constraint **)
let iterate_parsed_linear_constraint = fold_parsed_linear_constraint binunit
(** Iterate over a non-linear constraint **)
let iterate_parsed_nonlinear_constraint = fold_parsed_nonlinear_constraint binunit


let string_of_parsed_factor_constructor = function
	| Parsed_DF_variable _ -> "variable"
	| Parsed_DF_constant _ -> "constant"
	| Parsed_DF_expression _ -> "expression"
	| Parsed_DF_unary_min _ -> "minus"
	| Parsed_rational_of_int_function _ -> "rational_of_int"
	| Parsed_pow_function _ -> "pow"
	| Parsed_shift_left _ -> "shift_left"
	| Parsed_shift_right _ -> "shift_right"
	| Parsed_fill_left _ -> "fill_left"
	| Parsed_fill_right _ -> "fill_right"
    | Parsed_log_and _ -> "log_and"
    | Parsed_log_or _ -> "log_or"
    | Parsed_log_xor _ -> "log_xor"
    | Parsed_log_not _ -> "log_not"

(* String of a parsed expression *)
(* Used for error message on type checking *)
let rec string_of_parsed_global_expression variable_infos = function
    | Parsed_global_expression expr -> string_of_parsed_boolean_expression variable_infos expr

and string_of_parsed_arithmetic_expression variable_infos = function
    | Parsed_DAE_plus (arithmetic_expr, term) ->
            (string_of_parsed_arithmetic_expression variable_infos arithmetic_expr) ^
            " + " ^
            (string_of_parsed_term variable_infos term)
    | Parsed_DAE_minus (arithmetic_expr, term) ->
            (string_of_parsed_arithmetic_expression variable_infos arithmetic_expr) ^
            " - " ^
            (string_of_parsed_term variable_infos term)
    | Parsed_DAE_term term ->
        string_of_parsed_term variable_infos term

and string_of_parsed_term variable_infos = function
    | Parsed_DT_mul (term, factor) ->
            (string_of_parsed_term variable_infos term) ^
            " * " ^
            (string_of_parsed_factor variable_infos factor)
    | Parsed_DT_div (term, factor) ->
            (string_of_parsed_term variable_infos term) ^
            " / " ^
            (string_of_parsed_factor variable_infos factor)
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
    | Parsed_DF_expression arithmetic_expr -> string_of_parsed_arithmetic_expression variable_infos arithmetic_expr
    | Parsed_DF_unary_min factor ->
        "-(" ^ (string_of_parsed_factor variable_infos factor) ^ ")"
    | Parsed_rational_of_int_function arithmetic_expr as factor ->
        string_of_parsed_factor_constructor factor
        ^ "(" ^ string_of_parsed_arithmetic_expression variable_infos arithmetic_expr ^ ")"
    | Parsed_pow_function (expr, exp_expr) as factor ->
        string_of_parsed_factor_constructor factor
        ^ "("
        ^ string_of_parsed_arithmetic_expression variable_infos expr
        ^ ","
        ^ string_of_parsed_arithmetic_expression variable_infos exp_expr
        ^ ")"
    | Parsed_shift_left (factor, expr)
    | Parsed_shift_right (factor, expr)
    | Parsed_fill_left (factor, expr)
    | Parsed_fill_right (factor, expr) as shift ->
        string_of_parsed_factor_constructor shift
        ^ "("
        ^ string_of_parsed_factor variable_infos factor
        ^ ", "
        ^ string_of_parsed_arithmetic_expression variable_infos expr
        ^ ")"
    | Parsed_log_and (l_factor, r_factor)
    | Parsed_log_or (l_factor, r_factor)
    | Parsed_log_xor (l_factor, r_factor) as log_op ->
        string_of_parsed_factor_constructor log_op
        ^ "("
        ^ string_of_parsed_factor variable_infos l_factor
        ^ ", "
        ^ string_of_parsed_factor variable_infos r_factor
        ^ ")"
    | Parsed_log_not factor as log_op ->
        string_of_parsed_factor_constructor log_op
        ^ "("
        ^ string_of_parsed_factor variable_infos factor
        ^ ")"

and string_of_parsed_boolean_expression variable_infos = function
    | Parsed_True -> "True"
    | Parsed_False -> "False"
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

and string_of_parsed_relop relop value_1 value_2 =
        match relop with
        | PARSED_OP_L		-> value_1 ^ " < " ^ value_2
        | PARSED_OP_LEQ	    -> value_1 ^ " <= " ^ value_2
        | PARSED_OP_EQ		-> value_1 ^ " = " ^ value_2
        | PARSED_OP_NEQ	    -> value_1 ^ " <> " ^ value_2
        | PARSED_OP_GEQ	    -> value_1 ^ " >= " ^ value_2
        | PARSED_OP_G		-> value_1 ^ " > " ^ value_2

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
	    ^ string_of_parsed_global_expression variable_infos expr

let string_of_parsed_nonlinear_constraint variable_infos = function
    | Parsed_true_nonlinear_constraint -> "True"
    | Parsed_false_nonlinear_constraint -> "False"
    | Parsed_nonlinear_constraint expr -> string_of_parsed_discrete_boolean_expression variable_infos expr


(* Try to reduce a parsed global expression, cannot take into account variables ! *)
(* This function is used for computing constant values *)
let rec try_reduce_parsed_global_expression constants expr =

    let rec try_reduce_parsed_global_expression_rec = function
        | Parsed_global_expression expr -> try_reduce_parsed_boolean_expression expr

    and try_reduce_parsed_boolean_expression = function
	    | Parsed_True -> DiscreteValue.bool_value_true
	    | Parsed_False -> DiscreteValue.bool_value_false
	    | Parsed_And (l_expr, r_expr) ->
	        DiscreteValue._and
                (try_reduce_parsed_boolean_expression l_expr)
                (try_reduce_parsed_boolean_expression r_expr)
	    | Parsed_Or (l_expr, r_expr) ->
	        DiscreteValue._or
                (try_reduce_parsed_boolean_expression l_expr)
                (try_reduce_parsed_boolean_expression r_expr)
	    | Parsed_Discrete_boolean_expression expr ->
	        try_reduce_parsed_discrete_boolean_expression expr

    and try_reduce_parsed_discrete_boolean_expression = function
        | Parsed_arithmetic_expression expr ->
            try_reduce_parsed_arithmetic_expression constants expr
        | Parsed_expression (l_expr, relop, r_expr) ->
            eval_parsed_relop
                relop
                (try_reduce_parsed_discrete_boolean_expression l_expr)
                (try_reduce_parsed_discrete_boolean_expression r_expr)
        | Parsed_expression_in (expr1, expr2, expr3) ->
		    (* Compute the first one to avoid redundancy *)
		    let expr1_evaluated = try_reduce_parsed_arithmetic_expression constants expr1 in
		    let expr2_evaluated = try_reduce_parsed_arithmetic_expression constants expr2 in
		    let expr3_evaluated = try_reduce_parsed_arithmetic_expression constants expr3 in
		    DiscreteValue._and
			    (DiscreteValue.leq expr2_evaluated expr1_evaluated)
			    (DiscreteValue.leq expr1_evaluated expr3_evaluated)
        | Parsed_boolean_expression expr ->
            try_reduce_parsed_boolean_expression expr
	    | Parsed_Not expr ->
	        DiscreteValue.not
	            (try_reduce_parsed_boolean_expression expr)

    and eval_parsed_relop relop value_1 value_2 =
        	match relop with
        	| PARSED_OP_L		-> DiscreteValue.l value_1 value_2
        	| PARSED_OP_LEQ	    -> DiscreteValue.leq value_1 value_2
        	| PARSED_OP_EQ		-> DiscreteValue.bool_equal value_1  value_2
        	| PARSED_OP_NEQ	    -> DiscreteValue.bool_neq value_1 value_2
        	| PARSED_OP_GEQ	    -> DiscreteValue.geq value_1 value_2
        	| PARSED_OP_G		-> DiscreteValue.g value_1  value_2

    in
    try_reduce_parsed_global_expression_rec expr

and try_reduce_parsed_arithmetic_expression constants expr =

    let rec try_reduce_parsed_arithmetic_expression_rec = function
        | Parsed_DAE_plus (arithmetic_expr, term) ->
            DiscreteValue.add
                (try_reduce_parsed_arithmetic_expression_rec arithmetic_expr)
                (try_reduce_parsed_term term)
        | Parsed_DAE_minus (arithmetic_expr, term) ->
            DiscreteValue.sub
                (try_reduce_parsed_arithmetic_expression_rec arithmetic_expr)
                (try_reduce_parsed_term term)
        | Parsed_DAE_term term ->
            try_reduce_parsed_term term

    and try_reduce_parsed_term = function
        | Parsed_DT_mul (term, factor) ->
            DiscreteValue.mul
                (try_reduce_parsed_term term)
                (try_reduce_parsed_factor factor)
        | Parsed_DT_div (term, factor) ->
            DiscreteValue.div
                (try_reduce_parsed_term term)
                (try_reduce_parsed_factor factor)
        | Parsed_DT_factor factor ->
            try_reduce_parsed_factor factor

    and try_reduce_parsed_factor = function
        | Parsed_DF_variable variable_name ->
            if (Hashtbl.mem constants variable_name) then (
                (* Retrieve the value of the global constant *)
                Hashtbl.find constants variable_name
            ) else
                raise (InternalError ("Unable to reduce a non-constant expression: " ^ variable_name ^ " found. It should be checked before."))
        | Parsed_DF_constant value -> value
        | Parsed_DF_expression arithmetic_expr -> try_reduce_parsed_arithmetic_expression_rec arithmetic_expr
        | Parsed_DF_unary_min factor ->
            DiscreteValue.neg (try_reduce_parsed_factor factor)
        | Parsed_rational_of_int_function expr ->
            (* Convert with no problem because it's already type checked *)
            DiscreteValue.convert_to_rational_value (try_reduce_parsed_arithmetic_expression_rec expr)
        | Parsed_pow_function (expr, exp) ->

            let reduced_expr = try_reduce_parsed_arithmetic_expression_rec expr in
            let reduced_exp = try_reduce_parsed_arithmetic_expression_rec exp in
            (* we have to know type of expr *)
            let value_type = DiscreteValue.discrete_type_of_value reduced_expr in
            (match value_type with
            | DiscreteValue.Var_type_discrete_number DiscreteValue.Var_type_discrete_rational ->
                let numconst_expr = DiscreteValue.numconst_value reduced_expr in
                let int_exp = DiscreteValue.int_value reduced_exp in
                let numconst_result = NumConst.pow numconst_expr int_exp in
                DiscreteValue.of_numconst numconst_result
            | DiscreteValue.Var_type_discrete_number DiscreteValue.Var_type_discrete_int ->
                let int_expr = DiscreteValue.int_value reduced_expr in
                let int_exp = DiscreteValue.int_value reduced_exp in
                let int_result = OCamlUtilities.pow int_expr int_exp in
                DiscreteValue.of_int int_result
            (* Should never happen *)
            | DiscreteValue.Var_type_discrete_bool
            | DiscreteValue.Var_type_discrete_binary_word _
            | DiscreteValue.Var_type_discrete_number DiscreteValue.Var_type_discrete_unknown_number as t ->
                raise (InternalError (
                    "Try to reduce a pow function on a "
                    ^ DiscreteValue.string_of_var_type_discrete t
                    ^ " expression, altough it was checked before by the type checker. Maybe type checking has failed before"
                ))
            )
        | Parsed_shift_left (factor, expr) ->

            let reduced_factor = try_reduce_parsed_factor factor in
            let reduced_expr = try_reduce_parsed_arithmetic_expression_rec expr in
            DiscreteValue.shift_left (Int32.to_int (DiscreteValue.int_value reduced_expr))  reduced_factor

        | Parsed_shift_right (factor, expr) ->

            let reduced_factor = try_reduce_parsed_factor factor in
            let reduced_expr = try_reduce_parsed_arithmetic_expression_rec expr in
            DiscreteValue.shift_right (Int32.to_int (DiscreteValue.int_value reduced_expr))  reduced_factor

        | Parsed_fill_left (factor, expr) ->

            let reduced_factor = try_reduce_parsed_factor factor in
            let reduced_expr = try_reduce_parsed_arithmetic_expression_rec expr in
            DiscreteValue.fill_left (Int32.to_int (DiscreteValue.int_value reduced_expr))  reduced_factor

        | Parsed_fill_right (factor, expr) ->

            let reduced_factor = try_reduce_parsed_factor factor in
            let reduced_expr = try_reduce_parsed_arithmetic_expression_rec expr in
            DiscreteValue.fill_right (Int32.to_int (DiscreteValue.int_value reduced_expr))  reduced_factor

        | Parsed_log_and (l_factor, r_factor) ->

            let reduced_l_factor = try_reduce_parsed_factor l_factor in
            let reduced_r_factor = try_reduce_parsed_factor r_factor in
            DiscreteValue.log_and reduced_l_factor reduced_r_factor

        | Parsed_log_or (l_factor, r_factor) ->

            let reduced_l_factor = try_reduce_parsed_factor l_factor in
            let reduced_r_factor = try_reduce_parsed_factor r_factor in
            DiscreteValue.log_or reduced_l_factor reduced_r_factor

        | Parsed_log_xor (l_factor, r_factor) ->

            let reduced_l_factor = try_reduce_parsed_factor l_factor in
            let reduced_r_factor = try_reduce_parsed_factor r_factor in
            DiscreteValue.log_xor reduced_l_factor reduced_r_factor

        | Parsed_log_not factor ->

            let reduced_factor = try_reduce_parsed_factor factor in
            DiscreteValue.log_not reduced_factor
    in
    try_reduce_parsed_arithmetic_expression_rec expr

let try_reduce_parsed_term constants term =
    let expr = Parsed_global_expression (Parsed_Discrete_boolean_expression (Parsed_arithmetic_expression (Parsed_DAE_term term))) in
    try_reduce_parsed_global_expression constants expr

let try_reduce_parsed_factor constants factor =
    let expr = Parsed_global_expression (Parsed_Discrete_boolean_expression (Parsed_arithmetic_expression (Parsed_DAE_term (Parsed_DT_factor factor)))) in
    try_reduce_parsed_global_expression constants expr



(** Utils **)

let is_constant variable_infos = function
    | Leaf_variable variable_name -> Hashtbl.mem variable_infos.constants variable_name
    | Leaf_constant _ -> true

let is_linear_constant variable_infos = function
    | Leaf_linear_variable (_, variable_name) -> Hashtbl.mem variable_infos.constants variable_name
    | Leaf_linear_constant _ -> true


let is_variable_defined variable_infos = function
    | Leaf_variable variable_name ->
        if not (List.mem variable_name variable_infos.variable_names) && not (Hashtbl.mem variable_infos.constants variable_name) then(
            (* TODO benjamin message or not message ? *)
            (* ImitatorUtilities.print_error (
                "The variable \"" ^ variable_name ^ "\" used in expression \""
                ^ string_of_parsed_boolean_expression variable_infos expr
                ^ "\" was not declared."
            ); *)
            false
        )
        else
            true
    | Leaf_constant _ -> true

let is_variable_defined_in_linear_expression variable_infos callback_fail = function
    | Leaf_linear_constant _ -> true
    | Leaf_linear_variable (_, variable_name) ->
        if not (List.mem variable_name variable_infos.variable_names) && not (Hashtbl.mem variable_infos.constants variable_name) then(
            callback_fail variable_name; false
        )
        else
            true

let is_only_discrete variable_infos = function
    | Leaf_constant _ -> true
    | Leaf_variable variable_name ->
        (* Constants are allowed *)
        (Hashtbl.mem variable_infos.constants variable_name)
        (* Or discrete *)
        ||
        try(
            let variable_index = Hashtbl.find variable_infos.index_of_variables variable_name in
            DiscreteValue.is_discrete_type (variable_infos.type_of_variables variable_index)
        ) with Not_found -> (
            (* Variable not found! *)
            (*** TODO: why is this checked here…? It should have been checked before ***)
            ImitatorUtilities.print_error ("The variable `" ^ variable_name ^ "` used in an update was not declared.");
            false
        )

let is_parsed_global_expression_constant variable_infos =
    for_all_in_parsed_global_expression (is_constant variable_infos)

let is_parsed_arithmetic_expression_constant variable_infos =
    for_all_in_parsed_discrete_arithmetic_expression (is_constant variable_infos)

(* Check that all variables in a parsed global expression are effectivily be defined *)
let all_variables_defined_in_parsed_global_expression variable_infos expr =
    for_all_in_parsed_global_expression (is_variable_defined variable_infos) expr

(* Check that all variables in a parsed parsed boolean expression are effectivily be defined *)
let all_variables_defined_in_parsed_boolean_expression variable_infos expr =
    for_all_in_parsed_boolean_expression (is_variable_defined variable_infos) expr

(* Check that all variables in a linear expression are effectivily be defined *)
let all_variables_defined_in_linear_expression variable_infos callback_fail expr =
    for_all_in_parsed_linear_expression (is_variable_defined_in_linear_expression variable_infos callback_fail) expr

(* Check that all variables in a linear constraint are effectivily be defined *)
let all_variables_defined_in_linear_constraint variable_infos callback_fail expr =
    for_all_in_parsed_linear_constraint
        (is_variable_defined_in_linear_expression variable_infos callback_fail)
        (function | Leaf_false_linear_constraint | Leaf_true_linear_constraint -> true) expr

(* Check that there is only discrete variables in a parsed parsed discrete boolean expression *)
let only_discrete_in_nonlinear_term variable_infos expr =
    for_all_in_parsed_discrete_boolean_expression (is_only_discrete variable_infos) expr

let is_parsed_linear_expression_constant variable_infos expr =
    for_all_in_parsed_linear_expression (is_linear_constant variable_infos) expr


(*------------------------------------------------------------*)
(* Gather all variable names used in a linear_expression *)
(*------------------------------------------------------------*)
let add_variable_of_linear_expression variables_used_ref = function
    | Leaf_linear_constant _ -> ()
    | Leaf_linear_variable (_, variable_name) ->
        (* Add the variable name to the set and update the reference *)
        variables_used_ref := StringSet.add variable_name !variables_used_ref

let add_variable_of_discrete_boolean_expression variables_used_ref = function
    | Leaf_constant _ -> ()
    | Leaf_variable variable_name ->
        (* Add the variable name to the set and update the reference *)
        variables_used_ref := StringSet.add variable_name !variables_used_ref


let get_variables_in_parsed_global_expression_with_accumulator variables_used_ref =
    iterate_parsed_global_expression (add_variable_of_discrete_boolean_expression variables_used_ref)

let get_variables_in_parsed_boolean_expression_with_accumulator variables_used_ref =
    iterate_parsed_boolean_expression (add_variable_of_discrete_boolean_expression variables_used_ref)

let get_variables_in_parsed_discrete_boolean_expression_with_accumulator variables_used_ref =
    iterate_parsed_discrete_boolean_expression (add_variable_of_discrete_boolean_expression variables_used_ref)

let get_variables_in_linear_expression_with_accumulator variables_used_ref =
    iterate_parsed_linear_expression (add_variable_of_linear_expression variables_used_ref)

let get_variables_in_linear_constraint_with_accumulator variables_used_ref =
    iterate_parsed_linear_constraint
        (add_variable_of_linear_expression variables_used_ref)
        (function | Leaf_true_linear_constraint | Leaf_false_linear_constraint -> ())

let get_variables_in_nonlinear_constraint_with_accumulator variables_used_ref =
    iterate_parsed_nonlinear_constraint
        (add_variable_of_discrete_boolean_expression variables_used_ref)
        (function | Leaf_true_nonlinear_constraint | Leaf_false_nonlinear_constraint -> ())


(* Create and wrap an accumulator then return result directly *)
let wrap_accumulator f expr =
    let variables_used_ref = ref StringSet.empty in
    f variables_used_ref expr;
    !variables_used_ref

let get_variables_in_parsed_global_expression =
    wrap_accumulator get_variables_in_parsed_global_expression_with_accumulator

let get_variables_in_parsed_discrete_boolean_expression =
    wrap_accumulator get_variables_in_parsed_discrete_boolean_expression_with_accumulator

let get_variables_in_linear_expression =
    wrap_accumulator get_variables_in_linear_expression_with_accumulator

let get_variables_in_linear_constraint =
    wrap_accumulator get_variables_in_linear_constraint_with_accumulator

let get_variables_in_nonlinear_constraint =
    wrap_accumulator get_variables_in_nonlinear_constraint_with_accumulator

let variable_infos_of_parsed_model (parsed_model : useful_parsing_model_information) =
    {
        constants = parsed_model.constants;
        variable_names = parsed_model.variable_names;
        index_of_variables = parsed_model.index_of_variables;
        type_of_variables = parsed_model.type_of_variables;
        removed_variable_names = parsed_model.removed_variable_names;
    }