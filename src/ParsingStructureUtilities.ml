(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: General fonctions for map, filter, traverse parsing structure tree
 *
 * File contributors : Benjamin L.
 * Created           : 2021/03/05
 * Last modified     : 2021/03/05
 *
 ************************************************************)

open Exceptions
open ParsingStructure


(* Map the leafs of an arithmetic expression according to map_function *)
(* Leafs are Parsed_DF_variable, Parsed_DF_constant *)
let map_parsed_arithmetic_expression_leafs map_function arithmetic_expr =

    let rec map_parsed_arithmetic_expression_leafs_rec = function
        | Parsed_DAE_plus (arithmetic_expression, term)
        | Parsed_DAE_minus (arithmetic_expression, term) ->
            List.rev_append (map_parsed_arithmetic_expression_leafs_rec arithmetic_expression) (map_parsed_term_leafs term)
        | Parsed_DAE_term term ->
            map_parsed_term_leafs term

    and map_parsed_term_leafs = function
        | Parsed_DT_mul (term, factor)
        | Parsed_DT_div (term, factor) ->
            List.rev_append (map_parsed_term_leafs term) (map_parsed_factor_leafs factor)
        | Parsed_DT_factor factor ->
            map_parsed_factor_leafs factor

    and map_parsed_factor_leafs = function
        | Parsed_DF_variable _
        | Parsed_DF_constant _ as leaf -> [map_function leaf]
        | Parsed_DF_unary_min factor -> map_parsed_factor_leafs factor
        | Parsed_DF_expression arithmetic_expr -> map_parsed_arithmetic_expression_leafs_rec arithmetic_expr
    in

    map_parsed_arithmetic_expression_leafs_rec arithmetic_expr


(* String of a parsed expression *)
(* Used for error message on type checking *)
let rec string_of_parsed_global_expression parsed_model = function
    | Parsed_global_expression expr -> string_of_parsed_boolean_expression parsed_model expr

and string_of_parsed_arithmetic_expression parsed_model = function
    | Parsed_DAE_plus (arithmetic_expr, term) ->
            (string_of_parsed_arithmetic_expression parsed_model arithmetic_expr) ^
            " + " ^
            (string_of_parsed_term parsed_model term)
    | Parsed_DAE_minus (arithmetic_expr, term) ->
            (string_of_parsed_arithmetic_expression parsed_model arithmetic_expr) ^
            " - " ^
            (string_of_parsed_term parsed_model term)
    | Parsed_DAE_term term ->
        string_of_parsed_term parsed_model term

and string_of_parsed_term parsed_model = function
    | Parsed_DT_mul (term, factor) ->
            (string_of_parsed_term parsed_model term) ^
            " * " ^
            (string_of_parsed_factor parsed_model factor)
    | Parsed_DT_div (term, factor) ->
            (string_of_parsed_term parsed_model term) ^
            " / " ^
            (string_of_parsed_factor parsed_model factor)
    | Parsed_DT_factor factor ->
        string_of_parsed_factor parsed_model factor

and string_of_parsed_factor parsed_model = function
    | Parsed_DF_variable variable_name ->
        if (Hashtbl.mem parsed_model.constants variable_name) then (
            (* Retrieve the value of the global constant *)
            let value = Hashtbl.find parsed_model.constants variable_name in
            variable_name
            ^ "="
            ^ DiscreteValue.string_of_value value
        ) else
            variable_name
    | Parsed_DF_constant value -> DiscreteValue.string_of_value value
    | Parsed_DF_expression arithmetic_expr -> string_of_parsed_arithmetic_expression parsed_model arithmetic_expr
    | Parsed_DF_unary_min factor ->
        "-(" ^ (string_of_parsed_factor parsed_model factor) ^ ")"
    | Builtin_function_rational_of_int arithmetic_expr ->
        "rational_of_int(" ^ (string_of_parsed_arithmetic_expression parsed_model arithmetic_expr) ^ ")"

and string_of_parsed_boolean_expression parsed_model = function
    | Parsed_True -> "True"
    | Parsed_False -> "False"
    | Parsed_And (l_expr, r_expr) ->
            (string_of_parsed_boolean_expression parsed_model l_expr) ^
            " & " ^
            (string_of_parsed_boolean_expression parsed_model r_expr)
    | Parsed_Or (l_expr, r_expr) ->
            (string_of_parsed_boolean_expression parsed_model l_expr) ^
            " | " ^
            (string_of_parsed_boolean_expression parsed_model r_expr)
    | Parsed_Not expr ->
            "not (" ^ (string_of_parsed_boolean_expression parsed_model expr) ^ ")"
    | Parsed_Discrete_boolean_expression expr ->
        string_of_parsed_discrete_boolean_expression parsed_model expr

and string_of_parsed_discrete_boolean_expression parsed_model = function
    | Parsed_arithmetic_expression expr ->
        string_of_parsed_arithmetic_expression parsed_model expr
    | Parsed_expression (l_expr, relop, r_expr) ->
        string_of_parsed_relop
            relop
            (string_of_parsed_arithmetic_expression parsed_model l_expr)
            (string_of_parsed_arithmetic_expression parsed_model r_expr)
    | Parsed_expression_in (expr1, expr2, expr3) ->
        (* Compute the first one to avoid redundancy *)
        let str_expr1 = string_of_parsed_arithmetic_expression parsed_model expr1 in
        let str_expr2 = string_of_parsed_arithmetic_expression parsed_model expr2 in
        let str_expr3 = string_of_parsed_arithmetic_expression parsed_model expr3 in
        str_expr1 ^ " in [" ^ str_expr2 ^ ".." ^ str_expr3 ^ "]"
    | Parsed_boolean_expression expr ->
        string_of_parsed_boolean_expression parsed_model expr

and string_of_parsed_relop relop value_1 value_2 =
        match relop with
        | PARSED_OP_L		-> value_1 ^ " < " ^ value_2
        | PARSED_OP_LEQ	    -> value_1 ^ " <= " ^ value_2
        | PARSED_OP_EQ		-> value_1 ^ " = " ^ value_2
        | PARSED_OP_NEQ	    -> value_1 ^ " <> " ^ value_2
        | PARSED_OP_GEQ	    -> value_1 ^ " >= " ^ value_2
        | PARSED_OP_G		-> value_1 ^ " > " ^ value_2


let string_of_parsed_nonlinear_constraint parsed_model = function
    | Parsed_true_nonlinear_constraint -> "True"
    | Parsed_false_nonlinear_constraint -> "False"
    | Parsed_nonlinear_constraint expr -> string_of_parsed_discrete_boolean_expression parsed_model expr


(* Try to reduce a parsed global expression, cannot take into account variables ! *)
(* This function is used for computing constant values *)
let try_reduce_parsed_global_expression constants expr =

    let rec try_reduce_parsed_global_expression_rec = function
        | Parsed_global_expression expr -> try_reduce_parsed_boolean_expression expr

    and try_reduce_parsed_arithmetic_expression = function
        | Parsed_DAE_plus (arithmetic_expr, term) ->
            DiscreteValue.add
                (try_reduce_parsed_arithmetic_expression arithmetic_expr)
                (try_reduce_parsed_term term)
        | Parsed_DAE_minus (arithmetic_expr, term) ->
            DiscreteValue.sub
                (try_reduce_parsed_arithmetic_expression arithmetic_expr)
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
                raise (InvalidExpression ("Use of variable " ^ variable_name ^ " in assignment is forbidden"))
        | Parsed_DF_constant value -> value
        | Parsed_DF_expression arithmetic_expr -> try_reduce_parsed_arithmetic_expression arithmetic_expr
        | Parsed_DF_unary_min factor ->
            DiscreteValue.neg (try_reduce_parsed_factor factor)

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
	    | Parsed_Not expr ->
	        DiscreteValue.not
	            (try_reduce_parsed_boolean_expression expr)
	    | Parsed_Discrete_boolean_expression expr ->
	        try_reduce_parsed_discrete_boolean_expression expr

    and try_reduce_parsed_discrete_boolean_expression = function
        | Parsed_arithmetic_expression expr ->
            try_reduce_parsed_arithmetic_expression expr
        | Parsed_expression (l_expr, relop, r_expr) ->
            eval_parsed_relop
                relop
                (try_reduce_parsed_arithmetic_expression l_expr)
                (try_reduce_parsed_arithmetic_expression r_expr)
        | Parsed_expression_in (expr1, expr2, expr3) ->
		    (* Compute the first one to avoid redundancy *)
		    let expr1_evaluated = try_reduce_parsed_arithmetic_expression expr1 in
		    let expr2_evaluated = try_reduce_parsed_arithmetic_expression expr2 in
		    let expr3_evaluated = try_reduce_parsed_arithmetic_expression expr3 in
		    DiscreteValue._and
			    (DiscreteValue.leq expr2_evaluated expr1_evaluated)
			    (DiscreteValue.leq expr1_evaluated expr3_evaluated)
        | Parsed_boolean_expression expr ->
            try_reduce_parsed_boolean_expression expr

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

let try_reduce_parsed_term constants term =
    let expr = Parsed_global_expression (Parsed_Discrete_boolean_expression (Parsed_arithmetic_expression (Parsed_DAE_term term))) in
    try_reduce_parsed_global_expression constants expr

let try_reduce_parsed_factor constants factor =
    let expr = Parsed_global_expression (Parsed_Discrete_boolean_expression (Parsed_arithmetic_expression (Parsed_DAE_term (Parsed_DT_factor factor)))) in
    try_reduce_parsed_global_expression constants expr

(* Try to reduce a parsed global expression, cannot take into account variables ! *)
(* This function is used for computing constant values *)
let try_reduce_parsed_global_expression_with_model useful_parsing_model_information (* expr *) =
    (* Get constants *)
    let constants = useful_parsing_model_information.constants in
    try_reduce_parsed_global_expression constants