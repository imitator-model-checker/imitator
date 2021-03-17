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
    | Parsed_DB_variable variable_name ->
        if (Hashtbl.mem parsed_model.constants variable_name) then (
            (* Retrieve the value of the global constant *)
            let value = Hashtbl.find parsed_model.constants variable_name in
            DiscreteValue.string_of_value value
        ) else
            variable_name

and string_of_parsed_relop relop value_1 value_2 =
        match relop with
        | PARSED_OP_L		-> value_1 ^ " < " ^ value_2
        | PARSED_OP_LEQ	    -> value_1 ^ " <= " ^ value_2
        | PARSED_OP_EQ		-> value_1 ^ " = " ^ value_2
        | PARSED_OP_NEQ	    -> value_1 ^ " <> " ^ value_2
        | PARSED_OP_GEQ	    -> value_1 ^ " >= " ^ value_2
        | PARSED_OP_G		-> value_1 ^ " > " ^ value_2