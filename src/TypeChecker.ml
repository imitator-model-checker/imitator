(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Contain all functions making type checking or type operations on a parsing structure
 *
 * File contributors : Benjamin L.
 * Created           : 2021/03/17
 * Last modified     : 2021/03/17
 *
 ************************************************************)

open ParsingStructure
open ParsingStructureUtilities

(* Type error exception *)
exception TypeError of string
exception InternalError of string

let get_type_mixin_error_message l_type r_type str_expr =
    "The expression \""
    ^ str_expr
    ^ "\" mix different types : "
    ^ (DiscreteValue.string_of_var_type l_type)
    ^ ", "
    ^ (DiscreteValue.string_of_var_type r_type)

let get_triplet_type_mixin_error_message type1 type2 type3 str_expr =
    "The expression \""
    ^ str_expr
    ^ "\" mix different types : "
    ^ (DiscreteValue.string_of_var_type type1)
    ^ ", "
    ^ (DiscreteValue.string_of_var_type type2)
    ^ ", "
    ^ (DiscreteValue.string_of_var_type type3)



(* Check if a value is compatible with given type *)
let check_value_compatible_with_type value var_type =
    let value_type = DiscreteValue.var_type_of_value value in
    DiscreteValue.is_type_compatibles value_type var_type

(* Try to resolve the specific type of an expression according to literals and variables used *)
(* Doing type checking of the expression at the same time*)
let rec resolve_expression_type parsed_model = function
    | Parsed_global_expression expr -> resolve_parsed_boolean_expression_type parsed_model expr

and resolve_parsed_boolean_expression_type parsed_model = function
    | Parsed_True
    | Parsed_False -> DiscreteValue.Var_type_discrete DiscreteValue.Var_type_discrete_bool
    | Parsed_Not expr -> resolve_parsed_boolean_expression_type parsed_model expr
    | Parsed_And (l_expr, r_expr)
    | Parsed_Or (l_expr, r_expr) as be ->
        let l_type = resolve_parsed_boolean_expression_type parsed_model l_expr in
        let r_type = resolve_parsed_boolean_expression_type parsed_model r_expr in
        (* Check that left and right types are boolean *)
        if not (DiscreteValue.is_bool_type l_type && DiscreteValue.is_bool_type r_type) then (
            let error_msg =
                "The expression \""
                ^ (string_of_parsed_boolean_expression parsed_model be)
                ^ "\" is not of type bool"
            in
            raise (TypeError error_msg)
        )
        else
            l_type
    | Parsed_Discrete_boolean_expression expr -> resolve_parsed_discrete_boolean_expression_type parsed_model expr

and resolve_parsed_discrete_boolean_expression_type parsed_model = function
    | Parsed_arithmetic_expression expr -> resolve_parsed_discrete_arithmetic_expression_type parsed_model expr
    | Parsed_boolean_expression expr -> resolve_parsed_boolean_expression_type parsed_model expr
    | Parsed_expression (l_expr, relop, r_expr) as parsed_discrete_boolean_expression ->
        let l_type = resolve_parsed_discrete_arithmetic_expression_type parsed_model l_expr in
        let r_type = resolve_parsed_discrete_arithmetic_expression_type parsed_model r_expr in
        (* Check if it's an ordered comparison *)
        let is_ordered_comparison = (match relop with | PARSED_OP_EQ | PARSED_OP_NEQ -> false | _ -> true) in
        (* If comparison is ordered, check that left and right types are number *)
        if is_ordered_comparison && not (DiscreteValue.is_number_type l_type && DiscreteValue.is_number_type r_type) then (
            let error_msg =
                "Left or right member of expression \""
                (* insert expression here *)
                ^ "\" is not a number"
            in
            raise (TypeError error_msg)
        )
        else
            if l_type <> r_type then (
                let error_msg = get_type_mixin_error_message l_type r_type (string_of_parsed_discrete_boolean_expression parsed_model parsed_discrete_boolean_expression) in
                raise (TypeError error_msg)
            )
            else
                DiscreteValue.Var_type_discrete DiscreteValue.Var_type_discrete_bool
    | Parsed_expression_in (expr, lower_expr, upper_expr) as dae ->
        let expr_type = resolve_parsed_discrete_arithmetic_expression_type parsed_model expr in
        let lower_type = resolve_parsed_discrete_arithmetic_expression_type parsed_model lower_expr in
        let upper_type = resolve_parsed_discrete_arithmetic_expression_type parsed_model upper_expr  in
        (* Check that left and right types are number *)
        if not (DiscreteValue.is_number_type expr_type && DiscreteValue.is_number_type lower_type && DiscreteValue.is_number_type upper_type) then (
            let error_msg =
                "Compared, lower or upper bound member of expression \""
                ^ (string_of_parsed_discrete_boolean_expression parsed_model dae)
                ^ "\" is not a number"
            in
            raise (TypeError error_msg)
        )
        else
            if expr_type <> upper_type || expr_type <> lower_type || upper_type <> lower_type then (
                let error_msg = get_triplet_type_mixin_error_message expr_type lower_type upper_type (string_of_parsed_discrete_boolean_expression parsed_model dae) in
                raise (TypeError error_msg)
            )
            else
                DiscreteValue.Var_type_discrete DiscreteValue.Var_type_discrete_bool

    (* Other, expression is a boolean expression *)
    | _ -> DiscreteValue.Var_type_discrete DiscreteValue.Var_type_discrete_bool

and resolve_parsed_discrete_arithmetic_expression_type parsed_model = function
    | Parsed_DAE_plus (expr, term)
    | Parsed_DAE_minus (expr, term) as dae_expr ->
        let l_type = resolve_parsed_discrete_arithmetic_expression_type parsed_model expr in
        let r_type = resolve_parsed_discrete_term_type parsed_model term in

        if l_type <> r_type then (
            let error_msg = get_type_mixin_error_message l_type r_type (string_of_parsed_arithmetic_expression parsed_model dae_expr) in
            raise (TypeError error_msg)
        )
        else
            l_type
    | Parsed_DAE_term term ->
        resolve_parsed_discrete_term_type parsed_model term

and resolve_parsed_discrete_term_type parsed_model = function
    | Parsed_DT_mul (term, factor)
    | Parsed_DT_div (term, factor) as dae_term ->
        let l_type = resolve_parsed_discrete_term_type parsed_model term in
        let r_type = resolve_parsed_discrete_factor_type parsed_model factor in
        if l_type <> r_type then (
            let error_msg =
                "The expression \""
                ^ (string_of_parsed_term parsed_model dae_term)
                ^ "\" mix different types : "
                ^ (DiscreteValue.string_of_var_type l_type)
                ^ ", "
                ^ (DiscreteValue.string_of_var_type r_type)
            in
            raise (TypeError error_msg)
        )
        else
            l_type
    | Parsed_DT_factor factor ->
        resolve_parsed_discrete_factor_type parsed_model factor

and resolve_parsed_discrete_factor_type parsed_model = function
    | Parsed_DF_variable variable_name ->
        if Hashtbl.mem parsed_model.index_of_variables variable_name then (
            (* Get type of variable *)
            let variable_index = Hashtbl.find parsed_model.index_of_variables variable_name in
            parsed_model.type_of_variables variable_index
        )
        else (
            if Hashtbl.mem parsed_model.constants variable_name then (
                (* Retrieve the value of the global constant *)
                let value = Hashtbl.find parsed_model.constants variable_name in
                (* Get type of constant *)
                DiscreteValue.var_type_of_value value
            ) else (
                raise (InternalError ("Impossible to find the index of variable `" ^ variable_name ^ "` although this should have been checked before."))
            )
        )
    | Parsed_DF_constant var_value ->
        (* Get var type of value *)
        DiscreteValue.var_type_of_value var_value
    | Parsed_DF_expression expr ->
        resolve_parsed_discrete_arithmetic_expression_type parsed_model expr
    | Parsed_DF_unary_min factor ->
        resolve_parsed_discrete_factor_type parsed_model factor

let check_type_of_nonlinear_constraint parsed_model = function
    (* It's ok non-linear constraint is of boolean type *)
    | Parsed_true_nonlinear_constraint
    | Parsed_false_nonlinear_constraint -> true
    | Parsed_nonlinear_constraint expr ->
        (* Convert discrete boolean expression of non-linear constraint to global expression and resolve type to check the type *)
        let global_expr = Parsed_global_expression (Parsed_Discrete_boolean_expression expr) in
        let expression_type = resolve_expression_type parsed_model global_expr in
        DiscreteValue.is_bool_type expression_type

(* Get a new parsed model with literal rationals implicitly converted to suitable number types *)
(* Example : i * 2 with i : int, convert 2 from rational to int *)
let rec implicit_convert_literal_of_parsed_global_expression parsed_model = function
    | Parsed_global_expression expr -> Parsed_global_expression (implicit_convert_literal_of_parsed_boolean_expression parsed_model expr)

(* Get *)
and implicit_convert_literal_of_parsed_boolean_expression parsed_model = function
    | Parsed_True -> Parsed_True
    | Parsed_False -> Parsed_False
    | Parsed_And (l_expr, r_expr) ->
        Parsed_And (
            (implicit_convert_literal_of_parsed_boolean_expression parsed_model l_expr),
            (implicit_convert_literal_of_parsed_boolean_expression parsed_model r_expr)
        )
    | Parsed_Or (l_expr, r_expr) ->
        Parsed_Or (
            (implicit_convert_literal_of_parsed_boolean_expression parsed_model l_expr),
            (implicit_convert_literal_of_parsed_boolean_expression parsed_model r_expr)
        )
    | Parsed_Not expr ->
        Parsed_Not
            (implicit_convert_literal_of_parsed_boolean_expression parsed_model expr)
    | Parsed_Discrete_boolean_expression expr ->
        Parsed_Discrete_boolean_expression (implicit_convert_literal_of_parsed_discrete_boolean_expression parsed_model expr)

and implicit_convert_literal_of_parsed_discrete_boolean_expression parsed_model = function
    | Parsed_arithmetic_expression expr ->
        Parsed_arithmetic_expression (implicit_convert_literal_of_parsed_arithmetic_expression parsed_model expr)

    (* rational ~ expr *)
    | Parsed_expression (Parsed_DAE_term (Parsed_DT_factor (Parsed_DF_constant value)), relop, r_expr) ->
        let converted_expr, converted_value = implicit_convert_value_from_arithmetic_expression parsed_model r_expr value in
        Parsed_expression (Parsed_DAE_term (Parsed_DT_factor (Parsed_DF_constant converted_value)), relop, converted_expr)

    (* expr ~ rational *)
    | Parsed_expression (l_expr, relop, Parsed_DAE_term (Parsed_DT_factor (Parsed_DF_constant value))) ->
        let converted_expr, converted_value = implicit_convert_value_from_arithmetic_expression parsed_model l_expr value in
        Parsed_expression (converted_expr, relop, Parsed_DAE_term (Parsed_DT_factor (Parsed_DF_constant converted_value)))

    | Parsed_expression (l_expr, relop, r_expr) ->
        Parsed_expression (
            (implicit_convert_literal_of_parsed_arithmetic_expression parsed_model l_expr),
            relop,
            (implicit_convert_literal_of_parsed_arithmetic_expression parsed_model r_expr)
        )
    | Parsed_expression_in (expr1, expr2, expr3) ->
        let expr1_evaluated = implicit_convert_literal_of_parsed_arithmetic_expression parsed_model expr1 in
        let expr2_evaluated = implicit_convert_literal_of_parsed_arithmetic_expression parsed_model expr2 in
        let expr3_evaluated = implicit_convert_literal_of_parsed_arithmetic_expression parsed_model expr3 in
        Parsed_expression_in (
            expr1_evaluated,
            expr2_evaluated,
            expr3_evaluated
        )
    | Parsed_boolean_expression expr ->
        Parsed_boolean_expression (implicit_convert_literal_of_parsed_boolean_expression parsed_model expr)
    | Parsed_DB_variable _ as variable -> variable

and implicit_convert_literal_of_parsed_arithmetic_expression parsed_model = function
    (* expr + rational *)
    | Parsed_DAE_plus (arithmetic_expr, Parsed_DT_factor (Parsed_DF_constant value)) ->
        let converted_expr, converted_value = implicit_convert_value_from_arithmetic_expression parsed_model arithmetic_expr value in
        Parsed_DAE_plus (converted_expr, Parsed_DT_factor (Parsed_DF_constant converted_value))
    (* rational + term *)
    | Parsed_DAE_plus (Parsed_DAE_term (Parsed_DT_factor (Parsed_DF_constant value)), term) ->
        let converted_term, converted_value = implicit_convert_value_from_term parsed_model term value in
        Parsed_DAE_plus (Parsed_DAE_term (Parsed_DT_factor (Parsed_DF_constant converted_value)), converted_term)
    (* expr - rational *)
    | Parsed_DAE_minus (arithmetic_expr, Parsed_DT_factor (Parsed_DF_constant value)) ->
        let converted_expr, converted_value = implicit_convert_value_from_arithmetic_expression parsed_model arithmetic_expr value in
        Parsed_DAE_minus (converted_expr, Parsed_DT_factor (Parsed_DF_constant converted_value))
    (* rational - term *)
    | Parsed_DAE_minus (Parsed_DAE_term (Parsed_DT_factor (Parsed_DF_constant value)), term) ->
        let converted_term, converted_value = implicit_convert_value_from_term parsed_model term value in
        Parsed_DAE_minus (Parsed_DAE_term (Parsed_DT_factor (Parsed_DF_constant converted_value)), converted_term)
    (* Normal arithmetic expression *)
    | Parsed_DAE_plus (arithmetic_expr, term) ->
        Parsed_DAE_plus (
            (implicit_convert_literal_of_parsed_arithmetic_expression parsed_model arithmetic_expr),
            (implicit_convert_literal_of_parsed_term parsed_model term)
        )
    | Parsed_DAE_minus (arithmetic_expr, term) ->
        Parsed_DAE_minus (
            (implicit_convert_literal_of_parsed_arithmetic_expression parsed_model arithmetic_expr),
            (implicit_convert_literal_of_parsed_term parsed_model term)
        )
    | Parsed_DAE_term term ->
        Parsed_DAE_term (implicit_convert_literal_of_parsed_term parsed_model term)

and implicit_convert_literal_of_parsed_term parsed_model = function
    (* term * rational *)
    | Parsed_DT_mul (term, Parsed_DF_constant value) ->
        let converted_term, converted_value = implicit_convert_value_from_term parsed_model term value in
        Parsed_DT_mul (converted_term, Parsed_DF_constant converted_value)

    (* rational * factor *)
    | Parsed_DT_mul (Parsed_DT_factor (Parsed_DF_constant value), factor) ->
        let converted_factor, converted_value = implicit_convert_value_from_factor parsed_model factor value in
        Parsed_DT_mul (Parsed_DT_factor (Parsed_DF_constant converted_value), converted_factor)

    (* term / rational *)
    | Parsed_DT_div (term, Parsed_DF_constant value) ->
        let converted_term, converted_value = implicit_convert_value_from_term parsed_model term value in
        Parsed_DT_div (converted_term, Parsed_DF_constant converted_value)

    (* rational / factor *)
    | Parsed_DT_div (Parsed_DT_factor (Parsed_DF_constant value), factor) ->
        let converted_factor, converted_value = implicit_convert_value_from_factor parsed_model factor value in
        Parsed_DT_div (Parsed_DT_factor (Parsed_DF_constant converted_value), converted_factor)

    | Parsed_DT_mul (term, factor) ->
        Parsed_DT_mul (
            (implicit_convert_literal_of_parsed_term parsed_model term),
            (implicit_convert_literal_of_parsed_factor parsed_model factor)
        )
    | Parsed_DT_div (term, factor) ->
        Parsed_DT_div (
            (implicit_convert_literal_of_parsed_term parsed_model term),
            (implicit_convert_literal_of_parsed_factor parsed_model factor)
        )
    | Parsed_DT_factor factor ->
        Parsed_DT_factor (implicit_convert_literal_of_parsed_factor parsed_model factor)

and implicit_convert_literal_of_parsed_factor parsed_model = function
    | Parsed_DF_variable _ as variable -> variable
    | Parsed_DF_constant _ as constant -> constant
    | Parsed_DF_expression arithmetic_expr ->
        Parsed_DF_expression (implicit_convert_literal_of_parsed_arithmetic_expression parsed_model arithmetic_expr)
    | Parsed_DF_unary_min factor ->
         Parsed_DF_unary_min (implicit_convert_literal_of_parsed_factor parsed_model factor)

and implicit_convert_literal_of_nonlinear_constraint parsed_model = function
    | Parsed_true_nonlinear_constraint -> Parsed_true_nonlinear_constraint
    | Parsed_false_nonlinear_constraint -> Parsed_false_nonlinear_constraint
    | Parsed_nonlinear_constraint expr -> Parsed_nonlinear_constraint (implicit_convert_literal_of_parsed_discrete_boolean_expression parsed_model expr)

and implicit_convert_value_from_arithmetic_expression parsed_model arithmetic_expr value =
    let converted_expr = implicit_convert_literal_of_parsed_arithmetic_expression parsed_model arithmetic_expr in
    let expr_type = resolve_parsed_discrete_arithmetic_expression_type parsed_model converted_expr in
    converted_expr, DiscreteValue.convert_value value expr_type

and implicit_convert_value_from_term parsed_model term value =
    let converted_term = implicit_convert_literal_of_parsed_term parsed_model term in
    let term_type = resolve_parsed_discrete_term_type parsed_model converted_term in
    converted_term, DiscreteValue.convert_value value term_type

and implicit_convert_value_from_factor parsed_model factor value =
    let converted_factor = implicit_convert_literal_of_parsed_factor parsed_model factor in
    let factor_type = resolve_parsed_discrete_factor_type parsed_model converted_factor in
    converted_factor, DiscreteValue.convert_value value factor_type

(* Get variable type given it's index *)
let get_type_of_variable parsed_model variable_index =
    parsed_model.type_of_variables variable_index

(* Get variable type given it's name *)
let get_type_of_variable_by_name parsed_model variable_name =
    (* Get the variable index *)
    let discrete_index = Hashtbl.find parsed_model.index_of_variables variable_name in
    get_type_of_variable parsed_model discrete_index

(* Check that an expression assigned to a variable is of the same type *)
(* If not, raise a TypeError exception with an error message *)
let check_type_assignment parsed_model variable_name expr =

    (* Function that construct type error message *)
    let get_error_message variable_name variable_type expr_type expr =
        "Variable "
        ^ variable_name
        ^ " of type "
        ^ (DiscreteValue.string_of_var_type variable_type)
        ^ " is not compatible with expression : \""
        ^ (string_of_parsed_global_expression parsed_model expr)
        ^ "\""
        ^ " of type "
        ^ (DiscreteValue.string_of_var_type expr_type)
    in

    (* Get variable type *)
    let variable_type = get_type_of_variable_by_name parsed_model variable_name in
    (* Resolve expression type *)
    let expression_type = resolve_expression_type parsed_model expr in
    (* Check expression / variable type consistency *)
    let is_consistent = DiscreteValue.is_type_compatibles variable_type expression_type in

    (* Not consistent ? raise a type error with appropriate message*)
    if not (is_consistent) then (
        raise (TypeError (get_error_message variable_name variable_type expression_type expr))
    );