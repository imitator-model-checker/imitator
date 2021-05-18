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
open ImitatorUtilities

(** Exceptions **)

(* Type error exception *)
exception TypeError of string
exception InternalError of string

type variable_name = string
type variable_index = int

(** Error messages **)

(* Error message when mixin of different types *)
let get_type_mixin_error_message l_type r_type str_expr =
    "The expression \""
    ^ str_expr
    ^ "\" mix different types : "
    ^ (DiscreteValue.string_of_var_type_discrete l_type)
    ^ ", "
    ^ (DiscreteValue.string_of_var_type_discrete r_type)

(* Error message when mixin of different types *)
let get_triplet_type_mixin_error_message type1 type2 type3 str_expr =
    "The expression \""
    ^ str_expr
    ^ "\" mix different types : "
    ^ (DiscreteValue.string_of_var_type_discrete type1)
    ^ ", "
    ^ (DiscreteValue.string_of_var_type_discrete type2)
    ^ ", "
    ^ (DiscreteValue.string_of_var_type_discrete type3)

(** Get variables types **)

(* Get var type of a variable given it's index *)
let get_type_of_variable parsed_model variable_index =
    parsed_model.type_of_variables variable_index



(* Get discrete type of a variable given it's index *)
let get_discrete_type_of_variable parsed_model variable_index =
    let var_type = get_type_of_variable parsed_model variable_index in
    DiscreteValue.discrete_type_of_var_type var_type

(* Get var type of a variable given it's name *)
let get_type_of_variable_by_name parsed_model variable_name =
    if Hashtbl.mem parsed_model.index_of_variables variable_name then (
        (* Get type of variable *)
        let variable_index = Hashtbl.find parsed_model.index_of_variables variable_name in
        let variable_type = get_type_of_variable parsed_model variable_index in
        variable_type
    ) else if Hashtbl.mem parsed_model.constants variable_name then (
        (* Retrieve the value of the global constant *)
        let value = Hashtbl.find parsed_model.constants variable_name in
        (* Get type of constant *)
        DiscreteValue.var_type_of_value value
    ) else
        raise (InternalError ("Impossible to find the index of variable `" ^ variable_name ^ "` although this should have been checked before."))


(* Get discrete type of a variable given it's name *)
let get_discrete_type_of_variable_by_name parsed_model variable_name =
    if Hashtbl.mem parsed_model.index_of_variables variable_name then (
        (* Get type of variable *)
        let variable_index = Hashtbl.find parsed_model.index_of_variables variable_name in
        let variable_type = get_discrete_type_of_variable parsed_model variable_index in
        variable_type
    ) else if Hashtbl.mem parsed_model.constants variable_name then (
        (* Retrieve the value of the global constant *)
        let value = Hashtbl.find parsed_model.constants variable_name in
        (* Get type of constant *)
        DiscreteValue.discrete_type_of_value value
    ) else
        raise (InternalError ("Impossible to find the index of variable `" ^ variable_name ^ "` although this should have been checked before."))


(** Conversions of expressions **)


(* Convert literals types of the expression to a given target type *)
let rec convert_literal_types_of_expression parsed_model target_type = function
    | Parsed_global_expression expr ->
        Parsed_global_expression (convert_literal_types_of_parsed_boolean_expression parsed_model target_type expr)

and convert_literal_types_of_parsed_boolean_expression parsed_model target_type = function
    | Parsed_True -> Parsed_True
    | Parsed_False -> Parsed_False
    | Parsed_And (l_expr, r_expr) ->
        let convert_l_expr = convert_literal_types_of_parsed_boolean_expression parsed_model target_type l_expr in
        let convert_r_expr = convert_literal_types_of_parsed_boolean_expression parsed_model target_type r_expr in
        Parsed_And (convert_l_expr, convert_r_expr)
    | Parsed_Or (l_expr, r_expr) ->
        let convert_l_expr = convert_literal_types_of_parsed_boolean_expression parsed_model target_type l_expr in
        let convert_r_expr = convert_literal_types_of_parsed_boolean_expression parsed_model target_type r_expr in
        Parsed_Or (convert_l_expr, convert_r_expr)
    | Parsed_Discrete_boolean_expression expr ->
        Parsed_Discrete_boolean_expression (convert_literal_types_of_parsed_discrete_boolean_expression parsed_model target_type expr)

and convert_literal_types_of_parsed_discrete_boolean_expression parsed_model target_type = function
    | Parsed_arithmetic_expression expr ->
        Parsed_arithmetic_expression (convert_literal_types_of_parsed_discrete_arithmetic_expression parsed_model target_type expr)
    | Parsed_boolean_expression expr ->
        Parsed_boolean_expression (convert_literal_types_of_parsed_boolean_expression parsed_model target_type expr)
    | Parsed_Not expr ->
        Parsed_Not (convert_literal_types_of_parsed_boolean_expression parsed_model target_type expr)
    | Parsed_expression (l_expr, relop, r_expr) ->
        let convert_l_expr = convert_literal_types_of_parsed_discrete_boolean_expression parsed_model target_type l_expr in
        let convert_r_expr = convert_literal_types_of_parsed_discrete_boolean_expression parsed_model target_type r_expr in
        Parsed_expression (convert_l_expr, relop, convert_r_expr)
    | Parsed_expression_in (expr, lower_expr, upper_expr) ->
        let convert_expr = convert_literal_types_of_parsed_discrete_arithmetic_expression parsed_model target_type expr in
        let convert_lower = convert_literal_types_of_parsed_discrete_arithmetic_expression parsed_model target_type lower_expr in
        let convert_upper = convert_literal_types_of_parsed_discrete_arithmetic_expression parsed_model target_type upper_expr  in
        Parsed_expression_in (convert_expr, convert_lower, convert_upper)

and convert_literal_types_of_parsed_discrete_arithmetic_expression parsed_model target_type = function
    | Parsed_DAE_plus (expr, term) ->
        let convert_expr = convert_literal_types_of_parsed_discrete_arithmetic_expression parsed_model target_type expr in
        let convert_term = convert_literal_types_of_parsed_discrete_term parsed_model target_type term in
        Parsed_DAE_plus (convert_expr, convert_term)
    | Parsed_DAE_minus (expr, term) ->
        let convert_expr = convert_literal_types_of_parsed_discrete_arithmetic_expression parsed_model target_type expr in
        let convert_term = convert_literal_types_of_parsed_discrete_term parsed_model target_type term in
        Parsed_DAE_minus (convert_expr, convert_term)
    | Parsed_DAE_term term ->
        Parsed_DAE_term (convert_literal_types_of_parsed_discrete_term parsed_model target_type term)

and convert_literal_types_of_parsed_discrete_term parsed_model target_type = function
    | Parsed_DT_mul (term, factor) ->
        let convert_term = convert_literal_types_of_parsed_discrete_term parsed_model target_type term in
        let convert_factor = convert_literal_types_of_parsed_discrete_factor parsed_model target_type factor in
        Parsed_DT_mul (convert_term, convert_factor)
    | Parsed_DT_div (term, factor) ->
        let convert_term = convert_literal_types_of_parsed_discrete_term parsed_model target_type term in
        let convert_factor = convert_literal_types_of_parsed_discrete_factor parsed_model target_type factor in
        Parsed_DT_div (convert_term, convert_factor)
    | Parsed_DT_factor factor ->
        Parsed_DT_factor (convert_literal_types_of_parsed_discrete_factor parsed_model target_type factor)

and convert_literal_types_of_parsed_discrete_factor parsed_model target_type = function
    | Parsed_DF_variable _ as variable -> variable
    | Parsed_DF_constant var_value ->

        print_message Verbose_high ("\tConvert literal number value " ^ (DiscreteValue.string_of_value var_value) ^ " to " ^ (DiscreteValue.string_of_var_type_discrete target_type));
        Parsed_DF_constant (DiscreteValue.convert_value_to_discrete_type var_value target_type)
    | Parsed_DF_expression expr ->
        Parsed_DF_expression (convert_literal_types_of_parsed_discrete_arithmetic_expression parsed_model target_type expr)
    | Parsed_rational_of_int_function expr ->
        (* as it was already type checked, we convert inner expression of the function to int *)
        let inner_target_type = DiscreteValue.Var_type_discrete_number DiscreteValue.Var_type_discrete_int in
        Parsed_rational_of_int_function (convert_literal_types_of_parsed_discrete_arithmetic_expression parsed_model inner_target_type expr)
    | Parsed_DF_unary_min factor ->
        Parsed_DF_unary_min (convert_literal_types_of_parsed_discrete_factor parsed_model target_type factor)

let convert_literal_types_of_nonlinear_constraint parsed_model target_type = function
    (* It's ok non-linear constraint is of boolean type *)
    | Parsed_true_nonlinear_constraint -> Parsed_true_nonlinear_constraint
    | Parsed_false_nonlinear_constraint -> Parsed_false_nonlinear_constraint
    | Parsed_nonlinear_constraint expr ->
        Parsed_nonlinear_constraint (convert_literal_types_of_parsed_discrete_boolean_expression parsed_model target_type expr)





let rec infer_expression parsed_model = function
    | Parsed_global_expression expr ->
        let convert_expr, discrete_type = infer_parsed_boolean_expression parsed_model expr in
        Parsed_global_expression convert_expr, discrete_type

and infer_parsed_boolean_expression parsed_model = function

    | Parsed_True -> Parsed_True, DiscreteValue.Var_type_discrete_bool
    | Parsed_False -> Parsed_False, DiscreteValue.Var_type_discrete_bool

    | Parsed_And (l_expr, r_expr) ->
        let (convert_l_expr, convert_r_expr), discrete_type = check_and_convert_boolean_expression parsed_model l_expr r_expr in
        Parsed_And (convert_l_expr, convert_r_expr), discrete_type

    | Parsed_Or (l_expr, r_expr) ->
        let (convert_l_expr, convert_r_expr), discrete_type = check_and_convert_boolean_expression parsed_model l_expr r_expr in
        Parsed_Or (convert_l_expr, convert_r_expr), discrete_type

    | Parsed_Discrete_boolean_expression expr ->
        let infer_expr, discrete_type = infer_parsed_discrete_boolean_expression parsed_model expr in
        Parsed_Discrete_boolean_expression infer_expr, discrete_type

and infer_parsed_discrete_boolean_expression parsed_model = function
    | Parsed_arithmetic_expression expr ->
        let infer_expr, discrete_type = infer_parsed_discrete_arithmetic_expression parsed_model expr in
        Parsed_arithmetic_expression (infer_expr), discrete_type

    | Parsed_boolean_expression expr ->
        let infer_expr, discrete_type = infer_parsed_boolean_expression parsed_model expr in
        Parsed_boolean_expression (infer_expr), discrete_type

    | Parsed_Not expr ->
        let infer_expr, discrete_type = infer_parsed_boolean_expression parsed_model expr in
        Parsed_Not (infer_expr), discrete_type

    | Parsed_expression (l_expr, relop, r_expr) as expr ->

        let infer_l_expr, l_type = infer_parsed_discrete_boolean_expression parsed_model l_expr in
        let infer_r_expr, r_type = infer_parsed_discrete_boolean_expression parsed_model r_expr in

        (* Check if two types are compatibles *)
        if not (DiscreteValue.is_discrete_type_compatibles l_type r_type) then
            raise (TypeError (""))
        else if (DiscreteValue.is_discrete_type_unknown_number_type l_type && DiscreteValue.is_discrete_type_unknown_number_type r_type) then (
            (* No number type are deduced from tree, because there is only literal numbers *)
            (* So at this point, we convert all literals to rationals *)
            let target_type = DiscreteValue.Var_type_discrete_number DiscreteValue.Var_type_discrete_rational in

            print_message Verbose_high (
                "\tInfer literals of expression " ^
                string_of_parsed_discrete_boolean_expression parsed_model expr ^
                " as " ^
                DiscreteValue.string_of_var_type_discrete target_type
            );
            print_message Verbose_high (
                "\tInfer expression type " ^
                string_of_parsed_discrete_boolean_expression parsed_model expr ^
                " as " ^
                DiscreteValue.string_of_var_type_discrete DiscreteValue.Var_type_discrete_bool
            );

            let convert_l_expr = convert_literal_types_of_parsed_discrete_boolean_expression parsed_model target_type infer_l_expr in
            let convert_r_expr = convert_literal_types_of_parsed_discrete_boolean_expression parsed_model target_type infer_r_expr in

            Parsed_expression (convert_l_expr, relop, convert_r_expr), DiscreteValue.Var_type_discrete_bool
        )
        else if (DiscreteValue.is_discrete_type_unknown_number_type l_type) then (
            (* Convert *)
            let convert_l_expr = convert_literal_types_of_parsed_discrete_boolean_expression parsed_model r_type infer_l_expr in
            Parsed_expression (convert_l_expr, relop, infer_r_expr), DiscreteValue.Var_type_discrete_bool
        )
        else if (DiscreteValue.is_discrete_type_unknown_number_type r_type) then (
            (* Convert *)
            let convert_r_expr = convert_literal_types_of_parsed_discrete_boolean_expression parsed_model l_type infer_r_expr in
            Parsed_expression (infer_l_expr, relop, convert_r_expr), DiscreteValue.Var_type_discrete_bool
        )
        else (
            print_message Verbose_high (
                "\tInfer expression type " ^
                string_of_parsed_discrete_boolean_expression parsed_model expr ^
                " as " ^
                DiscreteValue.string_of_var_type_discrete DiscreteValue.Var_type_discrete_bool
            );
            Parsed_expression (infer_l_expr, relop, infer_r_expr), DiscreteValue.Var_type_discrete_bool
        )

    | Parsed_expression_in (expr, lower_expr, upper_expr) ->
        (* TODO fill ! *)
        Parsed_expression_in (expr, lower_expr, upper_expr), DiscreteValue.Var_type_discrete_bool

and infer_parsed_discrete_arithmetic_expression parsed_model = function
    | Parsed_DAE_plus (expr, term) ->
        let (convert_l_expr, convert_r_expr), discrete_type = check_and_convert_arithmetic_expression parsed_model expr term in
        Parsed_DAE_plus (convert_l_expr, convert_r_expr), discrete_type

    | Parsed_DAE_minus (expr, term) ->
        let (convert_l_expr, convert_r_expr), discrete_type = check_and_convert_arithmetic_expression parsed_model expr term in
        Parsed_DAE_minus (convert_l_expr, convert_r_expr), discrete_type

    | Parsed_DAE_term term ->
        let infer_term, discrete_type = infer_parsed_discrete_term parsed_model term in
        Parsed_DAE_term infer_term, discrete_type

and infer_parsed_discrete_term parsed_model = function
    | Parsed_DT_mul (term, factor) ->
        let (convert_l_expr, convert_r_expr), discrete_type = check_and_convert_term parsed_model term factor in
        Parsed_DT_mul (convert_l_expr, convert_r_expr), discrete_type

    | Parsed_DT_div (term, factor) ->
        let (convert_l_expr, convert_r_expr), discrete_type = check_and_convert_term parsed_model term factor in
        Parsed_DT_div (convert_l_expr, convert_r_expr), discrete_type

    | Parsed_DT_factor factor ->
        let infer_factor, factor_type = infer_parsed_discrete_factor parsed_model factor in
        Parsed_DT_factor infer_factor, factor_type

and check_and_convert_boolean_expression parsed_model l_expr r_expr =
        let infer_l_expr, l_type = infer_parsed_boolean_expression parsed_model l_expr in
        let infer_r_expr, r_type = infer_parsed_boolean_expression parsed_model r_expr in

        (* Check if two types are bool *)
        if not (DiscreteValue.is_discrete_type_bool_type l_type && DiscreteValue.is_discrete_type_bool_type r_type) then
            raise (TypeError (""))
        else
            (infer_l_expr, infer_r_expr), DiscreteValue.Var_type_discrete_bool

and check_and_convert_arithmetic_expression parsed_model expr term =
        let infer_expr, l_type = infer_parsed_discrete_arithmetic_expression parsed_model expr in
        let infer_term, r_type = infer_parsed_discrete_term parsed_model term in

        (* Check if two types are number *)
        if not (DiscreteValue.is_discrete_type_number_type l_type && DiscreteValue.is_discrete_type_number_type r_type) then
            raise (TypeError (""))
        else if (DiscreteValue.is_discrete_type_unknown_number_type l_type && DiscreteValue.is_discrete_type_unknown_number_type r_type) then
            (infer_expr, infer_term), DiscreteValue.Var_type_discrete_number DiscreteValue.Var_type_discrete_unknown_number
        else if (DiscreteValue.is_discrete_type_unknown_number_type l_type) then (
            (* Convert *)
            let convert_expr = convert_literal_types_of_parsed_discrete_arithmetic_expression parsed_model l_type infer_expr in
            (convert_expr, infer_term), l_type
        )
        else (
            (* Convert *)
            let convert_term = convert_literal_types_of_parsed_discrete_term parsed_model r_type infer_term in
            (infer_expr, convert_term), r_type
        )

and check_and_convert_term parsed_model term factor =
        let infer_term, l_type = infer_parsed_discrete_term parsed_model term in
        let infer_factor, r_type = infer_parsed_discrete_factor parsed_model factor in

        (* Check if two types are number *)
        if not (DiscreteValue.is_discrete_type_number_type l_type && DiscreteValue.is_discrete_type_number_type r_type) then
            raise (TypeError (""))
        else if (DiscreteValue.is_discrete_type_unknown_number_type l_type && DiscreteValue.is_discrete_type_unknown_number_type r_type) then
            (infer_term, infer_factor), DiscreteValue.Var_type_discrete_number DiscreteValue.Var_type_discrete_unknown_number
        else if (DiscreteValue.is_discrete_type_unknown_number_type l_type) then (
            (* Convert *)
            let convert_term = convert_literal_types_of_parsed_discrete_term parsed_model l_type infer_term in
            (convert_term, infer_factor), l_type
        )
        else (
            (* Convert *)
            let convert_factor = convert_literal_types_of_parsed_discrete_factor parsed_model r_type infer_factor in
            (infer_term, convert_factor), r_type
        )

and infer_parsed_discrete_factor parsed_model = function
    | Parsed_DF_variable variable_name as variable ->
        (* Get discrete type of variable *)
        let discrete_type = get_discrete_type_of_variable_by_name parsed_model variable_name in
        Parsed_DF_variable variable_name, discrete_type

    | Parsed_DF_constant var_value ->
        let discrete_type = DiscreteValue.discrete_type_of_value var_value in
        Parsed_DF_constant var_value, discrete_type

    | Parsed_DF_expression expr ->
        let infer_expr, expr_type = infer_parsed_discrete_arithmetic_expression parsed_model expr in
        Parsed_DF_expression infer_expr, expr_type

    | Parsed_rational_of_int_function expr ->
        let infer_expr, expr_type = infer_parsed_discrete_arithmetic_expression parsed_model expr in
        (* Check that expr type is a int type *)
        if (not (DiscreteValue.is_discrete_type_int_type expr_type || DiscreteValue.is_discrete_type_unknown_number_type expr_type)) then
            raise (TypeError (""))
        else (
            (* Set target type to int *)
            let target_type = DiscreteValue.Var_type_discrete_number DiscreteValue.Var_type_discrete_int in
            (* Convert all literal of the expression to int *)
            let convert_expr = convert_literal_types_of_parsed_discrete_arithmetic_expression parsed_model target_type expr in
            (* Return converted expression and it's type *)
            Parsed_rational_of_int_function convert_expr, target_type
        )

    | Parsed_DF_unary_min factor ->
        let infer_factor, factor_type = infer_parsed_discrete_factor parsed_model factor in
        Parsed_DF_unary_min infer_factor, factor_type

and infer_nonlinear_constraint parsed_model = function
    (* It's ok non-linear constraint is of boolean type *)
    | Parsed_true_nonlinear_constraint -> Parsed_true_nonlinear_constraint, DiscreteValue.Var_type_discrete_bool
    | Parsed_false_nonlinear_constraint -> Parsed_false_nonlinear_constraint, DiscreteValue.Var_type_discrete_bool
    | Parsed_nonlinear_constraint expr ->
        let convert_expr, discrete_type = infer_parsed_discrete_boolean_expression parsed_model expr in
        Parsed_nonlinear_constraint convert_expr, discrete_type


let rec discrete_type_of_expression parsed_model = function
    | Parsed_global_expression expr ->
        discrete_type_of_parsed_boolean_expression parsed_model expr

and discrete_type_of_parsed_boolean_expression parsed_model = function
	| Parsed_True
	| Parsed_False
	| Parsed_And _
	| Parsed_Or _ -> DiscreteValue.Var_type_discrete_bool
	| Parsed_Discrete_boolean_expression expr ->
	    discrete_type_of_parsed_discrete_boolean_expression parsed_model expr

and discrete_type_of_parsed_discrete_boolean_expression parsed_model = function
    | Parsed_arithmetic_expression expr ->
        discrete_type_of_parsed_discrete_arithmetic_expression parsed_model expr
	| Parsed_expression _
	| Parsed_expression_in _
	| Parsed_Not _ ->
	    DiscreteValue.Var_type_discrete_bool
	| Parsed_boolean_expression expr ->
	    discrete_type_of_parsed_boolean_expression parsed_model expr

and discrete_type_of_parsed_discrete_arithmetic_expression parsed_model = function
	| Parsed_DAE_plus (_, term)
	| Parsed_DAE_minus (_, term)
	| Parsed_DAE_term term ->
        discrete_type_of_parsed_discrete_term parsed_model term

and discrete_type_of_parsed_discrete_term parsed_model = function
	| Parsed_DT_mul (_, factor)
	| Parsed_DT_div (_, factor)
	| Parsed_DT_factor factor ->
	    discrete_type_of_parsed_discrete_factor parsed_model factor

and discrete_type_of_parsed_discrete_factor parsed_model = function
	| Parsed_DF_variable variable_name ->
	    get_discrete_type_of_variable_by_name parsed_model variable_name
	| Parsed_DF_constant value ->
	    DiscreteValue.discrete_type_of_value value

	| Parsed_DF_unary_min factor ->
	    discrete_type_of_parsed_discrete_factor parsed_model factor
	| Parsed_DF_expression expr
	| Parsed_rational_of_int_function expr ->
	    discrete_type_of_parsed_discrete_arithmetic_expression parsed_model expr



(* Check type compatibility of discrete var type variables, constants and literals used in expression *)
(* and try to resolve the global discrete type of an expression according to literals and variables used *)
let rec get_expression_discrete_type parsed_model = function
    | Parsed_global_expression expr ->
        let expr_type = get_parsed_boolean_expression_discrete_type parsed_model expr in
        expr_type

and get_parsed_boolean_expression_discrete_type parsed_model = function
    | Parsed_True
    | Parsed_False -> DiscreteValue.Var_type_discrete_bool
    | Parsed_And (l_expr, r_expr)
    | Parsed_Or (l_expr, r_expr) as parsed_boolean_expression ->
        let l_type = get_parsed_boolean_expression_discrete_type parsed_model l_expr in
        let r_type = get_parsed_boolean_expression_discrete_type parsed_model r_expr in
        (*
        (* Check that left and right types are boolean *)
        if not (DiscreteValue.is_discrete_type_bool_type l_type && DiscreteValue.is_discrete_type_bool_type r_type) then (
            let error_msg =
                "The expression \""
                ^ (string_of_parsed_boolean_expression parsed_model be)
                ^ "\" is not of type bool: "
                ^ (DiscreteValue.string_of_var_type_discrete l_type)
                ^ ","
                ^ (DiscreteValue.string_of_var_type_discrete r_type)
            in
            raise (TypeError error_msg)
        )
        else
            l_type
        *)
        if l_type <> r_type then (
            let error_msg = get_type_mixin_error_message l_type r_type (string_of_parsed_boolean_expression parsed_model parsed_boolean_expression) in
            raise (TypeError error_msg)
        )
        else
            (* Arbitrary return left member type *)
            l_type

    | Parsed_Discrete_boolean_expression expr -> get_parsed_discrete_boolean_expression_discrete_type parsed_model expr

and get_parsed_discrete_boolean_expression_discrete_type parsed_model = function
    | Parsed_arithmetic_expression expr -> get_parsed_discrete_arithmetic_expression_discrete_type parsed_model expr
    | Parsed_Not expr
    | Parsed_boolean_expression expr -> get_parsed_boolean_expression_discrete_type parsed_model expr
    | Parsed_expression (l_expr, relop, r_expr) as parsed_discrete_boolean_expression ->

        let l_type = get_parsed_discrete_boolean_expression_discrete_type parsed_model l_expr in
        let r_type = get_parsed_discrete_boolean_expression_discrete_type parsed_model r_expr in
        (* Check if it's an ordered comparison *)
        let is_ordered_comparison = (match relop with | PARSED_OP_EQ | PARSED_OP_NEQ -> false | _ -> true) in

        if DiscreteValue.is_discrete_type_unknown_number_type l_type && DiscreteValue.is_discrete_type_unknown_number_type r_type then
            (* Arbitrary return l_type *)
            l_type
        else if DiscreteValue.is_discrete_type_unknown_number_type l_type && DiscreteValue.is_discrete_type_number_type r_type then
            r_type
        else if DiscreteValue.is_discrete_type_unknown_number_type r_type && DiscreteValue.is_discrete_type_number_type l_type then
            l_type
        (* If comparison is ordered, check that left and right types are number *)
        else if is_ordered_comparison && not (DiscreteValue.is_discrete_type_number_type l_type && DiscreteValue.is_discrete_type_number_type r_type) then (
            let error_msg =
                "Left or right member of expression \""
                ^ (string_of_parsed_discrete_boolean_expression parsed_model parsed_discrete_boolean_expression)
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
                (* Arbitrary return left member type *)
                l_type
    | Parsed_expression_in (expr, lower_expr, upper_expr) as dae ->
        let expr_type = get_parsed_discrete_arithmetic_expression_discrete_type parsed_model expr in
        let lower_type = get_parsed_discrete_arithmetic_expression_discrete_type parsed_model lower_expr in
        let upper_type = get_parsed_discrete_arithmetic_expression_discrete_type parsed_model upper_expr  in
        (* Check that left and right types are number *)
        if not (DiscreteValue.is_discrete_type_number_type expr_type && DiscreteValue.is_discrete_type_number_type lower_type && DiscreteValue.is_discrete_type_number_type upper_type) then (
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
                (* Arbitrary return expression member type *)
                expr_type

and get_parsed_discrete_arithmetic_expression_discrete_type parsed_model = function
    | Parsed_DAE_plus (expr, term)
    | Parsed_DAE_minus (expr, term) as dae_expr ->
        let l_type = get_parsed_discrete_arithmetic_expression_discrete_type parsed_model expr in
        let r_type = get_parsed_discrete_term_discrete_type parsed_model term in

        if DiscreteValue.is_discrete_type_unknown_number_type l_type && DiscreteValue.is_discrete_type_unknown_number_type r_type then
            (* Arbitrary return l_type *)
            l_type
        else if DiscreteValue.is_discrete_type_unknown_number_type l_type && DiscreteValue.is_discrete_type_number_type r_type then
            r_type
        else if DiscreteValue.is_discrete_type_unknown_number_type r_type && DiscreteValue.is_discrete_type_number_type l_type then
            l_type
        else if l_type <> r_type then (
            let error_msg = get_type_mixin_error_message l_type r_type (string_of_parsed_arithmetic_expression parsed_model dae_expr) in
            raise (TypeError error_msg)
        )
        else
            l_type
    | Parsed_DAE_term term ->
        get_parsed_discrete_term_discrete_type parsed_model term

and get_parsed_discrete_term_discrete_type parsed_model = function
    | Parsed_DT_mul (term, factor)
    | Parsed_DT_div (term, factor) as dae_term ->
        let l_type = get_parsed_discrete_term_discrete_type parsed_model term in
        let r_type = get_parsed_discrete_factor_discrete_type parsed_model factor in

        if DiscreteValue.is_discrete_type_unknown_number_type l_type && DiscreteValue.is_discrete_type_number_type r_type then
            r_type
        else if DiscreteValue.is_discrete_type_unknown_number_type r_type && DiscreteValue.is_discrete_type_number_type l_type then
            l_type
        else
        if l_type <> r_type then (
            let error_msg =
                "The expression \""
                ^ (string_of_parsed_term parsed_model dae_term)
                ^ "\" mix different types : "
                ^ (DiscreteValue.string_of_var_type_discrete l_type)
                ^ ", "
                ^ (DiscreteValue.string_of_var_type_discrete r_type)
            in
            raise (TypeError error_msg)
        )
        else
            l_type
    | Parsed_DT_factor factor ->
        get_parsed_discrete_factor_discrete_type parsed_model factor

and get_parsed_discrete_factor_discrete_type parsed_model = function
    | Parsed_DF_variable variable_name ->
        (* TODO benjamin replace by call to get_discrete_type_of_variable_by_name *)
        if Hashtbl.mem parsed_model.index_of_variables variable_name then (
            (* Get type of variable *)
            let variable_index = Hashtbl.find parsed_model.index_of_variables variable_name in
            let variable_type = get_discrete_type_of_variable parsed_model variable_index in
            variable_type
        )
        else (
            if Hashtbl.mem parsed_model.constants variable_name then (
                (* Retrieve the value of the global constant *)
                let value = Hashtbl.find parsed_model.constants variable_name in
                (* Get type of constant *)
                DiscreteValue.discrete_type_of_value value
            ) else (
                raise (InternalError ("Impossible to find the index of variable `" ^ variable_name ^ "` although this should have been checked before."))
            )
        )
    | Parsed_DF_constant var_value ->
        DiscreteValue.discrete_type_of_value var_value
    | Parsed_DF_expression expr ->
        get_parsed_discrete_arithmetic_expression_discrete_type parsed_model expr
    | Parsed_rational_of_int_function expr ->
        let expr_type = get_parsed_discrete_arithmetic_expression_discrete_type parsed_model expr in
        if not (DiscreteValue.is_discrete_type_unknown_number_type expr_type || DiscreteValue.is_discrete_type_int_type expr_type) then (
            raise (
                TypeError (
                    "Expression \""
                    ^ (string_of_parsed_arithmetic_expression parsed_model expr)
                    ^ "\" of type "
                    ^ (DiscreteValue.string_of_var_type_discrete expr_type)
                    ^ " is not an int expression"
                )
            )
        ) else
            DiscreteValue.Var_type_discrete_number DiscreteValue.Var_type_discrete_rational
    | Parsed_DF_unary_min factor ->
        get_parsed_discrete_factor_discrete_type parsed_model factor

let rec get_expression_type parsed_model = function
    | Parsed_global_expression expr ->
        get_parsed_boolean_expression_type parsed_model expr

and get_parsed_boolean_expression_type parsed_model = function
    | Parsed_True
    | Parsed_False ->
        DiscreteExpressions.Expression_type_discrete_bool DiscreteValue.Var_type_discrete_bool
    | Parsed_And (expr, _)
    | Parsed_Or (expr, _) ->

        (* Get var type of arithmetic expression *)
        let discrete_type = get_parsed_boolean_expression_discrete_type parsed_model expr in
        (* Return typed expression *)
        DiscreteExpressions.Expression_type_discrete_bool discrete_type

    | Parsed_Discrete_boolean_expression expr -> get_parsed_discrete_boolean_expression_type parsed_model expr

and get_parsed_discrete_boolean_expression_type parsed_model = function

    | Parsed_expression (expr, _, _) ->
        (* Get var type of arithmetic expression *)
        let discrete_type = get_parsed_discrete_boolean_expression_discrete_type parsed_model expr in
        (* Return typed expression *)
        DiscreteExpressions.Expression_type_discrete_bool discrete_type

    | Parsed_expression_in (expr, _, _) ->

        (* Get var type of arithmetic expression *)
        let discrete_type = get_parsed_discrete_arithmetic_expression_discrete_type parsed_model expr in
        (* Return typed expression *)
        DiscreteExpressions.Expression_type_discrete_bool discrete_type

    | Parsed_Not expr
    | Parsed_boolean_expression expr -> get_parsed_boolean_expression_type parsed_model expr

    | Parsed_arithmetic_expression expr ->

        (* Get var type of arithmetic expression *)
        let discrete_type = get_parsed_discrete_arithmetic_expression_discrete_type parsed_model expr in

        (* Check before, it should be a number type *)
        let expr_type = (
            match discrete_type with
                | DiscreteValue.Var_type_discrete_number number_type -> DiscreteExpressions.Expression_type_discrete_arithmetic number_type
                | DiscreteValue.Var_type_discrete_bool -> DiscreteExpressions.Expression_type_discrete_bool DiscreteValue.Var_type_discrete_bool
        ) in
        expr_type


let get_nonlinear_constraint_type parsed_model = function
    | Parsed_true_nonlinear_constraint
    | Parsed_false_nonlinear_constraint ->
        DiscreteExpressions.Expression_type_discrete_bool DiscreteValue.Var_type_discrete_bool
    | Parsed_nonlinear_constraint expr ->
        get_parsed_discrete_boolean_expression_type parsed_model expr


let get_nonlinear_constraint_discrete_type parsed_model = function
    (* It's ok non-linear constraint is of boolean type *)
    | Parsed_true_nonlinear_constraint -> DiscreteValue.Var_type_discrete_bool
    | Parsed_false_nonlinear_constraint -> DiscreteValue.Var_type_discrete_bool
    | Parsed_nonlinear_constraint expr ->
        let expr_var_type_discrete = get_parsed_discrete_boolean_expression_discrete_type parsed_model expr in
        (* If type is an unknown number, we choose that expression is rational *)
        (* else get the expression type *)
        if DiscreteValue.is_discrete_type_unknown_number_type expr_var_type_discrete then
            DiscreteValue.Var_type_discrete_number DiscreteValue.Var_type_discrete_rational
        else
            expr_var_type_discrete

(** Resolve expression type **)

(* Resolve and convert literal *)
let resolve_expression_type parsed_model expr =

    (* Get var type of the expression, deduced by the used variables *)
    let expr_var_type_discrete = get_expression_discrete_type parsed_model expr in
    (*
    (* If type cannot be resolved (no variable for example, turn to rational *)
    let expr_var_type_discrete = (
        if DiscreteValue.is_unknown_number_type expr_var_type_discrete then
            DiscreteValue.var_type_rational
        else
            expr_var_type_discrete
    ) in *)

    print_message Verbose_high (
        "Literals of expression \""
        ^ (string_of_parsed_global_expression parsed_model expr)
        ^ "\" should be uniformized to "
        ^ (DiscreteValue.string_of_var_type_discrete expr_var_type_discrete)
    );

    (* Uniformize expression by converting literals number to correct type *)
    let uniformly_typed_expr = convert_literal_types_of_expression parsed_model expr_var_type_discrete expr in

    (* Get expression type *)
    let expr_type = get_expression_type parsed_model uniformly_typed_expr in

    print_message Verbose_high (
        "Resolve expression type of \""
        ^ (string_of_parsed_global_expression parsed_model expr)
        ^ "\" as "
        ^ (DiscreteExpressions.string_of_expression_type expr_type)
    );

    (* Return uniform typed expression and it's type *)
    uniformly_typed_expr, expr_type

(* Resolve and convert literal *)
let resolve_bool_expression_type parsed_model expr =

    (* Get var type of the expression, deduced by the used variables *)
    let expr_var_type_discrete = get_parsed_boolean_expression_discrete_type parsed_model expr in

    (* If type cannot be resolved (no variable for example, turn to rational *)
    let expr_var_type_discrete = (
        if DiscreteValue.is_discrete_type_unknown_number_type expr_var_type_discrete then
            DiscreteValue.Var_type_discrete_number DiscreteValue.Var_type_discrete_rational
        else
            expr_var_type_discrete
    ) in

    print_message Verbose_high (
        "Literals of expression \""
        ^ (string_of_parsed_boolean_expression parsed_model expr)
        ^ "\" should be uniformized to "
        ^ (DiscreteValue.string_of_var_type_discrete expr_var_type_discrete)
    );

    (* Uniformize expression by converting literals number to correct type *)
    let uniformly_typed_expr = convert_literal_types_of_parsed_boolean_expression parsed_model expr_var_type_discrete expr in

    (* Get expression type *)
    let expr_type = get_parsed_boolean_expression_type parsed_model uniformly_typed_expr in

    print_message Verbose_high (
        "Resolve expression type of \""
        ^ (string_of_parsed_boolean_expression parsed_model expr)
        ^ "\" as "
        ^ (DiscreteExpressions.string_of_expression_type expr_type)
    );

    (* Return uniform typed expression and it's type *)
    uniformly_typed_expr, expr_type


(* Resolve and convert implicitly literals *)
(*
let resolve_nonlinear_constraint_type parsed_model expr =

    (* Get var type of the expression, deduced by the used variables *)
    let expr_var_type_discrete = get_nonlinear_constraint_discrete_type parsed_model expr in

    (* If type cannot be resolved (no variable for example, turn to rational *)
    let expr_var_type_discrete =
        if DiscreteValue.is_discrete_type_unknown_number_type expr_var_type_discrete then
            DiscreteValue.Var_type_discrete_number DiscreteValue.Var_type_discrete_rational
        else
            expr_var_type_discrete
    in

    print_message Verbose_high (
        "Literals of non linear expression \""
        ^ (string_of_parsed_nonlinear_constraint parsed_model expr)
        ^ "\" should be uniformized to "
        ^ (DiscreteValue.string_of_var_type_discrete expr_var_type_discrete)
    );

    (* Uniformize expression by converting literals number to correct type *)
    let uniformly_typed_nonlinear_constraint = convert_literal_types_of_nonlinear_constraint parsed_model expr_var_type_discrete expr in

    (* Get expression type *)
    let expr_type = get_nonlinear_constraint_type parsed_model uniformly_typed_nonlinear_constraint in

    print_message Verbose_high (
        "Resolve expression type of \""
        ^ (string_of_parsed_nonlinear_constraint parsed_model expr)
        ^ "\" as "
        ^ (DiscreteExpressions.string_of_expression_type expr_type)
    );

    (* Return uniform typed expression and it's type *)
    uniformly_typed_nonlinear_constraint, expr_type
*)

(*
let check_expression parsed_model expr =
    (* Resolve expression type and get uniformly typed expression *)
    let uniformly_typed_expr, expr_type = resolve_expression_type parsed_model expr in
*)
let check_nonlinear_constraint parsed_model nonlinear_constraint =

    let uniformly_typed_nonlinear_constraint, discrete_type = infer_nonlinear_constraint parsed_model nonlinear_constraint in

    (* Check that non-linear constraint is a boolean expression *)
    match discrete_type with
    | DiscreteValue.Var_type_discrete_bool -> uniformly_typed_nonlinear_constraint, discrete_type
    | _ -> raise (TypeError (
        "Guard or invariant expression \""
        ^ (string_of_parsed_nonlinear_constraint parsed_model nonlinear_constraint)
        ^ "\" is not a boolean expression"
    ))


(* Type check guard / invariant *)
(* return a tuple containing the expression uniformly typed and the resolved type of the expression *)
let check_guard parsed_model guard =

    let resolved_nonlinear_constraints = List.map (check_nonlinear_constraint parsed_model) guard in

    let uniformly_typed_nonlinear_constraints = List.map (fun (u, _) -> u) resolved_nonlinear_constraints in
    let nonlinear_constraint_types = List.map (fun (_, t) -> t) resolved_nonlinear_constraints in

    uniformly_typed_nonlinear_constraints, List.hd nonlinear_constraint_types

(* Type check an update *)
(* return a tuple containing the update uniformly typed and the resolved type of the expression *)
let check_update parsed_model variable_name expr =

    (* Resolve expression type and get uniformly typed expression *)
    let uniformly_typed_expr, expr_type = resolve_expression_type parsed_model expr in
    (* Get assigned variable type *)
    let var_type_discrete = get_discrete_type_of_variable_by_name parsed_model variable_name in

    (*  *)
    let typed_expr =
        (* Check var_type_discrete is compatible with expression type, if yes, convert expression *)
        if not (DiscreteExpressions.is_var_type_discrete_compatible_with_expr_type var_type_discrete expr_type) then (
            raise (TypeError (
                "Variable \""
                ^ variable_name
                ^ "\" of type "
                ^ (DiscreteValue.string_of_var_type_discrete var_type_discrete)
                ^ " is not compatible with expression \""
                ^ (ParsingStructureUtilities.string_of_parsed_global_expression parsed_model uniformly_typed_expr)
                ^ "\" of "
                ^ (DiscreteExpressions.string_of_expression_type expr_type)
                )
            )
        (* Check if expression type is resolved as unknown number *)
        ) else if DiscreteExpressions.is_unknown_number_expression_type expr_type then (
            (* If the expression type is unknown number, and as expression type and var type are compatible *)
            (* convert expression type to variable type *)
            print_message Verbose_high (
                "Reconvert update expression "
                ^ (string_of_parsed_global_expression parsed_model expr)
                ^ " to "
                ^ (DiscreteValue.string_of_var_type_discrete var_type_discrete)
            );
            convert_literal_types_of_expression parsed_model var_type_discrete uniformly_typed_expr
        (* Else, just return the new typed expression *)
        ) else if DiscreteExpressions.is_bool_of_unknown_number_expression_type expr_type then (
            print_message Verbose_high (
                "Reconvert update expression literals "
                ^ (string_of_parsed_global_expression parsed_model expr)
                ^ " to rational"
            );
            convert_literal_types_of_expression parsed_model (DiscreteValue.Var_type_discrete_number DiscreteValue.Var_type_discrete_rational) uniformly_typed_expr
        ) else
            uniformly_typed_expr
    in
    variable_name, typed_expr


(* Type check a conditional expression *)
(* return a tuple containing the conditional expression uniformly typed and the resolved type of the expression *)
let check_conditional parsed_model expr =

    let uniformly_typed_bool_expr, expr_type = resolve_bool_expression_type parsed_model expr in

    (* Check that non-linear constraint is a boolean expression *)
    match expr_type with
    | DiscreteExpressions.Expression_type_discrete_bool discrete_type -> uniformly_typed_bool_expr, discrete_type
    | _ ->
        raise (TypeError (
            "Expression \""
            ^ (string_of_parsed_boolean_expression parsed_model expr)
            ^ "\" in conditional statement, is not a boolean expression"
            )
        )

let check_type_of_nonlinear_constraint parsed_model = function
    (* It's ok non-linear constraint is of boolean type *)
    | Parsed_true_nonlinear_constraint
    | Parsed_false_nonlinear_constraint -> true
    | Parsed_nonlinear_constraint expr ->
        let expr_var_type_discrete = get_parsed_discrete_boolean_expression_discrete_type parsed_model expr in
        DiscreteValue.is_discrete_type_bool_type expr_var_type_discrete

(* Check that an expression assigned to a variable is of the same type *)
(* If not, raise a TypeError exception with an error message *)
let check_type_assignment parsed_model variable_name expr =

    (* Function that construct type error message *)
    let get_error_message variable_name variable_type expr_type expr =
        "Variable "
        ^ variable_name
        ^ " of type "
        ^ (DiscreteValue.string_of_var_type_discrete variable_type)
        ^ " is not compatible with expression : \""
        ^ (string_of_parsed_global_expression parsed_model expr)
        ^ "\""
        ^ " of type "
        ^ (DiscreteValue.string_of_var_type_discrete expr_type)
    in

    (* Get variable type *)
    let variable_type = get_discrete_type_of_variable_by_name parsed_model variable_name in
    (* Resolve expression type *)
    let expr_var_type_discrete = get_expression_discrete_type parsed_model expr in
    (* Check expression / variable type consistency *)
    let is_consistent = DiscreteValue.is_discrete_type_compatibles variable_type expr_var_type_discrete in
(*    print_message Verbose_standard ("Variable " ^ variable_name ^ " of type " ^ (DiscreteValue.string_of_var_type_discrete variable_type)*)
(*    ^ " is compatible with expr of type " ^ (DiscreteValue.string_of_var_type_discrete expr_var_type_discrete));*)
    (* Not consistent ? raise a type error with appropriate message*)
    if not (is_consistent) then (
        raise (TypeError (get_error_message variable_name variable_type expr_var_type_discrete expr))
    );;


(* Check that constant declarations are well typed *)
let check_constant_declarations evaluated_constants =

    (* Check type consistency between constant type and value *)
    let is_types_consistents = List.for_all (fun (name, _, value, var_type) ->
        let is_compatible = DiscreteValue.check_value_compatible_with_type value var_type in
        (* If not compatibles, display an error message *)
        if not (is_compatible) then (
            print_error ("Constant "
                ^ name
                ^ " of type "
                ^ (DiscreteValue.string_of_var_type var_type)
                ^ " is not compatible with value "
                ^ (DiscreteValue.string_of_value value)
                ^ " of type "
                ^ (DiscreteValue.string_of_var_type_discrete (DiscreteValue.discrete_type_of_value value))
            )
        );
        is_compatible
    ) evaluated_constants in

    if not (is_types_consistents) then (
        raise (TypeError "Bad constant declaration(s)")
    );

    (* Convert value assigned to the constant to the type of the constant *)
    (* and create tuples for representing constants as name * value *)
    let constant_tuples = List.map (fun (name, _, value, var_type) ->
        let discrete_type = DiscreteValue.discrete_type_of_var_type var_type in
        name, DiscreteValue.convert_value_to_discrete_type value discrete_type
    ) evaluated_constants in


    constant_tuples