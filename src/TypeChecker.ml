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

(* Type error exception *)
exception TypeError of string
exception InternalError of string

(* Error message when mixin of different types *)
let get_type_mixin_error_message l_type r_type str_expr =
    "The expression \""
    ^ str_expr
    ^ "\" mix different types : "
    ^ (DiscreteValue.string_of_var_type l_type)
    ^ ", "
    ^ (DiscreteValue.string_of_var_type r_type)

(* Error message when mixin of different types *)
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




(* Get a new parsed model with literal rationals implicitly converted to suitable number types *)
(* Example : i * 2 with i : int, convert 2 from rational to int *)

(* Get variable type given it's index *)
let get_type_of_variable parsed_model variable_index =
    parsed_model.type_of_variables variable_index

(* Get variable type given it's name *)
let get_type_of_variable_by_name parsed_model variable_name =
    (* Get the variable index *)
    let discrete_index = Hashtbl.find parsed_model.index_of_variables variable_name in
    get_type_of_variable parsed_model discrete_index

(* TODO benjamin rename all convert_literal_ by uniform_var_type_of_expression, something like that *)
let rec convert_literal_types_of_expression parsed_model target_type = function
    | Parsed_global_expression expr ->
        Parsed_global_expression (convert_literal_types_of_parsed_boolean_expression parsed_model target_type expr)

and convert_literal_types_of_parsed_boolean_expression parsed_model target_type = function
    | Parsed_True -> Parsed_True
    | Parsed_False -> Parsed_False
    | Parsed_Not expr ->
        Parsed_Not (convert_literal_types_of_parsed_boolean_expression parsed_model target_type expr)
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
    | Parsed_expression (l_expr, relop, r_expr) ->
        let convert_l_expr = convert_literal_types_of_parsed_discrete_arithmetic_expression parsed_model target_type l_expr in
        let convert_r_expr = convert_literal_types_of_parsed_discrete_arithmetic_expression parsed_model target_type r_expr in
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
    | Parsed_DAE_minus (expr, term) as dae_expr ->
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
    | Parsed_DT_div (term, factor) as dae_term ->
        let convert_term = convert_literal_types_of_parsed_discrete_term parsed_model target_type term in
        let convert_factor = convert_literal_types_of_parsed_discrete_factor parsed_model target_type factor in
        Parsed_DT_div (convert_term, convert_factor)
    | Parsed_DT_factor factor ->
        Parsed_DT_factor (convert_literal_types_of_parsed_discrete_factor parsed_model target_type factor)

and convert_literal_types_of_parsed_discrete_factor parsed_model target_type = function
    | Parsed_DF_variable _ as variable -> variable
    | Parsed_DF_constant var_value ->
        (* TODO benjamin change verbose mode *)
        print_message Verbose_standard ("\tConvert literal number value " ^ (DiscreteValue.string_of_value var_value) ^ " to " ^ (DiscreteValue.string_of_var_type target_type));
        Parsed_DF_constant (DiscreteValue.convert_value var_value target_type)
    | Parsed_DF_expression expr ->
        Parsed_DF_expression (convert_literal_types_of_parsed_discrete_arithmetic_expression parsed_model target_type expr)
    | Parsed_DF_unary_min factor ->
        Parsed_DF_unary_min (convert_literal_types_of_parsed_discrete_factor parsed_model target_type factor)

let convert_literal_types_of_nonlinear_constraint parsed_model target_type = function
    (* It's ok non-linear constraint is of boolean type *)
    | Parsed_true_nonlinear_constraint -> Parsed_true_nonlinear_constraint
    | Parsed_false_nonlinear_constraint -> Parsed_false_nonlinear_constraint
    | Parsed_nonlinear_constraint expr ->
        Parsed_nonlinear_constraint (convert_literal_types_of_parsed_discrete_boolean_expression parsed_model target_type expr)





(* TODO benjamin rename all functions below get_expression_var_type *)
(* Try to resolve the specific type of an expression according to literals and variables used *)
(* Doing type checking of the expression at the same time *)
let rec get_expression_type parsed_model = function
    | Parsed_global_expression expr as global_expr ->
        let expr_type = get_parsed_boolean_expression_type parsed_model expr in
        (* If type is a number, we choose that expression is rational *)
        (* else get the expression type *)
        if DiscreteValue.is_unknown_number_type expr_type then
            DiscreteValue.Var_type_discrete (DiscreteValue.Var_type_discrete_number DiscreteValue.Var_type_discrete_rational)
        else
            expr_type

and get_parsed_boolean_expression_type parsed_model = function
    | Parsed_True
    | Parsed_False -> DiscreteValue.Var_type_discrete DiscreteValue.Var_type_discrete_bool
    | Parsed_Not expr -> get_parsed_boolean_expression_type parsed_model expr
    | Parsed_And (l_expr, r_expr)
    | Parsed_Or (l_expr, r_expr) as be ->
        let l_type = get_parsed_boolean_expression_type parsed_model l_expr in
        let r_type = get_parsed_boolean_expression_type parsed_model r_expr in
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
    | Parsed_Discrete_boolean_expression expr -> get_parsed_discrete_boolean_expression_type parsed_model expr

and get_parsed_discrete_boolean_expression_type parsed_model = function
    | Parsed_arithmetic_expression expr -> get_parsed_discrete_arithmetic_expression_type parsed_model expr
    | Parsed_boolean_expression expr -> get_parsed_boolean_expression_type parsed_model expr
    | Parsed_expression (l_expr, relop, r_expr) as parsed_discrete_boolean_expression ->

        let l_type = get_parsed_discrete_arithmetic_expression_type parsed_model l_expr in
        let r_type = get_parsed_discrete_arithmetic_expression_type parsed_model r_expr in
        (* Check if it's an ordered comparison *)
        let is_ordered_comparison = (match relop with | PARSED_OP_EQ | PARSED_OP_NEQ -> false | _ -> true) in

        if DiscreteValue.is_unknown_number_type l_type && DiscreteValue.is_unknown_number_type r_type then
            (* Arbitrary return l_type *)
            l_type
        else if DiscreteValue.is_unknown_number_type l_type && DiscreteValue.is_number_type r_type then
            r_type
        else if DiscreteValue.is_unknown_number_type r_type && DiscreteValue.is_number_type l_type then
            l_type
        (* If comparison is ordered, check that left and right types are number *)
        else if is_ordered_comparison && not (DiscreteValue.is_number_type l_type && DiscreteValue.is_number_type r_type) then (
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
        let expr_type = get_parsed_discrete_arithmetic_expression_type parsed_model expr in
        let lower_type = get_parsed_discrete_arithmetic_expression_type parsed_model lower_expr in
        let upper_type = get_parsed_discrete_arithmetic_expression_type parsed_model upper_expr  in
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
                (* Arbitrary return expression member type *)
                expr_type

    (* Other, expression is a boolean expression *)
    | Parsed_DB_variable _ -> DiscreteValue.Var_type_discrete DiscreteValue.Var_type_discrete_bool

and get_parsed_discrete_arithmetic_expression_type parsed_model = function
    | Parsed_DAE_plus (expr, term)
    | Parsed_DAE_minus (expr, term) as dae_expr ->
        let l_type = get_parsed_discrete_arithmetic_expression_type parsed_model expr in
        let r_type = get_parsed_discrete_term_type parsed_model term in

        if DiscreteValue.is_unknown_number_type l_type && DiscreteValue.is_unknown_number_type r_type then
            (* Arbitrary return l_type *)
            l_type
        else if DiscreteValue.is_unknown_number_type l_type && DiscreteValue.is_number_type r_type then
            r_type
        else if DiscreteValue.is_unknown_number_type r_type && DiscreteValue.is_number_type l_type then
            l_type
        else if l_type <> r_type then (
            let error_msg = get_type_mixin_error_message l_type r_type (string_of_parsed_arithmetic_expression parsed_model dae_expr) in
            raise (TypeError error_msg)
        )
        else
            l_type
    | Parsed_DAE_term term ->
        get_parsed_discrete_term_type parsed_model term

and get_parsed_discrete_term_type parsed_model = function
    | Parsed_DT_mul (term, factor)
    | Parsed_DT_div (term, factor) as dae_term ->
        let l_type = get_parsed_discrete_term_type parsed_model term in
        let r_type = get_parsed_discrete_factor_type parsed_model factor in

        if DiscreteValue.is_unknown_number_type l_type && DiscreteValue.is_number_type r_type then
            r_type
        else if DiscreteValue.is_unknown_number_type r_type && DiscreteValue.is_number_type l_type then
            l_type
        else
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
        get_parsed_discrete_factor_type parsed_model factor

and get_parsed_discrete_factor_type parsed_model = function
    | Parsed_DF_variable variable_name ->
        if Hashtbl.mem parsed_model.index_of_variables variable_name then (
            (* Get type of variable *)
            let variable_index = Hashtbl.find parsed_model.index_of_variables variable_name in
            let variable_type = parsed_model.type_of_variables variable_index in
            DiscreteValue.inner_type_of variable_type
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
        DiscreteValue.var_type_of_value var_value
    | Parsed_DF_expression expr ->
        get_parsed_discrete_arithmetic_expression_type parsed_model expr
    | Parsed_DF_unary_min factor ->
        get_parsed_discrete_factor_type parsed_model factor

(* TODO benjamin rename all function below y get_expression_type *)
let rec get_expression_type_new parsed_model = function
    | Parsed_global_expression expr as global_expr ->
        get_parsed_boolean_expression_type_new parsed_model expr

and get_parsed_boolean_expression_type_new parsed_model = function
    | Parsed_True
    | Parsed_False ->
        DiscreteValue.Expression_type_discrete_bool DiscreteValue.Var_type_discrete_bool
    | Parsed_Not expr
    | Parsed_And (expr, _)
    | Parsed_Or (expr, _) ->

        (* Get var type of arithmetic expression *)
        let inner_type = get_parsed_boolean_expression_type parsed_model expr in

        (* Check before, it should be a discrete type *)
        let discrete_type = (
            match inner_type with
            | Var_type_discrete discrete_type -> discrete_type
            | x -> raise (TypeError ("Bad expression type " ^ (DiscreteValue.string_of_var_type x) ^ " for bool "))
        ) in
        (* Return typed expression *)
        DiscreteValue.Expression_type_discrete_bool discrete_type

    | Parsed_Discrete_boolean_expression expr -> get_parsed_discrete_boolean_expression_type_new parsed_model expr

and get_parsed_discrete_boolean_expression_type_new parsed_model = function

    | Parsed_expression (expr, _, _)
    | Parsed_expression_in (expr, _, _) ->

        (* Get var type of arithmetic expression *)
        let inner_type = get_parsed_discrete_arithmetic_expression_type parsed_model expr in

        (* Check before, it should be a number type *)
        let discrete_type = (
            match inner_type with
            | Var_type_discrete discrete_type -> discrete_type
            | x -> raise (TypeError ("Bad expression type " ^ (DiscreteValue.string_of_var_type x) ^ " for bool"))
        ) in
        (* Return typed expression *)
        DiscreteValue.Expression_type_discrete_bool discrete_type

    | Parsed_boolean_expression expr -> get_parsed_boolean_expression_type_new parsed_model expr
    | Parsed_arithmetic_expression expr ->

        (* Get var type of arithmetic expression *)
        let inner_type = get_parsed_discrete_arithmetic_expression_type parsed_model expr in

        (* Check before, it should be a number type *)
        let expr_type = (
            match inner_type with
                | Var_type_discrete (Var_type_discrete_number number_type) -> DiscreteValue.Expression_type_discrete_number number_type
                | Var_type_discrete Var_type_discrete_bool -> DiscreteValue.Expression_type_discrete_bool DiscreteValue.Var_type_discrete_bool
                | x -> raise (TypeError ("Bad expression type " ^ (DiscreteValue.string_of_var_type x) ^ " for number"))
        ) in
        expr_type

    | Parsed_DB_variable _ ->
        DiscreteValue.Expression_type_discrete_bool DiscreteValue.Var_type_discrete_bool


let get_nonlinear_constraint_type_new parsed_model = function
    | Parsed_true_nonlinear_constraint
    | Parsed_false_nonlinear_constraint ->
        DiscreteValue.Expression_type_discrete_bool DiscreteValue.Var_type_discrete_bool
    | Parsed_nonlinear_constraint expr ->
        get_parsed_discrete_boolean_expression_type_new parsed_model expr




let get_nonlinear_constraint_type parsed_model = function
    (* It's ok non-linear constraint is of boolean type *)
    | Parsed_true_nonlinear_constraint -> DiscreteValue.Var_type_discrete DiscreteValue.Var_type_discrete_bool
    | Parsed_false_nonlinear_constraint -> DiscreteValue.Var_type_discrete DiscreteValue.Var_type_discrete_bool
    | Parsed_nonlinear_constraint expr ->
        let expr_type = get_parsed_discrete_boolean_expression_type parsed_model expr in
        (* If type is an unknown number, we choose that expression is rational *)
        (* else get the expression type *)
        if DiscreteValue.is_unknown_number_type expr_type then
            DiscreteValue.Var_type_discrete (DiscreteValue.Var_type_discrete_number DiscreteValue.Var_type_discrete_rational)
        else
            expr_type

(* TODO benjamin try to encapsulate functions below in module with functor for no repeat *)

(* Resolve and convert literal *)
let resolve_expression_type parsed_model expr =

    (* Get var type of the expression, deduced by the used variables *)
    let expr_var_type = get_expression_type parsed_model expr in

    (* If type cannot be resolved (no variable for example, turn to rational *)
    let expr_var_type = (
        if DiscreteValue.is_unknown_number_type expr_var_type then
            DiscreteValue.var_type_rational
        else
            expr_var_type
    ) in
    (* TODO benjamin change verbose mode *)
    print_message Verbose_standard (
        "Literals of expression \""
        ^ (string_of_parsed_global_expression parsed_model expr)
        ^ "\" should be uniformized to "
        ^ (DiscreteValue.string_of_var_type expr_var_type)
    );

    (* Uniformize expression by converting literals number to correct type *)
    let uniformly_typed_expr = convert_literal_types_of_expression parsed_model expr_var_type expr in

    (* Get expression type *)
    let expr_type = get_expression_type_new parsed_model uniformly_typed_expr in
    (* TODO benjamin change verbose mode *)
    print_message Verbose_standard (
        "Resolve expression type of \""
        ^ (string_of_parsed_global_expression parsed_model expr)
        ^ "\" as "
        ^ (DiscreteValue.string_of_expression_type expr_type)
    );

    (* Return uniform typed expression and it's type *)
    uniformly_typed_expr, expr_type

(* Resolve and convert literal *)
let resolve_bool_expression_type parsed_model expr =

    (* Get var type of the expression, deduced by the used variables *)
    let expr_var_type = get_parsed_boolean_expression_type parsed_model expr in

    (* If type cannot be resolved (no variable for example, turn to rational *)
    let expr_var_type = (
        if DiscreteValue.is_unknown_number_type expr_var_type then
            DiscreteValue.var_type_rational
        else
            expr_var_type
    ) in
    (* TODO benjamin change verbose mode *)
    print_message Verbose_standard (
        "Literals of expression \""
        ^ (string_of_parsed_boolean_expression parsed_model expr)
        ^ "\" should be uniformized to "
        ^ (DiscreteValue.string_of_var_type expr_var_type)
    );

    (* Uniformize expression by converting literals number to correct type *)
    let uniformly_typed_expr = convert_literal_types_of_parsed_boolean_expression parsed_model expr_var_type expr in

    (* Get expression type *)
    let expr_type = get_parsed_boolean_expression_type_new parsed_model uniformly_typed_expr in
    (* TODO benjamin change verbose mode *)
    print_message Verbose_standard (
        "Resolve expression type of \""
        ^ (string_of_parsed_boolean_expression parsed_model expr)
        ^ "\" as "
        ^ (DiscreteValue.string_of_expression_type expr_type)
    );

    (* Return uniform typed expression and it's type *)
    uniformly_typed_expr, expr_type

(* Resolve and convert implicitly literals *)
let resolve_nonlinear_constraint_type parsed_model expr =

    (* Get var type of the expression, deduced by the used variables *)
    let expr_var_type = get_nonlinear_constraint_type parsed_model expr in

    (* If type cannot be resolved (no variable for example, turn to rational *)
    let expr_var_type =
        if DiscreteValue.is_unknown_number_type expr_var_type then
            DiscreteValue.var_type_rational
        else
            expr_var_type
    in
    (* TODO benjamin change verbose mode *)
    print_message Verbose_standard (
        "Literals of non linear expression \""
        ^ (string_of_parsed_nonlinear_constraint parsed_model expr)
        ^ "\" should be uniformized to "
        ^ (DiscreteValue.string_of_var_type expr_var_type)
    );

    (* Uniformize expression by converting literals number to correct type *)
    let uniformly_typed_nonlinear_constraint = convert_literal_types_of_nonlinear_constraint parsed_model expr_var_type expr in

    (* Get expression type *)
    let expr_type = get_nonlinear_constraint_type_new parsed_model uniformly_typed_nonlinear_constraint in
    (* TODO benjamin change verbose mode *)
    print_message Verbose_standard (
        "Resolve expression type of \""
        ^ (string_of_parsed_nonlinear_constraint parsed_model expr)
        ^ "\" as "
        ^ (DiscreteValue.string_of_expression_type expr_type)
    );

    (* Return uniform typed expression and it's type *)
    uniformly_typed_nonlinear_constraint, expr_type

let resolve_guard_type parsed_model guard =

    let resolved_nonlinear_constraints = List.map (resolve_nonlinear_constraint_type parsed_model) guard in

    let uniformly_typed_nonlinear_constraints = List.map (fun (u, _) -> u) resolved_nonlinear_constraints in
    let nonlinear_constraint_types = List.map (fun (_, t) -> t) resolved_nonlinear_constraints in

    (* TODO benjamin type check of guard *)

    uniformly_typed_nonlinear_constraints, List.hd nonlinear_constraint_types


let check_type_of_nonlinear_constraint parsed_model = function
    (* It's ok non-linear constraint is of boolean type *)
    | Parsed_true_nonlinear_constraint
    | Parsed_false_nonlinear_constraint -> true
    | Parsed_nonlinear_constraint expr ->
        let expr_type = get_parsed_discrete_boolean_expression_type parsed_model expr in
        DiscreteValue.is_bool_type expr_type

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
    let expr_type = get_expression_type parsed_model expr in
    (* Check expression / variable type consistency *)
    let is_consistent = DiscreteValue.is_type_compatibles variable_type expr_type in

    (* Not consistent ? raise a type error with appropriate message*)
    if not (is_consistent) then (
        raise (TypeError (get_error_message variable_name variable_type expr_type expr))
    );;