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
 * Last modified     : 2021/09/20
 *
 ************************************************************)

open ParsingStructure
open ParsingStructureUtilities
open ImitatorUtilities
open OCamlUtilities
open Exceptions

type variable_name = string
type variable_index = int

(** Error messages **)

(* Error message when mixin of different types *)
let type_mixin_error_message str_expr types =
    let str_types_array = Array.map DiscreteType.string_of_var_type_discrete types in
    let str_types = OCamlUtilities.string_of_array_of_string_with_sep ", " str_types_array in
    "Expression `"
    ^ str_expr
    ^ "` mixes different types: "
    ^ str_types
    ^ "."

let not_arithmetic_expr_message str_expr types =
    let str_types_array = Array.map DiscreteType.string_of_var_type_discrete types in
    let str_types = OCamlUtilities.string_of_array_of_string_with_sep ", " str_types_array in
    "The expression `"
    ^ str_expr
    ^ "` is not an arithmetic expression: "
    ^ str_types
    ^ "."

let not_boolean_expr_message str_expr types =
    let str_types_array = Array.map DiscreteType.string_of_var_type_discrete types in
    let str_types = OCamlUtilities.string_of_array_of_string_with_sep ", " str_types_array in
    "The expression `"
    ^ str_expr
    ^ "` is not a boolean expression: "
    ^ str_types
    ^ "."

let infer_expr_as_message str_expr target_type =
    "`"
    ^ str_expr
    ^ "` as "
    ^ DiscreteType.string_of_var_type_discrete target_type
    ^ "."

(* Print expression inference message *)
let print_infer_expr_message str_expr target_type =
    print_message Verbose_high (
        "\tInfer expression "
        ^ infer_expr_as_message str_expr target_type
    )

(* Print inference message *)
let print_infer_message str_expr target_type =
    print_message Verbose_high (
        "\tInfer literals of expression "
        ^ infer_expr_as_message str_expr target_type
    )

(* Print number value inference message *)
let print_infer_literal_number_message value target_type =
    print_message Verbose_high (
        "\tInfer literal number value "
        ^ infer_expr_as_message (DiscreteValue.string_of_value value) target_type
    )

(** Get variables types **)

(* Get var type of a variable given it's index *)
let get_type_of_variable variable_infos variable_index =
    variable_infos.type_of_variables variable_index

(* Get discrete type of a variable given it's index *)
let get_discrete_type_of_variable variable_infos variable_index =
    let var_type = get_type_of_variable variable_infos variable_index in
    DiscreteType.discrete_type_of_var_type var_type

(* Get var type of a variable given it's name *)
let get_type_of_variable_by_name variable_infos variable_name =
    if Hashtbl.mem variable_infos.index_of_variables variable_name then (
        (* Get type of variable *)
        let variable_index = Hashtbl.find variable_infos.index_of_variables variable_name in
        let variable_type = get_type_of_variable variable_infos variable_index in
        variable_type
    ) else if Hashtbl.mem variable_infos.constants variable_name then (
        (* Retrieve the value of the global constant *)
        let value = Hashtbl.find variable_infos.constants variable_name in
        (* Get type of constant *)
        DiscreteValue.var_type_of_value value
    ) else
        raise (InternalError ("Impossible to find the index of variable `" ^ variable_name ^ "` although this should have been checked before."))


(* Get discrete type of a variable given it's name *)
let get_discrete_type_of_variable_by_name variable_infos variable_name =
    let var_type = get_type_of_variable_by_name variable_infos variable_name in
    DiscreteType.discrete_type_of_var_type var_type


(** Conversions of expressions **)


(* Convert literals types of an expression to a given target type *)
let rec convert_literals_of_expression variable_infos target_type = function
    | Parsed_global_expression expr ->
        Parsed_global_expression (convert_literals_of_parsed_boolean_expression variable_infos target_type expr)

(* Convert literals types of a Boolean expression to a given target type *)
and convert_literals_of_parsed_boolean_expression variable_infos target_type = function
    | Parsed_And (l_expr, r_expr) ->
        let convert_l_expr = convert_literals_of_parsed_boolean_expression variable_infos target_type l_expr in
        let convert_r_expr = convert_literals_of_parsed_boolean_expression variable_infos target_type r_expr in
        Parsed_And (convert_l_expr, convert_r_expr)
    | Parsed_Or (l_expr, r_expr) ->
        let convert_l_expr = convert_literals_of_parsed_boolean_expression variable_infos target_type l_expr in
        let convert_r_expr = convert_literals_of_parsed_boolean_expression variable_infos target_type r_expr in
        Parsed_Or (convert_l_expr, convert_r_expr)
    | Parsed_Discrete_boolean_expression expr ->
        Parsed_Discrete_boolean_expression (convert_literals_of_parsed_discrete_boolean_expression variable_infos target_type expr)

(* Convert literals types of a discrete Boolean expression to a given target type *)
and convert_literals_of_parsed_discrete_boolean_expression variable_infos target_type = function
    | Parsed_arithmetic_expression expr ->
        Parsed_arithmetic_expression (convert_literals_of_parsed_discrete_arithmetic_expression variable_infos target_type expr)
    | Parsed_boolean_expression expr ->
        Parsed_boolean_expression (convert_literals_of_parsed_boolean_expression variable_infos target_type expr)
    | Parsed_Not expr ->
        Parsed_Not (convert_literals_of_parsed_boolean_expression variable_infos target_type expr)
    | Parsed_expression (l_expr, relop, r_expr) ->
        let convert_l_expr = convert_literals_of_parsed_discrete_boolean_expression variable_infos target_type l_expr in
        let convert_r_expr = convert_literals_of_parsed_discrete_boolean_expression variable_infos target_type r_expr in
        Parsed_expression (convert_l_expr, relop, convert_r_expr)
    | Parsed_expression_in (expr, lower_expr, upper_expr) ->
        let convert_expr = convert_literals_of_parsed_discrete_arithmetic_expression variable_infos target_type expr in
        let convert_lower = convert_literals_of_parsed_discrete_arithmetic_expression variable_infos target_type lower_expr in
        let convert_upper = convert_literals_of_parsed_discrete_arithmetic_expression variable_infos target_type upper_expr  in
        Parsed_expression_in (convert_expr, convert_lower, convert_upper)

(* Convert literals types of an arithmetic expression to a given target type *)
and convert_literals_of_parsed_discrete_arithmetic_expression variable_infos target_type = function
    | Parsed_DAE_plus (expr, term) ->
        let convert_expr = convert_literals_of_parsed_discrete_arithmetic_expression variable_infos target_type expr in
        let convert_term = convert_literals_of_parsed_discrete_term variable_infos target_type term in
        Parsed_DAE_plus (convert_expr, convert_term)
    | Parsed_DAE_minus (expr, term) ->
        let convert_expr = convert_literals_of_parsed_discrete_arithmetic_expression variable_infos target_type expr in
        let convert_term = convert_literals_of_parsed_discrete_term variable_infos target_type term in
        Parsed_DAE_minus (convert_expr, convert_term)
    | Parsed_DAE_term term ->
        Parsed_DAE_term (convert_literals_of_parsed_discrete_term variable_infos target_type term)

(* Convert literals types of a term to a given target type *)
and convert_literals_of_parsed_discrete_term variable_infos target_type = function
    | Parsed_DT_mul (term, factor) ->
        let convert_term = convert_literals_of_parsed_discrete_term variable_infos target_type term in
        let convert_factor = convert_literals_of_parsed_discrete_factor variable_infos target_type factor in
        Parsed_DT_mul (convert_term, convert_factor)
    | Parsed_DT_div (term, factor) ->
        let convert_term = convert_literals_of_parsed_discrete_term variable_infos target_type term in
        let convert_factor = convert_literals_of_parsed_discrete_factor variable_infos target_type factor in
        Parsed_DT_div (convert_term, convert_factor)
    | Parsed_DT_factor factor ->
        Parsed_DT_factor (convert_literals_of_parsed_discrete_factor variable_infos target_type factor)

(* Convert literals types of a factor to a given target type *)
and convert_literals_of_parsed_discrete_factor variable_infos target_type = function
    | Parsed_DF_variable _ as variable -> variable
    | Parsed_DF_constant var_value as constant ->
        let discrete_type = DiscreteValue.discrete_type_of_value var_value in

        (* for example a * pow(a, 2) with a rational, reconvert 2 to rational, it's bad, check if there is another way to avoid it! *)
        if DiscreteType.is_discrete_type_unknown_number_type discrete_type then (
            print_infer_literal_number_message var_value target_type;
            Parsed_DF_constant (DiscreteValue.convert_value_to_discrete_type var_value target_type)
        ) else
            constant

    | Parsed_DF_array expr_array ->
            (* Convert array elements to the inner type of the array target type *)
            let converted_array = Array.map (convert_literals_of_parsed_boolean_expression variable_infos target_type) expr_array in
            Parsed_DF_array converted_array
    | Parsed_DF_list expr_list ->
            (* Convert array elements to the inner type of the array target type *)
            let converted_list = List.map (convert_literals_of_parsed_boolean_expression variable_infos target_type) expr_list in
            Parsed_DF_list converted_list
    | Parsed_DF_access (factor, index_expr) ->
        Parsed_DF_access (
            convert_literals_of_parsed_discrete_factor variable_infos target_type factor,
            index_expr
        )
    | Parsed_DF_expression expr ->
        Parsed_DF_expression (convert_literals_of_parsed_discrete_arithmetic_expression variable_infos target_type expr)
    | Parsed_rational_of_int_function expr ->
        (* As it was already type checked, we can convert inner expression literal numbers of the function to int *)
        let inner_target_type = DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_int in
        Parsed_rational_of_int_function (convert_literals_of_parsed_discrete_arithmetic_expression variable_infos inner_target_type expr)
    | Parsed_pow_function (expr, exp_expr) ->
        Parsed_pow_function (
            convert_literals_of_parsed_discrete_arithmetic_expression variable_infos target_type expr,
            convert_literals_of_parsed_discrete_arithmetic_expression variable_infos target_type exp_expr
        )
    | Parsed_shift_function (fun_type, factor, expr) ->
        Parsed_shift_function (
            fun_type,
            convert_literals_of_parsed_discrete_factor variable_infos target_type factor,
            convert_literals_of_parsed_discrete_arithmetic_expression variable_infos target_type expr
        )
    | Parsed_bin_log_function (fun_type, l_factor, r_factor) ->
        Parsed_bin_log_function (
            fun_type,
            convert_literals_of_parsed_discrete_factor variable_infos target_type l_factor,
            convert_literals_of_parsed_discrete_factor variable_infos target_type r_factor
        )
    | Parsed_log_not factor ->
        Parsed_log_not
            (convert_literals_of_parsed_discrete_factor variable_infos target_type factor)
    | Parsed_array_concat (l_factor, r_factor) ->
        Parsed_array_concat (
            convert_literals_of_parsed_discrete_factor variable_infos target_type l_factor,
            convert_literals_of_parsed_discrete_factor variable_infos target_type r_factor
        )
    | Parsed_list_cons (expr, factor) ->
        Parsed_list_cons (
            convert_literals_of_parsed_boolean_expression variable_infos target_type expr,
            convert_literals_of_parsed_discrete_factor variable_infos target_type factor
        )
    | Parsed_DF_unary_min factor ->
        Parsed_DF_unary_min (convert_literals_of_parsed_discrete_factor variable_infos target_type factor)

(* Convert literals types of a non-linear constraint to a given target type *)
let convert_literals_of_nonlinear_constraint variable_infos target_type = function
    (* It's ok non-linear constraint is of Boolean type *)
    | Parsed_nonlinear_constraint expr ->
        Parsed_nonlinear_constraint (convert_literals_of_parsed_discrete_boolean_expression variable_infos target_type expr)

(** Type checking and inference **)

(* Type checking error types that can arise on arithmetic expression *)
type number_type_compatibility_error =
    | No_number_error
    | Number_type_mixin_error
    | Both_unknown_number_error
    | Left_unknown_number_error
    | Right_unknown_number_error

(* Check number types compatibility and return convenient error type *)
let check_number_type_compatibility l_type r_type =
    if not (DiscreteType.is_discrete_type_holding_number_type l_type && DiscreteType.is_discrete_type_holding_number_type r_type) then
        No_number_error
    else if DiscreteType.is_discrete_type_holding_known_number_type l_type && DiscreteType.is_discrete_type_holding_known_number_type r_type && l_type <> r_type then
        Number_type_mixin_error
    else if DiscreteType.is_discrete_type_holding_unknown_number_type l_type && DiscreteType.is_discrete_type_holding_unknown_number_type r_type then
        Both_unknown_number_error
    else if DiscreteType.is_discrete_type_holding_unknown_number_type l_type then
        Left_unknown_number_error
    else
        Right_unknown_number_error


(* Type check and infer literal numbers of global expression *)
let rec infer_expression variable_infos = function
    | Parsed_global_expression expr ->
        let convert_expr, discrete_type = infer_parsed_boolean_expression variable_infos expr in
        Parsed_global_expression convert_expr, discrete_type

(* Type check and infer literal numbers of Boolean expression *)
and infer_parsed_boolean_expression variable_infos = function

    | Parsed_And (l_expr, r_expr) as expr ->
        let (convert_l_expr, convert_r_expr), discrete_type = type_check_bool_operation variable_infos l_expr r_expr expr in
        Parsed_And (convert_l_expr, convert_r_expr), discrete_type

    | Parsed_Or (l_expr, r_expr) as expr ->
        let (convert_l_expr, convert_r_expr), discrete_type = type_check_bool_operation variable_infos l_expr r_expr expr in
        Parsed_Or (convert_l_expr, convert_r_expr), discrete_type

    | Parsed_Discrete_boolean_expression expr ->
        let infer_expr, discrete_type = infer_parsed_discrete_boolean_expression variable_infos expr in
        Parsed_Discrete_boolean_expression infer_expr, discrete_type

(* Type check and return inferred bool components *)
and type_check_bool_operation variable_infos l_expr r_expr expr =
        (* Infer left / right expressions *)
        let infer_l_expr, l_type = infer_parsed_boolean_expression variable_infos l_expr in
        let infer_r_expr, r_type = infer_parsed_boolean_expression variable_infos r_expr in

        (* Check if two types are bool *)
        if not (DiscreteType.is_discrete_type_bool_type l_type && DiscreteType.is_discrete_type_bool_type r_type) then
            raise (TypeError (not_boolean_expr_message (string_of_parsed_boolean_expression variable_infos expr) [|l_type; r_type|]))
        else
            (infer_l_expr, infer_r_expr), DiscreteType.Var_type_discrete_bool

(* Type checking and infer literal numbers of discrete Boolean expression *)
and infer_parsed_discrete_boolean_expression variable_infos = function
    | Parsed_arithmetic_expression expr ->
        let infer_expr, discrete_type = infer_parsed_discrete_arithmetic_expression variable_infos expr in
        Parsed_arithmetic_expression (infer_expr), discrete_type

    | Parsed_boolean_expression expr ->
        let infer_expr, discrete_type = infer_parsed_boolean_expression variable_infos expr in
        Parsed_boolean_expression (infer_expr), discrete_type

    | Parsed_Not expr as not_expr ->
        (* Infer inner expression *)
        let infer_expr, discrete_type = infer_parsed_boolean_expression variable_infos expr in

        (* Check if 'not' contains a bool expression *)
        if not (DiscreteType.is_discrete_type_bool_type discrete_type) then
            raise (TypeError (not_boolean_expr_message (string_of_parsed_discrete_boolean_expression variable_infos not_expr) [|discrete_type|]))
        else
            Parsed_Not (infer_expr), discrete_type

    | Parsed_expression (l_expr, relop, r_expr) as expr ->

        (* Infer left / right expressions *)
        let infer_l_expr, l_type = infer_parsed_discrete_boolean_expression variable_infos l_expr in
        let infer_r_expr, r_type = infer_parsed_discrete_boolean_expression variable_infos r_expr in

        (* Prepare inference message *)
        let get_infer_expr_message = print_infer_message (string_of_parsed_discrete_boolean_expression variable_infos expr) in
        (* Print expression inference message *)
        print_infer_expr_message (string_of_parsed_discrete_boolean_expression variable_infos expr) DiscreteType.Var_type_discrete_bool;

        if not (DiscreteType.is_discrete_type_compatibles l_type r_type) then
            raise (TypeError (type_mixin_error_message (string_of_parsed_discrete_boolean_expression variable_infos expr) [|l_type; r_type|]))
        else (

            let convert_l_expr, convert_r_expr = (

                match check_number_type_compatibility l_type r_type with

                | Both_unknown_number_error ->
                    (* No number type are deduced from tree, because there is only literal numbers *)
                    (* So at this point, we convert all literals to rationals *)
                    let target_type = DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_rational in
                    get_infer_expr_message target_type;

                    (* Convert left / right expression to rational *)
                    let convert_l_expr = convert_literals_of_parsed_discrete_boolean_expression variable_infos target_type infer_l_expr in
                    let convert_r_expr = convert_literals_of_parsed_discrete_boolean_expression variable_infos target_type infer_r_expr in

                    convert_l_expr, convert_r_expr

                | Left_unknown_number_error ->
                    (* Convert all literals of left expression to inner type of right expression *)
                    let target_type = DiscreteType.extract_inner_type r_type in
                    get_infer_expr_message r_type;

                    (* Convert left expression to right expression type *)
                    let convert_l_expr = convert_literals_of_parsed_discrete_boolean_expression variable_infos target_type infer_l_expr in
                    convert_l_expr, infer_r_expr

                | Right_unknown_number_error ->
                    (* Convert all literals of right expression to inner type of left expression *)
                    let target_type = DiscreteType.extract_inner_type l_type in
                    get_infer_expr_message l_type;

                    (* Convert right expression to left expression type *)
                    let convert_r_expr = convert_literals_of_parsed_discrete_boolean_expression variable_infos target_type infer_r_expr in
                    infer_l_expr, convert_r_expr

                | No_number_error
                | Number_type_mixin_error ->
                    infer_l_expr, infer_r_expr
            )
            in
            Parsed_expression (convert_l_expr, relop, convert_r_expr), DiscreteType.Var_type_discrete_bool
        )




    | Parsed_expression_in (expr, lower_expr, upper_expr) as in_expr ->

        let infer_expr, expr_type = infer_parsed_discrete_arithmetic_expression variable_infos expr in
        let infer_lower_expr, lower_type = infer_parsed_discrete_arithmetic_expression variable_infos lower_expr in
        let infer_upper_expr, upper_type = infer_parsed_discrete_arithmetic_expression variable_infos upper_expr in

        (* Prepare checkings for readability *)
        let all_types = [expr_type; lower_type; upper_type] in
        let types_combination = list_combination all_types all_types in

        let is_all_number = lazy (List.for_all (fun t -> DiscreteType.is_discrete_type_number_type t) all_types) in
        let is_all_unknown_number = lazy (List.for_all (fun t -> DiscreteType.is_discrete_type_unknown_number_type t) all_types) in
        let is_type_conflict = lazy (List.exists (fun (t1, t2) -> not (DiscreteType.is_discrete_type_unknown_number_type t1 || DiscreteType.is_discrete_type_unknown_number_type t2) && t1 <> t2) types_combination) in

        (* One of theses are not a number *)
        if not (Lazy.force is_all_number) then
            raise (TypeError (not_arithmetic_expr_message (string_of_parsed_discrete_boolean_expression variable_infos in_expr) [|expr_type; lower_type; upper_type|]))

        (* Types are differents *)
        else if Lazy.force is_type_conflict then
            raise (TypeError (type_mixin_error_message (string_of_parsed_discrete_boolean_expression variable_infos in_expr) [|expr_type; lower_type; upper_type|]))

        (* All are unknown numbers *)
        else if (Lazy.force is_all_unknown_number) then (
            (* No number type are deduced from tree, because there is only literal numbers *)
            (* So at this point, we convert all literals to rationals *)
            let target_type = DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_rational in

            print_infer_message (string_of_parsed_discrete_boolean_expression variable_infos in_expr) target_type;

            let convert_expr = convert_literals_of_parsed_discrete_arithmetic_expression variable_infos target_type infer_expr in
            let convert_lower_expr = convert_literals_of_parsed_discrete_arithmetic_expression variable_infos target_type infer_lower_expr in
            let convert_upper_expr = convert_literals_of_parsed_discrete_arithmetic_expression variable_infos target_type infer_upper_expr in

            Parsed_expression_in (convert_expr, convert_lower_expr, convert_upper_expr), DiscreteType.Var_type_discrete_bool
        )
        else if not (DiscreteType.is_discrete_type_unknown_number_type expr_type) then (

            print_infer_message (string_of_parsed_discrete_boolean_expression variable_infos in_expr) expr_type;

            (* Convert lower and upper expression to expr_type *)
            let convert_lower_expr = convert_literals_of_parsed_discrete_arithmetic_expression variable_infos expr_type infer_lower_expr in
            let convert_upper_expr = convert_literals_of_parsed_discrete_arithmetic_expression variable_infos expr_type infer_upper_expr in

            Parsed_expression_in (infer_expr, convert_lower_expr, convert_upper_expr), DiscreteType.Var_type_discrete_bool
        )
        else if not (DiscreteType.is_discrete_type_unknown_number_type lower_type) then (

            print_infer_message (string_of_parsed_discrete_boolean_expression variable_infos in_expr) lower_type;

            (* Convert expr and upper expression to lower_type *)
            let convert_expr = convert_literals_of_parsed_discrete_arithmetic_expression variable_infos lower_type infer_expr in
            let convert_upper_expr = convert_literals_of_parsed_discrete_arithmetic_expression variable_infos lower_type infer_upper_expr in

            Parsed_expression_in (convert_expr, infer_lower_expr, convert_upper_expr), DiscreteType.Var_type_discrete_bool
        )
        else (

            print_infer_message (string_of_parsed_discrete_boolean_expression variable_infos in_expr) upper_type;

            (* Convert expr and lower expression to upper_type *)
            let convert_expr = convert_literals_of_parsed_discrete_arithmetic_expression variable_infos upper_type infer_expr in
            let convert_lower_expr = convert_literals_of_parsed_discrete_arithmetic_expression variable_infos upper_type infer_lower_expr in

            Parsed_expression_in (convert_expr, convert_lower_expr, infer_upper_expr), DiscreteType.Var_type_discrete_bool
        )


(* Type checking and infer literal numbers of arithmetic expression *)
and infer_parsed_discrete_arithmetic_expression variable_infos =

    let rec inner_infer_parsed_discrete_arithmetic_expression = function
        | Parsed_DAE_plus (expr, term) as arithmetic_expr ->
            let (convert_l_expr, convert_r_expr), discrete_type = type_check_and_convert_arithmetic_expression variable_infos expr term arithmetic_expr in
            Parsed_DAE_plus (convert_l_expr, convert_r_expr), discrete_type

        | Parsed_DAE_minus (expr, term) as arithmetic_expr ->
            let (convert_l_expr, convert_r_expr), discrete_type = type_check_and_convert_arithmetic_expression variable_infos expr term arithmetic_expr in
            Parsed_DAE_minus (convert_l_expr, convert_r_expr), discrete_type

        | Parsed_DAE_term term ->
            let infer_term, discrete_type = infer_parsed_discrete_term variable_infos term in
            Parsed_DAE_term infer_term, discrete_type

    and type_check_and_convert_arithmetic_expression variable_infos expr term arithmetic_expr =
            let infer_expr, l_type = infer_parsed_discrete_arithmetic_expression variable_infos expr in
            let infer_term, r_type = infer_parsed_discrete_term variable_infos term in

            let error_type = check_number_type_compatibility l_type r_type in
            match error_type with
            | No_number_error ->
                raise (TypeError (not_arithmetic_expr_message (string_of_parsed_arithmetic_expression variable_infos arithmetic_expr) [|l_type; r_type|]))
            | Number_type_mixin_error ->
                raise (TypeError (type_mixin_error_message (string_of_parsed_arithmetic_expression variable_infos arithmetic_expr) [|l_type; r_type|]))
            | Both_unknown_number_error ->
                (infer_expr, infer_term), DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_unknown_number
            | Left_unknown_number_error ->
                (* Convert *)
                let convert_expr = convert_literals_of_parsed_discrete_arithmetic_expression variable_infos r_type infer_expr in
                (convert_expr, infer_term), r_type
            | Right_unknown_number_error ->
                (* Convert *)
                let convert_term = convert_literals_of_parsed_discrete_term variable_infos l_type infer_term in
                (infer_expr, convert_term), l_type

    in
    inner_infer_parsed_discrete_arithmetic_expression

(* Type checking and infer literal numbers of term *)
and infer_parsed_discrete_term variable_infos = function
    | Parsed_DT_mul (term, factor) as expr_term ->
        let (convert_l_expr, convert_r_expr), discrete_type = type_check_and_convert_term variable_infos term factor expr_term in

        Parsed_DT_mul (convert_l_expr, convert_r_expr), discrete_type

    (* Specific case, literal rational => constant / constant *)
    (* Should be reduced before... *)
    | Parsed_DT_div ((Parsed_DT_factor (Parsed_DF_constant lv)), Parsed_DF_constant rv) ->

        (* Doing division *)
        let l_numconst = DiscreteValue.to_numconst_value lv in
        let r_numconst = DiscreteValue.to_numconst_value rv in
        let numconst_value = NumConst.div l_numconst r_numconst in

        (* Check if result is representable by an int *)
        let is_int = NumConst.is_int numconst_value in

        (* If it's representable by an int, it can be a rational or an int *)
        if is_int then (
            let discrete_value = DiscreteValue.Number_value numconst_value in
            Parsed_DT_factor (Parsed_DF_constant discrete_value), DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_unknown_number
        )
        (* If it's not representable by an int, it's a rational *)
        else (
            let discrete_value = DiscreteValue.Rational_value numconst_value in
            Parsed_DT_factor (Parsed_DF_constant discrete_value), DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_rational
        )

    | Parsed_DT_div (term, factor) as expr_term ->
        let (convert_l_expr, convert_r_expr), discrete_type = type_check_and_convert_term variable_infos term factor expr_term in
        Parsed_DT_div (convert_l_expr, convert_r_expr), discrete_type

    | Parsed_DT_factor factor ->
        let infer_factor, factor_type = infer_parsed_discrete_factor variable_infos factor in
        Parsed_DT_factor infer_factor, factor_type

and type_check_and_convert_term variable_infos term factor expr_term =
        let infer_term, l_type = infer_parsed_discrete_term variable_infos term in
        let infer_factor, r_type = infer_parsed_discrete_factor variable_infos factor in

        let error_type = check_number_type_compatibility l_type r_type in
        match error_type with
        | No_number_error ->
            raise (TypeError (not_arithmetic_expr_message (string_of_parsed_term variable_infos expr_term) [|l_type; r_type|]))
        | Number_type_mixin_error ->
            raise (TypeError (type_mixin_error_message (string_of_parsed_term variable_infos expr_term) [|l_type; r_type|]))
        | Both_unknown_number_error ->
            (infer_term, infer_factor), DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_unknown_number
        | Left_unknown_number_error ->
            (* Convert *)
            let convert_term = convert_literals_of_parsed_discrete_term variable_infos r_type infer_term in
            (convert_term, infer_factor), r_type
        | Right_unknown_number_error ->
            (* Convert *)
            let convert_factor = convert_literals_of_parsed_discrete_factor variable_infos l_type infer_factor in
            (infer_term, convert_factor), l_type

(* Type checking and infer literal numbers of factor *)
and infer_parsed_discrete_factor variable_infos = function
    | Parsed_DF_variable variable_name ->
        (* Get discrete type of variable *)
        let discrete_type = get_discrete_type_of_variable_by_name variable_infos variable_name in
        Parsed_DF_variable variable_name, discrete_type

    | Parsed_DF_constant var_value ->
        let discrete_type = DiscreteValue.discrete_type_of_value var_value in
        Parsed_DF_constant var_value, discrete_type

    | Parsed_DF_array expr_array as df_array ->

        (* Infer type of each elements of array and convert to a list *)
        let infer_expr_array = Array.map (infer_parsed_boolean_expression variable_infos) expr_array in
        let infer_expr_list = Array.to_list infer_expr_array in

        (* Check if there is any number in array that is known type (or type that holding known type) *)
        let known_number = List.filter (fun (_, discrete_type) -> DiscreteType.is_discrete_type_holding_known_number_type discrete_type) infer_expr_list in
        (* Check if there is any number in array that is unknown type (or type that holding unknown type) *)
(*        let unknown_number = List.filter (fun (_, discrete_type) -> DiscreteType.is_discrete_type_holding_unknown_number_type discrete_type) infer_expr_list in*)

        (* Infer type of array *)
        let infer_expr_array =
            if List.length known_number > 0 then (
                let first_known_number_expr, target_type = List.nth known_number 0 in

                Array.map (fun (expr, discrete_type) ->
                    if DiscreteType.is_discrete_type_holding_unknown_number_type discrete_type then (
                        let inner_target_type = DiscreteType.extract_inner_type target_type in
                        convert_literals_of_parsed_boolean_expression variable_infos inner_target_type expr, target_type
                    )
                    else
                        expr, discrete_type
                ) infer_expr_array
            )
            else
                infer_expr_array
        in

        (*  *)
        let discrete_types = Array.map (fun (_, discrete_type) -> discrete_type) infer_expr_array in
        let converted_expr_array = Array.map (fun (converted_expr, _) -> converted_expr) infer_expr_array in

        (* Check if all elements in array had the same types *)
        let first_type =
            if Array.length discrete_types > 0 then
                Array.get discrete_types 0
            else
                DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_unknown_number
        in

        let all_same = Array.for_all (fun discrete_type -> discrete_type = first_type) discrete_types in

        (* If not all the same, type error ! *)
        if not all_same then (
            raise (TypeError (type_mixin_error_message (string_of_parsed_factor variable_infos df_array) discrete_types))

        (* If all the same type, just set infer type to array of 'a *)
        ) else (
            let infer_type = DiscreteType.Var_type_discrete_array (first_type, Array.length expr_array) in

            print_infer_expr_message (string_of_parsed_factor variable_infos df_array) infer_type;

            Parsed_DF_array converted_expr_array, infer_type
        )

    | Parsed_DF_list expr_list as df_list ->

        (* Infer type of each elements of list and convert to a list *)
        let infer_expr_list = List.map (infer_parsed_boolean_expression variable_infos) expr_list in

        (* Check if there is any number in list that is known type (or type that holding known type) *)
        let known_number = List.filter (fun (_, discrete_type) -> DiscreteType.is_discrete_type_holding_known_number_type discrete_type) infer_expr_list in

        (* Infer type of list *)
        let infer_expr_list =
            if List.length known_number > 0 then (
                let first_known_number_expr, target_type = List.nth known_number 0 in
                List.map (fun (expr, discrete_type) ->
                    if DiscreteType.is_discrete_type_holding_unknown_number_type discrete_type then (
                        let inner_target_type = DiscreteType.extract_inner_type target_type in
                        convert_literals_of_parsed_boolean_expression variable_infos inner_target_type expr, target_type
                    )
                    else
                        expr, discrete_type
                ) infer_expr_list
            )
            else
                infer_expr_list
        in

        (*  *)
        let discrete_types = List.map (fun (_, discrete_type) -> discrete_type) infer_expr_list in
        let discrete_types_array = Array.of_list discrete_types in
        let converted_expr_list = List.map (fun (converted_expr, _) -> converted_expr) infer_expr_list in

        (* Check if all elements in array had the same types *)
        (* TODO benjamin REFACTOR use DiscreteValue.default_type *)
        let first_type = if List.length discrete_types > 0 then List.nth discrete_types 0 else (DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_rational) in
        let all_same = List.for_all (fun discrete_type -> discrete_type = first_type) discrete_types in

        (* If not all the same, type error ! *)
        if not all_same then (
            raise (TypeError (type_mixin_error_message (string_of_parsed_factor variable_infos df_list) discrete_types_array))

        (* If all the same type, just set infer type to array of 'a *)
        ) else (
            let infer_type = DiscreteType.Var_type_discrete_list first_type in

            print_infer_expr_message (string_of_parsed_factor variable_infos df_list) infer_type;

            Parsed_DF_list converted_expr_list, infer_type
        )

    | Parsed_DF_access (factor, index_expr) as access ->

        let infer_factor, discrete_type = infer_parsed_discrete_factor variable_infos factor in
        let infer_index_expr, index_type = infer_parsed_discrete_arithmetic_expression variable_infos index_expr in

        (* If type is not an array or a list, access is used in wrong context ! *)
        let infer_type = (
            match discrete_type with
            | DiscreteType.Var_type_discrete_array (inner_type, _)
            | DiscreteType.Var_type_discrete_list inner_type -> inner_type
            | _ -> raise (TypeError (
                "Trying to make an access to a non array or non list variable at `"
                ^ ParsingStructureUtilities.string_of_parsed_factor variable_infos access
                ^ "`"
            ))
        )
        in

        let convert_index_expr =
            if DiscreteType.is_discrete_type_unknown_number_type index_type then (
                let index_target_type = DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_int in
                convert_literals_of_parsed_discrete_arithmetic_expression variable_infos index_target_type infer_index_expr
            )
            else if DiscreteType.is_discrete_type_int_type index_type then
                infer_index_expr
            else
                raise (TypeError (
                    "Index should be a int expression at `"
                    ^ ParsingStructureUtilities.string_of_parsed_factor variable_infos access
                    ^ "`, `"
                    ^ ParsingStructureUtilities.string_of_parsed_arithmetic_expression variable_infos infer_index_expr
                    ^ "` is not a int expression: "
                    ^ DiscreteType.string_of_var_type_discrete index_type
                ))
        in

        Parsed_DF_access (infer_factor, convert_index_expr), infer_type

    | Parsed_DF_expression expr ->
        let infer_expr, expr_type = infer_parsed_discrete_arithmetic_expression variable_infos expr in
        Parsed_DF_expression infer_expr, expr_type

    | Parsed_rational_of_int_function expr as int_expr ->
        let infer_expr, expr_type = infer_parsed_discrete_arithmetic_expression variable_infos expr in

        (* Check that expr type is a int type *)
        if not (DiscreteType.is_discrete_type_int_type expr_type || DiscreteType.is_discrete_type_unknown_number_type expr_type) then
            raise (TypeError (
                "The expression `"
                ^ string_of_parsed_arithmetic_expression variable_infos expr
                ^ "` contained in `"
                ^ string_of_parsed_factor variable_infos int_expr
                ^ "` expression, is not of type int: "
                ^ DiscreteType.string_of_var_type_discrete expr_type
            ))
        else (
            (* Set target type to int *)
            let target_type = DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_int in

            print_infer_expr_message (string_of_parsed_factor variable_infos int_expr) (DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_rational);

            print_infer_message (string_of_parsed_factor variable_infos int_expr) target_type;

            (* Convert all literal of the expression to int *)
            let convert_expr = convert_literals_of_parsed_discrete_arithmetic_expression variable_infos target_type expr in

            (* Return converted expression and it's type *)
            Parsed_rational_of_int_function convert_expr, DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_rational
        )

    | Parsed_pow_function (expr, exp) as pow_expr ->
        let infer_expr, l_type = infer_parsed_discrete_arithmetic_expression variable_infos expr in
        let infer_exp, r_type = infer_parsed_discrete_arithmetic_expression variable_infos exp in

        (* Check that two expression are arithmetic *)
        if not (DiscreteType.is_discrete_type_number_type l_type && DiscreteType.is_discrete_type_number_type r_type) then (
            raise (TypeError (not_arithmetic_expr_message (string_of_parsed_factor variable_infos pow_expr) [|l_type; r_type|]))
        )
        (* Check that right expression (exponent) is int, otherwise raise an error *)
        else if not (DiscreteType.is_discrete_type_unknown_number_type r_type || DiscreteType.is_discrete_type_int_type r_type) then (
            raise (TypeError (
                    "Exponent of expression `"
                    ^ ParsingStructureUtilities.string_of_parsed_factor variable_infos pow_expr
                    ^ "` is not an integer"
                )
            );
        );

        (* If right expression unknown convert auto to int *)
        let converted_exp = if DiscreteType.is_discrete_type_unknown_number_type r_type then (
            (* convert *)
            print_message Verbose_high "\tInfer exponent as int";
            let target_type = DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_int in
            convert_literals_of_parsed_discrete_arithmetic_expression variable_infos target_type exp
        ) else
            infer_exp
        in

        let converted_expr, result_type = infer_expr, l_type in

        print_infer_expr_message (string_of_parsed_factor variable_infos pow_expr) result_type;

        (* Return converted expression and it's type *)
        Parsed_pow_function (converted_expr, converted_exp), result_type


    | Parsed_shift_function (fun_type, factor, expr) as shift ->

        let infer_factor, l_type = infer_parsed_discrete_factor variable_infos factor in
        let infer_expr, r_type = infer_parsed_discrete_arithmetic_expression variable_infos expr in

        (* factor should be binary word *)
        if not (DiscreteType.is_discrete_type_binary_word_type l_type) then (
            raise (TypeError (
                "Left member of expression `"
                ^ ParsingStructureUtilities.string_of_parsed_factor variable_infos shift
                ^ "` is not a binary word"
            ))
        );

        (* expr should be int *)
        if not (DiscreteType.is_discrete_type_unknown_number_type r_type || DiscreteType.is_discrete_type_int_type r_type) then (
            raise (TypeError (
                "Right member of expression `"
                ^ ParsingStructureUtilities.string_of_parsed_factor variable_infos shift
                ^ "` is not an int expression"
            ))
        );

        (* If right expression unknown convert auto to int *)
        let converted_expr = if DiscreteType.is_discrete_type_unknown_number_type r_type then (
            (* convert *)
            let target_type = DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_int in

            print_infer_expr_message (string_of_parsed_arithmetic_expression variable_infos expr) target_type;

            convert_literals_of_parsed_discrete_arithmetic_expression variable_infos target_type expr
        ) else
            infer_expr
        in



        let typed_shift =
        (match fun_type with
        | Parsed_shift_left
        | Parsed_shift_right ->
            Parsed_shift_function (fun_type, infer_factor, converted_expr), l_type

        | Parsed_fill_left
        | Parsed_fill_right ->

            (* As we have to deduce length of binary word according to it's shifting value, *)
            (* only constant expression can be use as value of shifting *)

            if not (ParsingStructureUtilities.is_parsed_arithmetic_expression_constant variable_infos converted_expr) then
                raise (TypeError (
                    "Shift parameter of "
                    ^ label_of_parsed_factor_constructor shift
                    ^ " function should be a constant expression."
                    ^ " Expression `"
                    ^ ParsingStructureUtilities.string_of_parsed_arithmetic_expression variable_infos converted_expr
                    ^ "` is not constant."
                ));

            let shift_value = ParsingStructureUtilities.try_reduce_parsed_arithmetic_expression variable_infos.constants converted_expr in
            let base_length = (match l_type with DiscreteType.Var_type_discrete_binary_word l -> l | _ -> raise (InternalError "never happen")) in
            let length = base_length + Int32.to_int (DiscreteValue.int_value shift_value) in
            Parsed_shift_function (fun_type, infer_factor, converted_expr), DiscreteType.Var_type_discrete_binary_word length

        )
        in

        let _, t = typed_shift in

        print_infer_expr_message (string_of_parsed_factor variable_infos shift) t;

        typed_shift

    | Parsed_bin_log_function (fun_type, l_factor, r_factor) as log_op ->

        let infer_l_factor, l_type = infer_parsed_discrete_factor variable_infos l_factor in
        let infer_r_factor, r_type = infer_parsed_discrete_factor variable_infos r_factor in

        (* factors should be binary words of the same length *)
        if not (DiscreteType.is_discrete_type_binary_word_type l_type && DiscreteType.is_discrete_type_binary_word_type r_type) then (
            raise (TypeError (
                "Left or right member of expression `"
                ^ ParsingStructureUtilities.string_of_parsed_factor variable_infos log_op
                ^ "` is not a binary word: "
                ^ DiscreteType.string_of_var_type_discrete l_type
                ^ ", "
                ^ DiscreteType.string_of_var_type_discrete r_type
            ))
        );

        if l_type <> r_type then (
            raise (TypeError (type_mixin_error_message (string_of_parsed_factor variable_infos log_op) [|l_type; r_type|]))
        );

        print_infer_expr_message (string_of_parsed_factor variable_infos log_op) l_type;
        Parsed_bin_log_function (fun_type, infer_l_factor, infer_r_factor), l_type

    | Parsed_log_not factor as log_op ->
        let infer_factor, discrete_type = infer_parsed_discrete_factor variable_infos factor in

        (* factors should be binary words of the same length *)
        if not (DiscreteType.is_discrete_type_binary_word_type discrete_type) then (
            raise (TypeError (
                "Member of expression `"
                ^ ParsingStructureUtilities.string_of_parsed_factor variable_infos log_op
                ^ "` is not a binary word: "
                ^ DiscreteType.string_of_var_type_discrete discrete_type
            ))
        ) else (
            Parsed_log_not infer_factor, discrete_type
        )
    | Parsed_array_concat (factor_0, factor_1) as func ->

        let infer_factor_0, discrete_type_0 = infer_parsed_discrete_factor variable_infos factor_0 in
        let infer_factor_1, discrete_type_1 = infer_parsed_discrete_factor variable_infos factor_1 in

        (* Check that the two factor are arrays *)
        (
        match discrete_type_0, discrete_type_1 with
        | DiscreteType.Var_type_discrete_array (inner_type_0, length_0),  DiscreteType.Var_type_discrete_array (inner_type_1, length_1) ->

            if not (DiscreteType.is_discrete_type_compatibles inner_type_0 inner_type_1) then
                raise (TypeError (type_mixin_error_message (string_of_parsed_factor variable_infos func) [|discrete_type_0; discrete_type_1|]))
            else (

                let convert_factor_0, convert_factor_1, convert_type = (
                    match check_number_type_compatibility inner_type_0 inner_type_1 with
                    | No_number_error
                    | Both_unknown_number_error ->
                        infer_factor_0, infer_factor_1, inner_type_0
                    | Left_unknown_number_error ->
                        convert_literals_of_parsed_discrete_factor variable_infos inner_type_1 infer_factor_0,
                        infer_factor_1,
                        inner_type_1
                    | Right_unknown_number_error ->
                        infer_factor_0,
                        convert_literals_of_parsed_discrete_factor variable_infos inner_type_0 infer_factor_1,
                        inner_type_0
                    | Number_type_mixin_error ->
                        raise (InternalError "Compatibility type should be checked previously")
                )
                in

                let array_type = DiscreteType.Var_type_discrete_array (convert_type, length_0 + length_1) in
                print_infer_expr_message (string_of_parsed_factor variable_infos func) array_type;
                Parsed_array_concat (convert_factor_0, convert_factor_1), array_type

            )

        | _ ->
            raise (TypeError (
                "Left or right member of expression `"
                ^ ParsingStructureUtilities.string_of_parsed_factor variable_infos func
                ^ "` is not an array: "
                ^ DiscreteType.string_of_var_type_discrete discrete_type_0
                ^ ", "
                ^ DiscreteType.string_of_var_type_discrete discrete_type_1
            ))
        )

    | Parsed_list_cons (expr, factor) as func ->

        let infer_expr, expr_type = infer_parsed_boolean_expression variable_infos expr in
        let infer_factor, factor_type = infer_parsed_discrete_factor variable_infos factor in

        (* Factor must be a list *)
        (match factor_type with
        | DiscreteType.Var_type_discrete_list inner_type ->
            (* Check that list elements are the same type of expr *)
            let convert_expr, convert_factor, convert_type =
                match check_number_type_compatibility inner_type expr_type with
                | No_number_error
                | Both_unknown_number_error ->
                    infer_expr, infer_factor, expr_type
                | Left_unknown_number_error ->
                    convert_literals_of_parsed_boolean_expression variable_infos inner_type infer_expr,
                    infer_factor,
                    inner_type
                | Right_unknown_number_error ->
                    infer_expr,
                    convert_literals_of_parsed_discrete_factor variable_infos expr_type infer_factor,
                    expr_type
                | Number_type_mixin_error ->
                    raise (InternalError "Compatibility type should be checked previously")
            in
            print_infer_expr_message (string_of_parsed_factor variable_infos func) convert_type;
            Parsed_list_cons (convert_expr, convert_factor), DiscreteType.Var_type_discrete_list convert_type

        | _ ->
            raise (TypeError (
                "Left or right member of expression `"
                ^ ParsingStructureUtilities.string_of_parsed_factor variable_infos func
                ^ "` is not a list: "
                ^ DiscreteType.string_of_var_type_discrete expr_type
                ^ ", "
                ^ DiscreteType.string_of_var_type_discrete factor_type
            ))
        )

    | Parsed_DF_unary_min factor ->
        let infer_factor, factor_type = infer_parsed_discrete_factor variable_infos factor in
        Parsed_DF_unary_min infer_factor, factor_type

(* Type checking and infer literal numbers of non-linear constraint *)
and infer_nonlinear_constraint variable_infos = function
    (* It's ok non-linear constraint is of Boolean type *)
    | Parsed_nonlinear_constraint expr ->
        let convert_expr, discrete_type = infer_parsed_discrete_boolean_expression variable_infos expr in
        Parsed_nonlinear_constraint convert_expr, discrete_type


(* Type checking and infer literal numbers of simple predicate *)
let rec infer_parsed_simple_predicate variable_infos = function
	| Parsed_discrete_boolean_expression expr ->
	    let convert_expr, discrete_type = infer_parsed_discrete_boolean_expression variable_infos expr in

        if not (DiscreteType.is_discrete_type_bool_type discrete_type) then (
            raise (TypeError (
                "Expression `"
                ^ string_of_parsed_discrete_boolean_expression variable_infos expr
                ^ "` in property, is not a boolean expression: "
                ^ DiscreteType.string_of_var_type_discrete discrete_type
            ))
        )
        else
	        Parsed_discrete_boolean_expression convert_expr, discrete_type
	| Parsed_loc_predicate predicate as loc_predicate -> loc_predicate, DiscreteType.Var_type_discrete_bool
	| Parsed_state_predicate_true -> Parsed_state_predicate_true, DiscreteType.Var_type_discrete_bool
	| Parsed_state_predicate_false -> Parsed_state_predicate_false, DiscreteType.Var_type_discrete_bool
	| Parsed_state_predicate_accepting -> Parsed_state_predicate_accepting, DiscreteType.Var_type_discrete_bool

and infer_parsed_state_predicate variable_infos = function
	| Parsed_state_predicate_OR (expr1, expr2) ->
	    let convert_expr1, _ = infer_parsed_state_predicate variable_infos expr1 in
	    let convert_expr2, _ = infer_parsed_state_predicate variable_infos expr2 in
		Parsed_state_predicate_OR (convert_expr1, convert_expr2), DiscreteType.Var_type_discrete_bool

	| Parsed_state_predicate_term term ->
	    let convert_term, discrete_type = infer_parsed_state_predicate_term variable_infos term in
	    Parsed_state_predicate_term convert_term, discrete_type

and infer_parsed_state_predicate_term variable_infos = function
	| Parsed_state_predicate_term_AND (term1, term2) ->
        let convert_term1, _ = infer_parsed_state_predicate_term variable_infos term1 in
        let convert_term2, _ = infer_parsed_state_predicate_term variable_infos term2 in
		Parsed_state_predicate_term_AND (convert_term1, convert_term2), DiscreteType.Var_type_discrete_bool

	| Parsed_state_predicate_factor factor ->
	    let convert_factor, discrete_type = infer_parsed_state_predicate_factor variable_infos factor in
	    Parsed_state_predicate_factor convert_factor, discrete_type

and infer_parsed_state_predicate_factor variable_infos = function
	| Parsed_state_predicate_factor_NOT factor ->
	    let convert_factor, discrete_type = infer_parsed_state_predicate_factor variable_infos factor in
	    Parsed_state_predicate_factor_NOT convert_factor, discrete_type
	| Parsed_simple_predicate predicate ->
	    let convert_predicate, discrete_type = infer_parsed_simple_predicate variable_infos predicate in
	    Parsed_simple_predicate convert_predicate, discrete_type
	| Parsed_state_predicate predicate ->
	    let convert_predicate, discrete_type = infer_parsed_state_predicate variable_infos predicate in
	    Parsed_state_predicate convert_predicate, discrete_type



(** Resolve expression discrete type **)

let rec discrete_type_of_expression variable_infos = function
    | Parsed_global_expression expr ->
        discrete_type_of_parsed_boolean_expression variable_infos expr

and discrete_type_of_parsed_boolean_expression variable_infos = function
	| Parsed_And _
	| Parsed_Or _ -> DiscreteType.Var_type_discrete_bool
	| Parsed_Discrete_boolean_expression expr ->
	    discrete_type_of_parsed_discrete_boolean_expression variable_infos expr

and discrete_type_of_parsed_discrete_boolean_expression variable_infos = function
    | Parsed_arithmetic_expression expr ->
        discrete_type_of_parsed_discrete_arithmetic_expression variable_infos expr
	| Parsed_expression _
	| Parsed_expression_in _
	| Parsed_Not _ ->
	    DiscreteType.Var_type_discrete_bool
	| Parsed_boolean_expression expr ->
	    discrete_type_of_parsed_boolean_expression variable_infos expr

and discrete_type_of_parsed_discrete_arithmetic_expression variable_infos = function
	| Parsed_DAE_plus (_, term)
	| Parsed_DAE_minus (_, term)
	| Parsed_DAE_term term ->
        discrete_type_of_parsed_discrete_term variable_infos term

and discrete_type_of_parsed_discrete_term variable_infos = function
	| Parsed_DT_mul (_, factor)
	| Parsed_DT_div (_, factor)
	| Parsed_DT_factor factor ->
	    discrete_type_of_parsed_discrete_factor variable_infos factor

and discrete_type_of_parsed_discrete_factor variable_infos = function
	| Parsed_DF_variable variable_name ->
	    get_discrete_type_of_variable_by_name variable_infos variable_name
	| Parsed_DF_constant value ->
	    DiscreteValue.discrete_type_of_value value

    | Parsed_DF_array expr_array ->
        (* Arbitrary take the first item of array *)
        (* TODO benjamin REFACTOR use default_type from DiscreteValue here below *)
        let inner_type = if Array.length expr_array > 0 then discrete_type_of_parsed_boolean_expression variable_infos (Array.get expr_array 0) else (DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_rational) in
        let length = Array.length expr_array in
        DiscreteType.Var_type_discrete_array (inner_type, length)

    | Parsed_DF_list expr_list ->
        (* Arbitrary take the first item of list *)
        (* TODO benjamin REFACTOR use default_type from DiscreteValue here below *)
        let inner_type = if List.length expr_list > 0 then discrete_type_of_parsed_boolean_expression variable_infos (List.nth expr_list 0) else (DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_rational) in
        DiscreteType.Var_type_discrete_list inner_type

	| Parsed_DF_unary_min factor ->
	    discrete_type_of_parsed_discrete_factor variable_infos factor
    | Parsed_DF_access (factor, _) as access ->
        let discrete_type = discrete_type_of_parsed_discrete_factor variable_infos factor in
        (* Unwrap type from array, because of access *)
        (match discrete_type with
        | DiscreteType.Var_type_discrete_array (inner_type, _)
        | DiscreteType.Var_type_discrete_list inner_type -> inner_type
        | _ -> raise (TypeError ("Unable to access an index of a non-array / non-list: " ^ string_of_parsed_factor variable_infos access))
        )
	| Parsed_DF_expression expr ->
	    discrete_type_of_parsed_discrete_arithmetic_expression variable_infos expr
	| Parsed_rational_of_int_function expr ->
	    DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_rational
    | Parsed_pow_function (expr, exp) ->
        (* Pow function result type depends of the left member type *)
        discrete_type_of_parsed_discrete_arithmetic_expression variable_infos expr
    | Parsed_shift_function (_, factor, _)
    | Parsed_bin_log_function (_, factor, _)
    | Parsed_log_not factor ->
        (* Shift result type is a binary word of length depending on the left member length *)
        (* Logical and, or, xor, not depend on one member length (arbitrary, because already type checked!) *)
        discrete_type_of_parsed_discrete_factor variable_infos factor

    | Parsed_array_concat (factor_0, factor_1) ->
        let parameter_type_0 = discrete_type_of_parsed_discrete_factor variable_infos factor_0 in
        let parameter_type_1 = discrete_type_of_parsed_discrete_factor variable_infos factor_1 in

        (match parameter_type_0, parameter_type_1 with
        | DiscreteType.Var_type_discrete_array (inner_type_0, length_0), DiscreteType.Var_type_discrete_array (inner_type_1, length_1) ->
            (* Arbitrary use inner_type of parameter 0, because already type checked!) *)
            (* But array length of array concatenation is equal to length of first array plus length of second array *)
            DiscreteType.Var_type_discrete_array (inner_type_0, length_0 + length_1)
        | _ -> raise (TypeError "fill this message") (* TODO benjamin CLEAN set message *)
        )

    | Parsed_list_cons (_, factor) ->
        (* Already type checked, the type is of the type of list (in factor) Var_type_discrete_list x *)
        discrete_type_of_parsed_discrete_factor variable_infos factor


(** Checking functions **)

let check_parsed_state_predicate variable_infos predicate =
    let uniformly_typed_state_predicate, discrete_type = infer_parsed_state_predicate variable_infos predicate in
    uniformly_typed_state_predicate, discrete_type

(* Type non-linear constraint *)
(* return a tuple containing the non-linear constraint uniformly typed and the resolved type of the expression *)
let check_nonlinear_constraint variable_infos nonlinear_constraint =

    let uniformly_typed_nonlinear_constraint, discrete_type = infer_nonlinear_constraint variable_infos nonlinear_constraint in
    print_message Verbose_high ("nonlinear constraint " ^ (string_of_parsed_nonlinear_constraint variable_infos nonlinear_constraint) ^ " was checked ");
    (* Check that non-linear constraint is a Boolean expression *)
    match discrete_type with
    | DiscreteType.Var_type_discrete_bool -> uniformly_typed_nonlinear_constraint, discrete_type
    | _ -> raise (TypeError (
        "Guard or invariant expression `"
        ^ (string_of_parsed_nonlinear_constraint variable_infos nonlinear_constraint)
        ^ "` is not a Boolean expression"
    ))


(* Type check guard / invariant *)
(* return a tuple containing the expression uniformly typed and the resolved type of the expression *)
let check_guard variable_infos guard =

    let resolved_nonlinear_constraints = List.map (check_nonlinear_constraint variable_infos) guard in

    let uniformly_typed_nonlinear_constraints = List.map (fun (u, _) -> u) resolved_nonlinear_constraints in
    let nonlinear_constraint_types = List.map (fun (_, t) -> t) resolved_nonlinear_constraints in

    uniformly_typed_nonlinear_constraints, List.hd nonlinear_constraint_types


let rec infer_variable_access variable_infos = function
    | Variable_name variable_name ->
        (* Get assigned variable type *)
        let var_type = get_type_of_variable_by_name variable_infos variable_name in
        let discrete_var_type = DiscreteType.discrete_type_of_var_type var_type in
        Variable_name variable_name, discrete_var_type

    | Variable_access (variable_access, index_expr) ->

        let converted_variable_access, var_type_discrete = infer_variable_access variable_infos variable_access in
        let infer_index_expr, index_expr_type = infer_parsed_discrete_arithmetic_expression variable_infos index_expr in

        (* If a number convert *)
        let converted_index_expr =
            if DiscreteType.is_discrete_type_unknown_number_type index_expr_type then (
                (* *)
                let target_type = DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_int in
                convert_literals_of_parsed_discrete_arithmetic_expression variable_infos target_type infer_index_expr
            ) else (
                infer_index_expr
            )
        in

        (* Check is an array *)
        let infer_var_type = (
            match var_type_discrete with
            | DiscreteType.Var_type_discrete_array (inner_type, _) -> inner_type
            | _ -> raise (TypeError "Trying to make an access to a non-array or a non-list variable")
        )
        in
        Variable_access (converted_variable_access, converted_index_expr), infer_var_type



(* Type check an update *)
(* return a tuple containing the update uniformly typed and the resolved type of the expression *)
let check_update variable_infos variable_access expr =

    (* Resolve expression type and get uniformly typed expression *)
    let uniformly_typed_expr, expr_type = infer_expression variable_infos expr in
    (* Get assigned variable type *)
    let variable_name = ParsingStructureUtilities.variable_name_of_variable_access variable_access in
    let var_type = get_type_of_variable_by_name variable_infos variable_name in
    let var_type_discrete = DiscreteType.discrete_type_of_var_type var_type in

    let converted_variable_access, inner_var_type_discrete = infer_variable_access variable_infos variable_access in


    (*  *)
    let typed_expr =
        (* Check var_type_discrete is compatible with expression type, if yes, convert expression *)
         if not (DiscreteType.is_discrete_type_compatibles inner_var_type_discrete expr_type) then (
            raise (TypeError (
                "Variable `"
                ^ variable_name
                ^ "` of type "
                ^ (DiscreteType.string_of_var_type var_type)
                ^ " is not compatible with expression `"
                ^ (ParsingStructureUtilities.string_of_parsed_global_expression variable_infos uniformly_typed_expr)
                ^ "` of type "
                ^ (DiscreteType.string_of_var_type_discrete expr_type)
                )
            )
        )
        else if DiscreteType.is_discrete_type_holding_number_type var_type_discrete && DiscreteType.is_discrete_type_holding_number_type expr_type then (

            (* If the expression type is a number, and as expression type and var type are compatibles *)
            (* convert expression type to variable type (infer to variable type) *)
            print_message Verbose_high (
                "\tInfer update expression `"
                ^ (string_of_parsed_global_expression variable_infos expr)
                ^ "` to variable type "
                ^ (DiscreteType.string_of_var_type_discrete var_type_discrete)
            );

            (* Extract target type for conversion *)

            let target_type = DiscreteType.extract_inner_type var_type_discrete in
            convert_literals_of_expression variable_infos target_type uniformly_typed_expr
        ) else
            uniformly_typed_expr
    in
    converted_variable_access, typed_expr


(* Type check a conditional expression *)
(* return a tuple containing the conditional expression uniformly typed and the resolved type of the expression *)
let check_conditional variable_infos expr =

    let uniformly_typed_bool_expr, expr_type = infer_parsed_boolean_expression variable_infos expr in

    (* Check that non-linear constraint is a Boolean expression *)
    if DiscreteType.is_discrete_type_bool_type expr_type then
        uniformly_typed_bool_expr, expr_type
    else (
        raise (TypeError (
            "Expression `"
            ^ (string_of_parsed_boolean_expression variable_infos expr)
            ^ "` in conditional statement, is not a Boolean expression"
            )
        )
    )

(* Check that an expression assigned to a variable is of the same type *)
(* If not, raise a TypeError exception with an error message *)
let check_type_assignment variable_infos variable_name expr =

    (* Function that construct type error message *)
    let get_error_message variable_name variable_type expr_type expr =
        "Variable "
        ^ variable_name
        ^ " of type "
        ^ (DiscreteType.string_of_var_type_discrete variable_type)
        ^ " is not compatible with expression : `"
        ^ (string_of_parsed_global_expression variable_infos expr)
        ^ "`"
        ^ " of type "
        ^ (DiscreteType.string_of_var_type_discrete expr_type)
    in



    (* Get variable type *)
    let variable_type = get_discrete_type_of_variable_by_name variable_infos variable_name in
    (* Resolve expression type *)
    let infer_expr, expr_var_type_discrete = infer_expression variable_infos expr in

    (* Check expression / variable type consistency *)
    let is_consistent = DiscreteType.is_discrete_type_compatibles variable_type expr_var_type_discrete in

    (* Not consistent ? raise a type error with appropriate message*)
    if not (is_consistent) then (
        raise (TypeError (get_error_message variable_name variable_type expr_var_type_discrete infer_expr))
    )
    else
        infer_expr, expr_var_type_discrete


let check_constant_expression initialized_constants (name, expr, var_type) =
    let variable_infos = {
        constants = initialized_constants;
        variable_names = [];
        index_of_variables = Hashtbl.create 0;
        removed_variable_names = [];
        type_of_variables = (fun _ -> raise (TypeError "oops!"));
    }
    in
    let target_var_type = DiscreteType.discrete_type_of_var_type var_type in
    (* Infer expression type *)
    let infer_expr, discrete_type = infer_expression variable_infos expr in
    (* Check compatibility *)
    let is_compatible = DiscreteType.is_discrete_type_compatibles target_var_type discrete_type in

    (* If not compatibles, display an error message *)
    if not is_compatible then (
        print_error ("Constant "
            ^ name
            ^ " of type "
            ^ (DiscreteType.string_of_var_type_discrete target_var_type)
            ^ " is not compatible with expression `"
            ^ (ParsingStructureUtilities.string_of_parsed_global_expression variable_infos expr)
            ^ "` of type "
            ^ (DiscreteType.string_of_var_type_discrete discrete_type)
        );
        raise (TypeError "Bad constant declaration(s)")
    );

    let target_inner_type = DiscreteType.extract_inner_type target_var_type in

    (* If no type was deduce from expression, so it's a rational *)
    let converted_expr, converted_type = convert_literals_of_expression variable_infos target_inner_type infer_expr, target_var_type in
    converted_expr, converted_type

type yo =
    | YO1 of int
    | YO2 of int

(* Check that a discrete variable initialization is well typed *)
let check_discrete_init variable_infos variable_name expr =

    (* Get the variable index *)
    let discrete_index = Hashtbl.find variable_infos.index_of_variables variable_name in
    (* Get variable type *)
    let var_type = get_type_of_variable variable_infos discrete_index in
    let var_discrete_type = DiscreteType.discrete_type_of_var_type var_type in

    (* Check whether variable is clock or parameter *)
    let is_clock_or_parameter = var_type == DiscreteType.Var_type_clock || var_type == DiscreteType.Var_type_parameter in

    (* Check if variable is clock or parameter, it's forbidden to init clock or parameter in discrete section *)
    if (is_clock_or_parameter) then (
        raise (TypeError ("Initialisation of a " ^ (DiscreteType.string_of_var_type var_type) ^ " in discrete init state section is forbidden"))
    );

    (* Check expression / variable type consistency *)
    let infer_expr, expr_type = check_type_assignment variable_infos variable_name expr in

    (* If expression type was unknown number, *)
    (* and as we had already check the compatibility of variable type and expression type above *)
    (* we should convert expression type to variable type *)
    let converted_expr =
        if DiscreteType.is_discrete_type_holding_unknown_number_type expr_type then (

            print_message Verbose_high (
                "\tInfer expression type of `"
                ^ ParsingStructureUtilities.string_of_parsed_global_expression variable_infos infer_expr
                ^ "` as the same as assigned variable type: " ^ DiscreteType.string_of_var_type_discrete var_discrete_type
            );
            let target_type = DiscreteType.extract_inner_type var_discrete_type in
            convert_literals_of_expression variable_infos target_type infer_expr
        )
        else
            infer_expr
    in

    converted_expr
