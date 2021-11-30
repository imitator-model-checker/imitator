(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: This module aims to convert a parsed expression to a abstract typed expression
 *
 * File contributors : Benjamin L.
 * Created           : 2021/11/20
 * Last modified     : 2021/11/20
 *
 ************************************************************)

open Constants
open Exceptions
open ParsingStructure
open DiscreteExpressions
open DiscreteType
open TypeChecker2

type discrete_index = int

(* Variable kind type represent a variable or a constant kind *)
type variable_kind =
    | Variable_kind of discrete_index
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

(** Convert a Boolean operator to its abstract model *)
let convert_parsed_relop = function
	| PARSED_OP_L -> OP_L
	| PARSED_OP_LEQ	-> OP_LEQ
	| PARSED_OP_EQ	-> OP_EQ
	| PARSED_OP_NEQ	-> OP_NEQ
	| PARSED_OP_GEQ	-> OP_GEQ
	| PARSED_OP_G -> OP_G


let rec global_expression_of_typed_global_expression variable_infos = function
    | Typed_global_expr (expr, discrete_type) ->
        global_expression_of_typed_boolean_expression variable_infos expr discrete_type

(* TODO benjamin pas beau *)
and global_expression_of_typed_boolean_expression variable_infos expr = function
    | Var_type_discrete_number _ ->
        Arithmetic_expression (
            discrete_arithmetic_expression_of_typed_boolean_expression variable_infos expr
        )
    | Var_type_discrete_bool ->
        Bool_expression (
            bool_expression_of_typed_boolean_expression variable_infos expr
        )
    | Var_type_discrete_binary_word _ ->
        Binary_word_expression (
            binary_expression_of_typed_boolean_expression variable_infos expr
        )
    | Var_type_discrete_array _ ->
        Array_expression (
            array_expression_of_typed_boolean_expression variable_infos expr
        )
    | Var_type_discrete_list _ ->
        List_expression (
            list_expression_of_typed_boolean_expression variable_infos expr
        )

and discrete_arithmetic_expression_of_typed_boolean_expression variable_infos = function
	| Typed_discrete_bool_expr (expr, discrete_type) ->
	    (match discrete_type with
	    | Var_type_discrete_number Var_type_discrete_rational ->
	        Rational_arithmetic_expression (rational_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos expr)
	    | Var_type_discrete_number Var_type_discrete_int ->
	        Int_arithmetic_expression (int_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos expr)
	    | _ -> raise (InternalError "a")
	    )
	| _ -> raise (InternalError "b")

and discrete_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos = function
	| Typed_arithmetic_expr (expr, discrete_type) ->
	    (match discrete_type with
	    | Var_type_discrete_number Var_type_discrete_rational ->
	        Rational_arithmetic_expression (rational_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr)
	    | Var_type_discrete_number Var_type_discrete_int ->
	        Int_arithmetic_expression (int_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr)
	    | _ -> raise (InternalError "c")
	    )
	| _ -> raise (InternalError "d")

(* TODO benjamin CLEAN review this function *)
and discrete_arithmetic_expression_of_typed_discrete_arithmetic_expression variable_infos = function
	| Typed_plus (expr, term, discrete_number_type) ->
        (match discrete_number_type with
        | Var_type_discrete_rational ->
            Rational_arithmetic_expression (
                DAE_plus (
                    rational_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr,
                    rational_arithmetic_expression_of_typed_term variable_infos term
                )
            )
        | Var_type_discrete_int ->
            Int_arithmetic_expression (
                Int_plus (
                    int_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr,
                    int_arithmetic_expression_of_typed_term variable_infos term
                )
            )
        | _ -> raise (InternalError "e")
        )

	| Typed_minus (expr, term, discrete_number_type) ->
        (match discrete_number_type with
        | Var_type_discrete_rational ->
            Rational_arithmetic_expression (
                DAE_minus (
                    rational_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr,
                    rational_arithmetic_expression_of_typed_term variable_infos term
                )
            )
        | Var_type_discrete_int ->
            Int_arithmetic_expression (
                Int_minus (
                    int_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr,
                    int_arithmetic_expression_of_typed_term variable_infos term
                )
            )
        | _ -> raise (InternalError "f")
        )

	| Typed_term (term, discrete_type) ->
        (match discrete_type with
        | Var_type_discrete_number Var_type_discrete_rational ->
            Rational_arithmetic_expression (DAE_term (rational_arithmetic_expression_of_typed_term variable_infos term))
        | Var_type_discrete_number Var_type_discrete_int ->
            Int_arithmetic_expression (Int_term (int_arithmetic_expression_of_typed_term variable_infos term))
        | _ -> raise (InternalError "g")
        )

(* --------------------*)
(* Bool conversion *)
(* --------------------*)

and bool_expression_of_typed_boolean_expression variable_infos = function
	| Typed_And (l_expr, r_expr) ->
	    And_bool (
	        bool_expression_of_typed_boolean_expression variable_infos l_expr,
	        bool_expression_of_typed_boolean_expression variable_infos r_expr
	    )

	| Typed_Or (l_expr, r_expr) ->
	    Or_bool (
	        bool_expression_of_typed_boolean_expression variable_infos l_expr,
	        bool_expression_of_typed_boolean_expression variable_infos r_expr
	    )

    | Typed_discrete_bool_expr (expr, _) ->
        Discrete_boolean_expression (
            bool_expression_of_typed_discrete_boolean_expression variable_infos expr
        )

and bool_expression_of_typed_discrete_boolean_expression variable_infos = function
    | Typed_arithmetic_expr (expr, discrete_type) ->
        bool_expression_of_typed_arithmetic_expression variable_infos expr

	| Typed_comparison (l_expr, relop, r_expr, _) ->
	    bool_expression_of_typed_comparison variable_infos l_expr relop r_expr

	| Typed_comparison_in (in_expr, lw_expr, up_expr, _) ->
	    Expression_in (
	        discrete_arithmetic_expression_of_typed_discrete_arithmetic_expression variable_infos in_expr,
	        discrete_arithmetic_expression_of_typed_discrete_arithmetic_expression variable_infos lw_expr,
	        discrete_arithmetic_expression_of_typed_discrete_arithmetic_expression variable_infos up_expr
	    )

	| Typed_bool_expr (expr, _) ->
	    Boolean_expression (
	        bool_expression_of_typed_boolean_expression variable_infos expr
	    )

	| Typed_not_expr expr ->
	    Not_bool (
	        bool_expression_of_typed_boolean_expression variable_infos expr
	    )

and bool_expression_of_typed_comparison variable_infos l_expr relop r_expr =
    let discrete_type = TypeChecker2.type_of_typed_discrete_boolean_expression l_expr in
    match discrete_type with
    | Var_type_discrete_number _ ->
        (* TODO benjamin LOOK HERE, even if it was type checked before can potentially compare different type *)
        Expression (
            discrete_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos l_expr,
            convert_parsed_relop relop,
            discrete_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos r_expr
        )
    | _ -> raise (InternalError "h")

and bool_expression_of_typed_arithmetic_expression variable_infos = function
	| Typed_term (term, _) ->
	    bool_expression_of_typed_term variable_infos term
    | _ ->
        raise (InternalError "i")

and bool_expression_of_typed_term variable_infos = function
	| Typed_factor (factor, _) ->
	    bool_expression_of_typed_factor variable_infos factor
    | _ ->
        raise (InternalError "j")

and bool_expression_of_typed_factor variable_infos = function
	| Typed_variable (variable_name, discrete_type) ->
        let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
        (match variable_kind with
        | Constant_kind value -> DB_constant (DiscreteValue.bool_value value)
        | Variable_kind discrete_index -> DB_variable discrete_index
        )

	| Typed_constant (value, discrete_type) ->
	    DB_constant (DiscreteValue.bool_value value)

    | Typed_expr (expr, _) ->
        bool_expression_of_typed_arithmetic_expression variable_infos expr

    | Typed_access (factor, index_expr, discrete_type) ->
        Bool_access (
            expression_access_type_of_typed_factor variable_infos factor,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
        )

	| Typed_function_call (function_name, argument_expressions, _) ->
	    bool_expression_of_typed_function_call variable_infos argument_expressions function_name

	| _ -> raise (InternalError "k")

and bool_expression_of_typed_function_call variable_infos argument_expressions = function
    | "list_hd" ->
        let arg_0 = List.nth argument_expressions 0 in
        Bool_list_hd (
            list_expression_of_typed_boolean_expression variable_infos arg_0
        )
    (*
    | "list_mem" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in
        List_mem (
            global_expression_of_typed_boolean_expression variable_infos arg_0,
            list_expression_of_typed_boolean_expression variable_infos arg_1
        )
    *)
    | function_name -> raise (UndefinedFunction function_name)

(* --------------------*)
(* Rational conversion *)
(* --------------------*)

and rational_arithmetic_expression_of_typed_boolean_expression variable_infos = function
    | Typed_discrete_bool_expr (expr, _) ->
        rational_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos expr
    | _ -> raise (InternalError "l")

and rational_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos = function
    | Typed_arithmetic_expr (expr, discrete_type) ->
        rational_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr
    | _ ->
        raise (InternalError "m")

and rational_arithmetic_expression_of_typed_arithmetic_expression variable_infos = function
	| Typed_plus (expr, term, _) ->
	    DAE_plus (
	        rational_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr,
	        rational_arithmetic_expression_of_typed_term variable_infos term
	    )

	| Typed_minus (expr, term, _) ->
	    DAE_minus (
	        rational_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr,
	        rational_arithmetic_expression_of_typed_term variable_infos term
	    )

	| Typed_term (term, _) ->
	    DAE_term (
	        rational_arithmetic_expression_of_typed_term variable_infos term
	    )

and rational_arithmetic_expression_of_typed_term variable_infos = function
	| Typed_mul (term, factor, _) ->
	    DT_mul (
	        rational_arithmetic_expression_of_typed_term variable_infos term,
	        rational_arithmetic_expression_of_typed_factor variable_infos factor
	    )

	| Typed_div (term, factor, _) ->
	    DT_div (
	        rational_arithmetic_expression_of_typed_term variable_infos term,
	        rational_arithmetic_expression_of_typed_factor variable_infos factor
	    )

	| Typed_factor (factor, _) ->
	    DT_factor (
	        rational_arithmetic_expression_of_typed_factor variable_infos factor
	    )

and rational_arithmetic_expression_of_typed_factor variable_infos = function
	| Typed_unary_min factor ->
	    DF_unary_min (
	        rational_arithmetic_expression_of_typed_factor variable_infos factor
	    )

	| Typed_variable (variable_name, discrete_type) ->
        let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
        (match variable_kind with
        | Constant_kind value -> DF_constant (DiscreteValue.to_numconst_value value)
        | Variable_kind discrete_index -> DF_variable discrete_index
        )

	| Typed_constant (value, discrete_type) ->
	    DF_constant (DiscreteValue.to_numconst_value value)

	| Typed_expr (expr, _) ->
	    DF_expression (
	        rational_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr
        )

    | Typed_access (factor, index_expr, discrete_type) ->
        Rational_access (
            expression_access_type_of_typed_factor variable_infos factor,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
        )

	| Typed_function_call (function_name, argument_expressions, _) ->
	    rational_expression_of_typed_function_call variable_infos argument_expressions function_name

	| _ -> raise (InternalError "n")

and rational_expression_of_typed_function_call variable_infos argument_expressions = function
    | "pow" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in
        Rational_pow (
            rational_arithmetic_expression_of_typed_boolean_expression variable_infos arg_0,
            int_arithmetic_expression_of_typed_boolean_expression variable_infos arg_1
        )
    | "rational_of_int" ->
        let arg_0 = List.nth argument_expressions 0 in
        DF_rational_of_int (
            int_arithmetic_expression_of_typed_boolean_expression variable_infos arg_0
        )
    (*
    | "list_hd" ->
        let arg_0 = List.nth argument_expressions 0 in
        Rational_list_hd (
            list_expression_of_typed_boolean_expression variable_infos arg_0
        )
    *)
    | function_name -> raise (UndefinedFunction function_name)

(* --------------------*)
(* Int conversion *)
(* --------------------*)

and int_arithmetic_expression_of_typed_boolean_expression variable_infos = function
    | Typed_discrete_bool_expr (expr, _) ->
        int_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos expr
    | _ -> raise (InternalError "o")

and int_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos = function
    | Typed_arithmetic_expr (expr, discrete_type) ->
        int_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr
    | _ ->
        raise (InternalError "p")

and int_arithmetic_expression_of_typed_arithmetic_expression variable_infos = function
	| Typed_plus (expr, term, _) ->
	    Int_plus (
	        int_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr,
	        int_arithmetic_expression_of_typed_term variable_infos term
	    )

	| Typed_minus (expr, term, _) ->
	    Int_minus (
	        int_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr,
	        int_arithmetic_expression_of_typed_term variable_infos term
	    )

	| Typed_term (term, _) ->
	    Int_term (
	        int_arithmetic_expression_of_typed_term variable_infos term
	    )

and int_arithmetic_expression_of_typed_term variable_infos = function
	| Typed_mul (term, factor, _) ->
	    Int_mul (
	        int_arithmetic_expression_of_typed_term variable_infos term,
	        int_arithmetic_expression_of_typed_factor variable_infos factor
	    )

	| Typed_div (term, factor, _) ->
	    Int_div (
	        int_arithmetic_expression_of_typed_term variable_infos term,
	        int_arithmetic_expression_of_typed_factor variable_infos factor
	    )

	| Typed_factor (factor, _) ->
	    Int_factor (
	        int_arithmetic_expression_of_typed_factor variable_infos factor
	    )

and int_arithmetic_expression_of_typed_factor variable_infos = function
	| Typed_unary_min factor ->
	    Int_unary_min (
	        int_arithmetic_expression_of_typed_factor variable_infos factor
	    )

	| Typed_variable (variable_name, discrete_type) ->
        let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
        (match variable_kind with
        | Constant_kind value -> Int_constant (DiscreteValue.to_int_value value)
        | Variable_kind discrete_index -> Int_variable discrete_index
        )

	| Typed_constant (value, discrete_type) ->
	    Int_constant (DiscreteValue.to_int_value value)

	| Typed_expr (expr, _) ->
	    Int_expression (
	        int_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr
        )

    | Typed_access (factor, index_expr, discrete_type) ->
        Int_access (
            expression_access_type_of_typed_factor variable_infos factor,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
        )

	| Typed_function_call (function_name, argument_expressions, _) ->
	    int_expression_of_typed_function_call variable_infos argument_expressions function_name

	| _ -> raise (InternalError "q")

and int_expression_of_typed_function_call variable_infos argument_expressions = function
    | "pow" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in
        Int_pow (
            int_arithmetic_expression_of_typed_boolean_expression variable_infos arg_0,
            int_arithmetic_expression_of_typed_boolean_expression variable_infos arg_1
        )
    | "list_hd" ->
        let arg_0 = List.nth argument_expressions 0 in
        Int_list_hd (
            list_expression_of_typed_boolean_expression variable_infos arg_0
        )
    (* TODO benjamin, in the future replace raise by custom function call as comment below *)
    | function_name -> raise (UndefinedFunction function_name)

(* --------------------*)
(* Binary word conversion *)
(* --------------------*)

and binary_expression_of_typed_boolean_expression variable_infos = function
    | Typed_discrete_bool_expr (expr, _) ->
        binary_expression_of_typed_discrete_boolean_expression variable_infos expr
    | _ -> raise (InternalError "r")

and binary_expression_of_typed_discrete_boolean_expression variable_infos = function
    | Typed_arithmetic_expr (expr, discrete_type) ->
        binary_expression_of_typed_arithmetic_expression variable_infos expr
    | _ ->
        raise (InternalError "s")

and binary_expression_of_typed_arithmetic_expression variable_infos = function
	| Typed_term (term, _) ->
	        binary_expression_of_typed_term variable_infos term
    | _ ->
        raise (InternalError "t")

and binary_expression_of_typed_term variable_infos = function
	| Typed_factor (factor, _) ->
	        binary_expression_of_typed_factor variable_infos factor
    | _ ->
        raise (InternalError "u")

and binary_expression_of_typed_factor variable_infos = function
	| Typed_variable (variable_name, discrete_type) ->

	    let binary_word_length =
	        match discrete_type with
	        | Var_type_discrete_binary_word length -> length
	        | _ -> raise (InternalError "v")
	    in

        let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
        (match variable_kind with
        | Constant_kind value -> Binary_word_constant (DiscreteValue.binary_word_value value)
        | Variable_kind discrete_index -> Binary_word_variable (discrete_index, binary_word_length)
        )

	| Typed_constant (value, discrete_type) ->
	    Binary_word_constant (DiscreteValue.binary_word_value value)

	| Typed_expr (expr, _) ->
        binary_expression_of_typed_arithmetic_expression variable_infos expr

    | Typed_access (factor, index_expr, discrete_type) ->
	    let binary_word_length =
	        match discrete_type with
	        | Var_type_discrete_binary_word length -> length
	        | _ -> raise (InternalError "w")
	    in

        Binary_word_access (
            expression_access_type_of_typed_factor variable_infos factor,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr,
            binary_word_length
        )

	| Typed_function_call (function_name, argument_expressions, discrete_type) ->
	    let binary_word_length =
	        match discrete_type with
	        | Var_type_discrete_binary_word length -> length
	        | _ -> raise (InternalError "x")
	    in

	    binary_expression_of_typed_function_call variable_infos binary_word_length argument_expressions function_name

	| _ -> raise (InternalError "y")

and binary_expression_of_typed_function_call variable_infos binary_word_length argument_expressions = function
    | "shift_left" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in

        Logical_shift_left (
            binary_expression_of_typed_boolean_expression variable_infos arg_0,
            int_arithmetic_expression_of_typed_boolean_expression variable_infos arg_1,
            binary_word_length
        )
    | "shift_right" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in

        Logical_shift_right (
            binary_expression_of_typed_boolean_expression variable_infos arg_0,
            int_arithmetic_expression_of_typed_boolean_expression variable_infos arg_1,
            binary_word_length
        )
    | "fill_left" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in

        Logical_fill_left (
            binary_expression_of_typed_boolean_expression variable_infos arg_0,
            int_arithmetic_expression_of_typed_boolean_expression variable_infos arg_1,
            binary_word_length
        )
    | "fill_right" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in

        Logical_fill_right (
            binary_expression_of_typed_boolean_expression variable_infos arg_0,
            int_arithmetic_expression_of_typed_boolean_expression variable_infos arg_1,
            binary_word_length
        )
    | "logand" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in

        Logical_and (
            binary_expression_of_typed_boolean_expression variable_infos arg_0,
            binary_expression_of_typed_boolean_expression variable_infos arg_1,
            binary_word_length
        )
    | "logor" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in

        Logical_or (
            binary_expression_of_typed_boolean_expression variable_infos arg_0,
            binary_expression_of_typed_boolean_expression variable_infos arg_1,
            binary_word_length
        )
    | "logxor" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in

        Logical_xor (
            binary_expression_of_typed_boolean_expression variable_infos arg_0,
            binary_expression_of_typed_boolean_expression variable_infos arg_1,
            binary_word_length
        )

    | "lognot" ->
        let arg_0 = List.nth argument_expressions 0 in

        Logical_not (
            binary_expression_of_typed_boolean_expression variable_infos arg_0,
            binary_word_length
        )
    | "list_hd" ->
        let arg_0 = List.nth argument_expressions 0 in
        Binary_word_list_hd (
            list_expression_of_typed_boolean_expression variable_infos arg_0
        )
    | function_name -> raise (UndefinedFunction function_name)

(* --------------------*)
(* Array conversion *)
(* --------------------*)

and array_expression_of_typed_boolean_expression variable_infos = function
    | Typed_discrete_bool_expr (expr, _) ->
        array_expression_of_typed_discrete_boolean_expression variable_infos expr
    | _ -> raise (InternalError "z")

and array_expression_of_typed_discrete_boolean_expression variable_infos = function
    | Typed_arithmetic_expr (expr, discrete_type) ->
        array_expression_of_typed_arithmetic_expression variable_infos expr
    | _ ->
        raise (InternalError "aa")

and array_expression_of_typed_arithmetic_expression variable_infos = function
	| Typed_term (term, _) ->
        array_expression_of_typed_term variable_infos term
    | _ ->
        raise (InternalError "ab")

and array_expression_of_typed_term variable_infos = function
	| Typed_factor (factor, _) ->
        array_expression_of_typed_factor variable_infos factor
    | _ ->
        raise (InternalError "ac")

and array_expression_of_typed_factor variable_infos = function
	| Typed_variable (variable_name, discrete_type) ->
        let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
        (match variable_kind with
        | Constant_kind value -> Array_constant (DiscreteValue.array_value value)
        | Variable_kind discrete_index -> Array_variable discrete_index
        )

	| Typed_constant (value, discrete_type) ->
	    Array_constant (DiscreteValue.array_value value)

    | Typed_array (expr_array, discrete_type) ->
        let inner_type =
            match discrete_type with
            | Var_type_discrete_array (inner_type, _) -> inner_type
            | _ -> raise (InternalError "")
        in
        Literal_array (Array.map (fun expr -> global_expression_of_typed_boolean_expression variable_infos expr inner_type) expr_array)

	| Typed_expr (expr, _) ->
        array_expression_of_typed_arithmetic_expression variable_infos expr

    | Typed_access (factor, index_expr, discrete_type) ->
        Array_access (
            expression_access_type_of_typed_factor variable_infos factor,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
        )

	| Typed_function_call (function_name, argument_expressions, _) ->
	    array_expression_of_typed_function_call variable_infos argument_expressions function_name

	| _ -> raise (InternalError "ad")

and array_expression_of_typed_function_call variable_infos argument_expressions = function
    | "array_append" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in
        Array_concat (
            array_expression_of_typed_boolean_expression variable_infos arg_0,
            array_expression_of_typed_boolean_expression variable_infos arg_1
        )
    | "list_hd" ->
        let arg_0 = List.nth argument_expressions 0 in
        Array_list_hd (
            list_expression_of_typed_boolean_expression variable_infos arg_0
        )
    | function_name -> raise (UndefinedFunction function_name)
    (*
    Array_function_call (
        name,
        List.map (global_expression_of_parsed_boolean_expression variable_infos) argument_expressions
    )
    *)

(* --------------------*)
(* List conversion *)
(* --------------------*)

and list_expression_of_typed_boolean_expression variable_infos = function
    | Typed_discrete_bool_expr (expr, _) ->
        list_expression_of_typed_discrete_boolean_expression variable_infos expr
    | _ -> raise (InternalError "ae")

and list_expression_of_typed_discrete_boolean_expression variable_infos = function
    | Typed_arithmetic_expr (expr, discrete_type) ->
        list_expression_of_typed_arithmetic_expression variable_infos expr
    | _ ->
        raise (InternalError "af")

and list_expression_of_typed_arithmetic_expression variable_infos = function
	| Typed_term (term, _) ->
        list_expression_of_typed_term variable_infos term
    | _ ->
        raise (InternalError "ag")

and list_expression_of_typed_term variable_infos = function
	| Typed_factor (factor, _) ->
        list_expression_of_typed_factor variable_infos factor
    | _ ->
        raise (InternalError "ah")

and list_expression_of_typed_factor variable_infos = function
	| Typed_variable (variable_name, discrete_type) ->
        let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
        (match variable_kind with
        | Constant_kind value -> List_constant (DiscreteValue.list_value value)
        | Variable_kind discrete_index -> List_variable discrete_index
        )

	| Typed_constant (value, discrete_type) ->
	    List_constant (DiscreteValue.list_value value)

    | Typed_list (expr_list, discrete_type) ->
        let inner_type =
            match discrete_type with
            | Var_type_discrete_list inner_type -> inner_type
            | _ -> raise (InternalError "")
        in
        Literal_list (List.map (fun expr -> global_expression_of_typed_boolean_expression variable_infos expr inner_type) expr_list)

	| Typed_expr (expr, _) ->
        list_expression_of_typed_arithmetic_expression variable_infos expr

    | Typed_access (factor, index_expr, discrete_type) ->
        List_access (
            expression_access_type_of_typed_factor variable_infos factor,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
        )

	| Typed_function_call (function_name, argument_expressions, _) ->
	    list_expression_of_typed_function_call variable_infos argument_expressions function_name

	| _ -> raise (InternalError "ai")

and list_expression_of_typed_function_call variable_infos argument_expressions = function
    (*
    | "list_cons" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in
        List_cons (
            global_expression_of_typed_boolean_expression variable_infos arg_0,
            list_expression_of_typed_boolean_expression variable_infos arg_1
        )
    *)
    | "list_hd" ->
        let arg_0 = List.nth argument_expressions 0 in
        List_list_hd (
            list_expression_of_typed_boolean_expression variable_infos arg_0
        )
    | "list_tl" ->
        let arg_0 = List.nth argument_expressions 0 in
        List_tl (
            list_expression_of_typed_boolean_expression variable_infos arg_0
        )
    | "list_rev" ->
        let arg_0 = List.nth argument_expressions 0 in
        List_rev (
            list_expression_of_typed_boolean_expression variable_infos arg_0
        )
    | function_name -> raise (UndefinedFunction function_name)

(* --------------------*)
(* Access conversion *)
(* --------------------*)

and expression_access_type_of_typed_factor variable_infos = function
    | Typed_array (_, discrete_type) as factor ->
        Expression_array_access (
            array_expression_of_typed_factor variable_infos factor
        )

    | Typed_list (_, discrete_type) as factor ->
        Expression_list_access (
            list_expression_of_typed_factor variable_infos factor
        )

    | _ ->
        raise (InternalError
            "An access on other element than an array or a list was found,
            although it was been type checked before."
        )

let convert_discrete_init3 variable_infos variable_name expr =
    (* Get typed expression *)
    let typed_expr = TypeChecker2.check_discrete_init3 variable_infos variable_name expr in
    (* Print *)
    ImitatorUtilities.print_message Verbose_standard (TypeChecker2.string_of_typed_global_expression variable_infos typed_expr);
    (* Convert *)
    global_expression_of_typed_global_expression variable_infos typed_expr



























(* ---------------------------------------- *)
(* ---------------------------------------- *)
(* ---------------------------------------- *)
(* ---------------------------------------- *)


(*
let convert_discrete_init variable_infos variable_name = function
    (* TODO benjamin CLEAN uniformize between global_expression and parsed_boolean_expression in check_discrete_init and bob *)
    | Parsed_global_expression expr as global_expr ->
        let discrete_type = TypeChecker2.check_discrete_init variable_infos variable_name global_expr in
        (* TODO benjamin RENAME bob *)
        bob variable_infos expr discrete_type
*)

(*

let rec bob variable_infos expr = function
    | Var_type_discrete_bool ->
        Bool_expression (bool_expression_of_parsed_boolean_expression variable_infos expr)
    | Var_type_discrete_number Var_type_discrete_rational ->
        Arithmetic_expression (Rational_arithmetic_expression (rational_expression_of_parsed_boolean_expression variable_infos expr))
    | Var_type_discrete_number Var_type_discrete_int ->
        Arithmetic_expression (Int_arithmetic_expression (int_expression_of_parsed_boolean_expression variable_infos expr))
    | Var_type_discrete_binary_word _ ->
        Binary_word_expression (binary_word_expression_of_parsed_boolean_expression variable_infos expr)
    | Var_type_discrete_array (inner_type, _) ->
        Array_expression (array_expression_of_parsed_boolean_expression variable_infos inner_type expr)
    | Var_type_discrete_list _ ->
        List_expression (list_expression_of_parsed_boolean_expression variable_infos expr)
    (* Should never happen *)
    | Var_type_discrete_number Var_type_discrete_unknown_number ->
        raise (InternalError "An expression still contains unknown literal number after type checking")

(* Convert a parsed global expression to an abstract model expression *)
and convert_parsed_global_expression variable_infos = function
    | Parsed_global_expression expr as global_expr ->
        (* TYPE CHECK *)
        let discrete_type = TypeChecker2.type_check_global_expression variable_infos global_expr in
        let discrete_type = default_type_if_needed discrete_type in

        match discrete_type with
        | Var_type_discrete_bool ->
            Bool_expression (bool_expression_of_parsed_boolean_expression variable_infos expr)
        | Var_type_discrete_number Var_type_discrete_rational ->
            Arithmetic_expression (Rational_arithmetic_expression (rational_expression_of_parsed_boolean_expression variable_infos expr))
        | Var_type_discrete_number Var_type_discrete_int ->
            Arithmetic_expression (Int_arithmetic_expression (int_expression_of_parsed_boolean_expression variable_infos expr))
        | Var_type_discrete_binary_word _ ->
            Binary_word_expression (binary_word_expression_of_parsed_boolean_expression variable_infos expr)
        | Var_type_discrete_array (inner_type, _)  ->
            Array_expression (array_expression_of_parsed_boolean_expression variable_infos inner_type expr)
        | Var_type_discrete_list _ ->
            List_expression (list_expression_of_parsed_boolean_expression variable_infos expr)
        (* Should never happen *)
        | Var_type_discrete_number Var_type_discrete_unknown_number ->
            raise (InternalError "An expression still contains unknown literal number after type checking")

and global_expression_of_parsed_boolean_expression variable_infos expr =
    convert_parsed_global_expression variable_infos (Parsed_global_expression expr)

and convert_parsed_boolean_expression variable_infos expr =
        (* TYPE CHECK *)
        let discrete_type = TypeChecker2.type_check_parsed_boolean_expression variable_infos expr in
        let discrete_type = default_type_if_needed discrete_type in

        match discrete_type with
        | Var_type_discrete_bool ->
            Bool_expression (bool_expression_of_parsed_boolean_expression variable_infos expr)
        | Var_type_discrete_number Var_type_discrete_rational ->
            Arithmetic_expression (Rational_arithmetic_expression (rational_expression_of_parsed_boolean_expression variable_infos expr))
        | Var_type_discrete_number Var_type_discrete_int ->
            Arithmetic_expression (Int_arithmetic_expression (int_expression_of_parsed_boolean_expression variable_infos expr))
        | Var_type_discrete_binary_word _ ->
            Binary_word_expression (binary_word_expression_of_parsed_boolean_expression variable_infos expr)
        | Var_type_discrete_array (inner_type, _)  ->
            Array_expression (array_expression_of_parsed_boolean_expression variable_infos inner_type expr)
        | Var_type_discrete_list _ ->
            List_expression (list_expression_of_parsed_boolean_expression variable_infos expr)
        (* Should never happen *)
        | Var_type_discrete_number Var_type_discrete_unknown_number ->
            raise (InternalError "An expression still contains unknown literal number after type checking")

(** Convert a boolean expression in its abstract model *)
(* Get typed bool expression of global parsed expression *)
(* discrete type is the inner type of the boolean expression, for example : *)
(* if x + 1 > 0 then x else y with x : int, give a Bool_expression (expr, Var_type_discrete_int) *)
and bool_expression_of_parsed_boolean_expression variable_infos = function
	| Parsed_And (e1,e2) -> And_bool ((bool_expression_of_parsed_boolean_expression variable_infos e1), (bool_expression_of_parsed_boolean_expression variable_infos e2))
	| Parsed_Or (e1, e2) -> Or_bool ((bool_expression_of_parsed_boolean_expression variable_infos e1), (bool_expression_of_parsed_boolean_expression variable_infos e2))
	| Parsed_Discrete_boolean_expression parsed_discrete_boolean_expression ->
	    (* TODO benjamin IMPORTANT check for true or false in order to return True_bool / False_bool *)
		Discrete_boolean_expression (bool_expression_of_parsed_discrete_boolean_expression variable_infos parsed_discrete_boolean_expression)

and bool_expression_of_parsed_discrete_boolean_expression variable_infos = function
    | Parsed_arithmetic_expression expr ->
        (* Search boolean variables, constants in DF_variable, DF_constant *)
        bool_expression_of_parsed_discrete_arithmetic_expression variable_infos expr

    | Parsed_expression (l_expr, relop, r_expr) ->
        let l_type = TypeChecker2.type_check_parsed_discrete_boolean_expression variable_infos l_expr in
        let r_type = TypeChecker2.type_check_parsed_discrete_boolean_expression variable_infos l_expr in
        let greater_defined_type = default_type_if_needed (greater_defined l_type r_type) in
        bool_expression_of_parsed_comparison variable_infos l_expr relop r_expr greater_defined_type

	| Parsed_expression_in (expr1, expr2, expr3) ->
	    Expression_in (
            arithmetic_expression_of_parsed_arithmetic_expression variable_infos expr1,
            arithmetic_expression_of_parsed_arithmetic_expression variable_infos expr2,
            arithmetic_expression_of_parsed_arithmetic_expression variable_infos expr3
		)
    | Parsed_boolean_expression parsed_boolean_expression ->
        Boolean_expression (bool_expression_of_parsed_boolean_expression variable_infos parsed_boolean_expression)
	| Parsed_Not e ->
	    Not_bool (bool_expression_of_parsed_boolean_expression variable_infos e)

and bool_expression_of_parsed_comparison variable_infos l_expr relop r_expr = function
    | Var_type_discrete_number Var_type_discrete_rational ->
        Expression (
            Rational_arithmetic_expression (rational_expression_of_parsed_discrete_boolean_expression variable_infos l_expr),
            convert_parsed_relop relop,
            Rational_arithmetic_expression (rational_expression_of_parsed_discrete_boolean_expression variable_infos r_expr)
        )
    | Var_type_discrete_number Var_type_discrete_int ->
        Expression (
            Int_arithmetic_expression (int_expression_of_parsed_discrete_boolean_expression variable_infos l_expr),
            convert_parsed_relop relop,
            Int_arithmetic_expression (int_expression_of_parsed_discrete_boolean_expression variable_infos r_expr)
        )
    | Var_type_discrete_bool ->
        Boolean_comparison (
            bool_expression_of_parsed_discrete_boolean_expression variable_infos l_expr,
            convert_parsed_relop relop,
            bool_expression_of_parsed_discrete_boolean_expression variable_infos r_expr
        )
    | Var_type_discrete_binary_word l ->
        Binary_comparison (
            binary_word_expression_of_parsed_discrete_boolean_expression variable_infos l_expr,
            convert_parsed_relop relop,
            binary_word_expression_of_parsed_discrete_boolean_expression variable_infos r_expr
        )
    | Var_type_discrete_array (inner_type, _) ->
        Array_comparison (
            array_expression_of_parsed_discrete_boolean_expression variable_infos inner_type l_expr,
            convert_parsed_relop relop,
            array_expression_of_parsed_discrete_boolean_expression variable_infos inner_type r_expr
        )
    | Var_type_discrete_list _ ->
        List_comparison (
            list_expression_of_parsed_discrete_boolean_expression variable_infos l_expr,
            convert_parsed_relop relop,
            list_expression_of_parsed_discrete_boolean_expression variable_infos r_expr
        )
    | Var_type_discrete_number Var_type_discrete_unknown_number ->
        raise (InvalidModel) (* should never happen, if type checking failed before *)


(* Search of boolean variables / constants in parsed discrete arithmetic expression *)
and bool_expression_of_parsed_discrete_arithmetic_expression variable_infos expr =

    let rec bool_expression_of_parsed_discrete_arithmetic_expression_rec = function
        | Parsed_DAE_term term -> bool_expression_of_parsed_term term
        | _ -> raise (InvalidModel) (* can only happen if type checking fail *)

    and bool_expression_of_parsed_term = function
        | Parsed_DT_factor factor -> bool_expression_of_parsed_factor factor
        | _ -> raise (InvalidModel) (* can only happen if type checking fail *)

    and bool_expression_of_parsed_factor = function
        | Parsed_DF_expression expr -> bool_expression_of_parsed_discrete_arithmetic_expression_rec expr

        | Parsed_DF_variable variable_name ->
            let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
            (match variable_kind with
            | Constant_kind value -> DB_constant (DiscreteValue.bool_value value)
            | Variable_kind discrete_index -> DB_variable discrete_index
            )
        | Parsed_DF_constant var_value ->
            let bool_value = DiscreteValue.bool_value var_value in
            DB_constant bool_value
        | Parsed_DF_access (factor, index_expr) ->
            Bool_access (
                expression_access_type_of_parsed_df_access variable_infos factor,
                int_arithmetic_expression_of_parsed_arithmetic_expression variable_infos index_expr
            )
        | Parsed_function_call (variable, argument_expressions) ->
            let name = ParsingStructureUtilities.function_name_of_parsed_factor variable in
            bool_expression_of_parsed_function_call variable_infos argument_expressions name
        (* Should never happen, because it was checked by type checker before *)
        | _ as factor ->
            raise (InternalError (
                "There is a call to \""
                ^ ParsingStructureUtilities.string_of_parsed_factor variable_infos factor
                ^ "\" in an boolean expression, although it was checked before by type checking. Maybe something fail in type checking"
            ))
    in
    bool_expression_of_parsed_discrete_arithmetic_expression_rec expr

and bool_expression_of_parsed_function_call variable_infos argument_expressions = function
    | "list_hd" ->
        let arg_0 = List.nth argument_expressions 0 in
        Bool_list_hd (
            list_expression_of_parsed_boolean_expression variable_infos arg_0
        )
    | "list_mem" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in
        List_mem (
            global_expression_of_parsed_boolean_expression variable_infos arg_0,
            list_expression_of_parsed_boolean_expression variable_infos arg_1
        )
    (* TODO benjamin, in the future replace raise by custom function call as comment below *)
    | function_name -> raise (UndefinedFunction function_name)
    (*
    Bool_function_call (
        name,
        List.map (global_expression_of_parsed_boolean_expression variable_infos) argument_expressions
    )
    *)

and arithmetic_expression_of_parsed_discrete_boolean_expression variable_infos = function
    | Parsed_arithmetic_expression expr -> arithmetic_expression_of_parsed_arithmetic_expression variable_infos expr
    | _ -> raise (InternalError "Unable to create an arithmetic expression from another constructor of `discrete_boolean_expression` than `Parsed_arithmetic_expression`")

(* Convert a parsed discrete arithmetic expression *)
(* to a rational arithmetic expression or an int arithmetic expression *)
(* according to the type of the parsed discrete arithmetic expression *)
and arithmetic_expression_of_parsed_arithmetic_expression variable_infos expr =

    (* Get the deduced type of the parsed arithmetic expression *)
    let discrete_type = TypeChecker2.type_check_parsed_discrete_arithmetic_expression variable_infos expr in
    let discrete_type = default_type_if_needed discrete_type in

    match discrete_type with
    | Var_type_discrete_number Var_type_discrete_rational ->
        Rational_arithmetic_expression (rational_expression_of_parsed_discrete_arithmetic_expression variable_infos expr)
    | Var_type_discrete_number Var_type_discrete_int ->
        Int_arithmetic_expression (int_arithmetic_expression_of_parsed_arithmetic_expression variable_infos expr)

    (* Other cases mean that type checking has failed *)
    | Var_type_discrete_bool
    | Var_type_discrete_binary_word _
    | Var_type_discrete_array _
    | Var_type_discrete_list _ as t ->
        raise (InternalError ("An arithmetic expression was deduced as " ^ string_of_var_type_discrete t ^ " expression, maybe type checking has failed before"))
    | Var_type_discrete_number Var_type_discrete_unknown_number ->
        raise (InternalError "An arithmetic expression still contains unknown literal numbers after type checking, maybe type checking has failed before")

(* Get typed rational expression of global parsed expression *)
(* Extract arithmetic expression from parsed_discrete_boolean_expression *)
and rational_expression_of_parsed_boolean_expression variable_infos = function
        | Parsed_Discrete_boolean_expression expr -> rational_expression_of_parsed_discrete_boolean_expression variable_infos expr
        | _ -> raise (InvalidModel) (* can only happen if type checking fail *)

and rational_expression_of_parsed_discrete_boolean_expression variable_infos = function
        | Parsed_arithmetic_expression expr ->
            rational_expression_of_parsed_discrete_arithmetic_expression variable_infos expr
        | _ -> raise (InvalidModel) (* can only happen if type checking fail *)

(* Convert a parsed discrete arithmetic expression to a rational arithmetic expression *)
and rational_expression_of_parsed_discrete_arithmetic_expression variable_infos (* expr *) =
    let rec rational_expression_of_parsed_discrete_arithmetic_expression_rec = function
        | Parsed_DAE_plus (expr , term) ->
            DAE_plus (
                (rational_expression_of_parsed_discrete_arithmetic_expression_rec expr),
                (rational_expression_of_parsed_term term)
            )
        | Parsed_DAE_minus (expr , term) ->
            DAE_minus (
                (rational_expression_of_parsed_discrete_arithmetic_expression_rec expr),
                (rational_expression_of_parsed_term term)
            )
        | Parsed_DAE_term term ->
            DAE_term (rational_expression_of_parsed_term term)

    and rational_expression_of_parsed_term = function
        | Parsed_DT_mul (term , factor) ->
            DT_mul (
                (rational_expression_of_parsed_term term),
                (rational_expression_of_parsed_factor factor)
            )
        | Parsed_DT_div (term, factor) ->
            DT_div (
                (rational_expression_of_parsed_term term) ,
                (rational_expression_of_parsed_factor factor)
            )
        | Parsed_DT_factor factor ->
            DT_factor (rational_expression_of_parsed_factor factor)

    and rational_expression_of_parsed_factor = function
        | Parsed_DF_variable variable_name ->
            let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
            (match variable_kind with
            | Constant_kind value -> DF_constant (DiscreteValue.to_numconst_value value)
            | Variable_kind discrete_index -> DF_variable discrete_index
            )
        | Parsed_DF_constant var_value ->
            DF_constant (DiscreteValue.to_numconst_value var_value)

        | Parsed_DF_access (factor, index_expr) ->

            Rational_access (
                expression_access_type_of_parsed_df_access variable_infos factor,
                int_arithmetic_expression_of_parsed_arithmetic_expression variable_infos index_expr
            )

        | Parsed_DF_expression expr -> DF_expression (rational_expression_of_parsed_discrete_arithmetic_expression_rec expr)
        | Parsed_rational_of_int_function expr -> DF_rational_of_int (int_arithmetic_expression_of_parsed_arithmetic_expression variable_infos expr)
        | Parsed_pow_function (expr, exp) -> Rational_pow (rational_expression_of_parsed_discrete_arithmetic_expression_rec expr, int_arithmetic_expression_of_parsed_arithmetic_expression variable_infos exp)
        | Parsed_DF_unary_min factor -> DF_unary_min (rational_expression_of_parsed_factor factor)
        | Parsed_function_call (variable, argument_expressions) ->
            let name = ParsingStructureUtilities.function_name_of_parsed_factor variable in
            rational_expression_of_parsed_function_call variable_infos argument_expressions name

        (* Should never happen, because it was checked by type checker before *)
        | _ as factor ->
            raise (InternalError (
                "There is a call to \""
                ^ ParsingStructureUtilities.string_of_parsed_factor variable_infos factor
                ^ "\" in an rational expression, although it was checked before by type checking. Maybe something fail in type checking"
            ))
    in
    rational_expression_of_parsed_discrete_arithmetic_expression_rec

and rational_expression_of_parsed_function_call variable_infos argument_expressions = function
    | "pow" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in
        Rational_pow (
            rational_expression_of_parsed_boolean_expression variable_infos arg_0,
            int_expression_of_parsed_boolean_expression variable_infos arg_1
        )
    | "rational_of_int" ->
        let arg_0 = List.nth argument_expressions 0 in
        DF_rational_of_int (
            int_expression_of_parsed_boolean_expression variable_infos arg_0
        )
    | "list_hd" ->
        let arg_0 = List.nth argument_expressions 0 in
        Rational_list_hd (
            list_expression_of_parsed_boolean_expression variable_infos arg_0
        )
    (* TODO benjamin, in the future replace raise by custom function call as comment below *)
    | function_name -> raise (UndefinedFunction function_name)
    (*
    Rational_function_call (
        name,
        List.map (global_expression_of_parsed_boolean_expression variable_infos) argument_expressions
    )
    *)

(* Get typed int expression of global parsed expression *)
(* Extract arithmetic expression from parsed_discrete_boolean_expression *)
and int_expression_of_parsed_boolean_expression variable_infos = function
        | Parsed_Discrete_boolean_expression expr -> int_expression_of_parsed_discrete_boolean_expression variable_infos expr
        | _ -> raise (InvalidModel) (* can only happen if type checking fail *)

and int_expression_of_parsed_discrete_boolean_expression variable_infos = function
    | Parsed_arithmetic_expression expr ->
        int_arithmetic_expression_of_parsed_arithmetic_expression variable_infos expr
    | _ -> raise (InvalidModel) (* can only happen if type checking fail *)

(* Convert a parsed discrete arithmetic expression to an int arithmetic expression *)
and int_arithmetic_expression_of_parsed_arithmetic_expression variable_infos (* expr *) =
    let rec int_arithmetic_expression_of_parsed_arithmetic_expression_rec = function
        | Parsed_DAE_plus (expr , term) ->
            Int_plus (
                (int_arithmetic_expression_of_parsed_arithmetic_expression_rec expr),
                (int_arithmetic_expression_of_parsed_term term)
            )
        | Parsed_DAE_minus (expr , term) ->
            Int_minus (
                (int_arithmetic_expression_of_parsed_arithmetic_expression_rec expr),
                (int_arithmetic_expression_of_parsed_term term)
            )
        | Parsed_DAE_term term ->
            Int_term (int_arithmetic_expression_of_parsed_term term)

    and int_arithmetic_expression_of_parsed_term = function
        | Parsed_DT_mul (term , factor) ->
            Int_mul (
                (int_arithmetic_expression_of_parsed_term term),
                (int_arithmetic_expression_of_parsed_factor factor)
            )
        | Parsed_DT_div (term, factor) as div ->

            ImitatorUtilities.print_message Verbose_low (
                "Non-integer division of type int may happen in the following expression `"
                ^ ParsingStructureUtilities.string_of_parsed_term variable_infos div
                ^ "`: this may lead to a truncated integer (rounded down), with a potentially invalid result. This will be checked at runtime, and a warning will be triggered if this happens."
            );

            Int_div (
                (int_arithmetic_expression_of_parsed_term term) ,
                (int_arithmetic_expression_of_parsed_factor factor)
            )
        | Parsed_DT_factor factor ->
            Int_factor (int_arithmetic_expression_of_parsed_factor factor)

    and int_arithmetic_expression_of_parsed_factor = function
        | Parsed_DF_variable variable_name ->

            let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
            (match variable_kind with
            | Constant_kind value -> Int_constant (DiscreteValue.to_int_value value)
            | Variable_kind discrete_index -> Int_variable discrete_index
            )

        | Parsed_DF_constant var_value -> Int_constant (DiscreteValue.to_int_value var_value)
        | Parsed_DF_expression expr -> Int_expression (int_arithmetic_expression_of_parsed_arithmetic_expression_rec expr)
        | Parsed_pow_function (expr, exp) -> Int_pow (int_arithmetic_expression_of_parsed_arithmetic_expression_rec expr, int_arithmetic_expression_of_parsed_arithmetic_expression_rec exp)
        | Parsed_DF_unary_min factor -> Int_unary_min (int_arithmetic_expression_of_parsed_factor factor)
        | Parsed_DF_access (factor, index_expr) ->

            Int_access (
                expression_access_type_of_parsed_df_access variable_infos factor,
                int_arithmetic_expression_of_parsed_arithmetic_expression variable_infos index_expr
            )
        | Parsed_function_call (variable, argument_expressions) ->
            let name = ParsingStructureUtilities.function_name_of_parsed_factor variable in
            int_arithmetic_expression_of_parsed_function_call variable_infos argument_expressions name

        (* Should never happen, because it was checked by type checker before *)
        | _ as factor ->
            raise (InternalError (
                "There is a call to \""
                ^ ParsingStructureUtilities.string_of_parsed_factor variable_infos factor
                ^ "\" in an int expression, although it was checked before by type checking. Maybe something fail in type checking"
            ))
    in
    int_arithmetic_expression_of_parsed_arithmetic_expression_rec

and int_arithmetic_expression_of_parsed_function_call variable_infos argument_expressions = function
    | "pow" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in
        Int_pow (
            int_expression_of_parsed_boolean_expression variable_infos arg_0,
            int_expression_of_parsed_boolean_expression variable_infos arg_1
        )
    | "list_hd" ->
        let arg_0 = List.nth argument_expressions 0 in
        Int_list_hd (
            list_expression_of_parsed_boolean_expression variable_infos arg_0
        )
    (* TODO benjamin, in the future replace raise by custom function call as comment below *)
    | function_name -> raise (UndefinedFunction function_name)
    (*
    Int_function_call (
        name,
        List.map (global_expression_of_parsed_boolean_expression variable_infos) argument_expressions
    )
    *)


(* Try to convert a parsed boolean expression to abstract binary word expression *)
and binary_word_expression_of_parsed_boolean_expression variable_infos = function
    (* A binary word can only be found in parsed factor *)
    | Parsed_Discrete_boolean_expression expr ->
        binary_word_expression_of_parsed_discrete_boolean_expression variable_infos expr
    (* Other cases mean that type checking has failed before *)
    | _ -> raise (InvalidModel) (* can only happen if type checking fail *)

(* Try to convert a parsed discrete boolean expression to abstract binary word expression *)
and binary_word_expression_of_parsed_discrete_boolean_expression variable_infos = function
    (* A binary word can only be found in parsed factor *)
    | Parsed_arithmetic_expression expr ->
        binary_word_expression_of_parsed_discrete_arithmetic_expression variable_infos expr
    (* Other cases mean that type checking has failed before *)
    | _ -> raise (InvalidModel) (* can only happen if type checking fail *)

(* Try to convert a parsed arithmetic expression to abstract binary word expression *)
and binary_word_expression_of_parsed_discrete_arithmetic_expression variable_infos = function
    (* A binary word can only be found in parsed factor *)
    | Parsed_DAE_term term ->
        binary_word_expression_of_parsed_term variable_infos term
    (* Other cases mean that type checking has failed before *)
    | _ -> raise (InvalidModel) (* can only happen if type checking fail *)

(* Try to convert a parsed term to abstract binary word expression *)
and binary_word_expression_of_parsed_term variable_infos = function
    (* A binary word can only be found in parsed factor *)
    | Parsed_DT_factor factor ->
        binary_word_expression_of_parsed_factor variable_infos factor
    (* Other cases mean that type checking has failed before *)
    | _ -> raise (InvalidModel) (* can only happen if type checking fail *)

(* Try to convert a parsed factor to abstract binary word expression *)
and binary_word_expression_of_parsed_factor variable_infos factor =

    let discrete_type = TypeChecker2.type_check_parsed_discrete_factor variable_infos factor in
    let binary_word_length =
        match discrete_type with
        | DiscreteType.Var_type_discrete_binary_word length -> length
        | _ -> raise (InternalError "Binary word expression hold another type than binary word, although it was type checked before.")
    in

    let binary_word_expression_of_parsed_factor_inner = function
    | Parsed_DF_variable variable_name ->

        let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
        (match variable_kind with
        | Constant_kind value -> Binary_word_constant (DiscreteValue.binary_word_value value)
        | Variable_kind discrete_index -> Binary_word_variable (discrete_index, binary_word_length)
        )

    | Parsed_DF_constant value ->
        let binary_word_value = DiscreteValue.binary_word_value value in
        Binary_word_constant binary_word_value

    | Parsed_DF_access (factor, index_expr) ->
        Binary_word_access (
            expression_access_type_of_parsed_df_access variable_infos factor,
            int_arithmetic_expression_of_parsed_arithmetic_expression variable_infos index_expr,
            binary_word_length
        )

    | Parsed_shift_function (fun_type, factor, expr) ->
        let binary_word_expr = binary_word_expression_of_parsed_factor variable_infos factor in
        let int_expr = int_arithmetic_expression_of_parsed_arithmetic_expression variable_infos expr in

        (match fun_type with
        | Parsed_shift_left -> Logical_shift_left (binary_word_expr, int_expr, binary_word_length)
        | Parsed_shift_right -> Logical_shift_right (binary_word_expr, int_expr, binary_word_length)
        | Parsed_fill_left -> Logical_fill_left (binary_word_expr, int_expr, binary_word_length)
        | Parsed_fill_right -> Logical_fill_right (binary_word_expr, int_expr, binary_word_length)
        )
    | Parsed_bin_log_function (fun_type, l_factor, r_factor) ->
        let l_binary_word_expr = binary_word_expression_of_parsed_factor variable_infos l_factor in
        let r_binary_word_expr = binary_word_expression_of_parsed_factor variable_infos r_factor in

        (match fun_type with
        | Parsed_log_and -> Logical_and (l_binary_word_expr, r_binary_word_expr, binary_word_length)
        | Parsed_log_or -> Logical_or (l_binary_word_expr, r_binary_word_expr, binary_word_length)
        | Parsed_log_xor -> Logical_xor (l_binary_word_expr, r_binary_word_expr, binary_word_length)
        )
    | Parsed_log_not factor ->
        Logical_not (
            binary_word_expression_of_parsed_factor variable_infos factor,
            binary_word_length
        )
    | Parsed_function_call (variable, argument_expressions) ->
        let name = ParsingStructureUtilities.function_name_of_parsed_factor variable in
        binary_word_expression_of_parsed_function_call variable_infos binary_word_length argument_expressions name

    | Parsed_DF_expression expression ->
        binary_word_expression_of_parsed_discrete_arithmetic_expression variable_infos expression
    | _ as factor ->
        raise (InternalError (
            "There is a call to \""
            ^ ParsingStructureUtilities.string_of_parsed_factor variable_infos factor
            ^ "\" in a binary expression, although it was checked before by type checking. Maybe something fail in type checking"
        ))
    in
    binary_word_expression_of_parsed_factor_inner factor

and binary_word_expression_of_parsed_function_call variable_infos binary_word_length argument_expressions = function
    | "shift_left" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in

        Logical_shift_left (
            binary_word_expression_of_parsed_boolean_expression variable_infos arg_0,
            int_expression_of_parsed_boolean_expression variable_infos arg_1,
            binary_word_length
        )
    | "shift_right" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in

        Logical_shift_right (
            binary_word_expression_of_parsed_boolean_expression variable_infos arg_0,
            int_expression_of_parsed_boolean_expression variable_infos arg_1,
            binary_word_length
        )
    | "fill_left" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in

        Logical_fill_left (
            binary_word_expression_of_parsed_boolean_expression variable_infos arg_0,
            int_expression_of_parsed_boolean_expression variable_infos arg_1,
            binary_word_length
        )
    | "fill_right" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in

        Logical_fill_right (
            binary_word_expression_of_parsed_boolean_expression variable_infos arg_0,
            int_expression_of_parsed_boolean_expression variable_infos arg_1,
            binary_word_length
        )
    | "logand" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in

        Logical_and (
            binary_word_expression_of_parsed_boolean_expression variable_infos arg_0,
            binary_word_expression_of_parsed_boolean_expression variable_infos arg_1,
            binary_word_length
        )
    | "logor" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in

        Logical_or (
            binary_word_expression_of_parsed_boolean_expression variable_infos arg_0,
            binary_word_expression_of_parsed_boolean_expression variable_infos arg_1,
            binary_word_length
        )
    | "logxor" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in

        Logical_xor (
            binary_word_expression_of_parsed_boolean_expression variable_infos arg_0,
            binary_word_expression_of_parsed_boolean_expression variable_infos arg_1,
            binary_word_length
        )

    | "lognot" ->
        let arg_0 = List.nth argument_expressions 0 in

        Logical_not (
            binary_word_expression_of_parsed_boolean_expression variable_infos arg_0,
            binary_word_length
        )
    | "list_hd" ->
        let arg_0 = List.nth argument_expressions 0 in
        Binary_word_list_hd (
            list_expression_of_parsed_boolean_expression variable_infos arg_0
        )
    (* TODO benjamin, in the future replace raise by custom function call as comment below *)
    | function_name -> raise (UndefinedFunction function_name)
    (*
    Binary_word_function_call (
        name,
        List.map (global_expression_of_parsed_boolean_expression variable_infos) argument_expressions
    )
    *)


(* Try to convert a parsed boolean expression to abstract array expression *)
and array_expression_of_parsed_boolean_expression variable_infos inner_type = function
    (* A array can only be found in parsed factor *)
    | Parsed_Discrete_boolean_expression expr ->
        array_expression_of_parsed_discrete_boolean_expression variable_infos inner_type expr
    (* Other cases mean that type checking has failed before *)
    | _ -> raise (InvalidModel) (* can only happen if type checking fail *)

(* Try to convert a parsed discrete boolean expression to abstract array expression *)
and array_expression_of_parsed_discrete_boolean_expression variable_infos inner_type = function
    (* A array can only be found in parsed factor *)
    | Parsed_arithmetic_expression expr ->
        array_expression_of_parsed_discrete_arithmetic_expression variable_infos inner_type expr
    (* Other cases mean that type checking has failed before *)
    | _ -> raise (InvalidModel) (* can only happen if type checking fail *)

(* Try to convert a parsed arithmetic expression to abstract array expression *)
and array_expression_of_parsed_discrete_arithmetic_expression variable_infos inner_type = function
    (* A array can only be found in parsed factor *)
    | Parsed_DAE_term term ->
        array_expression_of_parsed_term variable_infos inner_type term
    (* Other cases mean that type checking has failed before *)
    | _ -> raise (InvalidModel) (* can only happen if type checking fail *)

(* Try to convert a parsed term to abstract array expression *)
and array_expression_of_parsed_term variable_infos inner_type = function
    (* A array can only be found in parsed factor *)
    | Parsed_DT_factor factor ->
        array_expression_of_parsed_factor variable_infos inner_type factor
    (* Other cases mean that type checking has failed before *)
    | _ -> raise (InvalidModel) (* can only happen if type checking fail *)

(* Try to convert a parsed factor to abstract array expression *)
and array_expression_of_parsed_factor variable_infos inner_type = function
    | Parsed_DF_variable variable_name ->

        let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
        (match variable_kind with
        | Constant_kind value ->

            (match value with
            | Array_value values ->
                Array_constant (Array.map (fun value -> DiscreteValue.convert_value_to_discrete_type value inner_type) values)
            (* TODO benjamin add message *)
            | _ -> raise (InternalError "")
            )
        | Variable_kind discrete_index ->

            Array_variable discrete_index
        )

    | Parsed_DF_array expr_array ->
        Literal_array (Array.map (fun expr -> bob variable_infos expr inner_type) expr_array)

    | Parsed_DF_access (factor, index_expr) ->
        Array_access (
            expression_access_type_of_parsed_df_access variable_infos factor,
            int_arithmetic_expression_of_parsed_arithmetic_expression variable_infos index_expr
        )

    | Parsed_array_append (factor_0, factor_1) ->
        Array_concat (
            array_expression_of_parsed_factor variable_infos inner_type factor_0,
            array_expression_of_parsed_factor variable_infos inner_type factor_1
        )
    | Parsed_function_call (variable, argument_expressions) ->
        let name = ParsingStructureUtilities.function_name_of_parsed_factor variable in
        array_expression_of_parsed_function_call variable_infos inner_type argument_expressions name

    | _ as factor ->
        raise (InternalError (
            "Use of \""
            ^ ParsingStructureUtilities.string_of_parsed_factor variable_infos factor
            ^ "\" in an array expression, although it was checked before by type checking. Maybe something fail in type checking"
        ))

and array_expression_of_parsed_function_call variable_infos inner_type argument_expressions = function
    | "array_append" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in
        Array_concat (
            array_expression_of_parsed_boolean_expression variable_infos inner_type arg_0,
            array_expression_of_parsed_boolean_expression variable_infos inner_type arg_1
        )
    | "list_hd" ->
        let arg_0 = List.nth argument_expressions 0 in
        Array_list_hd (
            list_expression_of_parsed_boolean_expression variable_infos arg_0
        )
    (* TODO benjamin, in the future replace raise by custom function call as comment below *)
    | function_name -> raise (UndefinedFunction function_name)
    (*
    Array_function_call (
        name,
        List.map (global_expression_of_parsed_boolean_expression variable_infos) argument_expressions
    )
    *)

(* Try to convert a parsed discrete boolean expression to abstract list expression *)
and list_expression_of_parsed_boolean_expression variable_infos = function
    (* A list can only be found in parsed factor *)
    | Parsed_Discrete_boolean_expression expr ->
        list_expression_of_parsed_discrete_boolean_expression variable_infos expr
    (* Other cases mean that type checking has failed before *)
    | _ -> raise (InvalidModel) (* can only happen if type checking fail *)

(* Try to convert a parsed discrete boolean expression to abstract list expression *)
and list_expression_of_parsed_discrete_boolean_expression variable_infos = function
    (* A list can only be found in parsed factor *)
    | Parsed_arithmetic_expression expr ->
        list_expression_of_parsed_discrete_arithmetic_expression variable_infos expr
    (* Other cases mean that type checking has failed before *)
    | _ -> raise (InvalidModel) (* can only happen if type checking fail *)

(* Try to convert a parsed arithmetic expression to abstract list expression *)
and list_expression_of_parsed_discrete_arithmetic_expression variable_infos = function
    (* A list can only be found in parsed factor *)
    | Parsed_DAE_term term ->
        list_expression_of_parsed_term variable_infos term
    (* Other cases mean that type checking has failed before *)
    | _ -> raise (InvalidModel) (* can only happen if type checking fail *)

(* Try to convert a parsed term to abstract list expression *)
and list_expression_of_parsed_term variable_infos = function
    (* A list can only be found in parsed factor *)
    | Parsed_DT_factor factor ->
        list_expression_of_parsed_factor variable_infos factor
    (* Other cases mean that type checking has failed before *)
    | _ -> raise (InvalidModel) (* can only happen if type checking fail *)

(* Try to convert a parsed factor to abstract list expression *)
and list_expression_of_parsed_factor variable_infos = function
    | Parsed_DF_variable variable_name ->

        let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
        (match variable_kind with
        | Constant_kind value -> List_constant (DiscreteValue.list_value value)
        | Variable_kind discrete_index -> List_variable discrete_index
        )

    | Parsed_DF_list expr_list ->
        Literal_list (List.map (fun expr -> convert_parsed_global_expression variable_infos (Parsed_global_expression expr)) expr_list)

    | Parsed_DF_access (factor, index_expr) ->
        List_access (
            expression_access_type_of_parsed_df_access variable_infos factor,
            int_arithmetic_expression_of_parsed_arithmetic_expression variable_infos index_expr
        )
    | Parsed_list_cons (expr, factor) ->
        List_cons (
            convert_parsed_boolean_expression variable_infos expr,
            list_expression_of_parsed_factor variable_infos factor
        )
    | Parsed_function_call (variable, argument_expressions) ->
        let name = ParsingStructureUtilities.function_name_of_parsed_factor variable in
        list_expression_of_parsed_function_call variable_infos argument_expressions name

    | _ as factor ->
        raise (InternalError (
            "Use of \""
            ^ ParsingStructureUtilities.string_of_parsed_factor variable_infos factor
            ^ "\" in a list expression, although it was checked before by type checking. Maybe something fail in type checking"
        ))

and list_expression_of_parsed_function_call variable_infos argument_expressions = function
    | "list_cons" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in
        List_cons (
            convert_parsed_boolean_expression variable_infos arg_0,
            list_expression_of_parsed_boolean_expression variable_infos arg_1
        )
    | "list_hd" ->
        let arg_0 = List.nth argument_expressions 0 in
        List_list_hd (
            list_expression_of_parsed_boolean_expression variable_infos arg_0
        )
    | "list_tl" ->
        let arg_0 = List.nth argument_expressions 0 in
        List_tl (
            list_expression_of_parsed_boolean_expression variable_infos arg_0
        )
    | "list_rev" ->
        let arg_0 = List.nth argument_expressions 0 in
        List_rev (
            list_expression_of_parsed_boolean_expression variable_infos arg_0
        )
    (* TODO benjamin, in the future replace raise by custom function call as comment below *)
    | function_name -> raise (UndefinedFunction function_name)
    (*
    List_function_call (
        name,
        List.map (global_expression_of_parsed_boolean_expression variable_infos) argument_expressions
    )
    *)

and expression_access_type_of_parsed_df_access variable_infos factor =
    (* Check discrete type for differentiate arrays and lists *)
    let discrete_type = TypeChecker2.type_check_parsed_discrete_factor variable_infos factor in

    match discrete_type with
    | Var_type_discrete_array (inner_type, _) ->
        Expression_array_access (array_expression_of_parsed_factor variable_infos inner_type factor)
    | Var_type_discrete_list _ ->
        Expression_list_access (list_expression_of_parsed_factor variable_infos factor)
    | _ ->
        raise (InternalError
            "An access on other element than an array or a list was found, although it was been type checked before."
        )

*)