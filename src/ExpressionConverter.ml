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


(* Convert a parsed global expression to an abstract model expression *)
let rec convert_parsed_global_expression variable_infos = function
    | Parsed_global_expression expr as global_expr ->
        (* TYPE CHECK *)
        let discrete_type = TypeChecker.discrete_type_of_expression variable_infos global_expr in

        match discrete_type with
        | DiscreteType.Var_type_discrete_bool ->
            Bool_expression (bool_expression_of_parsed_boolean_expression variable_infos expr)
        | DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_rational ->
            Arithmetic_expression (Rational_arithmetic_expression (rational_expression_of_parsed_boolean_expression variable_infos expr))
        | DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_int ->
            Arithmetic_expression (Int_arithmetic_expression (int_expression_of_parsed_boolean_expression variable_infos expr))
        | DiscreteType.Var_type_discrete_binary_word _ ->
            Binary_word_expression (binary_word_expression_of_parsed_boolean_expression variable_infos expr)
        | DiscreteType.Var_type_discrete_array _ ->
            Array_expression (array_expression_of_parsed_boolean_expression variable_infos expr)
        | DiscreteType.Var_type_discrete_list _ ->
            List_expression (list_expression_of_parsed_boolean_expression variable_infos expr)
        (* Should never happen *)
        | DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_unknown_number ->
            raise (InternalError "An expression still contains unknown literal number after type checking")

and global_expression_of_parsed_boolean_expression variable_infos expr =
    convert_parsed_global_expression variable_infos (Parsed_global_expression expr)

and convert_parsed_boolean_expression variable_infos expr =
        (* TYPE CHECK *)
        let discrete_type = TypeChecker.discrete_type_of_parsed_boolean_expression variable_infos expr in

        match discrete_type with
        | DiscreteType.Var_type_discrete_bool ->
            Bool_expression (bool_expression_of_parsed_boolean_expression variable_infos expr)
        | DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_rational ->
            Arithmetic_expression (Rational_arithmetic_expression (rational_expression_of_parsed_boolean_expression variable_infos expr))
        | DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_int ->
            Arithmetic_expression (Int_arithmetic_expression (int_expression_of_parsed_boolean_expression variable_infos expr))
        | DiscreteType.Var_type_discrete_binary_word _ ->
            Binary_word_expression (binary_word_expression_of_parsed_boolean_expression variable_infos expr)
        | DiscreteType.Var_type_discrete_array _ ->
            Array_expression (array_expression_of_parsed_boolean_expression variable_infos expr)
        | DiscreteType.Var_type_discrete_list _ ->
            List_expression (list_expression_of_parsed_boolean_expression variable_infos expr)
        (* Should never happen *)
        | DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_unknown_number ->
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
        let t = TypeChecker.discrete_type_of_parsed_discrete_boolean_expression variable_infos l_expr in
        bool_expression_of_parsed_comparison variable_infos l_expr relop r_expr t

	| Parsed_expression_in (expr1, expr2, expr3) -> Expression_in (
		(arithmetic_expression_of_parsed_arithmetic_expression variable_infos expr1),
		(arithmetic_expression_of_parsed_arithmetic_expression variable_infos expr2),
		(arithmetic_expression_of_parsed_arithmetic_expression variable_infos expr3)
		)
    | Parsed_boolean_expression parsed_boolean_expression ->
        Boolean_expression (bool_expression_of_parsed_boolean_expression variable_infos parsed_boolean_expression)
	| Parsed_Not e ->
	    Not_bool (bool_expression_of_parsed_boolean_expression variable_infos e)

and bool_expression_of_parsed_comparison variable_infos l_expr relop r_expr = function
    | DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_rational
    | DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_int ->
        Expression (
            (arithmetic_expression_of_parsed_discrete_boolean_expression variable_infos l_expr),
            (convert_parsed_relop relop),
            (arithmetic_expression_of_parsed_discrete_boolean_expression variable_infos r_expr)
        )
    | DiscreteType.Var_type_discrete_bool ->
        Boolean_comparison (
            bool_expression_of_parsed_discrete_boolean_expression variable_infos l_expr,
            convert_parsed_relop relop,
            bool_expression_of_parsed_discrete_boolean_expression variable_infos r_expr
        )
    | DiscreteType.Var_type_discrete_binary_word l ->
        Binary_comparison (
            binary_word_expression_of_parsed_discrete_boolean_expression variable_infos l_expr,
            convert_parsed_relop relop,
            binary_word_expression_of_parsed_discrete_boolean_expression variable_infos r_expr
        )
    | DiscreteType.Var_type_discrete_array _ ->
        Array_comparison (
            array_expression_of_parsed_discrete_boolean_expression variable_infos l_expr,
            convert_parsed_relop relop,
            array_expression_of_parsed_discrete_boolean_expression variable_infos r_expr
        )
    | DiscreteType.Var_type_discrete_list _ ->
        List_comparison (
            list_expression_of_parsed_discrete_boolean_expression variable_infos l_expr,
            convert_parsed_relop relop,
            list_expression_of_parsed_discrete_boolean_expression variable_infos r_expr
        )
    | DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_unknown_number ->
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
    let discrete_type = TypeChecker.discrete_type_of_parsed_discrete_arithmetic_expression variable_infos expr in

    match discrete_type with
    | DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_rational ->
        Rational_arithmetic_expression (rational_expression_of_parsed_discrete_arithmetic_expression variable_infos expr)
    | DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_int ->
        Int_arithmetic_expression (int_arithmetic_expression_of_parsed_arithmetic_expression variable_infos expr)

    (* Other cases mean that type checking has failed *)
    | DiscreteType.Var_type_discrete_bool
    | DiscreteType.Var_type_discrete_binary_word _
    | DiscreteType.Var_type_discrete_array _
    | DiscreteType.Var_type_discrete_list _ as t ->
        raise (InternalError ("An arithmetic expression was deduced as " ^ DiscreteType.string_of_var_type_discrete t ^ " expression, maybe type checking has failed before"))
    | DiscreteType.Var_type_discrete_number DiscreteType.Var_type_discrete_unknown_number ->
        raise (InternalError "An arithmetic expression still contains unknown literal numbers after type checking, maybe type checking has failed before")

(* Get typed rational expression of global parsed expression *)
(* Extract arithmetic expression from parsed_discrete_boolean_expression *)
and rational_expression_of_parsed_boolean_expression variable_infos (* expr *) =

    let rec rational_expression_of_parsed_boolean_expression = function
        | Parsed_Discrete_boolean_expression expr -> rational_expression_of_parsed_discrete_boolean_expression expr
        | _ -> raise (InvalidModel) (* can only happen if type checking fail *)

    and rational_expression_of_parsed_discrete_boolean_expression = function
        | Parsed_arithmetic_expression expr ->
            rational_expression_of_parsed_discrete_arithmetic_expression variable_infos expr
        | _ -> raise (InvalidModel) (* can only happen if type checking fail *)
    in
    rational_expression_of_parsed_boolean_expression (* expr *)

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

        | Parsed_DF_access (factor, index_expr) ->

            Rational_access (
                expression_access_type_of_parsed_df_access variable_infos factor,
                int_arithmetic_expression_of_parsed_arithmetic_expression variable_infos index_expr
            )

        | Parsed_DF_constant var_value -> DF_constant (DiscreteValue.to_numconst_value var_value)
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
                ^ "\" in an boolean expression, although it was checked before by type checking. Maybe something fail in type checking"
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
and int_expression_of_parsed_boolean_expression variable_infos (* expr *) =

    let rec int_expression_of_parsed_boolean_expression = function
        | Parsed_Discrete_boolean_expression expr -> int_expression_of_parsed_discrete_boolean_expression expr
        | _ -> raise (InvalidModel) (* can only happen if type checking fail *)

    and int_expression_of_parsed_discrete_boolean_expression = function
        | Parsed_arithmetic_expression expr ->
            int_arithmetic_expression_of_parsed_arithmetic_expression variable_infos expr
        | _ -> raise (InvalidModel) (* can only happen if type checking fail *)
    in
    int_expression_of_parsed_boolean_expression (* expr *)

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
            | Constant_kind value -> Int_constant (DiscreteValue.int_value value)
            | Variable_kind discrete_index -> Int_variable discrete_index
            )

        | Parsed_DF_constant var_value -> Int_constant (DiscreteValue.int_value var_value)
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
                ^ "\" in an boolean expression, although it was checked before by type checking. Maybe something fail in type checking"
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

    let discrete_type = TypeChecker.discrete_type_of_parsed_discrete_factor variable_infos factor in
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
and array_expression_of_parsed_boolean_expression variable_infos = function
    (* A array can only be found in parsed factor *)
    | Parsed_Discrete_boolean_expression expr ->
        array_expression_of_parsed_discrete_boolean_expression variable_infos expr
    (* Other cases mean that type checking has failed before *)
    | _ -> raise (InvalidModel) (* can only happen if type checking fail *)

(* Try to convert a parsed discrete boolean expression to abstract array expression *)
and array_expression_of_parsed_discrete_boolean_expression variable_infos = function
    (* A array can only be found in parsed factor *)
    | Parsed_arithmetic_expression expr ->
        array_expression_of_parsed_discrete_arithmetic_expression variable_infos expr
    (* Other cases mean that type checking has failed before *)
    | _ -> raise (InvalidModel) (* can only happen if type checking fail *)

(* Try to convert a parsed arithmetic expression to abstract array expression *)
and array_expression_of_parsed_discrete_arithmetic_expression variable_infos = function
    (* A array can only be found in parsed factor *)
    | Parsed_DAE_term term ->
        array_expression_of_parsed_term variable_infos term
    (* Other cases mean that type checking has failed before *)
    | _ -> raise (InvalidModel) (* can only happen if type checking fail *)

(* Try to convert a parsed term to abstract array expression *)
and array_expression_of_parsed_term variable_infos = function
    (* A array can only be found in parsed factor *)
    | Parsed_DT_factor factor ->
        array_expression_of_parsed_factor variable_infos factor
    (* Other cases mean that type checking has failed before *)
    | _ -> raise (InvalidModel) (* can only happen if type checking fail *)

(* Try to convert a parsed factor to abstract array expression *)
and array_expression_of_parsed_factor variable_infos = function
    | Parsed_DF_variable variable_name ->

        let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
        (match variable_kind with
        | Constant_kind value -> Array_constant (DiscreteValue.array_value value)
        | Variable_kind discrete_index -> Array_variable discrete_index
        )

    | Parsed_DF_array expr_array ->
        Literal_array (Array.map (fun expr -> convert_parsed_global_expression variable_infos (Parsed_global_expression expr)) expr_array)

    | Parsed_DF_access (factor, index_expr) ->
        Array_access (
            expression_access_type_of_parsed_df_access variable_infos factor,
            int_arithmetic_expression_of_parsed_arithmetic_expression variable_infos index_expr
        )

    | Parsed_array_append (factor_0, factor_1) ->
        Array_concat (
            array_expression_of_parsed_factor variable_infos factor_0,
            array_expression_of_parsed_factor variable_infos factor_1
        )
    | Parsed_function_call (variable, argument_expressions) ->
        let name = ParsingStructureUtilities.function_name_of_parsed_factor variable in
        array_expression_of_parsed_function_call variable_infos argument_expressions name

    | _ as factor ->
        raise (InternalError (
            "Use of \""
            ^ ParsingStructureUtilities.string_of_parsed_factor variable_infos factor
            ^ "\" in an array expression, although it was checked before by type checking. Maybe something fail in type checking"
        ))

and array_expression_of_parsed_function_call variable_infos argument_expressions = function
    | "array_append" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in
        Array_concat (
            array_expression_of_parsed_boolean_expression variable_infos arg_0,
            array_expression_of_parsed_boolean_expression variable_infos arg_1
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
    let discrete_type = TypeChecker.discrete_type_of_parsed_discrete_factor variable_infos factor in

    match discrete_type with
    | Var_type_discrete_array _ ->
        Expression_array_access (array_expression_of_parsed_factor variable_infos factor)
    | Var_type_discrete_list _ ->
        Expression_list_access (list_expression_of_parsed_factor variable_infos factor)
    | _ ->
        raise (InternalError
            "An access on other element than an array or a list was found, although it was been type checked before."
        )
