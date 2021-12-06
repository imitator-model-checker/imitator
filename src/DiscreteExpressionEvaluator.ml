open DiscreteExpressions
open Exceptions

(* Get operator function from relop *)
let operator_of_relop = function
    | OP_L -> (<)
    | OP_LEQ -> (<=)
    | OP_EQ -> (=)
    | OP_NEQ -> (<>)
    | OP_GEQ -> (>=)
    | OP_G -> (>)

let rec eval_global_expression discrete_valuation = function
    | Arithmetic_expression expr -> eval_discrete_arithmetic_expression discrete_valuation expr
    | Bool_expression expr -> DiscreteValue.Bool_value (is_boolean_expression_satisfied discrete_valuation expr)
    | Binary_word_expression expr -> DiscreteValue.Binary_word_value (eval_binary_word_expression discrete_valuation expr)
    | Array_expression expr -> DiscreteValue.Array_value (eval_array_expression discrete_valuation expr)
    | List_expression expr -> DiscreteValue.List_value (eval_list_expression discrete_valuation expr)

and eval_discrete_arithmetic_expression discrete_valuation = function
    | Rational_arithmetic_expression expr ->
        DiscreteValue.Rational_value (eval_rational_expression discrete_valuation expr)
    | Int_arithmetic_expression expr ->
        DiscreteValue.Int_value (eval_int_expression discrete_valuation expr)

and eval_rational_expression discrete_valuation expr =
    let rec eval_rational_expression_rec = function
        | DAE_plus (expr, term) ->
            NumConst.add
                (eval_rational_expression_rec expr)
                (eval_rational_term term)
        | DAE_minus (expr, term) ->
            NumConst.sub
                (eval_rational_expression_rec expr)
                (eval_rational_term term)
        | DAE_term term ->
            eval_rational_term term

    and eval_rational_term = function
        | DT_mul (term, factor) ->
            NumConst.mul
            (eval_rational_term term)
            (eval_rational_factor factor)
        | DT_div (term, factor) ->
            let numerator	= (eval_rational_term term) in
            let denominator	= (eval_rational_factor factor) in

            (* Check for 0-denominator *)
            if NumConst.equal denominator NumConst.zero then(
                raise (Exceptions.Division_by_0 ("Division by 0 found when trying to perform " ^ (NumConst.to_string numerator) ^ " / " ^ (NumConst.to_string denominator) ^ ""))
            );

            (* Divide *)
            NumConst.div
                numerator
                denominator

        | DT_factor factor ->
            eval_rational_factor factor

    and eval_rational_factor = function
        | DF_variable variable_index ->
            DiscreteValue.numconst_value (discrete_valuation variable_index)
        | DF_constant variable_value ->
            variable_value;
        | Rational_access (access_type, index_expr) ->
            let value = get_expression_access_value discrete_valuation index_expr access_type in
            DiscreteValue.numconst_value value
        | DF_expression expr ->
            eval_rational_expression_rec expr
        | DF_rational_of_int expr ->
(*            ImitatorUtilities.print_message Verbose_standard "Evaluate a int expression";*)
            ImitatorUtilities.print_warning
                "Conversion of an int expression to a rational expression
                may cause overflow if your platform doesn't manage `int` as an exact 32 bits integer.";
            NumConst.numconst_of_int (Int32.to_int (eval_int_expression discrete_valuation expr))
        | Rational_pow (expr, exp) ->
            NumConst.pow (eval_rational_expression_rec expr) (eval_int_expression discrete_valuation exp)
        | Rational_list_hd (list_expr) ->
            let list = eval_list_expression discrete_valuation list_expr in
            let value = List.hd list in
            DiscreteValue.numconst_value value
        | DF_unary_min factor ->
            NumConst.neg (eval_rational_factor factor)
    in
    eval_rational_expression_rec expr

and eval_int_expression discrete_valuation (* expr *) =
    let rec eval_int_expression_rec = function
        | Int_plus (expr, term) ->
            Int32.add
                (eval_int_expression_rec expr)
                (eval_int_term term)
        | Int_minus (expr, term) ->
            Int32.sub
                (eval_int_expression_rec expr)
                (eval_int_term term)
        | Int_term term ->
            eval_int_term term

    and eval_int_term = function
        | Int_mul (term, factor) ->
            Int32.mul
                (eval_int_term term)
                (eval_int_factor factor)
        | Int_div (term, factor) ->
            let numerator	= (eval_int_term term) in
            let denominator	= (eval_int_factor factor) in

            (* Check for 0-denominator *)
            if Int32.equal denominator Int32.zero then (
                raise (Exceptions.Division_by_0 ("Division by 0 found when trying to perform " ^ (Int32.to_string numerator) ^ " / " ^ (Int32.to_string denominator) ^ ""))
            );

            (* Check for non-int division *)
            if OCamlUtilities.modulo numerator denominator <> Int32.zero then
                ImitatorUtilities.print_warning (
                    "Non-integer division of type int was spotted! This means that an int variable was rounded down to the nearest integer instead of a rational result (`"
                    ^ Int32.to_string numerator ^ " / " ^ Int32.to_string denominator
                    ^ "`). The overall result may now be invalid."
                );

            (* Divide *)
            Int32.div
                numerator
                denominator

        | Int_factor factor ->
            eval_int_factor factor

    and eval_int_factor = function
        | Int_variable variable_index ->
            DiscreteValue.int_value (discrete_valuation variable_index)
        | Int_constant variable_value ->
            variable_value;
        | Int_expression expr ->
            eval_int_expression_rec expr
        | Int_unary_min factor ->
            Int32.neg (eval_int_factor factor)
        | Int_access (access_type, index_expr) ->
            let value = get_expression_access_value discrete_valuation index_expr access_type in
            DiscreteValue.int_value value
        | Int_pow (expr, exp) ->
            OCamlUtilities.pow (eval_int_expression_rec expr) (eval_int_expression_rec exp)
        | Int_list_hd (list_expr) ->
            let list = eval_list_expression discrete_valuation list_expr in
            let value = List.hd list in
            DiscreteValue.int_value value
    in
    eval_int_expression_rec

(** Check if a boolean expression is satisfied *)
and is_boolean_expression_satisfied discrete_valuation = function
    | True_bool -> true
    | False_bool -> false
    | And_bool (b1, b2) -> (is_boolean_expression_satisfied discrete_valuation b1) && (is_boolean_expression_satisfied discrete_valuation b2) (* conjunction *)
    | Or_bool (b1, b2) -> (is_boolean_expression_satisfied discrete_valuation b1) || (is_boolean_expression_satisfied discrete_valuation b2) (* disjunction *)
    | Discrete_boolean_expression dbe -> check_discrete_boolean_expression discrete_valuation dbe

(** Check if a discrete boolean expression is satisfied *)
and check_discrete_boolean_expression discrete_valuation = function
    | DB_variable variable_index ->
        DiscreteValue.bool_value (discrete_valuation variable_index)
    | DB_constant value ->
        value
    | Bool_access (access_type, index_expr) ->
        let value = get_expression_access_value discrete_valuation index_expr access_type in
        DiscreteValue.bool_value value
    (** Discrete arithmetic expression of the form Expr ~ Expr *)
    (* TODO benjamin WARNING here we compare a DiscreteValue.discrete_value type with operator it's bad *)
    (* We just have to create a Rational_comparison and a Int_comparison to solve this *)
    | Expression (l_expr, relop, r_expr) ->
        (operator_of_relop relop)
            (eval_discrete_arithmetic_expression discrete_valuation l_expr)
            (eval_discrete_arithmetic_expression discrete_valuation r_expr)
    | Boolean_comparison (l_expr, relop, r_expr) ->
         (operator_of_relop relop)
             (check_discrete_boolean_expression discrete_valuation l_expr)
             (check_discrete_boolean_expression discrete_valuation r_expr)
    | Binary_comparison (l_expr, relop, r_expr) ->
        (operator_of_relop relop)
            (eval_binary_word_expression discrete_valuation l_expr)
            (eval_binary_word_expression discrete_valuation r_expr)
    | Array_comparison (l_expr, relop, r_expr) ->
        (operator_of_relop relop)
            (eval_array_expression discrete_valuation l_expr)
            (eval_array_expression discrete_valuation r_expr)
    | List_comparison (l_expr, relop, r_expr) ->
        (operator_of_relop relop)
            (eval_list_expression discrete_valuation l_expr)
            (eval_list_expression discrete_valuation r_expr)

    (** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
    | Expression_in (discrete_arithmetic_expression_1, discrete_arithmetic_expression_2, discrete_arithmetic_expression_3) ->
        (* Compute the first one to avoid redundancy *)
        let expr1_evaluated = eval_discrete_arithmetic_expression discrete_valuation discrete_arithmetic_expression_1 in
            (eval_discrete_arithmetic_expression discrete_valuation discrete_arithmetic_expression_2)
            <=
            expr1_evaluated
            &&
            expr1_evaluated
            <=
            (eval_discrete_arithmetic_expression discrete_valuation discrete_arithmetic_expression_3)
    | Boolean_expression boolean_expression ->
        is_boolean_expression_satisfied discrete_valuation boolean_expression
    | Not_bool b ->
        not (is_boolean_expression_satisfied discrete_valuation b) (* negation *)
    | Bool_list_hd list_expr ->
        let list = eval_list_expression discrete_valuation list_expr in
        let value = List.hd list in
        DiscreteValue.bool_value value
    | List_mem (expr, list_expr) ->
        let value = eval_global_expression discrete_valuation expr in
        let list = eval_list_expression discrete_valuation list_expr in
        List.mem value list

and eval_binary_word_expression discrete_valuation = function
    | Logical_shift_left (binary_word, expr, _) ->
        BinaryWord.shift_left
            (eval_binary_word_expression discrete_valuation binary_word)
            (Int32.to_int (eval_int_expression discrete_valuation expr))
    | Logical_shift_right (binary_word, expr, _) ->
        BinaryWord.shift_right
            (eval_binary_word_expression discrete_valuation binary_word)
            (Int32.to_int (eval_int_expression discrete_valuation expr))
    | Logical_fill_left (binary_word, expr, _) ->
        BinaryWord.fill_left
            (eval_binary_word_expression discrete_valuation binary_word)
            (Int32.to_int (eval_int_expression discrete_valuation expr))
    | Logical_fill_right (binary_word, expr, _) ->
        BinaryWord.fill_right
            (eval_binary_word_expression discrete_valuation binary_word)
            (Int32.to_int (eval_int_expression discrete_valuation expr))
    | Logical_and (l_binary_word, r_binary_word, _) ->
        BinaryWord.log_and
            (eval_binary_word_expression discrete_valuation l_binary_word)
            (eval_binary_word_expression discrete_valuation r_binary_word)
    | Logical_or (l_binary_word, r_binary_word, _) ->
        BinaryWord.log_or
            (eval_binary_word_expression discrete_valuation l_binary_word)
            (eval_binary_word_expression discrete_valuation r_binary_word)
    | Logical_xor (l_binary_word, r_binary_word, _) ->
        BinaryWord.log_xor
            (eval_binary_word_expression discrete_valuation l_binary_word)
            (eval_binary_word_expression discrete_valuation r_binary_word)
    | Logical_not (binary_word, _) ->
        BinaryWord.log_not
            (eval_binary_word_expression discrete_valuation binary_word)

    | Binary_word_constant value -> value
    | Binary_word_variable (variable_index, _) ->
        DiscreteValue.binary_word_value (discrete_valuation variable_index)

    | Binary_word_access (access_type, index_expr, _) ->
        let value = get_expression_access_value discrete_valuation index_expr access_type in
        DiscreteValue.binary_word_value value
    | Binary_word_list_hd list_expr ->
        let list = eval_list_expression discrete_valuation list_expr in
        DiscreteValue.binary_word_value (List.hd list)

and eval_array_expression discrete_valuation = function
    | Literal_array array ->
        Array.map (fun expr -> eval_global_expression discrete_valuation expr) array
    | Array_variable variable_index ->
        DiscreteValue.array_value (discrete_valuation variable_index)
    | Array_constant values ->
        values
    | Array_access (access_type, index_expr) ->
        let value = get_expression_access_value discrete_valuation index_expr access_type in
        DiscreteValue.array_value value
    | Array_concat (array_expr_0, array_expr_1) ->
        let array_0 = eval_array_expression discrete_valuation array_expr_0 in
        let array_1 = eval_array_expression discrete_valuation array_expr_1 in
        Array.append array_0 array_1
    | Array_list_hd list_expr ->
        let list = eval_list_expression discrete_valuation list_expr in
        DiscreteValue.array_value (List.hd list)

and eval_list_expression discrete_valuation = function
    | Literal_list list ->
        List.map (fun expr -> eval_global_expression discrete_valuation expr) list
    | List_variable variable_index ->
        DiscreteValue.list_value (discrete_valuation variable_index)
    | List_constant values ->
        values
    | List_access (access_type, index_expr) ->
        let value = get_expression_access_value discrete_valuation index_expr access_type in
        DiscreteValue.list_value value
    | List_cons (expr, list_expr) ->
        let list = eval_list_expression discrete_valuation list_expr in
        let value = eval_global_expression discrete_valuation expr in
        value :: list
    | List_list_hd list_expr ->
        let list = eval_list_expression discrete_valuation list_expr in
        DiscreteValue.list_value (List.hd list)
    | List_tl list_expr ->
        let list = eval_list_expression discrete_valuation list_expr in
        List.tl list
    | List_rev list_expr ->
        let list = eval_list_expression discrete_valuation list_expr in
        List.rev list

and get_array_value_at discrete_valuation array_expr index_expr =

    let values = eval_array_expression discrete_valuation array_expr in
    let index = eval_int_expression discrete_valuation index_expr in
    let int_index = Int32.to_int index in

    if int_index >= Array.length values || int_index < 0 then (
        let str_index = string_of_int int_index in
        let str_values = OCamlUtilities.string_of_array_of_string_with_sep ", " (Array.map (fun value -> DiscreteValue.string_of_value value) values) in
        raise (Out_of_bound ("Array index out of range: `" ^ str_index ^ "` for array " ^ str_values))
    );

    Array.get values int_index

and get_list_value_at discrete_valuation array_expr index_expr =

    let values = eval_list_expression discrete_valuation array_expr in
    let index = eval_int_expression discrete_valuation index_expr in
    let int_index = Int32.to_int index in

    if int_index >= List.length values || int_index < 0 then (
        let str_index = string_of_int int_index in
        let str_values = OCamlUtilities.string_of_list_of_string_with_sep ", " (List.map (fun value -> DiscreteValue.string_of_value value) values) in
        raise (Out_of_bound ("List index out of range: `" ^ str_index ^ "` for list " ^ str_values))
    );

    List.nth values int_index

and get_expression_access_value discrete_valuation index_expr = function
    | Expression_array_access array_expr ->
        get_array_value_at discrete_valuation array_expr index_expr
    | Expression_list_access list_expr ->
        get_list_value_at discrete_valuation list_expr index_expr


(* Wrap a scalar value to an array value in function of the modified index of an old value *)
(* For example old_value[0] = 1 with old value = [0, 1] would wrap new_value into an array as new_value = [1, 1] *)
(* This function is used to assign an element of an array at a given index *)
(* a = [[[1, 2], [3, 4]], [[5, 6], [7, 8]]] *)
(* a[1][1][0] = 0 *)
(* new_value = [[[1, 2], [3, 4]], [[5, 6], [0, 8]]] *)
(* a[1] = [[5, 6], [7, 8]] *)
(* a[1][1] = [7, 8] *)
(* a[1][1][0] = 7 *)
let pack_value variable_names discrete_valuation old_value new_value variable_access =

    let rec pack_value_rec = function
        | Discrete_variable_index discrete_index -> old_value, [||], None
        | Discrete_variable_access (inner_variable_access, index_expr) ->

            let old_value, _, _ = pack_value_rec inner_variable_access in

            (* Compute index *)
            let index = Int32.to_int (eval_int_expression discrete_valuation index_expr) in
(*            ImitatorUtilities.print_message Verbose_standard ("access index: " ^ string_of_int index ^ "for " ^ DiscreteValue.string_of_value old_value);*)
            (* Get inner array of discrete value of old value *)
            let old_array = DiscreteValue.array_value old_value in

            (* Check bounds *)
            if index >= Array.length old_array || index < 0 then (
                let str_variable_access = DiscreteExpressions.string_of_discrete_variable_access variable_names variable_access in
                raise (Out_of_bound ("Array index out of range: `" ^ str_variable_access ^ "`"))
            );

            (* Get element at given index *)
            let unpacked_old_array = old_array.(index) in
(*            ImitatorUtilities.print_message Verbose_standard ("unpacked old array: " ^ DiscreteValue.string_of_value unpacked_old_array);*)
            unpacked_old_array, old_array, Some index
    in
    let unpacked_old_array, old_array, some_index = pack_value_rec variable_access in
    match some_index with
    | Some index ->
        old_array.(index) <- new_value;
(*        ImitatorUtilities.print_message Verbose_standard ("packed new value is: " ^ DiscreteValue.string_of_value old_value);*)
        old_value
    | None -> new_value




let rec try_reduce_global_expression = function
    | Arithmetic_expression expr -> try_reduce_discrete_arithmetic_expression expr
    | Bool_expression expr -> DiscreteValue.Bool_value (try_reduce_boolean_expression expr)
    | Binary_word_expression expr -> DiscreteValue.Binary_word_value (try_reduce_binary_word_expression expr)
    | Array_expression expr -> DiscreteValue.Array_value (try_reduce_array_expression expr)
    | List_expression expr -> DiscreteValue.List_value (try_reduce_list_expression expr)

and try_reduce_discrete_arithmetic_expression = function
    | Rational_arithmetic_expression expr ->
        DiscreteValue.Rational_value (try_reduce_rational_expression expr)
    | Int_arithmetic_expression expr ->
        DiscreteValue.Int_value (try_reduce_int_expression expr)

and try_reduce_rational_expression expr =
    let rec try_reduce_rational_expression_rec = function
        | DAE_plus (expr, term) ->
            NumConst.add
                (try_reduce_rational_expression_rec expr)
                (try_reduce_rational_term term)
        | DAE_minus (expr, term) ->
            NumConst.sub
                (try_reduce_rational_expression_rec expr)
                (try_reduce_rational_term term)
        | DAE_term term ->
            try_reduce_rational_term term

    and try_reduce_rational_term = function
        | DT_mul (term, factor) ->
            NumConst.mul
            (try_reduce_rational_term term)
            (try_reduce_rational_factor factor)
        | DT_div (term, factor) ->
            let numerator	= (try_reduce_rational_term term) in
            let denominator	= (try_reduce_rational_factor factor) in

            (* Check for 0-denominator *)
            if NumConst.equal denominator NumConst.zero then(
                raise (Exceptions.Division_by_0 ("Division by 0 found when trying to perform " ^ (NumConst.to_string numerator) ^ " / " ^ (NumConst.to_string denominator) ^ ""))
            );

            (* Divide *)
            NumConst.div
                numerator
                denominator

        | DT_factor factor ->
            try_reduce_rational_factor factor

    and try_reduce_rational_factor = function
        | DF_variable variable_index ->
            raise (InternalError ("Unable to reduce a non-constant expression."))
        | DF_constant variable_value ->
            variable_value
        | Rational_access (access_type, index_expr) ->
            let value = try_reduce_expression_access_value index_expr access_type in
            DiscreteValue.numconst_value value
        | DF_expression expr ->
            try_reduce_rational_expression_rec expr
        | DF_rational_of_int expr ->
(*            ImitatorUtilities.print_message Verbose_standard "Evaluate a int expression";*)
            ImitatorUtilities.print_warning
                "Conversion of an int expression to a rational expression
                may cause overflow if your platform doesn't manage `int` as an exact 32 bits integer.";
            NumConst.numconst_of_int (Int32.to_int (try_reduce_int_expression expr))
        | Rational_pow (expr, exp) ->
            NumConst.pow (try_reduce_rational_expression_rec expr) (try_reduce_int_expression exp)
        | Rational_list_hd (list_expr) ->
            let list = try_reduce_list_expression list_expr in
            let value = List.hd list in
            DiscreteValue.numconst_value value
        | DF_unary_min factor ->
            NumConst.neg (try_reduce_rational_factor factor)
    in
    try_reduce_rational_expression_rec expr

and try_reduce_int_expression expr =
    let rec try_reduce_int_expression_rec = function
        | Int_plus (expr, term) ->
            Int32.add
                (try_reduce_int_expression_rec expr)
                (try_reduce_int_term term)
        | Int_minus (expr, term) ->
            Int32.sub
                (try_reduce_int_expression_rec expr)
                (try_reduce_int_term term)
        | Int_term term ->
            try_reduce_int_term term

    and try_reduce_int_term = function
        | Int_mul (term, factor) ->
            Int32.mul
                (try_reduce_int_term term)
                (try_reduce_int_factor factor)
        | Int_div (term, factor) ->
            let numerator	= (try_reduce_int_term term) in
            let denominator	= (try_reduce_int_factor factor) in

            (* Check for 0-denominator *)
            if Int32.equal denominator Int32.zero then (
                raise (Exceptions.Division_by_0 ("Division by 0 found when trying to perform " ^ (Int32.to_string numerator) ^ " / " ^ (Int32.to_string denominator) ^ ""))
            );

            (* Check for non-int division *)
            if OCamlUtilities.modulo numerator denominator <> Int32.zero then
                ImitatorUtilities.print_warning (
                    "Non-integer division of type int was spotted! This means that an int variable was rounded down to the nearest integer instead of a rational result (`"
                    ^ Int32.to_string numerator ^ " / " ^ Int32.to_string denominator
                    ^ "`). The overall result may now be invalid."
                );

            (* Divide *)
            Int32.div
                numerator
                denominator

        | Int_factor factor ->
            try_reduce_int_factor factor

    and try_reduce_int_factor = function
        | Int_variable variable_index ->
            raise (InternalError ("Unable to reduce a non-constant expression."))
        | Int_constant variable_value ->
            variable_value;
        | Int_expression expr ->
            try_reduce_int_expression_rec expr
        | Int_unary_min factor ->
            Int32.neg (try_reduce_int_factor factor)
        | Int_access (access_type, index_expr) ->
            let value = try_reduce_expression_access_value index_expr access_type in
            DiscreteValue.int_value value
        | Int_pow (expr, exp) ->
            OCamlUtilities.pow (try_reduce_int_expression_rec expr) (try_reduce_int_expression_rec exp)
        | Int_list_hd (list_expr) ->
            let list = try_reduce_list_expression list_expr in
            let value = List.hd list in
            DiscreteValue.int_value value
    in
    try_reduce_int_expression_rec expr

(** Check if a boolean expression is satisfied *)
and try_reduce_boolean_expression = function
    | True_bool -> true
    | False_bool -> false
    | And_bool (b1, b2) -> (try_reduce_boolean_expression b1) && (try_reduce_boolean_expression b2) (* conjunction *)
    | Or_bool (b1, b2) -> (try_reduce_boolean_expression b1) || (try_reduce_boolean_expression b2) (* disjunction *)
    | Discrete_boolean_expression dbe -> try_reduce_discrete_boolean_expression dbe

(** Check if a discrete boolean expression is satisfied *)
and try_reduce_discrete_boolean_expression = function
    | DB_variable variable_index ->
        raise (InternalError ("Unable to reduce a non-constant expression."))
    | DB_constant value ->
        value
    | Bool_access (access_type, index_expr) ->
        let value = try_reduce_expression_access_value index_expr access_type in
        DiscreteValue.bool_value value
    (** Discrete arithmetic expression of the form Expr ~ Expr *)
    (* TODO benjamin WARNING here we compare a DiscreteValue.discrete_value type with operator it's bad *)
    (* We just have to create a Rational_comparison and a Int_comparison to solve this *)
    | Expression (l_expr, relop, r_expr) ->
        (operator_of_relop relop)
            (try_reduce_discrete_arithmetic_expression l_expr)
            (try_reduce_discrete_arithmetic_expression r_expr)
    | Boolean_comparison (l_expr, relop, r_expr) ->
         (operator_of_relop relop)
             (try_reduce_discrete_boolean_expression l_expr)
             (try_reduce_discrete_boolean_expression r_expr)
    | Binary_comparison (l_expr, relop, r_expr) ->
        (operator_of_relop relop)
            (try_reduce_binary_word_expression l_expr)
            (try_reduce_binary_word_expression r_expr)
    | Array_comparison (l_expr, relop, r_expr) ->
        (operator_of_relop relop)
            (try_reduce_array_expression l_expr)
            (try_reduce_array_expression r_expr)
    | List_comparison (l_expr, relop, r_expr) ->
        (operator_of_relop relop)
            (try_reduce_list_expression l_expr)
            (try_reduce_list_expression r_expr)

    (** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
    | Expression_in (discrete_arithmetic_expression_1, discrete_arithmetic_expression_2, discrete_arithmetic_expression_3) ->
        (* Compute the first one to avoid redundancy *)
        let expr1_evaluated = try_reduce_discrete_arithmetic_expression discrete_arithmetic_expression_1 in
            (try_reduce_discrete_arithmetic_expression discrete_arithmetic_expression_2)
            <=
            expr1_evaluated
            &&
            expr1_evaluated
            <=
            (try_reduce_discrete_arithmetic_expression discrete_arithmetic_expression_3)
    | Boolean_expression boolean_expression ->
        try_reduce_boolean_expression boolean_expression
    | Not_bool b ->
        not (try_reduce_boolean_expression b) (* negation *)
    | Bool_list_hd list_expr ->
        let list = try_reduce_list_expression list_expr in
        let value = List.hd list in
        DiscreteValue.bool_value value
    | List_mem (expr, list_expr) ->
        let value = try_reduce_global_expression expr in
        let list = try_reduce_list_expression list_expr in
        List.mem value list

and try_reduce_binary_word_expression = function
    | Logical_shift_left (binary_word, expr, _) ->
        BinaryWord.shift_left
            (try_reduce_binary_word_expression binary_word)
            (Int32.to_int (try_reduce_int_expression expr))
    | Logical_shift_right (binary_word, expr, _) ->
        BinaryWord.shift_right
            (try_reduce_binary_word_expression binary_word)
            (Int32.to_int (try_reduce_int_expression expr))
    | Logical_fill_left (binary_word, expr, _) ->
        BinaryWord.fill_left
            (try_reduce_binary_word_expression binary_word)
            (Int32.to_int (try_reduce_int_expression expr))
    | Logical_fill_right (binary_word, expr, _) ->
        BinaryWord.fill_right
            (try_reduce_binary_word_expression binary_word)
            (Int32.to_int (try_reduce_int_expression expr))
    | Logical_and (l_binary_word, r_binary_word, _) ->
        BinaryWord.log_and
            (try_reduce_binary_word_expression l_binary_word)
            (try_reduce_binary_word_expression r_binary_word)
    | Logical_or (l_binary_word, r_binary_word, _) ->
        BinaryWord.log_or
            (try_reduce_binary_word_expression l_binary_word)
            (try_reduce_binary_word_expression r_binary_word)
    | Logical_xor (l_binary_word, r_binary_word, _) ->
        BinaryWord.log_xor
            (try_reduce_binary_word_expression l_binary_word)
            (try_reduce_binary_word_expression r_binary_word)
    | Logical_not (binary_word, _) ->
        BinaryWord.log_not
            (try_reduce_binary_word_expression binary_word)

    | Binary_word_constant value -> value
    | Binary_word_variable (variable_index, _) ->
        raise (InternalError ("Unable to reduce a non-constant expression."))
    | Binary_word_access (access_type, index_expr, _) ->
        let value = try_reduce_expression_access_value index_expr access_type in
        DiscreteValue.binary_word_value value
    | Binary_word_list_hd list_expr ->
        let list = try_reduce_list_expression list_expr in
        DiscreteValue.binary_word_value (List.hd list)

and try_reduce_array_expression = function
    | Literal_array array ->
        Array.map (fun expr -> try_reduce_global_expression expr) array
    | Array_variable variable_index ->
        raise (InternalError ("Unable to reduce a non-constant expression."))
    | Array_constant values ->
        values
    | Array_access (access_type, index_expr) ->
        let value = try_reduce_expression_access_value index_expr access_type in
        DiscreteValue.array_value value
    | Array_concat (array_expr_0, array_expr_1) ->
        let array_0 = try_reduce_array_expression array_expr_0 in
        let array_1 = try_reduce_array_expression array_expr_1 in
        Array.append array_0 array_1
    | Array_list_hd list_expr ->
        let list = try_reduce_list_expression list_expr in
        DiscreteValue.array_value (List.hd list)

and try_reduce_list_expression = function
    | Literal_list list ->
        List.map (fun expr -> try_reduce_global_expression expr) list
    | List_variable variable_index ->
        raise (InternalError ("Unable to reduce a non-constant expression."))
    | List_constant values ->
        values
    | List_access (access_type, index_expr) ->
        let value = try_reduce_expression_access_value index_expr access_type in
        DiscreteValue.list_value value
    | List_cons (expr, list_expr) ->
        let list = try_reduce_list_expression list_expr in
        let value = try_reduce_global_expression expr in
        value :: list
    | List_list_hd list_expr ->
        let list = try_reduce_list_expression list_expr in
        DiscreteValue.list_value (List.hd list)
    | List_tl list_expr ->
        let list = try_reduce_list_expression list_expr in
        List.tl list
    | List_rev list_expr ->
        let list = try_reduce_list_expression list_expr in
        List.rev list

and try_reduce_array_value_at array_expr index_expr =

    let values = try_reduce_array_expression array_expr in
    let index = try_reduce_int_expression index_expr in
    let int_index = Int32.to_int index in

    if int_index >= Array.length values || int_index < 0 then (
        let str_index = string_of_int int_index in
        let str_values = OCamlUtilities.string_of_array_of_string_with_sep ", " (Array.map (fun value -> DiscreteValue.string_of_value value) values) in
        raise (Out_of_bound ("Array index out of range: `" ^ str_index ^ "` for array " ^ str_values))
    );

    Array.get values int_index

and try_reduce_list_value_at array_expr index_expr =

    let values = try_reduce_list_expression array_expr in
    let index = try_reduce_int_expression index_expr in
    let int_index = Int32.to_int index in

    if int_index >= List.length values || int_index < 0 then (
        let str_index = string_of_int int_index in
        let str_values = OCamlUtilities.string_of_list_of_string_with_sep ", " (List.map (fun value -> DiscreteValue.string_of_value value) values) in
        raise (Out_of_bound ("List index out of range: `" ^ str_index ^ "` for list " ^ str_values))
    );

    List.nth values int_index

and try_reduce_expression_access_value index_expr = function
    | Expression_array_access array_expr ->
        try_reduce_array_value_at array_expr index_expr
    | Expression_list_access list_expr ->
        try_reduce_list_value_at list_expr index_expr



(* TODO benjamin REPLACE BY A REAL EVALUATION OF CONSTANT and not this tricky function using try *)
let is_global_expression_constant expr =
    try (
        let reduced = try_reduce_global_expression expr in
        true
    )
    with _ -> false