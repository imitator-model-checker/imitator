open DiscreteExpressions



let rec eval_global_expression discrete_valuation = function
    | Arithmetic_expression expr -> eval_discrete_arithmetic_expression discrete_valuation expr
    | Bool_expression expr -> DiscreteValue.Bool_value (is_boolean_expression_satisfied discrete_valuation expr)
    | Binary_word_expression expr -> DiscreteValue.Binary_word_value (eval_discrete_binary_word_expression discrete_valuation expr)

and eval_discrete_arithmetic_expression discrete_valuation = function
    | Rational_arithmetic_expression expr ->
(*        ImitatorUtilities.print_message Verbose_standard "Evaluate a rational expression !!!";*)
        DiscreteValue.Rational_value (eval_rational_expression discrete_valuation expr)
    | Int_arithmetic_expression expr ->
(*        ImitatorUtilities.print_message Verbose_standard "Evaluate a int expression !!!";*)
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
        | DF_expression expr ->
            eval_rational_expression_rec expr
        | DF_rational_of_int expr ->
            (* TODO benjamin WARNING conversion from int32 to int ! *)
(*            ImitatorUtilities.print_message Verbose_standard "Evaluate a int expression";*)
            NumConst.numconst_of_int (Int32.to_int (eval_int_expression discrete_valuation expr))
        | DF_pow (expr, exp) ->
            NumConst.pow (eval_rational_expression_rec expr) (eval_int_expression discrete_valuation exp)
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
            if Int32.equal denominator Int32.zero then(
                raise (Exceptions.Division_by_0 ("Division by 0 found when trying to perform " ^ (Int32.to_string numerator) ^ " / " ^ (Int32.to_string denominator) ^ ""))
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
        | Int_pow (expr, exp) ->
            OCamlUtilities.pow (eval_int_expression_rec expr) (eval_int_expression_rec exp)
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
        (* TODO benjamin here, we unwrap from discrete, ..., bad ! we have to remove discrete value in abstract model *)
        DiscreteValue.bool_value (discrete_valuation variable_index)
    | DB_constant constant ->
        (* TODO benjamin here, we unwrap from discrete, ..., bad ! we have to remove discrete value in abstract model *)
        DiscreteValue.bool_value constant
    (** Discrete arithmetic expression of the form Expr ~ Expr *)
    (* TODO benjamin WARNING here we compare discrete value with operator it's bad *)
    | Expression (l_expr, relop, r_expr) ->
        eval_discrete_relop
            relop
            (eval_discrete_arithmetic_expression discrete_valuation l_expr)
            (eval_discrete_arithmetic_expression discrete_valuation r_expr)
    | Boolean_comparison (l_expr, relop, r_expr) ->
         eval_discrete_boolean_relop
             relop
             (check_discrete_boolean_expression discrete_valuation l_expr)
             (check_discrete_boolean_expression discrete_valuation r_expr)
    | Binary_comparison (l_expr, relop, r_expr) ->
        eval_discrete_binary_relop
            relop
            (eval_discrete_binary_word_expression discrete_valuation l_expr)
            (eval_discrete_binary_word_expression discrete_valuation r_expr)
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
(* TODO benjamin refactor here ! *)
and eval_discrete_relop relop value_1 value_2 : bool =
    match relop with
    | OP_L		-> value_1 <  value_2
    | OP_LEQ	-> value_1 <= value_2
    | OP_EQ		-> value_1 =  value_2
    | OP_NEQ	-> value_1 <> value_2
    | OP_GEQ	-> value_1 >= value_2
    | OP_G		-> value_1 >  value_2
and eval_discrete_boolean_relop relop value_1 value_2 : bool =
    match relop with
    | OP_L		-> value_1 <  value_2
    | OP_LEQ	-> value_1 <= value_2
    | OP_EQ		-> value_1 =  value_2
    | OP_NEQ	-> value_1 <> value_2
    | OP_GEQ	-> value_1 >= value_2
    | OP_G		-> value_1 >  value_2
and eval_discrete_binary_relop relop value_1 value_2 : bool =
    match relop with
    | OP_L		-> value_1 <  value_2
    | OP_LEQ	-> value_1 <= value_2
    | OP_EQ		-> value_1 =  value_2
    | OP_NEQ	-> value_1 <> value_2
    | OP_GEQ	-> value_1 >= value_2
    | OP_G		-> value_1 >  value_2


and eval_discrete_binary_word_expression discrete_valuation = function
    | Logical_shift_left (binary_word, expr) -> BinaryWord.shift_left (eval_discrete_binary_word_expression discrete_valuation binary_word) (Int32.to_int (eval_int_expression discrete_valuation expr))
    | Logical_shift_right (binary_word, expr) -> BinaryWord.shift_right (eval_discrete_binary_word_expression discrete_valuation binary_word) (Int32.to_int (eval_int_expression discrete_valuation expr))
    | Binary_word_constant value -> value
    | Binary_word_variable variable_index ->
        (* TODO benjamin here, we unwrap from discrete, ..., bad ! we have to remove discrete value in abstract model *)
        DiscreteValue.binary_word_value (discrete_valuation variable_index)