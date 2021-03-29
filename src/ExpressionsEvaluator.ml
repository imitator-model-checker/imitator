open DiscreteExpressions
open ExpressionEvaluator

(* Functor for making a new evaluator module *)
module MakeEvaluator (Num : NumberType) (Convert : ConvertType with type num = Num.t) =
struct

    let eval_expression discrete_valuation expr =
        let rec eval_rational_expression_rec = function
            | DAE_plus (expr, term) ->
                Num.add
                    (eval_rational_expression_rec expr)
                    (eval_rational_term term)
            | DAE_minus (expr, term) ->
                Num.sub
                    (eval_rational_expression_rec expr)
                    (eval_rational_term term)
            | DAE_term term ->
                eval_rational_term term

        and eval_rational_term = function
            | DT_mul (term, factor) ->
                Num.mul
                (eval_rational_term term)
                (eval_rational_factor factor)
            | DT_div (term, factor) ->
                let numerator	= (eval_rational_term term) in
                let denominator	= (eval_rational_factor factor) in

                (* Check for 0-denominator *)
                if Num.equal denominator Num.zero then(
                    raise (Exceptions.Division_by_0 ("Division by 0 found when trying to perform " ^ (Num.to_string numerator) ^ " / " ^ (Num.to_string denominator) ^ ""))
                );

                (* Divide *)
                Num.div
                    numerator
                    denominator

            | DT_factor factor ->
                eval_rational_factor factor

        and eval_rational_factor = function
            | DF_variable variable_index ->
                Convert.get_value (discrete_valuation variable_index)
(*                DiscreteValue.numconst_value (discrete_valuation variable_index)*)
            | DF_constant variable_value ->
                Convert.get_value variable_value
(*                DiscreteValue.numconst_value variable_value*)
            | DF_expression expr ->
                eval_rational_expression_rec expr
            | DF_unary_min factor ->
                Num.neg (eval_rational_factor factor)

        in
        eval_rational_expression_rec expr


    let eval_discrete_relop relop value_1 value_2 : bool =
        match relop with
        | OP_L		-> value_1 <  value_2
        | OP_LEQ	-> value_1 <= value_2
        | OP_EQ		-> value_1 =  value_2
        | OP_NEQ	-> value_1 <> value_2
        | OP_GEQ	-> value_1 >= value_2
        | OP_G		-> value_1 >  value_2

    (** Check if a boolean expression is satisfied *)
    let rec is_boolean_expression_satisfied discrete_valuation = function
        | True_bool -> true
        | False_bool -> false
        | Not_bool b -> not (is_boolean_expression_satisfied discrete_valuation b) (* negation *)
        | And_bool (b1, b2) -> (is_boolean_expression_satisfied discrete_valuation b1) && (is_boolean_expression_satisfied discrete_valuation b2) (* conjunction *)
        | Or_bool (b1, b2) -> (is_boolean_expression_satisfied discrete_valuation b1) || (is_boolean_expression_satisfied discrete_valuation b2) (* disjunction *)
        | Discrete_boolean_expression dbe -> check_discrete_boolean_expression discrete_valuation dbe

    (** Check if a discrete boolean expression is satisfied *)
    and check_discrete_boolean_expression discrete_valuation = function
        (** Discrete arithmetic expression of the form variable *)
        | Discrete_arithmetic_expression expr ->
            let eval_expr = eval_discrete_arithmetic_expression discrete_valuation expr in
            (* No need to check type, because it was been at model conversion (ModelConverter) *)
            DiscreteValue.bool_value eval_expr
        (** Discrete arithmetic expression of the form Expr ~ Expr *)
        | Expression (discrete_arithmetic_expression_1, relop, discrete_arithmetic_expression_2) ->
            eval_discrete_relop
                relop
                (eval_discrete_arithmetic_expression discrete_valuation discrete_arithmetic_expression_1)
                (eval_discrete_arithmetic_expression discrete_valuation discrete_arithmetic_expression_2)

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
        | DB_variable variable_index ->
            (* DB_variable should be a bool value, so we can convert directly to bool with no problem *)
            DiscreteValue.bool_value (discrete_valuation variable_index)
        | DB_constant value ->
            value

end

(* Main module used for evaluation *)
module DiscreteExpressionsEvaluator : DiscreteExpressionsEvaluatorType =
struct

    (* Create a rational value evaluator *)
    module RationalEvaluator = MakeEvaluator(NumConst)(
        struct

            type num = NumConst.t

            let get_value =
                DiscreteValue.numconst_value
        end
    )

    (* Create a int value evaluator *)
    module IntEvaluator = MakeEvaluator(Int32)(
        struct

            type num = Int32.t

            let get_value =
                DiscreteValue.int_value
        end
    )

    (************************************************************)
    (** Evaluate global expressions with a valuation            *)
    (************************************************************)
    let eval_global_expression discrete_valuation = function
        | Rational_expression expr ->
            (* TODO benjamin remove message *)
    (*        ImitatorUtilities.print_message Verbose_standard ("Evaluate rational expression : " ^ (string_of_arithmetic_expression discrete_valuation expr));*)
            DiscreteValue.Rational_value (RationalEvaluator.eval_expression discrete_valuation expr)
        | Int_expression expr ->
            (* TODO benjamin remove message *)
    (*        ImitatorUtilities.print_message Verbose_standard ("Evaluate int expression : " ^ (string_of_arithmetic_expression discrete_valuation expr));*)
            DiscreteValue.Int_value (IntEvaluator.eval_expression discrete_valuation expr)
        | Bool_expression expr ->
            (* TODO benjamin bool expression of number *)
            let value = RationalEvaluator.is_boolean_expression_satisfied discrete_valuation expr in
            (* TODO benjamin remove message *)
    (*        ImitatorUtilities.print_message Verbose_standard ("Evaluate bool expression : ");*)
            DiscreteValue.Bool_value value
end