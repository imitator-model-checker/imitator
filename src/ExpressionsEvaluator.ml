open DiscreteExpressions
open ExpressionEvaluator

(* Functor for making a new evaluator module *)
module MakeEvaluator (Num : NumberType) (Convert : ConvertType with type num = Num.t) =
struct

    let eval_expression discrete_valuation expr =
        let rec eval_expression_rec = function
            | DAE_plus (expr, term) ->
                Num.add
                    (eval_expression_rec expr)
                    (eval_term term)
            | DAE_minus (expr, term) ->
                Num.sub
                    (eval_expression_rec expr)
                    (eval_term term)
            | DAE_term term ->
                eval_term term

        and eval_term = function
            | DT_mul (term, factor) ->
                Num.mul
                (eval_term term)
                (eval_number_factor factor)
            | DT_div (term, factor) ->
                let numerator	= (eval_term term) in
                let denominator	= (eval_number_factor factor) in

                (* Check for 0-denominator *)
                if Num.equal denominator Num.zero then(
                    raise (Exceptions.Division_by_0 ("Division by 0 found when trying to perform " ^ (Num.to_string numerator) ^ " / " ^ (Num.to_string denominator) ^ ""))
                );

                (* Divide *)
                Num.div
                    numerator
                    denominator

            | DT_factor factor ->
                eval_number_factor factor

        and eval_number_factor = function
            | DF_variable variable_index ->
                Convert.get_value (discrete_valuation variable_index)
            | DF_constant variable_value ->
                Convert.get_value variable_value
            | DF_expression expr ->
                eval_expression_rec expr
            | DF_unary_min factor ->
                Num.neg (eval_number_factor factor)

        in
        eval_expression_rec expr


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
        | DB_variable variable_index ->
            DiscreteValue.bool_value (discrete_valuation variable_index)
        | DB_constant constant ->
            DiscreteValue.bool_value constant
        (** Discrete arithmetic expression of the form Expr ~ Expr *)
        | Expression (discrete_arithmetic_expression_1, relop, discrete_arithmetic_expression_2) ->
            eval_discrete_relop
                relop
                (eval_expression discrete_valuation discrete_arithmetic_expression_1)
                (eval_expression discrete_valuation discrete_arithmetic_expression_2)

        (** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
        | Expression_in (discrete_arithmetic_expression_1, discrete_arithmetic_expression_2, discrete_arithmetic_expression_3) ->
            (* Compute the first one to avoid redundancy *)
            let expr1_evaluated = eval_expression discrete_valuation discrete_arithmetic_expression_1 in
                (eval_expression discrete_valuation discrete_arithmetic_expression_2)
                <=
                expr1_evaluated
                &&
                expr1_evaluated
                <=
                (eval_expression discrete_valuation discrete_arithmetic_expression_3)
        | Boolean_expression boolean_expression ->
            is_boolean_expression_satisfied discrete_valuation boolean_expression

end

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

(* Main module used for evaluation *)
module DiscreteExpressionsEvaluator : DiscreteExpressionsEvaluatorType =
struct

    (************************************************************)
    (** Evaluate discrete boolean expressions with a valuation  *)
    (************************************************************)
    let check_typed_discrete_boolean_expression discrete_valuation (expr, discrete_type) =

        match discrete_type with
        | DiscreteValue.Var_type_discrete_number DiscreteValue.Var_type_discrete_int -> IntEvaluator.check_discrete_boolean_expression discrete_valuation expr
        | DiscreteValue.Var_type_discrete_number DiscreteValue.Var_type_discrete_rational
        | DiscreteValue.Var_type_discrete_bool ->
            RationalEvaluator.check_discrete_boolean_expression discrete_valuation expr

    let is_typed_boolean_expression_satisfied discrete_valuation (expr, discrete_type) =
        ImitatorUtilities.print_message Verbose_standard ("CHECK CONDITIONAL EXPR " ^ (DiscreteValue.string_of_var_type_discrete discrete_type));
        match discrete_type with
        | DiscreteValue.Var_type_discrete_number DiscreteValue.Var_type_discrete_int -> IntEvaluator.is_boolean_expression_satisfied discrete_valuation expr
        | DiscreteValue.Var_type_discrete_number DiscreteValue.Var_type_discrete_rational
        | DiscreteValue.Var_type_discrete_bool ->
            RationalEvaluator.is_boolean_expression_satisfied discrete_valuation expr

    (************************************************************)
    (** Evaluate global expressions with a valuation            *)
    (************************************************************)
    let eval_global_expression discrete_valuation = function
        | Arithmetic_expression (expr, DiscreteValue.Var_type_discrete_rational) ->
(*            ImitatorUtilities.print_message Verbose_standard ("Evaluate rational expression : " ^ (string_of_arithmetic_expression discrete_valuation expr));*)
            DiscreteValue.Rational_value (RationalEvaluator.eval_expression discrete_valuation expr)
        | Arithmetic_expression (expr, DiscreteValue.Var_type_discrete_int) ->
(*            ImitatorUtilities.print_message Verbose_standard ("Evaluate int expression : " ^ (string_of_arithmetic_expression discrete_valuation expr));*)
            DiscreteValue.Int_value (IntEvaluator.eval_expression discrete_valuation expr)
        | Bool_expression (expr, discrete_var_type) ->
            let value = (
                match discrete_var_type with
                (* If boolean expression of bool, there is no number so we can use rational evaluator arbitrary *)
                | DiscreteValue.Var_type_discrete_bool
                | DiscreteValue.Var_type_discrete_number DiscreteValue.Var_type_discrete_rational ->
                    ImitatorUtilities.print_message Verbose_total ("Evaluate bool expression of rat");
                    RationalEvaluator.is_boolean_expression_satisfied discrete_valuation expr
                | DiscreteValue.Var_type_discrete_number DiscreteValue.Var_type_discrete_int ->
                    ImitatorUtilities.print_message Verbose_total ("Evaluate bool expression of int");
                    IntEvaluator.is_boolean_expression_satisfied discrete_valuation expr
            )
            in
            (* TODO benjamin remove message *)
    (*        ImitatorUtilities.print_message Verbose_standard ("Evaluate bool expression : ");*)
            DiscreteValue.Bool_value value


end