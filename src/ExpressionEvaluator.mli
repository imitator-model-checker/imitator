open DiscreteExpressions

(* Module type representing operation on numbers *)
module type NumberType =
sig
    type t

    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t

    val equal : t -> t -> bool
    val zero : t
    val neg : t -> t

    val to_string : t -> string

end

(* Module type for parametrized conversion of a discrete value *)
module type ConvertType =
sig
    type num

    val get_value : DiscreteValue.discrete_value -> num

end

(* Module type that represent a parametrized evaluator module *)
module type ExpressionsEvaluatorType =
sig
    type num

    val eval_expression : discrete_valuation -> discrete_arithmetic_expression -> num
    val is_boolean_expression_satisfied : discrete_valuation -> boolean_expression -> bool

end

(* Functor for making a new evaluator module *)
module MakeEvaluator (Num : NumberType) (Convert : ConvertType with type num = Num.t) : ExpressionsEvaluatorType with type num = Num.t

(* Main module used for evaluation *)
module type DiscreteExpressionsEvaluatorType =
sig
    val eval_global_expression : discrete_valuation -> global_expression -> DiscreteValue.discrete_value
end