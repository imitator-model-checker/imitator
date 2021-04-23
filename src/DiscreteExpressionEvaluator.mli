open DiscreteExpressions
open Automaton


val is_boolean_expression_satisfied : discrete_valuation -> boolean_expression -> bool
val check_discrete_boolean_expression : discrete_valuation -> discrete_boolean_expression -> bool
val eval_global_expression : discrete_valuation -> global_expression -> DiscreteValue.discrete_value