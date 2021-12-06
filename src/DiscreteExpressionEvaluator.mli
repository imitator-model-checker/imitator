open DiscreteExpressions
open Automaton


val is_boolean_expression_satisfied : discrete_valuation -> boolean_expression -> bool
val check_discrete_boolean_expression : discrete_valuation -> discrete_boolean_expression -> bool
val eval_global_expression : discrete_valuation -> global_expression -> DiscreteValue.discrete_value
val eval_int_expression : discrete_valuation -> int_arithmetic_expression -> Int32.t

val try_reduce_global_expression : global_expression -> DiscreteValue.discrete_value
val try_reduce_discrete_boolean_expression : discrete_boolean_expression -> bool

val is_global_expression_constant : global_expression -> bool

val pack_value : (Automaton.variable_index -> string) -> discrete_valuation -> DiscreteValue.discrete_value -> DiscreteValue.discrete_value -> discrete_variable_access -> DiscreteValue.discrete_value