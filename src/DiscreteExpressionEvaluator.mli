open DiscreteExpressions
open Automaton


val eval_boolean_expression : discrete_valuation option -> boolean_expression -> bool
val eval_discrete_boolean_expression : discrete_valuation option -> discrete_boolean_expression -> bool
val eval_global_expression : discrete_valuation option -> global_expression -> DiscreteValue.discrete_value


val try_reduce_global_expression : global_expression -> DiscreteValue.discrete_value
val try_reduce_rational_term : rational_term -> NumConst.t
val try_reduce_rational_factor : rational_factor -> NumConst.t

val is_global_expression_constant : global_expression -> bool

val pack_value : (Automaton.variable_index -> string) -> discrete_valuation option -> DiscreteValue.discrete_value -> DiscreteValue.discrete_value -> discrete_variable_access -> DiscreteValue.discrete_value