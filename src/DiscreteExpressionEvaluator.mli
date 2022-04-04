open DiscreteExpressions
open Automaton

val eval_boolean_expression : discrete_valuation option -> boolean_expression -> bool
val eval_discrete_boolean_expression : discrete_valuation option -> discrete_boolean_expression -> bool
val eval_global_expression : discrete_valuation option -> global_expression -> DiscreteValue.discrete_value

val try_eval_constant_global_expression : global_expression -> DiscreteValue.discrete_value
val try_eval_constant_rational_term : rational_term -> NumConst.t
val try_eval_constant_rational_factor : rational_factor -> NumConst.t

val eval_constant_global_expression_opt : global_expression -> DiscreteValue.discrete_value option
val eval_constant_rational_term_opt : rational_term -> NumConst.t option
val eval_constant_rational_factor_opt : rational_factor -> NumConst.t option

val is_global_expression_constant : global_expression -> bool

val pack_value : (Automaton.variable_index -> string) -> discrete_valuation option -> DiscreteValue.discrete_value -> DiscreteValue.discrete_value -> variable_update_type -> DiscreteValue.discrete_value