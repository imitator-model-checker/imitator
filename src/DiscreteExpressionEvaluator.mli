open DiscreteExpressions
open Automaton

(* Record that contain context (current location, current local variables) for evaluating an expression *)
type eval_context = {
    (* Valuation of global variables at the context (current location) *)
    discrete_valuation : discrete_valuation;
    (* Setter of global variables at the context (current location) *)
    discrete_setter : discrete_setter;
    (* Current local variables *)
    local_variables : variable_table;
}

val eval_global_expression : discrete_access option -> global_expression -> DiscreteValue.discrete_value
val eval_boolean_expression : discrete_access option -> boolean_expression -> bool
val eval_discrete_boolean_expression : discrete_access option -> discrete_boolean_expression -> bool

(** Checks whether a global_location satisfies a state_predicate; takes as argument the accepting condition of the model of the form `automaton_index -> location_index -> acceptance of location_index in automaton_index` *)
val match_state_predicate : discrete_access -> (automaton_index -> location_index -> bool) -> Location.global_location -> AbstractProperty.state_predicate-> bool

val try_eval_constant_global_expression : global_expression -> DiscreteValue.discrete_value
val try_eval_constant_rational_term : rational_term -> NumConst.t
val try_eval_constant_rational_factor : rational_factor -> NumConst.t

val eval_constant_global_expression_opt : global_expression -> DiscreteValue.discrete_value option
val eval_constant_rational_term_opt : rational_term -> NumConst.t option
val eval_constant_rational_factor_opt : rational_factor -> NumConst.t option

val is_global_expression_constant : global_expression -> bool

val pack_value : (* (Automaton.variable_index -> string) -> *) discrete_access -> DiscreteValue.discrete_value -> DiscreteValue.discrete_value -> variable_update_type -> DiscreteValue.discrete_value