open DiscreteExpressions
open Automaton

type functions_table = (variable_name, AbstractModel.fun_definition) Hashtbl.t

(* Record that contain context (current location, current local variables) for evaluating an expression *)
type eval_context = {
    (* Valuation of global variables at the context (current location) *)
    discrete_valuation : discrete_valuation;
    (* Setter of global variables at the context (current location) *)
    discrete_setter : discrete_setter;
    (* Current local variables *)
    local_variables : variable_table;
}

(* Result returned on delayed update *)
type delayed_update_result =
    | Delayed_update_recorded
    | Delayed_update_already_updated of discrete_index

val direct_update : functions_table option -> discrete_access -> update_type * global_expression -> unit
val delayed_update : functions_table option -> discrete_access -> (discrete_index, DiscreteValue.parsed_value) Hashtbl.t -> update_type * global_expression -> delayed_update_result
val eval_global_expression : functions_table option -> discrete_access option -> global_expression -> DiscreteValue.parsed_value
val eval_boolean_expression : functions_table option -> discrete_access option -> boolean_expression -> bool
val eval_discrete_boolean_expression : functions_table option -> discrete_access option -> discrete_boolean_expression -> bool

(* Check if a nonlinear constraint is satisfied *)
val check_nonlinear_constraint : functions_table option -> DiscreteExpressions.discrete_access -> nonlinear_constraint -> bool

(** Checks whether a global_location satisfies a state_predicate; takes as argument the accepting condition of the model of the form `automaton_index -> location_index -> acceptance of location_index in automaton_index` *)
val match_state_predicate : functions_table option -> discrete_access -> (automaton_index -> location_index -> bool) -> Location.global_location -> AbstractProperty.state_predicate-> bool

val try_eval_constant_global_expression : functions_table option -> global_expression -> DiscreteValue.parsed_value
val try_eval_constant_rational_term : functions_table option -> rational_term -> NumConst.t
val try_eval_constant_rational_factor : functions_table option -> rational_factor -> NumConst.t

val eval_constant_global_expression_opt : functions_table option -> global_expression -> DiscreteValue.parsed_value option
val eval_constant_rational_term_opt : functions_table option -> rational_term -> NumConst.t option
val eval_constant_rational_factor_opt : functions_table option -> rational_factor -> NumConst.t option

val eval_pow : string -> DiscreteValue.parsed_value list -> DiscreteValue.parsed_value
val eval_rational_of_int : string -> DiscreteValue.parsed_value list -> DiscreteValue.parsed_value
val eval_shift_left : string -> DiscreteValue.parsed_value list -> DiscreteValue.parsed_value
val eval_shift_right : string -> DiscreteValue.parsed_value list -> DiscreteValue.parsed_value
val eval_fill_left : string -> DiscreteValue.parsed_value list -> DiscreteValue.parsed_value
val eval_fill_right : string -> DiscreteValue.parsed_value list -> DiscreteValue.parsed_value
val eval_log_and : string -> DiscreteValue.parsed_value list -> DiscreteValue.parsed_value
val eval_log_or : string -> DiscreteValue.parsed_value list -> DiscreteValue.parsed_value
val eval_log_xor : string -> DiscreteValue.parsed_value list -> DiscreteValue.parsed_value
val eval_log_not : string -> DiscreteValue.parsed_value list -> DiscreteValue.parsed_value
val eval_array_append : string -> DiscreteValue.parsed_value list -> DiscreteValue.parsed_value
val eval_array_mem : string -> DiscreteValue.parsed_value list -> DiscreteValue.parsed_value
val eval_array_length : string -> DiscreteValue.parsed_value list -> DiscreteValue.parsed_value
val eval_list_is_empty : string -> DiscreteValue.parsed_value list -> DiscreteValue.parsed_value
val eval_list_cons : string -> DiscreteValue.parsed_value list -> DiscreteValue.parsed_value
val eval_list_hd : string -> DiscreteValue.parsed_value list -> DiscreteValue.parsed_value
val eval_list_tl : string -> DiscreteValue.parsed_value list -> DiscreteValue.parsed_value
val eval_list_rev : string -> DiscreteValue.parsed_value list -> DiscreteValue.parsed_value
val eval_list_mem : string -> DiscreteValue.parsed_value list -> DiscreteValue.parsed_value
val eval_list_length : string -> DiscreteValue.parsed_value list -> DiscreteValue.parsed_value
val eval_stack_push : string -> DiscreteValue.parsed_value list -> DiscreteValue.parsed_value
val eval_stack_pop : string -> DiscreteValue.parsed_value list -> DiscreteValue.parsed_value
val eval_stack_top : string -> DiscreteValue.parsed_value list -> DiscreteValue.parsed_value
val eval_stack_clear : string -> DiscreteValue.parsed_value list -> DiscreteValue.parsed_value
val eval_stack_is_empty : string -> DiscreteValue.parsed_value list -> DiscreteValue.parsed_value
val eval_stack_length : string -> DiscreteValue.parsed_value list -> DiscreteValue.parsed_value


val is_global_expression_constant : functions_table option -> global_expression -> bool

(*val pack_value : (* (Automaton.variable_index -> string) -> *) functions_table option -> eval_context option -> DiscreteValue.parsed_value -> DiscreteValue.parsed_value -> update_type -> DiscreteValue.parsed_value*)

