open CustomModules
open DiscreteExpressions
open LinearConstraint
open Automaton
open AbstractModel

type variable_table = (variable_name, AbstractValue.abstract_value) Hashtbl.t
type functions_table = (variable_name, AbstractModel.fun_definition) Hashtbl.t
type variable_name_table = variable_index -> variable_name
type clock_updates_history = (clock_index, pxd_linear_term) Hashtbl.t
type clock_updates_history_2 = (clock_index * pxd_linear_term) Queue.t

type discrete_valuation = Automaton.discrete_index -> AbstractValue.abstract_value
type discrete_setter = Automaton.discrete_index -> AbstractValue.abstract_value -> unit
type discrete_access = discrete_valuation * discrete_setter

(* Record that contain context (current location, current local variables) for evaluating an expression *)
type eval_context = {
    (* Valuation of global variables at the context (current location) *)
    discrete_valuation : discrete_valuation;
    (* Setter of global variables at the context (current location) *)
    discrete_setter : discrete_setter;
    (* Current local variables *)
    local_variables : variable_table list;
    (**)
    updated_clocks : clock_updates_history;
    updated_clocks_ordered : clock_updates_history_2;
}

(* Result returned on delayed update *)
type delayed_update_result =
    | Delayed_update_recorded
    | Delayed_update_already_updated of discrete_index

val create_eval_context : discrete_access -> eval_context
(* Get clocks that were updated effectively (found in eval context) *)
val effective_clock_updates : eval_context -> abstract_model -> clock_updates

val eval_global_expression : variable_name_table option -> functions_table option -> discrete_access option -> global_expression -> AbstractValue.abstract_value
val eval_boolean_expression : variable_name_table option -> functions_table option -> discrete_access option -> boolean_expression -> bool
val eval_discrete_boolean_expression : variable_name_table option -> functions_table option -> discrete_access option -> discrete_boolean_expression -> bool
val eval_seq_code_bloc : variable_name_table option -> functions_table option -> discrete_access -> seq_code_bloc_list -> unit
val eval_seq_code_bloc_with_context : variable_name_table option -> functions_table option -> eval_context -> seq_code_bloc_list -> unit

(* Check if a nonlinear constraint is satisfied *)
val check_nonlinear_constraint : variable_name_table option -> functions_table option -> discrete_access -> nonlinear_constraint -> bool

(** Checks whether a global_location satisfies a state_predicate; takes as argument the accepting condition of the model of the form `automaton_index -> location_index -> acceptance of location_index in automaton_index` *)
val match_state_predicate : variable_name_table option -> functions_table option -> discrete_access -> (automaton_index -> location_index -> bool) -> DiscreteState.global_location -> AbstractProperty.state_predicate-> bool

val try_eval_constant_global_expression : functions_table option -> global_expression -> AbstractValue.abstract_value
val try_eval_constant_rational_term : functions_table option -> rational_term -> NumConst.t
val try_eval_constant_rational_factor : functions_table option -> rational_factor -> NumConst.t

val eval_constant_global_expression_opt : functions_table option -> global_expression -> AbstractValue.abstract_value option
val eval_nonlinear_constraint_opt : functions_table option -> nonlinear_constraint -> bool option
val eval_constant_rational_term_opt : functions_table option -> rational_term -> NumConst.t option
val eval_constant_rational_factor_opt : functions_table option -> rational_factor -> NumConst.t option

val eval_pow : string -> AbstractValue.abstract_value list -> AbstractValue.abstract_value
val eval_mod : string -> AbstractValue.abstract_value list -> AbstractValue.abstract_value
val eval_int_div : string -> AbstractValue.abstract_value list -> AbstractValue.abstract_value
val eval_rational_of_int : string -> AbstractValue.abstract_value list -> AbstractValue.abstract_value
val eval_shift_left : string -> AbstractValue.abstract_value list -> AbstractValue.abstract_value
val eval_shift_right : string -> AbstractValue.abstract_value list -> AbstractValue.abstract_value
val eval_fill_left : string -> AbstractValue.abstract_value list -> AbstractValue.abstract_value
val eval_fill_right : string -> AbstractValue.abstract_value list -> AbstractValue.abstract_value
val eval_log_and : string -> AbstractValue.abstract_value list -> AbstractValue.abstract_value
val eval_log_or : string -> AbstractValue.abstract_value list -> AbstractValue.abstract_value
val eval_log_xor : string -> AbstractValue.abstract_value list -> AbstractValue.abstract_value
val eval_log_not : string -> AbstractValue.abstract_value list -> AbstractValue.abstract_value
val eval_array_append : string -> AbstractValue.abstract_value list -> AbstractValue.abstract_value
val eval_array_mem : string -> AbstractValue.abstract_value list -> AbstractValue.abstract_value
val eval_array_length : string -> AbstractValue.abstract_value list -> AbstractValue.abstract_value
val eval_list_is_empty : string -> AbstractValue.abstract_value list -> AbstractValue.abstract_value
val eval_list_cons : string -> AbstractValue.abstract_value list -> AbstractValue.abstract_value
val eval_list_hd : string -> AbstractValue.abstract_value list -> AbstractValue.abstract_value
val eval_list_tl : string -> AbstractValue.abstract_value list -> AbstractValue.abstract_value
val eval_list_rev : string -> AbstractValue.abstract_value list -> AbstractValue.abstract_value
val eval_list_mem : string -> AbstractValue.abstract_value list -> AbstractValue.abstract_value
val eval_list_length : string -> AbstractValue.abstract_value list -> AbstractValue.abstract_value
val eval_stack_push : string -> AbstractValue.abstract_value list -> AbstractValue.abstract_value
val eval_stack_pop : string -> AbstractValue.abstract_value list -> AbstractValue.abstract_value
val eval_stack_top : string -> AbstractValue.abstract_value list -> AbstractValue.abstract_value
val eval_stack_clear : string -> AbstractValue.abstract_value list -> AbstractValue.abstract_value
val eval_stack_is_empty : string -> AbstractValue.abstract_value list -> AbstractValue.abstract_value
val eval_stack_length : string -> AbstractValue.abstract_value list -> AbstractValue.abstract_value

val is_global_expression_constant : functions_table option -> global_expression -> bool

