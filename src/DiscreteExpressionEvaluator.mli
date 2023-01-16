(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: This module allows to evaluate guards, invariants, user defined functions and update code blocs
 *
 * File contributors : Benjamin L.
 * Created           : 2021/11/20
 *
 ************************************************************)

(* Utils modules *)
open CustomModules

(* Abstract model modules *)
open DiscreteExpressions
open LinearConstraint
open AbstractModel

(* Execution modules *)
open Automaton
open DiscreteState

(* Table of variable name by index *)
type variable_name_table = variable_index -> variable_name
(* Table of function (name, definition) *)
type functions_table = (variable_name, fun_definition) Hashtbl.t
(* Table of clock updates in (no order) *)
type clock_updates_table = (clock_index, pxd_linear_term) Hashtbl.t
(* Queue of ordered clock updates *)
type clock_updates_history = (clock_index * pxd_linear_term) Queue.t

(* Record that contains context (current location, current local variables) for evaluating an expression *)
type eval_context = {
    (* Valuation of global variables at the context (current location) *)
    discrete_valuation : discrete_valuation;
    (* Setter of global variables at the context (current location) *)
    discrete_setter : discrete_setter;
    (* Valuation of local variables at the context (current location) *)
    local_discrete_valuation : local_discrete_valuation;
    (* Setter of local variables at the context (current location) *)
    local_discrete_setter : local_discrete_setter;
    (* All clock updates *)
    updated_clocks : clock_updates_table;
    (* Ordered queue of clock updates *)
    updated_clocks_ordered : clock_updates_history;
}

(* Create an evaluation context with a discrete valuation function and a local variables table *)
val create_eval_context : discrete_access -> eval_context
(* Get clocks that were updated effectively (clock are found in eval context after a code bloc evaluation) *)
val effective_clock_updates : eval_context -> variable_name_table -> clock_updates

(* Evaluate an expression *)
val eval_global_expression : variable_name_table option -> functions_table option -> discrete_access option -> global_expression -> AbstractValue.abstract_value
(* Evaluate a boolean expression *)
val eval_boolean_expression : variable_name_table option -> functions_table option -> discrete_access option -> boolean_expression -> bool
(* Evaluate a discrete boolean expression *)
val eval_discrete_boolean_expression : variable_name_table option -> functions_table option -> discrete_access option -> discrete_boolean_expression -> bool
(* Evaluate sequential code bloc *)
val eval_seq_code_bloc : variable_name_table option -> functions_table option -> discrete_access -> seq_code_bloc_list -> unit
(* Evaluate sequential code bloc given an eval context *)
val eval_seq_code_bloc_with_context : variable_name_table option -> functions_table option -> eval_context -> seq_code_bloc_list -> unit

(* Check if a nonlinear constraint is satisfied *)
val check_nonlinear_constraint : variable_name_table option -> functions_table option -> discrete_access -> nonlinear_constraint -> bool
(* Checks whether a global_location satisfies a state_predicate; takes as argument the accepting condition of the model of the form `automaton_index -> location_index -> acceptance of location_index in automaton_index` *)
val match_state_predicate : variable_name_table option -> functions_table option -> discrete_access -> (automaton_index -> location_index -> bool) -> DiscreteState.global_location -> AbstractProperty.state_predicate-> bool

(* Try to evaluate a constant expression, if expression isn't constant, it raise an error *)
val try_eval_constant_global_expression : functions_table option -> global_expression -> AbstractValue.abstract_value
(* Try to evaluate a constant rational term, if expression isn't constant, it raise an error *)
val try_eval_constant_rational_term : functions_table option -> rational_term -> NumConst.t
(* Try to evaluate a constant rational factor, if expression isn't constant, it raise an error *)
val try_eval_constant_rational_factor : functions_table option -> rational_factor -> NumConst.t

(* Try to evaluate a constant expression, if expression isn't constant, it return None *)
val eval_constant_global_expression_opt : functions_table option -> global_expression -> AbstractValue.abstract_value option
(* Try to evaluate a constant non linear constraint, if expression isn't constant, it return None *)
val eval_nonlinear_constraint_opt : functions_table option -> nonlinear_constraint -> bool option
(* Try to evaluate a constant rational term, if expression isn't constant, it return None *)
val eval_constant_rational_term_opt : functions_table option -> rational_term -> NumConst.t option
(* Try to evaluate a constant rational factor, if expression isn't constant, it return None *)
val eval_constant_rational_factor_opt : functions_table option -> rational_factor -> NumConst.t option

(* Tricky function to know if an expression is constant *)
val is_global_expression_constant : functions_table option -> global_expression -> bool

(* --- Builtin-function evaluations --- *)

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

