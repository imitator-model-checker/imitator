(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: General fonctions for map, filter, traverse parsing structure tree
 *
 * File contributors : Benjamin L.
 * Created           : 2021/03/05
 * Last modified     : 2021/03/05
 *
 ************************************************************)

open ParsingStructure

val string_of_parsed_factor_constructor : parsed_discrete_factor -> string

(* Parsed expression to string *)
val string_of_parsed_global_expression : useful_parsing_model_information -> global_expression -> string
val string_of_parsed_boolean_expression : useful_parsing_model_information -> parsed_boolean_expression -> string
val string_of_parsed_discrete_boolean_expression : useful_parsing_model_information -> parsed_discrete_boolean_expression -> string
val string_of_parsed_arithmetic_expression : useful_parsing_model_information -> parsed_discrete_arithmetic_expression -> string
val string_of_parsed_term : useful_parsing_model_information -> parsed_discrete_term -> string
val string_of_parsed_factor : useful_parsing_model_information -> parsed_discrete_factor -> string
val string_of_parsed_relop : parsed_relop -> string -> string -> string

(* Parsed linear constraint to string *)
val string_of_parsed_linear_constraint : useful_parsing_model_information -> linear_constraint -> string
val string_of_linear_expression : useful_parsing_model_information -> linear_expression -> string
val string_of_linear_term : useful_parsing_model_information -> linear_term -> string
val string_of_parsed_init_state_predicate : useful_parsing_model_information -> parsed_init_state_predicate -> string

val string_of_parsed_nonlinear_constraint : useful_parsing_model_information -> nonlinear_constraint -> string

val try_reduce_parsed_global_expression : (variable_name, DiscreteValue.discrete_value) Hashtbl.t -> global_expression -> DiscreteValue.discrete_value
val try_reduce_parsed_global_expression_with_model : useful_parsing_model_information -> global_expression -> DiscreteValue.discrete_value

val try_reduce_parsed_term : (variable_name, DiscreteValue.discrete_value) Hashtbl.t -> parsed_discrete_term -> DiscreteValue.discrete_value
val try_reduce_parsed_factor : (variable_name, DiscreteValue.discrete_value) Hashtbl.t -> parsed_discrete_factor -> DiscreteValue.discrete_value