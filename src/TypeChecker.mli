(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Contain all functions making type checking or type operations on a parsing structure
 *
 * File contributors : Benjamin L.
 * Created           : 2021/03/17
 * Last modified     : 2021/03/17
 *
 ************************************************************)

open ParsingStructure
open AbstractProperty

type variable_name = string
type variable_index = int

(*val checkus :*)
(*    useful_parsing_model_information -> 'a -> 'b -> 'c*)
(*    -> (useful_parsing_model_information -> 'a -> 'a * DiscreteType.var_type_discrete)*)
(*    -> (useful_parsing_model_information -> 'b -> 'b * DiscreteType.var_type_discrete)*)
(*    -> (useful_parsing_model_information -> DiscreteType.var_type_discrete -> 'a -> 'a)*)
(*    -> (useful_parsing_model_information -> DiscreteType.var_type_discrete -> 'b -> 'b)*)
(*    -> (useful_parsing_model_information -> 'c -> string)*)
(*    -> ('a * 'b) * DiscreteType.var_type_discrete*)

(** Get variables types **)

(* Get var type of a variable given it's index *)
val get_type_of_variable : variable_infos -> variable_index -> DiscreteType.var_type
(* Get var type of a variable given it's name *)
val get_type_of_variable_by_name : variable_infos -> variable_name -> DiscreteType.var_type
(* Get discrete type of a variable given it's index *)
val get_discrete_type_of_variable : variable_infos -> variable_index -> DiscreteType.var_type_discrete
(* Get discrete type of a variable given it's name *)
val get_discrete_type_of_variable_by_name : variable_infos -> variable_name -> DiscreteType.var_type_discrete

(** Resolve expression type **)

(* Try to resolve the specific type of an expression according to literals and variables used *)
(* Doing type checking of the expression at the same time*)
(*val resolve_expression_type : variable_infos -> ParsingStructure.global_expression -> ParsingStructure.global_expression * DiscreteExpressions.expression_type*)

(** Type checking **)

val check_nonlinear_constraint : variable_infos -> ParsingStructure.nonlinear_constraint -> ParsingStructure.nonlinear_constraint * DiscreteType.var_type_discrete

(* Type check a guard / invariant *)
(* return a tuple containing the guard uniformly typed and the resolved type of the expression *)
val check_guard : variable_infos -> ParsingStructure.convex_predicate -> ParsingStructure.convex_predicate * DiscreteType.var_type_discrete

(* Type check an update *)
(* return a tuple containing the update uniformly typed and the resolved type of the expression *)
val check_update : variable_infos -> variable_access -> ParsingStructure.global_expression -> variable_access * ParsingStructure.global_expression

(* Type check a conditional expression *)
(* return a tuple containing the conditional expression uniformly typed and the resolved type of the expression *)
val check_conditional : variable_infos -> ParsingStructure.parsed_boolean_expression -> ParsingStructure.parsed_boolean_expression  * DiscreteType.var_type_discrete

(* Check that an expression assigned to a variable is of the same type *)
(* If not, raise a TypeError exception with an error message *)
val check_type_assignment : variable_infos -> variable_name -> ParsingStructure.global_expression -> ParsingStructure.global_expression * DiscreteType.var_type_discrete

(* Check that constant declarations are well typed *)
val check_constant_expression : (Automaton.variable_name , DiscreteValue.discrete_value) Hashtbl.t -> string * global_expression * DiscreteType.var_type -> global_expression * DiscreteType.var_type_discrete

(* Check that a discrete variable initialization is well typed *)
val check_discrete_init : variable_infos -> variable_name -> ParsingStructure.global_expression -> ParsingStructure.global_expression

(* Type check a state predicate *)
(* return a tuple containing the state predicate uniformly typed and the resolved type of the expression *)
val check_parsed_state_predicate : variable_infos -> parsed_state_predicate -> parsed_state_predicate * DiscreteType.var_type_discrete

val discrete_type_of_expression : variable_infos -> ParsingStructure.global_expression -> DiscreteType.var_type_discrete
val discrete_type_of_parsed_boolean_expression : variable_infos -> ParsingStructure.parsed_boolean_expression -> DiscreteType.var_type_discrete
val discrete_type_of_parsed_discrete_boolean_expression : variable_infos -> ParsingStructure.parsed_discrete_boolean_expression -> DiscreteType.var_type_discrete
val discrete_type_of_parsed_discrete_arithmetic_expression : variable_infos -> ParsingStructure.parsed_discrete_arithmetic_expression -> DiscreteType.var_type_discrete
val discrete_type_of_parsed_discrete_term : variable_infos -> ParsingStructure.parsed_discrete_term -> DiscreteType.var_type_discrete
val discrete_type_of_parsed_discrete_factor : variable_infos -> ParsingStructure.parsed_discrete_factor -> DiscreteType.var_type_discrete