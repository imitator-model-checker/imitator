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

(** Exceptions **)

(* Type error exception *)
exception TypeError of string

type variable_name = string
type variable_index = int

(** Get variables types **)

(* Get var type of a variable given it's index *)
val get_type_of_variable : ParsingStructure.useful_parsing_model_information -> variable_index -> DiscreteValue.var_type
(* Get var type of a variable given it's name *)
val get_type_of_variable_by_name : ParsingStructure.useful_parsing_model_information -> variable_name -> DiscreteValue.var_type
(* Get discrete type of a variable given it's index *)
val get_discrete_type_of_variable : ParsingStructure.useful_parsing_model_information -> variable_index -> DiscreteValue.var_type_discrete
(* Get discrete type of a variable given it's name *)
val get_discrete_type_of_variable_by_name : ParsingStructure.useful_parsing_model_information -> variable_name -> DiscreteValue.var_type_discrete

(** Resolve expression type **)

(* Try to resolve the specific type of an expression according to literals and variables used *)
(* Doing type checking of the expression at the same time*)
val resolve_expression_type : ParsingStructure.useful_parsing_model_information -> ParsingStructure.global_expression -> ParsingStructure.global_expression * DiscreteExpressions.expression_type

(** Type checking **)

val check_nonlinear_constraint : ParsingStructure.useful_parsing_model_information -> ParsingStructure.nonlinear_constraint -> ParsingStructure.nonlinear_constraint * DiscreteValue.var_type_discrete

(* Type check a guard / invariant *)
(* return a tuple containing the guard uniformly typed and the resolved type of the expression *)
val check_guard : ParsingStructure.useful_parsing_model_information -> ParsingStructure.convex_predicate -> ParsingStructure.convex_predicate * DiscreteValue.var_type_discrete

(* Type check an update *)
(* return a tuple containing the update uniformly typed and the resolved type of the expression *)
val check_update : ParsingStructure.useful_parsing_model_information -> string -> ParsingStructure.global_expression -> string * ParsingStructure.global_expression

(* Type check a conditional expression *)
(* return a tuple containing the conditional expression uniformly typed and the resolved type of the expression *)
val check_conditional : ParsingStructure.useful_parsing_model_information -> ParsingStructure.parsed_boolean_expression -> ParsingStructure.parsed_boolean_expression  * DiscreteValue.var_type_discrete

(* Check that an expression assigned to a variable is of the same type *)
(* If not, raise a TypeError exception with an error message *)
val check_type_assignment : ParsingStructure.useful_parsing_model_information -> variable_name -> ParsingStructure.global_expression -> unit

(* Check that constant declarations are well typed *)
val check_constant_declarations : (variable_name * ParsingStructure.global_expression * DiscreteValue.discrete_value * DiscreteValue.var_type) list -> (variable_name * DiscreteValue.discrete_value) list