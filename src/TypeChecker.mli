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

(* Type error exception *)
exception TypeError of string

(* Check if a value is compatible with given type *)
val check_value_compatible_with_type : DiscreteValue.discrete_value -> DiscreteValue.var_type -> bool

(* Try to resolve the specific type of an expression according to literals and variables used *)
(* Doing type checking of the expression at the same time*)
val resolve_expression_type : ParsingStructure.useful_parsing_model_information -> ParsingStructure.global_expression -> DiscreteValue.var_type

val check_type_of_nonlinear_constraint : ParsingStructure.useful_parsing_model_information -> ParsingStructure.nonlinear_constraint -> bool

(* Get new parsed model with literal rationals implicitly converted to suitable number type *)
(* Example : i * 2 with i : int, convert 2 from rational to int *)
val implicit_convert_literal_of_parsed_global_expression : ParsingStructure.useful_parsing_model_information -> ParsingStructure.global_expression -> ParsingStructure.global_expression
val implicit_convert_literal_of_nonlinear_constraint : ParsingStructure.useful_parsing_model_information -> ParsingStructure.nonlinear_constraint -> ParsingStructure.nonlinear_constraint

(* Check that an expression assigned to a variable is of the same type *)
(* If not, raise a TypeError exception with an error message *)
val check_type_assignment : ParsingStructure.useful_parsing_model_information -> string -> ParsingStructure.global_expression -> unit