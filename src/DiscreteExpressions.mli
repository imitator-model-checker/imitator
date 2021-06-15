(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: arithmetic and Boolean expressions on discrete variables
 *
 * File contributors : Étienne André, Dylan Marinho
 * Created           : 2019/12/10
 * Last modified     : 2021/03/05
 *
 ************************************************************)

(************************************************************)
(************************************************************)
(** Operators *)
(************************************************************)
(************************************************************)

(** Boolean operators *)

type relop = OP_L | OP_LEQ | OP_EQ | OP_NEQ | OP_GEQ | OP_G


(************************************************************)
(************************************************************)
(** Valuation *)
(************************************************************)
(************************************************************)
type discrete_valuation = Automaton.discrete_index -> DiscreteValue.discrete_value




(****************************************************************)
(** Arithmetic expressions for discrete variables *)
(****************************************************************)
type rational_arithmetic_expression =
	| DAE_plus of rational_arithmetic_expression * rational_term
	| DAE_minus of rational_arithmetic_expression * rational_term
	| DAE_term of rational_term

and rational_term =
	| DT_mul of rational_term * rational_factor
	| DT_div of rational_term * rational_factor
	| DT_factor of rational_factor

and rational_factor =
	| DF_variable of Automaton.variable_index
	| DF_constant of NumConst.t
	| DF_expression of rational_arithmetic_expression
	| DF_rational_of_int of int_arithmetic_expression
	| DF_unary_min of rational_factor
	| DF_pow of rational_arithmetic_expression * int_arithmetic_expression

(************************************************************)
(** Int arithmetic expressions for discrete variables *)
(************************************************************)
(************************************************************)
and int_arithmetic_expression =
	| Int_plus of int_arithmetic_expression * int_term
	| Int_minus of int_arithmetic_expression * int_term
	| Int_term of int_term

and int_term =
	| Int_mul of int_term * int_factor
	| Int_div of int_term * int_factor
	| Int_factor of int_factor

and int_factor =
	| Int_variable of Automaton.variable_index
	| Int_constant of Int32.t
	| Int_expression of int_arithmetic_expression
	| Int_unary_min of int_factor
    | Int_pow of int_arithmetic_expression * int_arithmetic_expression


type discrete_arithmetic_expression =
    | Rational_arithmetic_expression of rational_arithmetic_expression
    | Int_arithmetic_expression of int_arithmetic_expression

(************************************************************)
(************************************************************)
(************************************************************)
(** Binary word expressions for discrete variables *)
(************************************************************)
(************************************************************)

(** Binary word expression *)
type binary_word_expression =
    | Logical_shift_left of binary_word_expression * int
    | Logical_shift_right of binary_word_expression * int
    | Binary_word_constant of BinaryWord.t
    | Binary_word_variable of Automaton.variable_index


(************************************************************)
(************************************************************)
(************************************************************)
(** Boolean expressions for discrete variables *)
(************************************************************)
(************************************************************)

(** Boolean expression *)
type boolean_expression =
	| True_bool (** True *)
	| False_bool (** False *)
	| And_bool of boolean_expression * boolean_expression (** Conjunction *)
	| Or_bool of boolean_expression * boolean_expression (** Disjunction *)
	| Discrete_boolean_expression of discrete_boolean_expression

and discrete_boolean_expression =
	(** Discrete arithmetic expression of the form Expr ~ Expr *)
	| Expression of discrete_arithmetic_expression * relop * discrete_arithmetic_expression
    | Boolean_comparison of discrete_boolean_expression * relop * discrete_boolean_expression
    | Binary_comparison of binary_word_expression * relop * binary_word_expression
	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	| Expression_in of discrete_arithmetic_expression * discrete_arithmetic_expression * discrete_arithmetic_expression
	(** Parsed boolean expression of the form Expr ~ Expr, with ~ = { &, | } or not (Expr) *)
	| Boolean_expression of boolean_expression
	(** Parsed boolean expression of the form not(Expr ~ Expr), with ~ = { &, | } *)
	| Not_bool of boolean_expression (** Negation *)
	(** discrete variable in boolean expression *)
	| DB_variable of Automaton.variable_index
	(** discrete constant in boolean expression *)
	| DB_constant of DiscreteValue.discrete_value



(****************************************************************)
(** Global expression *)
(****************************************************************)
type global_expression =
    (* A typed expression *)
    | Arithmetic_expression of discrete_arithmetic_expression
    | Bool_expression of boolean_expression
    | Binary_word_expression of binary_word_expression

(* String *)
val customized_string_of_global_expression : Constants.customized_string -> (Automaton.variable_index -> string) -> global_expression -> string
val string_of_global_expression : (Automaton.variable_index -> string) -> global_expression -> string

val customized_string_of_arithmetic_expression : Constants.customized_boolean_string -> (Automaton.variable_index -> string) -> discrete_arithmetic_expression -> string
val string_of_arithmetic_expression : (Automaton.variable_index -> string) -> discrete_arithmetic_expression -> string

val customized_string_of_boolean_expression : Constants.customized_boolean_string -> (Automaton.variable_index -> string) -> boolean_expression -> string
val string_of_boolean_expression : (Automaton.variable_index -> string) -> boolean_expression -> string

val customized_string_of_discrete_boolean_expression : Constants.customized_boolean_string -> (Automaton.variable_index -> string) -> discrete_boolean_expression -> string
val string_of_discrete_boolean_expression : (Automaton.variable_index -> string) -> discrete_boolean_expression -> string

val customized_string_of_global_expression_for_jani : Constants.customized_string -> (Automaton.variable_index -> string) -> global_expression -> string
val customized_string_of_discrete_boolean_expression_for_jani : Constants.customized_string -> (Automaton.variable_index -> string) -> discrete_boolean_expression -> string
val customized_string_of_arithmetic_expression_for_jani : Constants.customized_string -> (Automaton.variable_index -> string) -> discrete_arithmetic_expression -> string
val string_of_arithmetic_expression_for_jani : (Automaton.variable_index -> string) -> discrete_arithmetic_expression -> string