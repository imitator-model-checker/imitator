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
(** Global expression *)
(****************************************************************)
type global_expression =
    (* A typed expression *)
    | Arithmetic_expression of discrete_arithmetic_expression
    | Bool_expression of boolean_expression
    | Binary_word_expression of binary_word_expression
    | Array_expression of array_expression
    | List_expression of list_expression

and discrete_arithmetic_expression =
    | Rational_arithmetic_expression of rational_arithmetic_expression
    | Int_arithmetic_expression of int_arithmetic_expression

(****************************************************************)
(** Arithmetic expressions for discrete variables *)
(****************************************************************)
and rational_arithmetic_expression =
	| Rational_plus of rational_arithmetic_expression * rational_term
	| Rational_minus of rational_arithmetic_expression * rational_term
	| Rational_term of rational_term

and rational_term =
	| Rational_mul of rational_term * rational_factor
	| Rational_div of rational_term * rational_factor
	| Rational_factor of rational_factor

and rational_factor =
	| Rational_variable of Automaton.variable_index
	| Rational_constant of NumConst.t
    | Rational_access of expression_access_type * int_arithmetic_expression
	| Rational_expression of rational_arithmetic_expression
	| Rational_unary_min of rational_factor
	| Rational_of_int of int_arithmetic_expression
	| Rational_pow of rational_arithmetic_expression * int_arithmetic_expression
	| Rational_list_hd of list_expression
(*	| Rational_function_call of string * global_expression list*)

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
    (* TODO benjamin here decline array_expression to int_array_expression *)
    | Int_access of expression_access_type * int_arithmetic_expression
    | Int_pow of int_arithmetic_expression * int_arithmetic_expression
    | Int_list_hd of list_expression
    | Array_length of array_expression
    | List_length of list_expression
(*    | Int_function_call of string * global_expression list*)

(************************************************************)
(************************************************************)
(************************************************************)
(** Boolean expressions for discrete variables *)
(************************************************************)
(************************************************************)

(** Boolean expression *)
and boolean_expression =
	| True_bool (** True *)
	| False_bool (** False *)
	| And_bool of boolean_expression * boolean_expression (** Conjunction *)
	| Or_bool of boolean_expression * boolean_expression (** Disjunction *)
	| Discrete_boolean_expression of discrete_boolean_expression

and discrete_boolean_expression =
	(** Discrete arithmetic expression of the form Expr ~ Expr *)
	(* TODO benjamin create another type regrouping all comparisons *)
    (* TODO benjamin look for Expression because even if it was type checked before it's structure can potentially compare different type *)
	| Expression of discrete_arithmetic_expression * relop * discrete_arithmetic_expression
    | Boolean_comparison of discrete_boolean_expression * relop * discrete_boolean_expression
    | Binary_comparison of binary_word_expression * relop * binary_word_expression
    | Array_comparison of array_expression * relop * array_expression
    | List_comparison of list_expression * relop * list_expression
	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	| Expression_in of discrete_arithmetic_expression * discrete_arithmetic_expression * discrete_arithmetic_expression
	(** Parsed boolean expression of the form Expr ~ Expr, with ~ = { &, | } or not (Expr) *)
	| Boolean_expression of boolean_expression
	(** Parsed boolean expression of the form not(Expr ~ Expr), with ~ = { &, | } *)
	| Not_bool of boolean_expression (** Negation *)
	(** discrete variable in boolean expression *)
	| Bool_variable of Automaton.variable_index
	(** discrete constant in boolean expression *)
	| Bool_constant of bool
	(** access to a boolean array **)
	(* TODO benjamin IMPORTANT here decline array_expression to bool_array_expression *)
    | Bool_access of expression_access_type * int_arithmetic_expression
    (* Add here some function on array *)
    | Bool_list_hd of list_expression
    | List_mem of global_expression * list_expression
    | Array_mem of global_expression * array_expression
(*    | Bool_function_call of string * global_expression list*)

(************************************************************)
(************************************************************)
(************************************************************)
(** Binary word expressions for discrete variables *)
(************************************************************)
(************************************************************)

(** Binary word expression *)
and binary_word_expression =
    | Logical_shift_left of binary_word_expression * int_arithmetic_expression * int
    | Logical_shift_right of binary_word_expression * int_arithmetic_expression * int
    | Logical_fill_left of binary_word_expression * int_arithmetic_expression * int
    | Logical_fill_right of binary_word_expression * int_arithmetic_expression * int
    | Logical_and of binary_word_expression * binary_word_expression * int
    | Logical_or of binary_word_expression * binary_word_expression * int
    | Logical_xor of binary_word_expression * binary_word_expression * int
    | Logical_not of binary_word_expression * int
    | Binary_word_constant of BinaryWord.t
    | Binary_word_variable of Automaton.variable_index * int
    | Binary_word_access of expression_access_type * int_arithmetic_expression * int
    | Binary_word_list_hd of list_expression
(*    | Binary_word_function_call of string * global_expression list*)

(** Array expression **)
and array_expression =
    | Literal_array of global_expression array
    | Array_constant of DiscreteValue.discrete_value array
    | Array_variable of Automaton.variable_index
    | Array_access of expression_access_type * int_arithmetic_expression
    (* Add here some functions on array *)
    | Array_concat of array_expression * array_expression
    | Array_list_hd of list_expression
(*    | Array_function_call of string * global_expression list*)

(** List expression **)
and list_expression =
    | Literal_list of global_expression list
    | List_constant of DiscreteValue.discrete_value list
    | List_variable of Automaton.variable_index
    | List_access of expression_access_type * int_arithmetic_expression
    (* Add here some functions on list *)
    | List_cons of global_expression * list_expression
    | List_list_hd of list_expression
    | List_list_tl of list_expression
    | List_rev of list_expression
(*    | List_function_call of string * global_expression list*)

and expression_access_type =
    | Expression_array_access of array_expression
    | Expression_list_access of list_expression

type discrete_variable_access =
    | Discrete_variable_index of Automaton.discrete_index
    | Discrete_variable_access of discrete_variable_access * int_arithmetic_expression

val is_linear_discrete_boolean_expression : discrete_boolean_expression -> bool

(* String *)

(* Constructors strings *)
val label_of_bool_factor : discrete_boolean_expression -> string
val label_of_rational_factor : rational_factor -> string
val label_of_int_factor : int_factor -> string
val label_of_binary_word_expression : binary_word_expression -> string
val label_of_array_expression : array_expression -> string
val label_of_list_expression : list_expression -> string

(* String representation of boolean according to customized string *)
val customized_string_of_bool_value : Constants.customized_boolean_string -> bool -> string
(* String representation of boolean operations according to customized string *)
val customized_string_of_boolean_operations : Constants.customized_boolean_string -> relop -> string

(* Expressions strings *)

val customized_string_of_global_expression : Constants.customized_string -> (Automaton.variable_index -> string) -> global_expression -> string
val string_of_global_expression : (Automaton.variable_index -> string) -> global_expression -> string

val customized_string_of_arithmetic_expression : Constants.customized_string -> (Automaton.variable_index -> string) -> discrete_arithmetic_expression -> string
val string_of_arithmetic_expression : (Automaton.variable_index -> string) -> discrete_arithmetic_expression -> string

val customized_string_of_int_arithmetic_expression : Constants.customized_string -> (Automaton.variable_index -> string) -> int_arithmetic_expression -> string
val string_of_int_arithmetic_expression : (Automaton.variable_index -> string) -> int_arithmetic_expression -> string

val customized_string_of_boolean_expression : Constants.customized_string -> (Automaton.variable_index -> string) -> boolean_expression -> string
val string_of_boolean_expression : (Automaton.variable_index -> string) -> boolean_expression -> string

val customized_string_of_discrete_boolean_expression : Constants.customized_string -> (Automaton.variable_index -> string) -> discrete_boolean_expression -> string
val string_of_discrete_boolean_expression : (Automaton.variable_index -> string) -> discrete_boolean_expression -> string

val customized_string_of_array_expression : Constants.customized_string -> (Automaton.variable_index -> string) -> array_expression -> string
val string_of_array_expression : (Automaton.variable_index -> string) -> array_expression -> string

val string_of_list_expression : (Automaton.variable_index -> string) -> list_expression -> string

val string_of_discrete_variable_access : (Automaton.variable_index -> string) -> discrete_variable_access -> string