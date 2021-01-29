(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: arithmetic and Boolean expressions on discrete variables
 *
 * File contributors : Étienne André
 * Created           : 2019/12/10
 * Last modified     : 2020/01/07
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
type discrete_valuation = Automaton.discrete_index -> Automaton.discrete_value


(************************************************************)
(************************************************************)
(** Arithmetic expressions for discrete variables *)
(************************************************************)
(************************************************************)
type discrete_arithmetic_expression =
	| DAE_plus of discrete_arithmetic_expression * discrete_term
	| DAE_minus of discrete_arithmetic_expression * discrete_term
	| DAE_term of discrete_term

and discrete_term =
	| DT_mul of discrete_term * discrete_factor
	| DT_div of discrete_term * discrete_factor
	| DT_factor of discrete_factor

and discrete_factor =
	| DF_variable of Automaton.variable_index
	| DF_constant of Automaton.variable_value
	| DF_expression of discrete_arithmetic_expression
	| DF_unary_min of discrete_factor

val eval_discrete_relop : relop -> Automaton.discrete_value -> Automaton.discrete_value -> bool

(************************************************************)
(** Evaluate a discrete arithmetic expression with a valuation *)
(************************************************************)

val eval_discrete_arithmetic_expression : discrete_valuation -> discrete_arithmetic_expression -> Automaton.discrete_value


(************************************************************)
(************************************************************)
(************************************************************)
(** Boolean expressions for discrete variables *)
(************************************************************)
(************************************************************)

type discrete_boolean_expression =
	(** Discrete arithmetic expression of the form Expr ~ Expr *)
	| Expression of discrete_arithmetic_expression * relop * discrete_arithmetic_expression
	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	| Expression_in of discrete_arithmetic_expression * discrete_arithmetic_expression * discrete_arithmetic_expression


(************************************************************)
(** Check whether a Boolean expression evaluates to true when valuated with a valuation *)
(************************************************************)

val check_discrete_boolean_expression : discrete_valuation -> discrete_boolean_expression -> bool

(* String *)

val customized_string_of_arithmetic_expression : Constants.customized_string -> (Automaton.variable_index -> string) -> discrete_arithmetic_expression -> string
val string_of_arithmetic_expression : (Automaton.variable_index -> string) -> discrete_arithmetic_expression -> string
