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
 * Last modified     : 2020/02/01
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
type rational_valuation = Automaton.discrete_index -> Automaton.rational_value


(************************************************************)
(************************************************************)
(** Arithmetic expressions for discrete variables *)
(************************************************************)
(************************************************************)
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
	| DF_constant of Automaton.constant_value
	| DF_expression of rational_arithmetic_expression
	| DF_unary_min of rational_factor

(************************************************************)
(** Evaluate a discrete arithmetic expression with a valuation *)
(************************************************************)

val eval_rational_arithmetic_expression : rational_valuation -> rational_arithmetic_expression -> Automaton.rational_value


(************************************************************)
(************************************************************)
(************************************************************)
(** Boolean expressions for discrete variables *)
(************************************************************)
(************************************************************)

type rational_boolean_expression =
	(** Discrete arithmetic expression of the form Expr ~ Expr *)
	| Expression of rational_arithmetic_expression * relop * rational_arithmetic_expression
	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	| Expression_in of rational_arithmetic_expression * rational_arithmetic_expression * rational_arithmetic_expression


(************************************************************)
(** Check whether a Boolean expression evaluates to true when valuated with a valuation *)
(************************************************************)

val check_rational_boolean_expression : rational_valuation -> rational_boolean_expression -> bool

